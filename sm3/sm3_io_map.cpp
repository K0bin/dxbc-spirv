#include <algorithm>

#include "sm3_converter.h"
#include "sm3_io_map.h"

#include "../util/util_log.h"

namespace dxbc_spv::sm3 {

IoMap::IoMap(Converter& converter)
: m_converter(converter) {}


IoMap::~IoMap() {

}


void IoMap::initialize(ir::Builder& builder) {
  const ShaderInfo& info = m_converter.getShaderInfo();

  if (info.getVersion().first >= 3u) {
    // Emit functions that pick a register using
    // a switch statement to allow relative addressing

    // Emit placeholders
    m_inputSwitchFunction = builder.add(ir::Op::Function(ir::Type(ir::ScalarType::eF32, 4)));

    if (info.getType() == ShaderType::eVertex) {
      /* Only VS outputs support relative addressing. */
      m_outputSwitchFunction = builder.add(ir::Op::Function(ir::Type()));
    }
  } else {
    // SM 1 & 2 have fixed VS output and PS input registers
    // that do not get explicitly declared.

    bool isInput = info.getType() == ShaderType::ePixel;

    ir::Type type(ir::ScalarType::eF32, 4u);

    // Normal
    if (!isInput) {
      // There is no register for the normal, we emit it in case the VS is used with fixed function.
      // So we get a little tricky and use an imaginary 13th output register.
      // Register type & index are only used for emitting debug naming and we handle that edge case there.
      dclIoVar(
        builder,
        RegisterType::eOutput,
        SM3VSOutputArraySize,
        { SemanticUsage::eNormal, 0u },
        WriteMask(ComponentBit::eAll)
      );
    }

    // Texture coords
    for (uint32_t i = 0u; i < SM2TexCoordCount; i++) {
      dclIoVar(
        builder,
        isInput ? RegisterType::ePixelTexCoord : RegisterType::eTexCoordOut,
        i,
        { SemanticUsage::eTexCoord, i },
        WriteMask(ComponentBit::eAll)
      );
    }

    // Colors
    for (uint32_t i = 0u; i < SM2ColorCount; i++) {
      dclIoVar(
        builder,
        isInput ? RegisterType::eInput : RegisterType::eColorOut,
        i,
        { SemanticUsage::eColor, i },
        WriteMask(ComponentBit::eAll)
      );
    }

    // Fog
    // There is no fog input register in the pixel shader that is accessible
    // to the shader. We do however need to pass the vertex shader calculated
    // fog value across to the fragment shader. Use an imaginary 11th input register.
    dclIoVar(
      builder,
      !isInput ? RegisterType::eRasterizerOut : RegisterType::eInput,
      !isInput ? uint32_t(RasterizerOutIndex::eRasterOutFog) : SM3PSInputArraySize,
      { SemanticUsage::eFog, 0u },
      WriteMask(ComponentBit::eAll)
    );
  }
}


void IoMap::finalize(ir::Builder& builder) {
  // Now that all dcl instructions are processed, we can emit the functions containing the switch statements.
  if (m_inputSwitchFunction) {
    ir::SsaDef cursor = builder.setCursor(m_inputSwitchFunction);
    auto inputSwitchFunction = emitDynamicLoadFunction(builder);
    builder.rewriteDef(m_inputSwitchFunction, inputSwitchFunction);
    m_inputSwitchFunction = inputSwitchFunction;
    builder.setCursor(cursor);
  }

  if (m_outputSwitchFunction) {
    ir::SsaDef cursor = builder.setCursor(m_outputSwitchFunction);
    auto outputSwitchFunction = emitDynamicStoreFunction(builder);
    builder.rewriteDef(m_outputSwitchFunction, outputSwitchFunction);
    m_outputSwitchFunction = outputSwitchFunction;
    builder.setCursor(cursor);
  }

  flushOutputs(builder);
}


bool IoMap::handleDclIoVar(ir::Builder& builder, const Instruction& op) {
  const auto& dst = op.getDst();
  const auto& dcl = op.getDcl();

  WriteMask componentMask = dst.getWriteMask(m_converter.getShaderInfo());
  if (m_converter.getShaderInfo().getVersion().first < 3) {
    componentMask = WriteMask(ComponentBit::eAll);
  }

  Semantic semantic = { dcl.getSemanticUsage(), dcl.getSemanticIndex() };

  dclIoVar(builder, dst.getRegisterType(), dst.getIndex(), semantic, componentMask);
  return true;
}


void IoMap::dclIoVar(
   ir::Builder& builder,
   RegisterType registerType,
   uint32_t     registerIndex,
   Semantic     semantic,
   WriteMask    componentMask) {

  bool isInput = registerType == RegisterType::eInput
    || registerType == RegisterType::eMiscType
    || registerType == RegisterType::ePixelTexCoord;

  bool foundExisting = false;
  for (auto& entry : m_variables) {
    if (entry.semantic == semantic && entry.registerType == registerType) {
      foundExisting = true;
      break;
    }
  }
  dxbc_spv_assert(!foundExisting);

  ShaderType shaderType = m_converter.getShaderInfo().getType();

  ir::OpCode opCode = isInput
    ? ir::OpCode::eDclInput
    : ir::OpCode::eDclOutput;

  ir::BuiltIn builtIn = ir::BuiltIn::ePosition;

  // Position must not be mapped to a regular input. SM3 still has a separate register for that.
  dxbc_spv_assert(shaderType == ShaderType::eVertex
    || semantic.usage != SemanticUsage::ePosition
    || registerType == RegisterType::eMiscType
    || registerType == RegisterType::eOutput);

  if (semantic.usage == SemanticUsage::ePointSize && semantic.index == 0u) {
    opCode = ir::OpCode::eDclOutputBuiltIn;
    builtIn = ir::BuiltIn::ePointSize;
  } else if (semantic.usage == SemanticUsage::ePosition && semantic.index == 0u) {
    opCode = isInput
      ? ir::OpCode::eDclInputBuiltIn
      : ir::OpCode::eDclOutputBuiltIn;
    builtIn = ir::BuiltIn::ePosition;
  } else if (registerType == RegisterType::eDepthOut) {
    builtIn = ir::BuiltIn::eDepth;
  } else if (registerType == RegisterType::eMiscType) {
    opCode = ir::OpCode::eDclInputBuiltIn;
    if (registerIndex == uint32_t(MiscTypeIndex(MiscTypeIndex::eMiscTypeFace))) {
      builtIn = ir::BuiltIn::eIsFrontFace;
    } else if (registerIndex == uint32_t(MiscTypeIndex::eMiscTypePosition)) {
      builtIn = ir::BuiltIn::ePosition;
    } else {
      // Invalid MiscType
      dxbc_spv_assert(false);
    }
  }

  bool isBuiltin = opCode != ir::OpCode::eDclInput && opCode != ir::OpCode::eDclOutput;

  bool supportsRelativeAddressing = m_converter.getShaderInfo().getVersion().first == 3
    && (m_converter.getShaderInfo().getType() == ShaderType::eVertex || isInput);

  uint32_t location = m_converter.getSemanticMap().getIoLocation(semantic);

  bool isScalar = registerType == RegisterType::eRasterizerOut
    && (registerIndex == uint32_t(RasterizerOutIndex::eRasterOutFog)
    || registerIndex == uint32_t(RasterizerOutIndex::eRasterOutPointSize));
  isScalar |= registerType == RegisterType::eMiscType && registerIndex == uint32_t(MiscTypeIndex::eMiscTypeFace);

  uint32_t typeVectorSize = isScalar ? 1u : util::popcnt(uint8_t(componentMask));
  ir::Type type(
    builtIn == ir::BuiltIn::eIsFrontFace ? ir::ScalarType::eBool : ir::ScalarType::eF32,
    typeVectorSize
  );

  ir::Op declaration = ir::Op(opCode, type)
    .addOperand(m_converter.getEntryPoint());

  if (!isBuiltin) {
      declaration.addOperand(location)
        .addOperand(util::tzcnt(uint8_t(componentMask)));

    if (isInput && shaderType == ShaderType::ePixel) {
      if (semantic.usage == SemanticUsage::eColor) {
        declaration.addOperand(ir::InterpolationModes(ir::InterpolationMode::eCentroid));
      }
    }
  } else {
    declaration.addOperand(builtIn);
  }

  auto& mapping = m_variables.emplace_back();
  mapping.semantic = semantic;
  mapping.registerType = registerType;
  mapping.registerIndex = registerIndex;
  mapping.location = location;
  mapping.wasWritten = supportsRelativeAddressing;
  mapping.componentMask = componentMask;
  mapping.baseType = declaration.getType();
  mapping.baseDef = builder.add(std::move(declaration));
  mapping.tempDefs = { };
  if (!isInput) {
    for (uint32_t i = 0u; i < typeVectorSize; i++) {
      mapping.tempDefs[i] = builder.add(ir::Op::DclTmp(ir::ScalarType::eF32, m_converter.getEntryPoint()));
    }
  }

  if (!isInput) {
    if (semantic == Semantic { SemanticUsage::eColor, 0u }) {
      /* The default for color 0 is 1.0, 1.0, 1.0, 1.0 */
      for (uint32_t i = 0u; i < typeVectorSize; i++) {
        builder.add(ir::Op::TmpStore(mapping.tempDefs[i], builder.makeConstant(1.0f)));
      }
    } else if (semantic.usage == SemanticUsage::eColor) {
      /* The default for other color registers is 0.0, 0.0, 0.0, 1.0.
       * TODO: If it's used with a SM3 PS, we need to export 0,0,0,0 as the default for color1.
       *       Implement that using a spec constant. */
      for (uint32_t i = 0u; i < typeVectorSize; i++) {
        builder.add(ir::Op::TmpStore(mapping.tempDefs[i], builder.makeConstant(i == 3u ? 1.0f : 0.0f)));
      }
    } else if (semantic.usage == SemanticUsage::eFog || isScalar) {
      /* The default for the fog register is 0.0 */
      builder.add(ir::Op::TmpStore(mapping.tempDefs[0u],
        builder.makeConstant(0.0f)));
    } else {
      /* The default for other registers is 0.0, 0.0, 0.0, 0.0 */
      for (uint32_t i = 0u; i < typeVectorSize; i++) {
        builder.add(ir::Op::TmpStore(mapping.tempDefs[i], builder.makeConstant(0.0f)));
      }
    }
  }

  emitDebugName(
    builder,
    mapping.baseDef,
    registerType,
    registerIndex,
    componentMask,
    mapping.semantic,
    isInput,
    false
  );

  for (uint32_t i = 0u; i < typeVectorSize && mapping.tempDefs[0u]; i++) {
    emitDebugName(
      builder,
      mapping.tempDefs[i],
      registerType,
      registerIndex,
      util::componentBit(Component(i)),
      mapping.semantic,
      isInput,
      true
    );
  }
}


bool IoMap::determineSemanticForRegister(RegisterType regType, uint32_t regIndex, Semantic* semantic) {
  switch (regType) {
    case RegisterType::eColorOut:
      *semantic = Semantic { SemanticUsage::eColor, regIndex };
      return true;

    case RegisterType::eInput:
      *semantic = Semantic { SemanticUsage::eColor, regIndex };
      return true;

    case RegisterType::eTexCoordOut:
      *semantic = Semantic { SemanticUsage::eTexCoord, regIndex };
      return true;

    case RegisterType::ePixelTexCoord:
      *semantic = Semantic { SemanticUsage::eTexCoord, regIndex };
      return true;

    case RegisterType::eDepthOut:
      *semantic = Semantic { SemanticUsage::eDepth, regIndex };
      return true;

    case RegisterType::eAttributeOut:
      switch (regIndex) {
        case uint32_t(RasterizerOutIndex::eRasterOutFog):
            *semantic = Semantic { SemanticUsage::eFog, 0u };
            return true;
        case uint32_t(RasterizerOutIndex::eRasterOutPointSize):
            *semantic = Semantic { SemanticUsage::ePointSize, 0u };
            return true;
        case uint32_t(RasterizerOutIndex::eRasterOutPosition):
            *semantic = Semantic { SemanticUsage::ePosition, 0u };
            return true;
      }
      break;

    case RegisterType::eMiscType:
      switch (regIndex) {
        case uint32_t(MiscTypeIndex::eMiscTypePosition):
            *semantic = Semantic { SemanticUsage::ePosition, 0u };
            return true;
        case uint32_t(MiscTypeIndex::eMiscTypeFace):
            /* There is no semantic usage for the front face. */
            break;
      }
      break;

    default: break;
  }
  return false;
}


ir::SsaDef IoMap::emitLoad(
        ir::Builder&            builder,
  const Instruction&            op,
  const Operand&                operand,
        WriteMask               componentMask,
        Swizzle                 swizzle,
        ir::ScalarType          type) {
  std::array<ir::SsaDef, 4u> components = { };
  if (!operand.hasRelativeAddressing()) {
    const IoVarInfo* ioVar = findIoVar(m_variables, operand.getRegisterType(), operand.getIndex());
    if (ioVar == nullptr) {
      Semantic semantic;
      bool foundSemantic = determineSemanticForRegister(operand.getRegisterType(), operand.getIndex(), &semantic);
      if (!foundSemantic) {
        m_converter.logOpError(op, "Failed to process I/O load.");
      } else {
        dclIoVar(builder, operand.getRegisterType(), operand.getIndex(), semantic,  WriteMask(ComponentBit::eAll));
        ioVar = &m_variables.back();
      }
    }

    for (auto c : swizzle.getReadMask(componentMask)) {
      auto componentIndex = uint8_t(util::componentFromBit(c));

      if (!ioVar) {
        components[componentIndex] = builder.add(ir::Op::Undef(type));
        continue;
      }

      bool isFrontFaceBuiltin = ioVar->registerType == RegisterType::eMiscType && ioVar->registerIndex == uint32_t(MiscTypeIndex::eMiscTypeFace);
      ir::SsaDef value;
      if (!isFrontFaceBuiltin) {
        auto baseType = ioVar->baseType.getBaseType(0u);
        ir::ScalarType varScalarType = ioVar->baseType.getBaseType(0u).getBaseType();
        ir::SsaDef addressConstant = ir::SsaDef();
        if (!baseType.isScalar()) {
          addressConstant = builder.makeConstant(uint32_t(componentIndex));
        }
        value = builder.add(ir::Op::InputLoad(varScalarType, ioVar->baseDef, addressConstant));
      } else {
        // The front face needs to be transformed from a bool to 1.0/-1.0.
        // It can only be loaded using a separate register, even on SM3.
        // So we don't need to handle it in the relative addressing function.
        dxbc_spv_assert(ioVar->baseType.isScalarType());
        value = builder.add(ir::Op::InputLoad(ioVar->baseType, ioVar->baseDef, ir::SsaDef()));
        value = emitFrontFaceFloat(builder, value);
      }
      components[componentIndex] = convertScalar(builder, type, value);
    }
  } else {
    dxbc_spv_assert(operand.getRegisterType() == RegisterType::eInput);
    dxbc_spv_assert(m_converter.getShaderInfo().getVersion().first >= 3);
    auto index = builder.makeConstant(int32_t(operand.getIndex()));
    auto relAddr = m_converter.loadAddress(builder,
      operand.getRelativeAddressingRegisterType(),
      operand.getRelativeAddressingSwizzle());
    index = builder.add(ir::Op::IAdd(ir::ScalarType::eI32, index, relAddr));
    index = builder.add(ir::Op::Cast(ir::ScalarType::eU32, index));
    dxbc_spv_assert(m_inputSwitchFunction);
    auto vec4Value = builder.add(ir::Op::FunctionCall(ir::Type(ir::ScalarType::eF32, 4u), m_inputSwitchFunction)
        .addOperand(index));
    for (auto c : swizzle.getReadMask(componentMask)) {
      auto componentIndex = uint8_t(util::componentFromBit(c));
      components[componentIndex] = convertScalar(
        builder,
        type,
        builder.add(ir::Op::CompositeExtract(type, vec4Value, builder.makeConstant(componentIndex)))
      );
    }
  }

  ir::SsaDef value = m_converter.composite(builder, ir::BasicType(type, util::popcnt(uint8_t(componentMask))), components.data(), swizzle, componentMask);

  return value;
}


ir::SsaDef IoMap::emitTexCoordLoad(
       ir::Builder&            builder,
 const Instruction&            op,
       uint32_t                regIdx,
       WriteMask               componentMask,
       Swizzle                 swizzle,
       ir::ScalarType          type) {
  std::array<ir::SsaDef, 4u> components = { };
  const IoVarInfo* ioVar = findIoVar(m_variables, RegisterType::ePixelTexCoord, regIdx);
  if (ioVar == nullptr) {
    Semantic semantic;
    bool foundSemantic = determineSemanticForRegister(RegisterType::ePixelTexCoord, regIdx, &semantic);
    if (!foundSemantic) {
      m_converter.logOpError(op, "Failed to process I/O load.");
    } else {
      dclIoVar(builder, RegisterType::ePixelTexCoord, regIdx, semantic,  WriteMask(ComponentBit::eAll));
      ioVar = &m_variables.back();
    }
  }

  for (auto c : swizzle.getReadMask(componentMask)) {
    auto componentIndex = uint8_t(util::componentFromBit(c));

    if (!ioVar) {
      components[componentIndex] = builder.add(ir::Op::Undef(type));
      continue;
    }

    ir::SsaDef addressConstant = builder.makeConstant(componentIndex);
    auto varScalarType = ioVar->baseType.getBaseType(0u).getBaseType();
    auto value = builder.add(ir::Op::InputLoad(varScalarType, ioVar->baseDef, addressConstant));
    components[componentIndex] = convertScalar(builder, type, value);
  }

  return m_converter.composite(builder, ir::BasicType(type, util::popcnt(uint8_t(componentMask))), components.data(), swizzle, componentMask);
}


bool IoMap::emitStore(
        ir::Builder&            builder,
  const Instruction&            op,
  const Operand&                operand,
        WriteMask               writeMask,
        ir::SsaDef              predicateVec,
        ir::SsaDef              value) {
  auto srcType = builder.getOp(value).getType();
  auto srcBaseType = srcType.getBaseType(0);
  auto srcScalarType = srcBaseType.getBaseType();

  if (!operand.hasRelativeAddressing()) {
    const IoVarInfo* ioVar = findIoVar(m_variables, operand.getRegisterType(), operand.getIndex());
    if (ioVar == nullptr) {
      Semantic semantic;
      bool foundSemantic = determineSemanticForRegister(operand.getRegisterType(), operand.getIndex(), &semantic);
      if (!foundSemantic) {
        m_converter.logOpError(op, "Failed to process I/O store.");
        return false;
      }
      dclIoVar(builder, operand.getRegisterType(), operand.getIndex(), semantic,  WriteMask(ComponentBit::eAll));
      ioVar = &m_variables.back();
    }

    bool isOutput = ioVar->registerType == RegisterType::eOutput
      || ioVar->registerType == RegisterType::eAttributeOut
      || ioVar->registerType == RegisterType::eColorOut
      || ioVar->registerType == RegisterType::eDepthOut;
    dxbc_spv_assert(isOutput);
    auto ioVarBaseType = ioVar->baseType.getBaseType(0u);
    ir::ScalarType ioVarScalarType = ioVarBaseType.getBaseType();

    uint32_t componentIndex = 0u;
    for (auto c : writeMask) {
      ir::SsaDef valueScalar = value;
      if (srcType.isVectorType()) {
        auto componentIndexConst = builder.makeConstant(componentIndex);
        valueScalar = builder.add(ir::Op::CompositeExtract(srcScalarType, value, componentIndexConst));
      }
      valueScalar = convertScalar(builder, ioVarScalarType, valueScalar);
      if (ioVar->semantic.usage == SemanticUsage::eColor && ioVar->semantic.index < 2 && m_converter.getShaderInfo().getVersion().first < 3) {
        // The color register cannot be dynamically indexed, so there's no need to do this in the dynamic store function.
        valueScalar = builder.add(ir::Op::FClamp(ioVarScalarType, valueScalar,
          builder.makeConstant(0.0f), builder.makeConstant(1.0f)));
      }

      ir::SsaDef predicateIf = ir::SsaDef();
      if (predicateVec) {
        /* Check if the matching component of the predicate register vector is true first. */
        auto condComponent = m_converter.extractFromVector(builder, predicateVec, componentIndex);
        predicateIf = builder.add(ir::Op::ScopedIf(ir::SsaDef(), condComponent));
      }

      builder.add(ir::Op::TmpStore(ioVar->tempDefs[uint32_t(util::componentFromBit(c))], valueScalar));

      if (predicateIf) {
        auto predicateIfEnd = builder.add(ir::Op::ScopedEndIf(predicateIf));
        builder.rewriteOp(predicateIf, ir::Op(builder.getOp(predicateIf)).setOperand(0u, predicateIfEnd));
      }

      componentIndex++;
    }
  } else {
    dxbc_spv_assert(operand.getRegisterType() == RegisterType::eOutput);
    dxbc_spv_assert(m_converter.getShaderInfo().getVersion().first >= 3);
    auto index = builder.makeConstant(operand.getIndex());
    auto relAddr = m_converter.loadAddress(builder,
      operand.getRelativeAddressingRegisterType(),
      operand.getRelativeAddressingSwizzle());
    index = builder.add(ir::Op::IAdd(ir::ScalarType::eU32, index, relAddr));
    dxbc_spv_assert(m_outputSwitchFunction);
    uint32_t componentIndex = 0u;
    for (auto c : writeMask) {
      auto dstComponentIndexConst = builder.makeConstant(uint32_t(util::componentFromBit(c)));
      auto componentIndexConst = builder.makeConstant(componentIndex);
      ir::SsaDef valueScalar = value;
      if (srcType.isVectorType()) {
        valueScalar = builder.add(ir::Op::CompositeExtract(srcScalarType, value, componentIndexConst));
      }
      valueScalar = convertScalar(builder, ir::ScalarType::eF32, valueScalar);

      ir::SsaDef predicateIf = ir::SsaDef();
      if (predicateVec) {
        /* Check if the matching component of the predicate register vector is true first. */
        auto condComponent = m_converter.extractFromVector(builder, predicateVec, componentIndex);
        predicateIf = builder.add(ir::Op::ScopedIf(ir::SsaDef(), condComponent));
      }
      builder.add(ir::Op::FunctionCall(ir::Type(), m_outputSwitchFunction)
        .addOperand(index)
        .addOperand(dstComponentIndexConst)
        .addOperand(valueScalar));
      if (predicateIf) {
        auto predicateIfEnd = builder.add(ir::Op::ScopedEndIf(predicateIf));
        builder.rewriteOp(predicateIf, ir::Op(builder.getOp(predicateIf)).setOperand(0u, predicateIfEnd));
      }

      componentIndex++;
    }
  }
  return true;
}


bool IoMap::emitDepthStore(ir::Builder &builder, const Instruction &op, ir::SsaDef value) {
  const IoVarInfo* ioVar = findIoVar(m_variables, RegisterType::eDepthOut, 0u);
  if (ioVar == nullptr) {
    Semantic semantic;
    bool foundSemantic = determineSemanticForRegister(RegisterType::eDepthOut, 0u, &semantic);
    if (!foundSemantic) {
      m_converter.logOpError(op, "Failed to process I/O store.");
      return false;
    }
    dclIoVar(builder, RegisterType::eDepthOut, 0u, semantic,  WriteMask(ComponentBit::eAll));
    ioVar = &m_variables.back();
  }
  ir::Type scalarType = ioVar->baseType.isVectorType() ? ioVar->baseType.getBaseType(0u) : ioVar->baseType;
  builder.add(ir::Op::TmpStore(ioVar->tempDefs[0u], value));
  return true;
}


ir::SsaDef IoMap::emitDynamicLoadFunction(ir::Builder& builder) const {
  auto indexParameter = builder.add(ir::Op::DclParam(ir::ScalarType::eU32));
  if (m_converter.m_options.includeDebugNames) {
    builder.add(ir::Op::DebugName(indexParameter, "reg"));
  }

  auto function = builder.add(
    ir::Op::Function(ir::Type(ir::ScalarType::eF32, 4u))
    .addOperand(indexParameter)
  );
  if (m_converter.m_options.includeDebugNames) {
    builder.add(ir::Op::DebugName(function, "loadInputDynamic"));
  }

  auto indexArg = builder.add(ir::Op::ParamLoad(ir::ScalarType::eU32, function, indexParameter));
  auto switchDef = builder.add(ir::Op::ScopedSwitch(ir::SsaDef(), indexArg));

  for (uint32_t i = 0u; i < MaxIoArraySize; i++) {
    const IoVarInfo* ioVar = nullptr;
    for (const auto& variable : m_variables) {
      if (variable.registerType == RegisterType::eInput && variable.registerIndex == i) {
        ioVar = &variable;
        break;
      }
    }
    if (ioVar == nullptr)
      continue;

    dxbc_spv_assert(ioVar != nullptr);
    builder.add(ir::Op::ScopedSwitchCase(switchDef, i));

    auto input = builder.add(ir::Op::InputLoad(ioVar->baseType, ioVar->baseDef, ir::SsaDef()));
    auto baseType = ioVar->baseType.getBaseType(0u);
    ir::SsaDef vec4 = input;
    if (baseType.getVectorSize() != 4u) {
      std::array<ir::SsaDef, 4u> components;
      for (uint32_t j = 0u; j < 4u; j++) {
        if ((baseType.isScalar() && j == 0) || j < baseType.getVectorSize()) {
          components[j] = builder.add(ir::Op::CompositeExtract(ir::ScalarType::eF32, input, builder.makeConstant(i)));;
        } else {
          components[j] = builder.makeConstant(0.0f);
        }
      }
      vec4 = m_converter.buildVector(builder, ir::ScalarType::eF32, components.size(), components.data());
    }
    builder.add(ir::Op::Return(ir::Type(ir::ScalarType::eF32, 4u), vec4));
    builder.add(ir::Op::ScopedSwitchBreak(switchDef));
  }

  auto switchEnd = builder.add(ir::Op::ScopedEndSwitch(switchDef));
  builder.rewriteOp(switchDef, ir::Op::ScopedSwitch(switchEnd, indexArg));

  builder.add(ir::Op::Return(ir::Type(ir::ScalarType::eF32, 4u), builder.makeConstant(0.0f, 0.0f, 0.0f, 0.0f)));

  builder.add(ir::Op::FunctionEnd());
  return function;
}


ir::SsaDef IoMap::emitDynamicStoreFunction(ir::Builder& builder) const {
  auto indexParameter = builder.add(ir::Op::DclParam(ir::ScalarType::eU32));
  if (m_converter.m_options.includeDebugNames) {
    builder.add(ir::Op::DebugName(indexParameter, "reg"));
  }

  auto componentParameter = builder.add(ir::Op::DclParam(ir::ScalarType::eU32));
  if (m_converter.m_options.includeDebugNames) {
    builder.add(ir::Op::DebugName(indexParameter, "c"));
  }

  auto valueParameter = builder.add(ir::Op::DclParam(ir::ScalarType::eF32));
  if (m_converter.m_options.includeDebugNames) {
    builder.add(ir::Op::DebugName(valueParameter, "value"));
  }

  auto function = builder.add(
    ir::Op::Function(ir::Type())
    .addOperand(indexParameter)
    .addOperand(componentParameter)
    .addOperand(valueParameter)
  );
  if (m_converter.m_options.includeDebugNames) {
    builder.add(ir::Op::DebugName(function, "storeOutputDynamic"));
  }

  auto indexArg = builder.add(ir::Op::ParamLoad(ir::ScalarType::eU32, function, indexParameter));
  auto componentArg = builder.add(ir::Op::ParamLoad(ir::ScalarType::eU32, function, componentParameter));
  auto valueArg = builder.add(ir::Op::ParamLoad(ir::ScalarType::eF32, function, valueParameter));
  auto switchDef = builder.add(ir::Op::ScopedSwitch(ir::SsaDef(), indexArg));

  for (uint32_t i = 0u; i < MaxIoArraySize; i++) {
    const IoVarInfo* ioVar = nullptr;
    for (const auto& variable : m_variables) {
      if (variable.registerType == RegisterType::eOutput && variable.registerIndex == i) {
        ioVar = &variable;
        break;
      }
    }
    if (ioVar == nullptr)
      continue;

    dxbc_spv_assert(ioVar != nullptr);
    builder.add(ir::Op::ScopedSwitchCase(switchDef, i));

    auto baseType = ioVar->baseType.getBaseType(0u);


    if (!baseType.isScalar()) {
      auto componentSwitchDef = builder.add(ir::Op::ScopedSwitch(ir::SsaDef(), componentArg));
      for (uint32_t j = 0u; j < baseType.getVectorSize(); j++) {
        builder.add(ir::Op::ScopedSwitchCase(componentSwitchDef, j));
        builder.add(ir::Op::TmpStore(ioVar->tempDefs[j], valueArg));
        builder.add(ir::Op::ScopedSwitchBreak(componentSwitchDef));
      }
      auto componentSwitchEnd = builder.add(ir::Op::ScopedEndSwitch(componentSwitchDef));
      builder.rewriteOp(componentSwitchDef, ir::Op::ScopedSwitch(componentSwitchEnd, componentArg));
    } else {
      builder.add(ir::Op::TmpStore(ioVar->tempDefs[0u], valueArg));
    }

    builder.add(ir::Op::ScopedSwitchBreak(switchDef));
  }

  auto switchEnd = builder.add(ir::Op::ScopedEndSwitch(switchDef));
  builder.rewriteOp(switchDef, ir::Op::ScopedSwitch(switchEnd, indexArg));

  builder.add(ir::Op::FunctionEnd());
  return function;
}


void IoMap::flushOutputs(ir::Builder& builder) const {
  for (const auto& variable : m_variables) {
    if (!variable.tempDefs[0u])
      continue;

    auto baseType = variable.baseType.getBaseType(0u);
    for (uint32_t i = 0u; i < baseType.getVectorSize(); i++) {
      auto temp = builder.add(ir::Op::TmpLoad(variable.baseType, variable.tempDefs[i]));
      builder.add(ir::Op::OutputStore(variable.baseDef, builder.makeConstant(i), temp));
    }
  }
}


ir::SsaDef IoMap::emitFrontFaceFloat(ir::Builder &builder, ir::SsaDef isFrontFaceDef) const {
  auto frontFaceValue = builder.makeConstant(1.0f);
  auto backFaceValue = builder.makeConstant(-1.0f);
  return builder.add(ir::Op::Select(ir::ScalarType::eF32, isFrontFaceDef, frontFaceValue, backFaceValue));
}


IoVarInfo* IoMap::findIoVar(IoVarList& list, RegisterType regType, uint32_t regIndex) {
  for (auto& e : list) {
    if (e.registerType == regType && e.registerIndex == regIndex) {
      return &e;
      break;
    }
  }

  return nullptr;
}


ir::SsaDef IoMap::convertScalar(ir::Builder& builder, ir::ScalarType dstType, ir::SsaDef value) {
  const auto& srcType = builder.getOp(value).getType();
  dxbc_spv_assert(srcType.isScalarType());

  auto scalarType = srcType.getBaseType(0u).getBaseType();

  if (scalarType == dstType)
    return value;

  return builder.add(ir::Op::ConsumeAs(dstType, value));
}


void IoMap::emitDebugName(
  ir::Builder& builder,
  ir::SsaDef def,
  RegisterType registerType,
  uint32_t registerIndex,
  WriteMask writeMask,
  Semantic semantic,
  bool isInput,
  bool isTemp) const {
  if (!m_converter.m_options.includeDebugNames)
    return;

  std::stringstream nameStream;
  if (semantic.usage != SemanticUsage::eNormal
    || (isInput && registerType == RegisterType::eRasterizerOut)
    || (!isInput && registerType == RegisterType::eMiscType)) {
    // There is no register type for normals, it's only emitted for FF emulation.
    // The other exceptions either only have input only or output only registers.
    nameStream << m_converter.makeRegisterDebugName(registerType, registerIndex, writeMask);
    nameStream << "_";
  }

  if (semantic.usage == SemanticUsage::eColor) {
    if (semantic.index == 0) {
      nameStream << "color";
    } else {
      nameStream << "specular" << std::to_string(semantic.index - 1u);
    }
  } else {
    nameStream << semantic.usage;
    if (semantic.usage == SemanticUsage::ePosition
      || semantic.usage == SemanticUsage::eNormal
      || semantic.usage == SemanticUsage::eTexCoord) {
      nameStream << semantic.index;
    }
  }

  if (isTemp)
    nameStream << "_temp";

  std::string name = nameStream.str();
  builder.add(ir::Op::DebugName(def, name.c_str()));
}

}
