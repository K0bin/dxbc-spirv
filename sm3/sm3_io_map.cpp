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
  IoSemanticMap& semanticMap = m_converter.getSemanticMap();

  if (info.getVersion().first >= 3u) {
    // Emit functions that pick a register using
    // a switch statement to allow relative addressing

    // Emit placeholders
    m_inputSwitchFunction = builder.add(ir::Op::Function(ir::Type(ir::ScalarType::eF32, 4)));
    m_outputSwitchFunction = builder.add(ir::Op::Function(ir::Type(ir::ScalarType::eF32, 4)));
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
  auto inputSwitchFunction = emitInputSwitchFunction(builder);
  builder.rewriteDef(m_inputSwitchFunction, inputSwitchFunction);

  auto outputSwitchFunction = emitOutputSwitchFunction(builder);
  builder.rewriteDef(m_outputSwitchFunction, outputSwitchFunction);
}

void IoMap::handleDclIoVar(ir::Builder& builder, const Instruction& op) {
  const auto& dst = op.getDst();
  const auto& dcl = op.getDcl();

  WriteMask componentMask = dst.getWriteMask(m_converter.getShaderInfo());
  if (m_converter.getShaderInfo().getVersion().first < 3) {
    componentMask = WriteMask(ComponentBit::eAll);
  }

  Semantic semantic = { dcl.getSemanticUsage(), dcl.getSemanticIndex() };

  dclIoVar(builder, dst.getRegisterType(), dst.getIndex(), semantic, componentMask);
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
    if (entry.semantic == semantic) {
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
  dxbc_spv_assert(semantic.usage != SemanticUsage::ePosition
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

  bool isBuiltin = opCode == ir::OpCode::eDclInput || opCode == ir::OpCode::eDclOutput;

  bool supportsRelativeAddressing = m_converter.getShaderInfo().getVersion().first == 3
    && (m_converter.getShaderInfo().getType() == ShaderType::eVertex || isInput);

  uint32_t location = m_converter.getSemanticMap().getIoLocation(semantic);

  bool isScalar = registerType == RegisterType::eRasterizerOut
    && (registerIndex == uint32_t(RasterizerOutIndex::eRasterOutFog)
    || registerIndex == uint32_t(RasterizerOutIndex::eRasterOutPointSize));
  isScalar |= registerType == RegisterType::eMiscType && registerIndex == uint32_t(MiscTypeIndex::eMiscTypeFace);

  ir::Type type(
    builtIn == ir::BuiltIn::eIsFrontFace ? ir::ScalarType::eBool : ir::ScalarType::eF32,
    isScalar ? 1u : 4u
  );

  ir::Op declaration = ir::Op(opCode, type)
    .addOperand(m_converter.getEntryPoint());

  if (!isBuiltin) {
      declaration.addOperand(location)
        .addOperand(util::tzcnt(uint8_t(componentMask)));

    if (isInput && shaderType == ShaderType::ePixel) {
      if (semantic.usage == SemanticUsage::eColor) {
        declaration.addOperand(ir::InterpolationModes(ir::InterpolationMode::eCentroid));
      } else {
        declaration.addOperand(ir::InterpolationModes(ir::InterpolationMode::eSample));
      }
    }
  } else {
    declaration.addOperand(builtIn);

    if (isInput && shaderType == ShaderType::ePixel) {
      declaration.addOperand(ir::InterpolationModes(ir::InterpolationMode::eSample));
    }
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

  emitDebugName(
    builder,
    mapping.baseDef,
    registerType,
    registerIndex,
    componentMask,
    mapping.semantic,
    isInput
  );
}


ir::SsaDef IoMap::emitLoad(
        ir::Builder&            builder,
  const Instruction&            op,
  const Operand&                operand,
        WriteMask               componentMask) {
  ir::SsaDef value;
  if (!operand.hasRelativeAddressing()) {
    const IoVarInfo* ioVar = nullptr;
    for (const auto& variable : m_variables) {
      if (variable.registerType == operand.getRegisterType() && variable.registerIndex == operand.getIndex()) {
        ioVar = &variable;
        break;
      }
    }
    if (ioVar == nullptr) {
      m_converter.logOpError(op, "Failed to process I/O load.");
      return ir::SsaDef();
    }

    value = builder.add(ir::Op::InputLoad(ioVar->baseType, ioVar->baseDef, ir::SsaDef()));

    if (ioVar->registerType == RegisterType::eMiscType && ioVar->registerIndex == uint32_t(MiscTypeIndex::eMiscTypeFace)) {
      // The front face can only be loaded using a separate register, even on SM3.
      // So we don't need to handle it in the relative addressing function.
      value = emitFrontFaceFloat(builder, value);
    }
  } else {
    dxbc_spv_assert(operand.getRegisterType() == RegisterType::eInput);
    dxbc_spv_assert(m_converter.getShaderInfo().getVersion().first >= 3);
    ir::Type indexType = ir::Type(ir::ScalarType::eU32);
    auto index = builder.add(ir::Op::Constant(operand.getIndex()));
    ir::SsaDef registerValue = { }; // TODO
    index = builder.add(ir::Op::IAdd(indexType, index, registerValue));
    dxbc_spv_assert(m_inputSwitchFunction);
    auto vec4Type = ir::Type(ir::ScalarType::eF32, 4u);
    value = builder.add(ir::Op::FunctionCall(vec4Type, m_inputSwitchFunction)
      .addOperand(index));
  }

  if (builder.getOp(value).getType().isScalarType()) {
    value = m_converter.broadcastScalar(builder, value, componentMask);
  } else {
    value = m_converter.swizzleVector(builder, value, operand.getSwizzle(m_converter.getShaderInfo()), componentMask);
  }

  return value;
}



bool IoMap::emitStore(
        ir::Builder&            builder,
  const Instruction&            op,
  const Operand&                operand,
        ir::SsaDef              value) {
  auto vec4Type = ir::Type(ir::ScalarType::eF32, 4u);
  WriteMask writeMask = operand.getWriteMask(m_converter.getShaderInfo());

  /* Write each component individually */
  uint32_t componentIndex = 0u;
  std::array<ir::SsaDef, 4u> components;
  for (auto c : writeMask) {
    ir::SsaDef baseScalar = m_converter.extractFromVector(builder, value, componentIndex++);
    components[uint8_t(componentFromBit(c))] = baseScalar;
  }
  ir::SsaDef vec4Value = m_converter.composite(builder, ir::BasicType(ir::ScalarType::eF32), components.data(), Swizzle::identity(), WriteMask(ComponentBit::eAll));

  if (!operand.hasRelativeAddressing()) {
    const IoVarInfo* ioVar = nullptr;
    for (const auto& variable : m_variables) {
      if (variable.registerType == operand.getRegisterType() && variable.registerIndex == operand.getIndex()) {
        ioVar = &variable;
        break;
      }
    }
    if (ioVar == nullptr) {
      m_converter.logOpError(op, "Failed to process I/O load.");
      return false;
    }

    bool isInput = ioVar->registerType == RegisterType::eInput
      || ioVar->registerType == RegisterType::eMiscType
      || ioVar->registerType == RegisterType::ePixelTexCoord;
    dxbc_spv_assert(isInput);
    builder.add(ir::Op::OutputStore(ioVar->baseDef, ir::SsaDef(), vec4Value));
  } else {
    dxbc_spv_assert(operand.getRegisterType() == RegisterType::eInput);
    dxbc_spv_assert(m_converter.getShaderInfo().getVersion().first >= 3);
    ir::Type indexType = ir::Type(ir::ScalarType::eU32);
    auto index = builder.add(ir::Op::Constant(operand.getIndex()));
    ir::SsaDef registerValue = { }; // TODO
    index = builder.add(ir::Op::IAdd(indexType, index, registerValue));
    dxbc_spv_assert(m_outputSwitchFunction);
    builder.add(ir::Op::FunctionCall(vec4Type, m_outputSwitchFunction)
      .addOperand(index)
      .addOperand(vec4Value));
  }
  return true;
}


ir::SsaDef IoMap::emitInputSwitchFunction(ir::Builder& builder) const {
  auto vec4Type = ir::Type(ir::ScalarType::eF32, 4u);
  auto uintType = ir::Type(ir::ScalarType::eU32);

  auto indexParameter = builder.add(ir::Op::DclParam(uintType));
  if (m_converter.m_options.includeDebugNames) {
    builder.add(ir::Op::DebugName(indexParameter, "index"));
  }

  auto function = builder.add(
    ir::Op::Function(vec4Type)
    .addOperand(indexParameter)
  );
  if (m_converter.m_options.includeDebugNames) {
    builder.add(ir::Op::DebugName(function, "loadInputDynamic"));
  }

  auto indexArg = builder.add(ir::Op::ParamLoad(uintType, function, indexParameter));
  auto switchDecl = ir::Op::ScopedSwitch(ir::SsaDef(), indexArg);
  auto switchDef = builder.add(switchDecl);

  for (uint32_t i = 0u; i < MaxIoArraySize; i++) {
    builder.add(ir::Op::ScopedSwitchCase(switchDef, i));

    const IoVarInfo* ioVar = nullptr;
    for (const auto& variable : m_variables) {
      if (variable.registerIndex == i) {
        ioVar = &variable;
        break;
      }
    }
    dxbc_spv_assert(ioVar != nullptr);
    dxbc_spv_assert(ioVar->baseType == vec4Type);

    auto input = builder.add(ir::Op::InputLoad(ioVar->baseType, ioVar->baseDef, ir::SsaDef()));
    builder.add(ir::Op::Return(vec4Type, input));
    builder.add(ir::Op::ScopedSwitchBreak(switchDef));
  }

  auto switchEnd = builder.add(ir::Op::ScopedEndSwitch(switchDef));
  switchDecl.setOperand(0u, switchEnd);

  builder.add(ir::Op::FunctionEnd());
  return function;
}

ir::SsaDef IoMap::emitOutputSwitchFunction(ir::Builder& builder) const {
  auto vec4Type = ir::Type(ir::ScalarType::eF32, 4u);
  auto uintType = ir::Type(ir::ScalarType::eU32);

  auto indexParameter = builder.add(ir::Op::DclParam(uintType));
  if (m_converter.m_options.includeDebugNames) {
    builder.add(ir::Op::DebugName(indexParameter, "index"));
  }

  auto valueParameter = builder.add(ir::Op::DclParam(vec4Type));
  if (m_converter.m_options.includeDebugNames) {
    builder.add(ir::Op::DebugName(valueParameter, "value"));
  }

  auto function = builder.add(
    ir::Op::Function(vec4Type)
    .addOperand(indexParameter)
    .addOperand(valueParameter)
  );
  if (m_converter.m_options.includeDebugNames) {
    builder.add(ir::Op::DebugName(function, "storeOutputDynamic"));
  }

  auto indexArg = builder.add(ir::Op::ParamLoad(uintType, function, indexParameter));
  auto valueArg = builder.add(ir::Op::ParamLoad(vec4Type, function, valueParameter));
  auto switchDecl = ir::Op::ScopedSwitch(ir::SsaDef(), indexArg);
  auto switchDef = builder.add(switchDecl);

  for (uint32_t i = 0u; i < MaxIoArraySize; i++) {
    builder.add(ir::Op::ScopedSwitchCase(switchDef, i));

    const IoVarInfo* ioVar = nullptr;
    for (const auto& variable : m_variables) {
      if (variable.registerIndex == i) {
        ioVar = &variable;
        break;
      }
    }
    dxbc_spv_assert(ioVar != nullptr);
    dxbc_spv_assert(ioVar->baseType == vec4Type);

    builder.add(ir::Op::OutputStore(ioVar->baseDef, ir::SsaDef(), valueArg));
    builder.add(ir::Op::ScopedSwitchBreak(switchDef));
  }

  auto switchEnd = builder.add(ir::Op::ScopedEndSwitch(switchDef));
  switchDecl.setOperand(0u, switchEnd);

  builder.add(ir::Op::FunctionEnd());
  return function;
}


ir::SsaDef IoMap::emitFrontFaceFloat(ir::Builder &builder, ir::SsaDef isFrontFaceDef) const {
  auto frontFaceValue = builder.add(ir::Op::Constant(1.0f));
  auto backFaceValue = builder.add(ir::Op::Constant(-1.0f));
  return builder.add(ir::Op::Select(ir::ScalarType::eF32, isFrontFaceDef, frontFaceValue, backFaceValue));
}



void IoMap::emitDebugName(
  ir::Builder& builder,
  ir::SsaDef def,
  RegisterType registerType,
  uint32_t registerIndex,
  WriteMask writeMask,
  Semantic semantic,
  bool isInput) const {
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

  std::string name = nameStream.str();
  builder.add(ir::Op::DebugName(def, name.c_str()));
}

}
