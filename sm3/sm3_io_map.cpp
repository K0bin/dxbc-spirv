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
    auto opCode = isInput
      ? ir::OpCode::eDclInput
      : ir::OpCode::eDclOutput;

    ir::Type type(ir::ScalarType::eF32, 4u);

    // Normal
    {
      auto declaration = ir::Op(opCode, type)
        .addOperand(m_converter.getEntryPoint())
        .addOperand(0u)
        .addOperand(0u);

      auto& mapping = m_variables.emplace_back();
      mapping.semantic = { SemanticUsage::eNormal, 0u };
      mapping.registerType = RegisterType::eRasterizerOut;
      mapping.registerIndex = 3u;
      mapping.location = semanticMap.getIoLocation(mapping.semantic);
      mapping.componentMask = WriteMask(ComponentBit::eAll);
      mapping.baseType = declaration.getType();
      mapping.baseDef = builder.add(std::move(declaration));

      emitDebugName(
        builder,
        mapping.baseDef,
        mapping.registerType,
        mapping.registerIndex,
        WriteMask(ComponentBit::eAll),
        mapping.semantic,
        isInput
      );
    }

    // Texture coords
    for (uint32_t i = 0u; i < SM2TexCoordCount; i++) {
      auto declaration = ir::Op(opCode, type)
        .addOperand(m_converter.getEntryPoint())
        .addOperand(i)
        .addOperand(0u);

      auto& mapping = m_variables.emplace_back();
      mapping.semantic = { SemanticUsage::eTexCoord, i };
      mapping.registerType = isInput ? RegisterType::ePixelTexCoord : RegisterType::eTexCoordOut;
      mapping.registerIndex = i;
      mapping.location = semanticMap.getIoLocation(mapping.semantic);
      mapping.componentMask = WriteMask(ComponentBit::eAll);
      mapping.baseType = declaration.getType();
      mapping.baseDef = builder.add(std::move(declaration));

      emitDebugName(
        builder,
        mapping.baseDef,
        mapping.registerType,
        mapping.registerIndex,
        WriteMask(ComponentBit::eAll),
        mapping.semantic,
        isInput
      );
    }

    // Colors
    for (uint32_t i = 0u; i < SM2ColorCount; i++) {
      auto declaration = ir::Op(opCode, type)
        .addOperand(m_converter.getEntryPoint())
        .addOperand(i)
        .addOperand(0u);

      auto& mapping = m_variables.emplace_back();
      mapping.semantic = { SemanticUsage::eColor, i };
      mapping.registerType = isInput ? RegisterType::eInput : RegisterType::eColorOut;
      mapping.registerIndex = semanticMap.getIoLocation(mapping.semantic);
      mapping.location = semanticMap.getIoLocation(mapping.semantic);
      mapping.componentMask = WriteMask(ComponentBit::eAll);
      mapping.baseType = declaration.getType();
      mapping.baseDef = builder.add(std::move(declaration));

      emitDebugName(
        builder,
        mapping.baseDef,
        mapping.registerType,
        mapping.registerIndex,
        WriteMask(ComponentBit::eAll),
        mapping.semantic,
        isInput
      );
    }

    // Fog
    {
      auto declaration = ir::Op(opCode, type)
        .addOperand(m_converter.getEntryPoint())
        .addOperand(0u)
        .addOperand(0u);

      auto& mapping = m_variables.emplace_back();
      mapping.semantic = { SemanticUsage::eFog, 0u };
      mapping.registerType = RegisterType::eRasterizerOut;
      mapping.registerIndex = semanticMap.getIoLocation(mapping.semantic);
      mapping.location = semanticMap.getIoLocation(mapping.semantic);
      mapping.componentMask = WriteMask(ComponentBit::eAll);
      mapping.baseType = declaration.getType();
      mapping.baseDef = builder.add(std::move(declaration));

      emitDebugName(
        builder,
        mapping.baseDef,
        mapping.registerType,
        mapping.registerIndex,
        WriteMask(ComponentBit::eAll),
        mapping.semantic,
        isInput
      );
    }
  }
}

void IoMap::finalize(ir::Builder& builder) {
  // Now that all dcl instructions are processed, we can emit the functions containing the switch statements.
  auto inputSwitchFunction = emitInputSwitchFunction(builder);
  builder.rewriteDef(m_inputSwitchFunction, inputSwitchFunction);

  auto outputSwitchFunction = emitOutputSwitchFunction(builder);
  builder.rewriteDef(m_outputSwitchFunction, outputSwitchFunction);
}

bool IoMap::handleDclIoVar(ir::Builder& builder, const Instruction& op) {
  const auto& dst = op.getDst();
  const auto& dcl = op.getDcl();
  bool isInput = dst.getRegisterType() == RegisterType::eInput;

  auto opCode = isInput
    ? ir::OpCode::eDclInput
    : ir::OpCode::eDclOutput;

  WriteMask componentMask = dst.getWriteMask(m_converter.getShaderInfo());
  if (m_converter.getShaderInfo().getVersion().first < 3) {
    componentMask = WriteMask(ComponentBit::eAll);
  }

  Semantic semantic = { dcl.getSemanticUsage(), dcl.getSemanticIndex() };

  bool foundExisting = false;
  for (auto& entry : m_variables) {
    if (entry.semantic == semantic) {
      foundExisting = true;
      break;
    }
  }
  dxbc_spv_assert(!foundExisting);

  ir::Type type(ir::ScalarType::eF32, 4u);

  auto declaration = ir::Op(opCode, type)
    .addOperand(m_converter.getEntryPoint())
    .addOperand(dst.getIndex())
    .addOperand(util::tzcnt(uint8_t(componentMask)));

  auto& mapping = m_variables.emplace_back();
  mapping.semantic = semantic;
  mapping.registerIndex = m_converter.getSemanticMap().getIoLocation(semantic);
  mapping.registerType = isInput ? RegisterType::eInput : RegisterType::eOutput;
  mapping.componentMask = componentMask;
  mapping.baseType = declaration.getType();
  mapping.baseDef = builder.add(std::move(declaration));

  emitDebugName(
    builder,
    mapping.baseDef,
    dst.getRegisterType(),
    dst.getIndex(),
    dst.getWriteMask(m_converter.getShaderInfo()),
    mapping.semantic,
    isInput
  );

  return true;
}


ir::SsaDef IoMap::emitLoad(
        ir::Builder&            builder,
  const Instruction&            op,
  const Operand&                operand,
        WriteMask               componentMask) {
  auto vec4Type = ir::Type(ir::ScalarType::eF32, 4u);
  auto inputType = Converter::makeVectorType(ir::ScalarType::eF32, componentMask);
  ir::SsaDef value;
  if (!operand.hasRelativeAddressing()) {
    const IoVarInfo* ioVar = nullptr;
    for (const auto& variable : m_variables) {
      if (variable.registerType == operand.getRegisterType() && variable.registerIndex == operand.getIndex()) {
        ioVar = &variable;
      }
    }
    if (ioVar == nullptr) {
      m_converter.logOpError(op, "Failed to process I/O load.");
      return ir::SsaDef();
    }
    dxbc_spv_assert(ioVar != nullptr);
    dxbc_spv_assert(ioVar->baseType == vec4Type);
    value = builder.add(ir::Op::InputLoad(ioVar->baseType, ioVar->baseDef, ir::SsaDef()));
  } else {
    dxbc_spv_assert(operand.getRegisterType() == RegisterType::eInput);
    ir::Type indexType = ir::Type(ir::ScalarType::eU32);
    auto index = builder.add(ir::Op::Constant(operand.getIndex()));
    ir::SsaDef registerValue = { }; // TODO
    index = builder.add(ir::Op::IAdd(indexType, index, registerValue));
    dxbc_spv_assert(m_inputSwitchFunction);
    value = builder.add(ir::Op::FunctionCall(vec4Type, m_inputSwitchFunction)
      .addOperand(index));
  }

  return value;
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
