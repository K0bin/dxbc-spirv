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
    // Emit scratch arrays that we copy input/output registers in and out
    // to allow relative addressing

    // Inputs
    uint32_t inputArraySize = info.getType() == ShaderType::eVertex ? 16u : 10u;

    auto inputArrayType = ir::Type(ir::ScalarType::eF32, 4u)
      .addArrayDimension(inputArraySize);

    auto inputDeclaration = ir::Op(ir::OpCode::eDclScratch, inputArrayType)
      .addOperand(m_converter.getEntryPoint());

    auto inputVar = m_inputRegisterArray.emplace();
    inputVar.baseType = inputArrayType;
    inputVar.baseDef = builder.add(std::move(inputDeclaration));

    if (m_converter.m_options.includeDebugNames) {
      auto name = m_converter.makeRegisterDebugName(RegisterType::eInput, 0, WriteMask(ComponentBit::eAll));
      builder.add(ir::Op::DebugName(inputVar.baseDef, name.c_str()));
    }

    // Outputs
    if (info.getType() == ShaderType::eVertex) {
      // PS Outputs don't support relative addressing
      auto outputArrayType = ir::Type(ir::ScalarType::eF32, 4u)
        .addArrayDimension(12u);

      auto outputDeclaration = ir::Op(ir::OpCode::eDclScratch, inputArrayType)
        .addOperand(m_converter.getEntryPoint());

      auto outputVar = m_outputRegisterArray.emplace();
      outputVar.baseType = inputArrayType;
      outputVar.baseDef = builder.add(std::move(outputDeclaration));

      if (m_converter.m_options.includeDebugNames) {
        auto name = m_converter.makeRegisterDebugName(RegisterType::eOutput, 0, WriteMask(ComponentBit::eAll));
        builder.add(ir::Op::DebugName(outputVar.baseDef, name.c_str()));
      }
    }
  } else {
    // SM 1 & 2 have fixed VS output and PS input registers
    // that do not get explicitly declared.

    bool isInput = info.getType() == ShaderType::ePixel;
    auto opCode = isInput
      ? ir::OpCode::eDclInput
      : ir::OpCode::eDclOutput;

    ir::Type type(ir::ScalarType::eF32, 4u);

    // Texture coords
    for (uint32_t i = 0u; i < SM2TexCoordCount; i++) {
      auto declaration = ir::Op(opCode, type)
        .addOperand(m_converter.getEntryPoint())
        .addOperand(i)
        .addOperand(0u);

      auto& mapping = m_variables.emplace_back();
      mapping.semanticUsage = SemanticUsage::eTexCoord;
      mapping.semanticIndex = i;
      mapping.componentMask = WriteMask(ComponentBit::eAll);
      mapping.baseType = declaration.getType();
      mapping.baseDef = builder.add(std::move(declaration));

      emitDebugName(
        builder,
        mapping.baseDef,
        isInput ? RegisterType::ePixelTexCoord : RegisterType::eTexCoordOut,
        i,
        WriteMask(ComponentBit::eAll),
        SemanticUsage::eTexCoord,
        i
      );
    }

    // Colors
    for (uint32_t i = 0u; i < SM2ColorCount; i++) {
      auto declaration = ir::Op(opCode, type)
        .addOperand(m_converter.getEntryPoint())
        .addOperand(i)
        .addOperand(0u);

      auto& mapping = m_variables.emplace_back();
      mapping.semanticUsage = SemanticUsage::eColor;
      mapping.semanticIndex = i;
      mapping.componentMask = WriteMask(ComponentBit::eAll);
      mapping.baseType = declaration.getType();
      mapping.baseDef = builder.add(std::move(declaration));

      emitDebugName(
        builder,
        mapping.baseDef,
        isInput ? RegisterType::eInput : RegisterType::eColorOut,
        i,
        WriteMask(ComponentBit::eAll),
        SemanticUsage::eColor,
        i
      );
    }
  }
}

bool IoMap::handleDclIoVar(ir::Builder& builder, const Instruction& op) {
  const auto& dst = op.getDst();
  const auto& dcl = op.getDcl();
  bool isInput = dst.getRegisterType() == RegisterType::eInput;

  auto opCode = isInput
    ? ir::OpCode::eDclInput
    : ir::OpCode::eDclOutput;

  WriteMask componentMask = dst.getWriteMask();
  if (m_converter.getShaderInfo().getVersion().first < 3) {
    componentMask = WriteMask(ComponentBit::eAll);
  }

  SemanticUsage semanticUsage = dcl.getSemanticUsage();
  uint32_t semanticIndex = dcl.getSemanticIndex();

  bool foundExisting = false;
  for (auto& entry : m_variables) {
    if (entry.semanticUsage == semanticUsage && entry.semanticIndex == semanticIndex) {
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
  mapping.semanticUsage = semanticUsage;
  mapping.semanticIndex = semanticIndex;
  mapping.componentMask = componentMask;
  mapping.baseType = declaration.getType();
  mapping.baseDef = builder.add(std::move(declaration));

  emitDebugName(
    builder,
    mapping.baseDef,
    dst.getRegisterType(),
    dst.getIndex(),
    dst.getWriteMask(),
    dcl.getSemanticUsage(),
    dcl.getSemanticIndex()
  );

  return true;
}


ir::SsaDef IoMap::emitLoad(
        ir::Builder&            builder,
  const Instruction&            op,
  const Operand&                operand,
        WriteMask               componentMask,
        ir::ScalarType          type) {
  auto index = loadRegisterIndices(builder, op, operand);

  auto result = loadIoRegister(builder, type, index.regType,
    index.vertexIndex, index.regIndexRelative, index.regIndexAbsolute,
    operand.getSwizzle(), componentMask);

  if (!result)
    m_converter.logOpError(op, "Failed to process I/O load.");

  return result;
}


void IoMap::emitDebugName(
  ir::Builder& builder,
  ir::SsaDef def,
  RegisterType registerType,
  uint32_t registerIndex,
  WriteMask writeMask,
  SemanticUsage semanticUsage,
  uint32_t semanticIndex) const {
  if (!m_converter.m_options.includeDebugNames)
    return;

  std::string name;

  if ((registerType == RegisterType::eOutput && m_converter.getShaderInfo().getVersion().first >= 3)
    || registerType == RegisterType::eInput) {
    switch (semanticUsage) {
      case SemanticUsage::ePosition:
        name = "position" + std::to_string(semanticIndex);
        break;
      case SemanticUsage::eBlendWeight:
        name = "weight";
        break;
      case SemanticUsage::eBlendIndices:
        name = "blend";
        break;
      case SemanticUsage::eNormal:
        name = "normal" + std::to_string(semanticIndex);
        break;
      case SemanticUsage::ePointSize:
        name = "psize";
        break;
      case SemanticUsage::eTexCoord:
        name = "texcoord" + std::to_string(semanticIndex);
        break;
      case SemanticUsage::eTangent:
        name = "tangent";
        break;
      case SemanticUsage::eBinormal:
        name = "binormal";
        break;
      case SemanticUsage::eTessFactor:
        name = "tessfactor";
        break;
      case SemanticUsage::ePositionT:
        name = "positiont";
        break;
      case SemanticUsage::eColor:
        if (semanticIndex == 0) {
          name = "color";
        } else {
          name = "specular" + std::to_string(semanticIndex - 1u);
        }
        break;
      case SemanticUsage::eFog:
        name = "fog";
        break;
      case SemanticUsage::eDepth:
        name = "depth";
        break;
      case SemanticUsage::eSample:
        name = "sample" + std::to_string(semanticIndex);
        break;
      default:
        name = "unknown";
        break;
    }
  } else {
    name = m_converter.makeRegisterDebugName(registerType, registerIndex, writeMask);
  }

  builder.add(ir::Op::DebugName(def, name.c_str()));
}

}
