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
      mapping.registerIndex = semanticMap.getIoLocation(mapping.semantic);
      mapping.componentMask = WriteMask(ComponentBit::eAll);
      mapping.baseType = declaration.getType();
      mapping.baseDef = builder.add(std::move(declaration));

      emitDebugName(
        builder,
        mapping.baseDef,
        RegisterType::eRasterizerOut,
        3,
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
      mapping.registerIndex = semanticMap.getIoLocation(mapping.semantic);
      mapping.componentMask = WriteMask(ComponentBit::eAll);
      mapping.baseType = declaration.getType();
      mapping.baseDef = builder.add(std::move(declaration));

      emitDebugName(
        builder,
        mapping.baseDef,
        isInput ? RegisterType::ePixelTexCoord : RegisterType::eTexCoordOut,
        i,
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
      mapping.registerIndex = semanticMap.getIoLocation(mapping.semantic);
      mapping.componentMask = WriteMask(ComponentBit::eAll);
      mapping.baseType = declaration.getType();
      mapping.baseDef = builder.add(std::move(declaration));

      emitDebugName(
        builder,
        mapping.baseDef,
        isInput ? RegisterType::eInput : RegisterType::eColorOut,
        i,
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
      mapping.registerIndex = semanticMap.getIoLocation(mapping.semantic);
      mapping.componentMask = WriteMask(ComponentBit::eAll);
      mapping.baseType = declaration.getType();
      mapping.baseDef = builder.add(std::move(declaration));

      emitDebugName(
        builder,
        mapping.baseDef,
        RegisterType::eRasterizerOut,
        uint32_t(RasterizerOutIndex::eRasterOutFog),
        WriteMask(ComponentBit::eAll),
        mapping.semantic,
        isInput
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
  Semantic semantic,
  bool isInput) const {
  if (!m_converter.m_options.includeDebugNames)
    return;

  std::string name;
  if (semantic.usage != SemanticUsage::eNormal
    || (isInput && registerType == RegisterType::eRasterizerOut)
    || (!isInput && registerType == RegisterType::eMiscType)) {
    // There is no register type for normals, it's only emitted for FF emulation.
    // The other exceptions either only have input only or output only registers.
    name = m_converter.makeRegisterDebugName(registerType, registerIndex, writeMask) + "_";
  }
  switch (semantic.usage) {
    case SemanticUsage::ePosition:
      name += "position" + std::to_string(semantic.index);
      break;
    case SemanticUsage::eBlendWeight:
      name += "weight";
      break;
    case SemanticUsage::eBlendIndices:
      name += "blend";
      break;
    case SemanticUsage::eNormal:
      name += "normal" + std::to_string(semantic.index);
      break;
    case SemanticUsage::ePointSize:
      name += "psize";
      break;
    case SemanticUsage::eTexCoord:
      name += "texcoord" + std::to_string(semantic.index);
      break;
    case SemanticUsage::eTangent:
      name += "tangent";
      break;
    case SemanticUsage::eBinormal:
      name += "binormal";
      break;
    case SemanticUsage::eTessFactor:
      name += "tessfactor";
      break;
    case SemanticUsage::ePositionT:
      name += "positiont";
      break;
    case SemanticUsage::eColor:
      if (semantic.index == 0) {
        name += "color";
      } else {
        name += "specular" + std::to_string(semantic.index - 1u);
      }
      break;
    case SemanticUsage::eFog:
      name += "fog";
      break;
    case SemanticUsage::eDepth:
      name += "depth";
      break;
    case SemanticUsage::eSample:
      name += "sample" + std::to_string(semantic.index);
      break;
    default:
      name += "unknown";
      break;
  }

  builder.add(ir::Op::DebugName(def, name.c_str()));
}

}
