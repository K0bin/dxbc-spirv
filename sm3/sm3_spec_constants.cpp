#include "sm3_spec_constants.h"

#include <sstream>

namespace dxbc_spv::sm3 {

ir::SsaDef SpecializationConstantsMap::getSpecConstDword(ir::Builder& builder, ir::SsaDef entryPoint, uint32_t idx) {
  if (!m_specConstantIds[idx])
    m_specConstantIds[idx] = builder.add(ir::Op::DclSpecConstant(ir::ScalarType::eU32, entryPoint, idx, 0u));

  return m_specConstantIds[idx];
}


ir::SsaDef SpecializationConstantsMap::getSpecUBODword(ir::Builder& builder, ir::SsaDef specUbo, uint32_t idx) const {
  auto member = builder.makeConstant(idx);
  auto dword = builder.add(ir::Op::BufferLoad(ir::ScalarType::eU32, specUbo, member, 4u));

  return dword;
}


ir::SsaDef SpecializationConstantsMap::getOptimizedBool(ir::Builder& builder, ir::SsaDef entryPoint) {
  // The spec constant at MaxNumSpecConstants is set to True
  // when this is an optimized pipeline.
  auto optimized = getSpecConstDword(builder, entryPoint, m_layout.getOptimizedDwordOffset());
  optimized = builder.add(ir::Op::INe(ir::ScalarType::eBool, optimized, builder.makeConstant(0u)));

  return optimized;
}


ir::SsaDef SpecializationConstantsMap::get(ir::Builder& builder, ir::SsaDef entryPoint, ir::SsaDef specUbo, SpecConstantId id) {
  return get(builder, entryPoint, specUbo, id, builder.makeConstant(0u), builder.makeConstant(32u));
}


ir::SsaDef SpecializationConstantsMap::get(ir::Builder& builder, ir::SsaDef entryPoint, ir::SsaDef specUbo, SpecConstantId id, ir::SsaDef bitOffset, ir::SsaDef bitCount, ir::SsaDef uboOverride) {
  ir::SsaDef& function = m_specConstFunctions[uint32_t(id)];
  if (!function) {
    auto cursor = builder.setCursor(m_functionInsertPoint);

    auto bitOffsetParam = builder.add(ir::Op::DclParam(ir::ScalarType::eU32));
    builder.add(ir::Op::DebugName(bitOffsetParam, "bitOffset"));
    auto bitCountParam = builder.add(ir::Op::DclParam(ir::ScalarType::eU32));
    builder.add(ir::Op::DebugName(bitOffsetParam, "bitCount"));

    function = builder.add(
        ir::Op::Function(ir::ScalarType::eU32)
        .addOperand(bitOffsetParam)
        .addOperand(bitCountParam)
    );

    auto bitOffsetArg = builder.add(ir::Op::ParamLoad(ir::ScalarType::eU32, function, bitOffsetParam));
    auto bitCountArg = builder.add(ir::Op::ParamLoad(ir::ScalarType::eU32, function, bitCountParam));

    const auto &layout = m_layout.getSpecConstantLayout(id);

    auto optimized = getOptimizedBool(builder, entryPoint);

    auto quickValue     = uboOverride ? uboOverride : getSpecUBODword(builder, specUbo, layout.dwordOffset);
    auto optimizedValue = getSpecConstDword(builder, entryPoint, layout.dwordOffset);

    auto val = builder.add(ir::Op::Select(ir::ScalarType::eU32, optimized, optimizedValue, quickValue));

    auto offset = builder.add(ir::Op::IAdd(ir::ScalarType::eU32, builder.makeConstant(layout.bitOffset), bitOffsetArg));
    auto count = builder.add(ir::Op::UMin(ir::ScalarType::eU32,
      builder.add(ir::Op::ISub(ir::ScalarType::eU32, builder.makeConstant(layout.sizeInBits), bitOffsetArg)),
      bitCountArg
    ));

    auto extractedVal = builder.add(ir::Op::UBitExtract(ir::ScalarType::eU32, val,
      offset, count));

    builder.add(ir::Op::Return(ir::ScalarType::eU32, extractedVal));
    builder.add(ir::Op::FunctionEnd());

    std::stringstream namestream;
    namestream << id;
    std::string name = namestream.str();
    builder.add(ir::Op::DebugName(function, name.c_str()));

    builder.setCursor(cursor);
  }

  return builder.add(ir::Op::FunctionCall(ir::ScalarType::eU32, function)
    .addOperand(bitOffset)
    .addOperand(bitCount));
}


uint32_t SpecializationConstantsMap::getSamplerSpecConstIndex(ShaderType shaderType, uint32_t perShaderSamplerIndex) {
  return m_layout.getSamplerSpecConstIndex(shaderType, perShaderSamplerIndex);
}


std::ostream& operator << (std::ostream& os, SpecConstantId id) {
  switch (id) {
    case SpecConstantId::eSpecSamplerType: return os << "SamplerType";
    case SpecConstantId::eSpecSamplerDepthMode: return os << "SamplerDepthMode";
    case SpecConstantId::eSpecAlphaCompareOp: return os << "AlphaCompareOp";
    case SpecConstantId::eSpecSamplerProjected: return os << "SamplerProjected";
    case SpecConstantId::eSpecSamplerNull: return os << "SamplerNull";
    case SpecConstantId::eSpecAlphaPrecisionBits: return os << "AlphaPrecisionBits";
    case SpecConstantId::eSpecFogEnabled: return os << "FogEnabled";
    case SpecConstantId::eSpecVertexFogMode: return os << "VertexFogMode";
    case SpecConstantId::eSpecPixelFogMode: return os << "PixelFogMode";
    case SpecConstantId::eSpecVertexShaderBools: return os << "VertexShaderBools";
    case SpecConstantId::eSpecPixelShaderBools: return os << "PixelShaderBools";
    case SpecConstantId::eSpecSamplerFetch4: return os << "SamplerFetch4";
    case SpecConstantId::eSpecSamplerDrefClamp: return os << "SamplerDrefClamp";
    case SpecConstantId::eSpecClipPlaneCount: return os << "ClipPlaneCount";
    case SpecConstantId::eSpecPointMode: return os << "PointMode";
    case SpecConstantId::eSpecDrefScaling: return os << "DrefScaling";
  }

  return os << "SpecConstantId(" << uint32_t(id) << ")";
}

}
