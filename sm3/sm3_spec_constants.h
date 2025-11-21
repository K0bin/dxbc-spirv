#pragma once

#include <cstdint>

#include "../ir/ir.h"
#include "../ir/ir_builder.h"

namespace dxbc_spv::sm3 {

enum class SpecConstantId : uint32_t {
  SpecSamplerType              = 0u,
  SpecSamplerDepthMode         = 1u,
  SpecAlphaCompareOp           = 2u,
  SpecSamplerProjected         = 3u,
  SpecSamplerNull              = 4u,
  SpecAlphaPrecisionBits       = 5u,
  SpecFogEnabled               = 6u,
  SpecVertexFogMode            = 7u,
  SpecPixelFogMode             = 8u,
  SpecVertexShaderBools        = 9u,
  SpecPixelShaderBools         = 10u,
  SpecSamplerFetch4            = 11u,
  SpecFFLastActiveTextureStage = 12u,
  SpecSamplerDrefClamp         = 13u,
  SpecClipPlaneCount           = 14u,
  SpecPointMode                = 15u,
  SpecDrefScaling              = 16u,
  /* 17 - 46 are spec constants used for fixed function shaders */
};

struct SpecializationConstantBits {
  uint32_t dwordOffset;
  uint32_t bitOffset;
  uint32_t sizeInBits;
};

class SpecializationConstantLayout {

public:

  virtual SpecializationConstantBits getSpecConstantLayout(SpecConstantId id) const = 0;

  virtual uint32_t getOptimizedDwordOffset() const = 0;

  virtual uint32_t getSamplerSpecConstIndex(ShaderType shaderType, uint32_t perShaderSamplerIndex) = 0;

};

class SpecializationConstantsMap {

public:

  SpecializationConstantsMap(SpecializationConstantLayout& layout)
  : m_layout(layout) { }

  ir::SsaDef get(ir::Builder& builder, ir::SsaDef entryPoint, ir::SsaDef specUbo, SpecConstantId id) {
    return get(builder, entryPoint, specUbo, id, 0, 32);
  }

  ir::SsaDef get(ir::Builder& builder, ir::SsaDef entryPoint, ir::SsaDef specUbo, SpecConstantId id, uint32_t bitOffset, uint32_t bitCount, ir::SsaDef uboOverride = { }) {
    const auto &layout = m_layout.getSpecConstantLayout(id);

    auto optimized = getOptimizedBool(builder, entryPoint);

    auto quickValue     = uboOverride ? uboOverride : getSpecUBODword(builder, specUbo, layout.dwordOffset);
    auto optimizedValue = getSpecConstDword(builder, entryPoint, layout.dwordOffset);

    auto val = builder.add(ir::Op::Select(ir::ScalarType::eU32, optimized, optimizedValue, quickValue));
    bitCount = std::min(bitCount, layout.sizeInBits - bitOffset);

    if (bitCount == 32)
      return val;

    return builder.add(ir::Op::UBitExtract(ir::ScalarType::eU32, val,
      builder.makeConstant(bitOffset + layout.bitOffset),
      builder.makeConstant(bitCount)));
  }

  uint32_t getSamplerSpecConstIndex(ShaderType shaderType, uint32_t perShaderSamplerIndex) {
    return m_layout.getSamplerSpecConstIndex(shaderType, perShaderSamplerIndex);
  }

private:

  ir::SsaDef getSpecConstDword(ir::Builder& builder, ir::SsaDef entryPoint, uint32_t idx) {
    if (!m_specConstantIds[idx])
      m_specConstantIds[idx] = builder.add(ir::Op::DclSpecConstant(ir::ScalarType::eU32, entryPoint, idx, 0u));

    return m_specConstantIds[idx];
  }

  ir::SsaDef getSpecUBODword(ir::Builder& builder, ir::SsaDef specUbo, uint32_t idx) {
    auto member = builder.makeConstant(idx);
    auto dword = builder.add(ir::Op::BufferLoad(ir::ScalarType::eU32, specUbo, member, 4u));

    return dword;
  }

  ir::SsaDef getOptimizedBool(ir::Builder& builder, ir::SsaDef entryPoint) {
    // The spec constant at MaxNumSpecConstants is set to True
    // when this is an optimized pipeline.
    auto optimized = getSpecConstDword(builder, entryPoint, m_layout.getOptimizedDwordOffset());
    optimized = builder.add(ir::Op::INe(ir::ScalarType::eBool, optimized, builder.makeConstant(0u)));

    return optimized;
  }

  std::array<ir::SsaDef, 32u> m_specConstantIds = {};

  SpecializationConstantLayout& m_layout;

};

}
