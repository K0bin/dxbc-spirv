#pragma once

#include <cstdint>

#include "../ir/ir.h"
#include "../ir/ir_builder.h"

#include "sm3_spec_constants_layout.h"

namespace dxbc_spv::sm3 {

class SpecializationConstantsMap {

public:

  SpecializationConstantsMap(SpecializationConstantLayout& layout)
  : m_layout(layout) { }

  ir::SsaDef get(ir::Builder& builder, ir::SsaDef entryPoint, ir::SsaDef specUbo, SpecConstantId id);

  ir::SsaDef get(ir::Builder& builder, ir::SsaDef entryPoint, ir::SsaDef specUbo, SpecConstantId id, ir::SsaDef bitOffset, ir::SsaDef bitCount, ir::SsaDef uboOverride = { });

  uint32_t getSamplerSpecConstIndex(ShaderType shaderType, uint32_t perShaderSamplerIndex);

  void setInsertCursor(ir::SsaDef cursor) {
    m_functionInsertPoint = cursor;
  }

private:

  ir::SsaDef getSpecConstDword(ir::Builder& builder, ir::SsaDef entryPoint, uint32_t idx);

  ir::SsaDef getSpecUBODword(ir::Builder& builder, ir::SsaDef specUbo, uint32_t idx) const;

  ir::SsaDef getOptimizedBool(ir::Builder& builder, ir::SsaDef entryPoint);

  std::array<ir::SsaDef, 32u> m_specConstantIds = { };

  std::array<ir::SsaDef, uint32_t(SpecConstantId::eSpecDrefScaling) + 1u> m_specConstFunctions = { };

  ir::SsaDef m_functionInsertPoint = { };

  SpecializationConstantLayout& m_layout;

};

}
