#pragma once

#include "../util/util_byte_stream.h"

#include "sm3_types.h"
#include "sm3_parser.h"
#include "sm3_resources.h"

namespace dxbc_spv::sm3 {

struct ImmediateFloatConstant {
  uint32_t index;

  Vec4<float> value;
};

using ImmediateFloatConstants = std::vector<ImmediateFloatConstant>;

struct ImmediateConstants {
  int32_t maxFloatIndex = -1;
  int32_t maxIntIndex   = -1;
  int32_t maxBoolIndex  = -1;

  ImmediateFloatConstants floats;
};

struct PrepassConstants {
  bool floatsAccessedDynamically = false;

  int32_t maxFloatIndex = -1;
  int32_t maxIntIndex   = -1;
  int32_t maxBoolIndex  = -1;

  uint32_t boolMask = 0u;
};

struct InputSignatureElement {
  uint32_t location;
  Semantic semantic;
};

using RenderTargetMask = uint8_t;
using SamplerMask      = uint32_t;
using Signature        = util::small_vector<InputSignatureElement, 16u>;

class Prepass {

public:

  Prepass(util::ByteReader code, bool isSWVP);

  bool runPrepass();

  ShaderInfo getShaderInfo() const {
    return m_parser.getShaderInfo();
  }

  size_t getLength() const {
    return m_length;
  }

  const PrepassConstants& getConstantsInfo() const {
    return m_constants;
  }

  const ImmediateConstants& getImmediateConstants() const {
    return m_immediateConstants;
  }

  RenderTargetMask getRenderTargetMask() const {
    return m_usedRTs;
  }

  SamplerMask getSamplerMask() const {
    return m_usedSamplers;
  }

  TextureType getTextureType(uint32_t index) const {
    return m_textureTypes[index];
  }

  bool isSWVP() const {
    return m_isSWVP;
  }

  uint32_t getInputSignatureSize() const {
    return m_inputSignature.size();
  }

  InputSignatureElement getInputSignatureElement(uint32_t index) const {
    return m_inputSignature[index];
  }

private:

  bool initParser(Parser& parser, util::ByteReader reader);

  bool handleInstruction(const Instruction& op);

  bool handleDef(const Instruction& op);

  bool handleTextureSample(const Instruction& op);

  bool handleDcl(const Instruction& op);

  util::ByteReader m_code;

  Parser           m_parser;

  bool             m_isSWVP;

  size_t m_length = 0u;

  PrepassConstants m_constants;

  ImmediateConstants m_immediateConstants;

  RenderTargetMask m_usedRTs = 0u;

  SamplerMask m_usedSamplers = 0u;

  std::array<TextureType, 16u> m_textureTypes = {};

  Signature m_inputSignature    = {};

};

}
