#include "sm3_prepass.h"

#include "../util/util_log.h"

namespace dxbc_spv::sm3 {
  Prepass::Prepass(util::ByteReader code, bool isSWVP)
    : m_code(code), m_isSWVP(isSWVP) {
    if (isSWVP)
      m_constants.boolMask = ~0u;
  }

  bool Prepass::runPrepass() {
    if (!initParser(m_parser, m_code))
      return false;

    while (m_parser) {
      Instruction instruction = m_parser.parseInstruction();

      if (!handleInstruction(instruction))
        return false;
    }

    m_length = m_parser.getByteOffset();

    return true;
  }

  bool Prepass::initParser(Parser& parser, util::ByteReader reader) {
    if (!reader) {
      Logger::err("No code chunk found in shader.");
      return false;
    }

    if (!(parser = Parser(reader))) {
      Logger::err("Failed to parse code chunk.");
      return false;
    }

    return true;
  }

  bool Prepass::handleInstruction(const Instruction& op) {
    /* Determine whether we're accessing float constants dynamically
     * because in that case, we'll need to copy the immediate constants
     * into the constant buffer inside DXVK. */
    for (uint32_t i = 0u; i < op.getSrcCount(); i++) {
      const auto& src = op.getSrc(i);
      auto registerType = src.getRegisterType();
      uint32_t index = src.getIndex();

      if (getShaderInfo().getType() == ShaderType::ePixel
        && op.hasDst()
        && op.getDst().getRegisterType() == RegisterType::eColorOut) {
        m_usedRTs |= 1u << op.getDst().getIndex();
      }

      if (getShaderInfo().getType() == ShaderType::eVertex
        && registerType == RegisterType::eInput
        && (m_inputSignature.size() < index + 1u || m_inputSignature[index] == Semantic { })) {
        setInputSignatureElement(index, { SemanticUsage::eColor, index });
        continue;
      }

      if (registerType == RegisterType::eConstInt) {
        m_constants.maxIntIndex = std::max(m_constants.maxIntIndex, index);
        continue;
      }

      if (registerType == RegisterType::eConstBool) {
        m_constants.maxBoolIndex = std::max(m_constants.maxBoolIndex, index);
        if (!m_isSWVP)
          m_constants.boolMask |= 1u << index;

        continue;
      }

      if (registerType != RegisterType::eConst
        && registerType != RegisterType::eConst2
        && registerType != RegisterType::eConst3
        && registerType != RegisterType::eConst4)
        continue;

      m_constants.maxFloatIndex = std::max(m_constants.maxFloatIndex, index);

      if (src.hasRelativeAddressing())
        continue;

      m_constants.floatsAccessedDynamically = true;
    }

    switch (op.getOpCode()) {
      case OpCode::eDef:
      case OpCode::eDefI:
      case OpCode::eDefB:
        if (!handleDef(op))
          return false;
        break;

      case OpCode::eTexLd:
      case OpCode::eTexBem:
      case OpCode::eTexBemL:
      case OpCode::eTexReg2Ar:
      case OpCode::eTexReg2Gb:
      case OpCode::eTexM3x2Tex:
      case OpCode::eTexM3x3Tex:
      case OpCode::eTexM3x3Spec:
      case OpCode::eTexM3x3VSpec:
      case OpCode::eTexReg2Rgb:
      case OpCode::eTexDp3Tex:
      case OpCode::eTexM3x2Depth:
      case OpCode::eTexDp3:
      case OpCode::eTexM3x3:
      case OpCode::eTexLdd:
      case OpCode::eTexLdl:
        if (!handleTextureSample(op))
          return false;
        break;

      case OpCode::eDcl:
        if (!handleDcl(op))
          return false;
        break;

      default: break;
    }

    return true;
  }

  bool Prepass::handleDef(const Instruction& op) {
    dxbc_spv_assert(op.hasDst());
    uint32_t index = op.getDst().getIndex();

    if (op.getOpCode() == OpCode::eDef) {
      m_immediateConstants.maxFloatIndex = std::max(m_immediateConstants.maxFloatIndex, index);

      dxbc_spv_assert(op.hasImm());
      auto imm = op.getImm();

      Vec4<float> value = {
        imm.getImmediate<float>(0u), imm.getImmediate<float>(1u),
        imm.getImmediate<float>(2u), imm.getImmediate<float>(3u)
      };
      m_immediateConstants.floats.push_back({ index, value });
    } else if (op.getOpCode() == OpCode::eDefI) {
      m_immediateConstants.maxIntIndex = std::max(m_immediateConstants.maxIntIndex, index);
    } else if (op.getOpCode() == OpCode::eDefB) {
      m_immediateConstants.maxBoolIndex = std::max(m_immediateConstants.maxBoolIndex, index);
    } else {
      return false;
    }

    return true;
  }


  bool Prepass::handleTextureSample(const Instruction& op) {
    uint32_t samplerIndex;
    auto dst = op.getDst();
    Operand src1;
    if (op.getSrcCount() >= 2u)
      src1 = op.getSrc(1u);

    switch (op.getOpCode()) {
      case OpCode::eTexLd:
        if (getShaderInfo().getVersion().first <= 1u)
          samplerIndex = dst.getIndex();
        else
          samplerIndex = src1.getIndex();
        break;

      case OpCode::eTexLdl:
      case OpCode::eTexLdd:
        samplerIndex = src1.getIndex();
        break;

      case OpCode::eTexReg2Ar:
      case OpCode::eTexReg2Gb:
      case OpCode::eTexReg2Rgb:
      case OpCode::eTexM3x2Tex:
      case OpCode::eTexM3x3Tex:
      case OpCode::eTexM3x3:
      case OpCode::eTexM3x2Depth:
      case OpCode::eTexM3x3Spec:
      case OpCode::eTexM3x3VSpec:
      case OpCode::eTexDp3Tex:
      case OpCode::eTexDp3:
      case OpCode::eTexBem:
      case OpCode::eTexBemL:
        samplerIndex = dst.getIndex();
        break;

      default: return false;
    }

    m_usedSamplers |= 1u << samplerIndex;

    return true;
  }


  bool Prepass::handleDcl(const Instruction& op) {
    dxbc_spv_assert(op.hasDcl());
    const auto& dcl = op.getDcl();
    dxbc_spv_assert(op.hasDst());
    const auto& dst = op.getDst();

    RegisterType registerType = dst.getRegisterType();
    uint32_t index = dst.getIndex();

    if (registerType == RegisterType::eSampler) {
      m_textureTypes[index] = dcl.getTextureType();
      m_usedSamplers |= 1u << index;
      return true;
    }

    if (registerType == RegisterType::eInput && getShaderInfo().getType() == ShaderType::eVertex) {
      setInputSignatureElement(index, { dcl.getSemanticUsage(), dcl.getSemanticIndex() });
      return true;
    }

    return true;
  }

}
