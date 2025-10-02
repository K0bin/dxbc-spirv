#pragma once

#include <optional>

#include "../util/util_byte_stream.h"
#include "../util/util_bit.h"

#include "../ir/ir.h"

#include "sm3_types.h"

namespace dxbc_spv::sm3 {

class Instruction;

/** Program type */
enum class ShaderType : uint32_t {
  eVertex   = 0u,
  ePixel    = 1u,
};

/** Shader code header */
class ShaderInfo {

public:

  ShaderInfo() = default;

  /** Constructs shader info. Note that the dword count must include
   *  the two dwords consumed by the info structure itself. */
  ShaderInfo(ShaderType type, uint32_t major, uint32_t minor)
  : m_token(((~0u & uint32_t(type)) << 16) | ((major & 0xff) << 8u) | (minor & 0xff)) { }

  /** Parses shader info */
  explicit ShaderInfo(util::ByteReader& reader);

  /** Extracts shader type from version token */
  ShaderType getType() const {
    return ShaderType(util::bextract(m_token, 16u, 1));
  }

  /** Extracts shader model version from version token */
  std::pair<uint32_t, uint32_t> getVersion() const {
    return std::make_pair(
      util::bextract(m_token, 8u, 8u),
      util::bextract(m_token, 0u, 8u));
  }

  /** Checks whether shader header is valid */
  explicit operator bool () const {
    return util::bextract(m_token, 17u, 15u) == 0x7fff;
  }

  /** Writes code header to binary blob. */
  bool write(util::ByteWriter& writer) const;

  uint32_t getToken() const
  {
    return m_token;
  }

private:

  uint32_t m_token = 0u;

  void resetOnError();

};


/** Operand type. Used internally in order to set up the instruction
 *  layout and to distinguish operand tokens from immediate values
 *  when parsing instructions. */
enum class OperandKind : uint8_t {
  eNone    = 0u,
  eDstReg  = 1u,
  eSrcReg  = 2u,
  eDclReg  = 3u,
  eImm32   = 4u,
  eRelAddr = 5u,
  ePred    = 6u,
};


/** Component selection */
enum class SelectionMode : uint32_t {
  eMask       = 0u,
  eSwizzle    = 1u,
  eSelect1    = 2u,
};



/** Component selection */
enum class ComponentCount : uint32_t {
  e1Component = 1u,
  e4Component = 4u,
};


/**
 * \brief Source operand modifiers
 *
 * These are applied after loading
 * an operand register.
 */
enum class OperandModifier : uint32_t {
  None    = 0u,  // r
  Neg     = 1u,  // -r
  Bias    = 2u,  // r - 0.5
  BiasNeg = 3u,  // -(r - 0.5)
  Sign    = 4u,  // fma(r, 2.0f, -1.0f)
  SignNeg = 5u,  // -fma(r, 2.0f, -1.0f)
  Comp    = 6u,  // 1 - r
  X2      = 7u,  // r * 2
  X2Neg   = 8u,  // -r * 2
  Dz      = 9u,  // r / r.z
  Dw      = 10u, // r / r.w
  Abs     = 11u, // abs(r)
  AbsNeg  = 12u, // -abs(r)
  Not     = 13u, // !r
};


/**
 * \brif Determines whether the shader uses a separate token to configure relative addressing
 */
inline bool hasExtraRelativeAddressingToken(OperandKind kind, const ShaderInfo& info) {
  return (info.getVersion().first >= 2 && kind == OperandKind::eSrcReg)
        || (info.getVersion().first >= 3 && kind == OperandKind::eDstReg);
}


/** Operand info */
struct OperandInfo {
  OperandKind     kind = { };
  ir::ScalarType  type = ir::ScalarType::eUnknown;
};


/** Operand info. Stores the base and extended operand tokens if
 *  applicable, as well as immediates or index values, depending
 *  on the operand type. */
class Operand {

public:

  Operand() = default;

  /** Creates operand and sets up the operand type.
   * Other properties need to be set manually. */
  Operand(OperandInfo info, RegisterType type)
    : m_token(((uint32_t(type) & 0b11000) << 11u) | (uint32_t(type) & 0b111) << 28u),
      m_info(info) { }

  /** Recursively parses operand in byte stream, index operands. */
  Operand(util::ByteReader& reader, const OperandInfo& info, Instruction& op, const ShaderInfo& shaderInfo);

  /** Operand metadata. Not part of the operand tokens, but useful
   *  when processing operands depending on the instruction layout. */
  OperandInfo getInfo() const {
    return m_info;
  }

  /** Queries component count */
  ComponentCount getComponentCount(const ShaderInfo& shaderInfo) const {
    return (getRegisterType() == RegisterType::eLoop
             || getRegisterType() == RegisterType::eConstBool
             || getRegisterType() == RegisterType::ePredicate
             || (getRegisterType() == RegisterType::eRasterizerOut && getIndex() == uint32_t(RasterizerOutIndex::eRasterOutFog))
             || (getRegisterType() == RegisterType::eRasterizerOut && getIndex() == uint32_t(RasterizerOutIndex::eRasterOutPointSize))
             || (getRegisterType() == RegisterType::eMiscType && getIndex() == uint32_t(MiscTypeIndex::eMiscTypeFace))
             || (getRegisterType() == RegisterType::eAddr && shaderInfo.getVersion().first == 1)
             || getRegisterType() == RegisterType::eDepthOut)
             ? ComponentCount::e1Component
             : ComponentCount::e4Component;
  }

  /** Queries component selection mode. Determines the existence
   *  of a swizzle or write mask in the operand token. */
  SelectionMode getSelectionMode() const {
    if (getRegisterType() == RegisterType::ePredicate
      || getRegisterType() == RegisterType::eConstBool
      || getRegisterType() == RegisterType::eLoop
      || getRegisterType() == RegisterType::eAddr
      || getRegisterType() == RegisterType::eDepthOut
      || (getRegisterType() == RegisterType::eMiscType && getIndex() == uint32_t(MiscTypeIndex::eMiscTypeFace))) {
      return SelectionMode::eSelect1;
    }
    return m_info.kind == OperandKind::eDstReg ? SelectionMode::eMask : SelectionMode::eSwizzle;
  }

  /** Queries component swizzle. Only useful for source operands. */
  Swizzle getSwizzle() const {
    return m_info.kind == OperandKind::eSrcReg
             ? Swizzle(util::bextract(m_token, 16u, 8u))
             : Swizzle::identity();
  }

  /** Queries component write mask. Only useful for destination
   *  operands. */
  WriteMask getWriteMask() const {
    return m_info.kind == OperandKind::eDstReg
             ? WriteMask(util::bextract(m_token, 16u, 4u))
             : WriteMask(0b1111);
  }

  /** Queries whether the results of the instruction get saturated (clamped to 0..1) before
    * writing them to the destination register. Only relevant for destination register operands. */
  bool isSaturated() const {
    return m_info.kind == OperandKind::eDstReg && util::bextract(m_token, 20, 1);
  }

  /** Queries the number of bits by which the results of the instructions get shifted before
    * writing them to the destination register. Only relevant for destination register operands */
  int8_t getShift() const {
    return m_info.kind == OperandKind::eDstReg
             ? int8_t((util::bextract(m_token, 24u, 4u) & 0b111) - (util::bextract(m_token, 24, 4) & 0b1000))
             : 0;
  }

  /** Queries whether the pixel shader input should be interpolated at the centroid.
    * Only relevant for shader inputs in destination register operands. */
  bool isCentroid() const {
    return util::bextract(m_token, 22u, 1u);
  }

  /** Queries whether the operation can be performed using reduced precision.
    * Only relevant for destination operands */
  bool isPartialPrecision() const {
    return util::bextract(m_token, 21, 1);
  }

  /** Queries register type */
  RegisterType getRegisterType() const {
    return RegisterType((util::bextract(m_token, 11u, 2u) << 3u)
      | util::bextract(m_token, 28u, 3u));
  }

  /** Queries absolute index. */
  uint32_t getIndex() const {
    return util::bextract(m_token, 0u, 11u);
  }

  /** Queries relative indexing operand index. */
  uint8_t getRelAddrOperand() const {
    return m_relAddr;
  }

  /** Queries whether the operand uses relative addressing. */
  bool hasRelativeAddressing() const {
    return util::bextract(m_token, 13u, 1u);
  }

  /** Queries operand modifier. */
  OperandModifier getModifier() const {
    return OperandModifier(util::bextract(m_token, 24u, 4u));
  }

  /** Queries the semantic usage.
   *  Only useful for declaration operands of shader inputs and outputs. */
  Usage getUsage() const {
    return Usage(util::bextract(m_token, 0u, 4u));
  }

  /** Queries the semantic usage index.
   *  Only useful for declaration operands of shader inputs and outputs. */
  uint32_t getUsageIndex() const {
    return util::bextract(m_token, 16u, 4u);
  }

  /** Queries the texture type. Only useful for declaration operands. */
  TextureType getTextureType() const {
    return TextureType(util::bextract(m_token, 27u, 4u));
  }

  /** Queries immediate value for a given component. */
  template<typename T, std::enable_if_t<std::is_arithmetic_v<T>, bool> = true>
  T getImmediate(uint32_t idx) const {
    util::uint_type_t<T> data;
    data = m_imm[idx];
    T result;
    std::memcpy(&result, &data, sizeof(result));
    return result;
  }

  /** Sets immediate value for a given component. */
  template<typename T, std::enable_if_t<(std::is_arithmetic_v<T>), bool> = true>
  Operand& setImmediate(uint32_t idx, T value) {
    util::uint_type_t<T> data;
    std::memcpy(&data, &value, sizeof(data));
    m_imm[idx] = data;
    return *this;
  }

  /** Checks operand info is valid. A default token of 0 is
   *  nonsensical since it refers to a 0-component temp. */
  explicit operator bool () const {
    return m_token != DefaultInvalidToken;
  }

private:

  // A nonsensical 33rd temp register.
  constexpr static uint32_t DefaultInvalidToken = 33u << 11u;

  uint32_t                  m_token = DefaultInvalidToken;
  std::array<uint32_t, 4u>  m_imm   = { };

  OperandInfo               m_info = { };

  uint8_t                   m_relAddr  = { };

  void resetOnError();

};

/** Instruction layout. Stores the number of operands, and
 *  whether each operand is a source or destination register
 *  or an immediate. */
struct InstructionLayout {
  util::small_vector<OperandInfo, 8u> operands = { };
};


class Instruction {

public:

  Instruction() = default;

  /** Initializes instruction with opcode and no operands. This will
   *  not interact with the instruction layout in any way. */
  explicit Instruction(OpCode opCode)
  : m_token(uint32_t(opCode)) { }

  /** Initializes instruction with opcode token. */
  explicit Instruction(uint32_t token)
  : m_token(token) { }

  /** Parses an instruction in a code chunk. */
  explicit Instruction(util::ByteReader& reader, const ShaderInfo& info);

  /** Extracts opcode from token */
  OpCode getOpCode() const {
    return OpCode(util::bextract(m_token, 0u, 16u));
  }

  bool isPredicated() const {
    return util::bextract(m_token, 28u, 1u);
  }

  bool isCoissued() const {
    return util::bextract(m_token, 31u, 1u);
  }

  uint32_t getOperandTokenCount(const ShaderInfo& info, const InstructionLayout& layout) const;

  /** Adds a raw operand and returns the absolute operand index.
   *  This will implicitly update the source, dest or immediate
   *  operand look-up tables depending on the operand kind. */
  uint32_t addOperand(const Operand& operand);

  /** Retrieves whether the instruction has a destination operand */
  bool hasDst() const {
    return m_dstOperand.has_value();
  }

  /** Retrieves whether the instruction has a declaration operand */
  bool hasDcl() const {
    return m_dclOperand.has_value();
  }

  /** Retrieves whether the instruction has a predication operand */
  bool hasPred() const {
    return m_predOperand.has_value();
  }

  /** Retrieves number of source operands */
  uint32_t getSrcCount() const {
    return uint32_t(m_srcOperands.size());
  }

  /** Retrieves number of immediate operands */
  uint32_t getImmCount() const {
    return uint32_t(m_immOperands.size());
  }

  /** Queries destination operand. */
  const Operand& getDst() const {
    return getRawOperand(m_dstOperand.value());
  }

  /** Queries destination operand. */
  const Operand& getDcl() const {
    return getRawOperand(m_dclOperand.value());
  }

  /** Queries destination operand. */
  const Operand& getPred() const {
    return getRawOperand(m_predOperand.value());
  }

  /** Queries source operand. */
  const Operand& getSrc(uint32_t index) const {
    return getRawOperand(m_srcOperands.at(index));
  }

  /** Queries immediate operand. */
  const Operand& getImm(uint32_t index) const {
    return getRawOperand(m_immOperands.at(index));
  }

  /** Queries raw operand index. Generally used
   *  when processing relative operand indices. */
  const Operand& getRawOperand(uint32_t index) const {
    dxbc_spv_assert(index < m_operands.size());
    return m_operands[index];
  }

  /** Queries instruction layout for the given instruction,
   *  with the given shader model in mind. The shader model
   *  changes the layout of resource declaration ops. */
  InstructionLayout getLayout(const ShaderInfo& info) const;

  /** Writes instruction and all its operands to a binary blob. */
  bool write(util::ByteWriter& writer, const ShaderInfo& info) const;

  /** Queries the comparison mode of the instruction.
    * Only relevant for a few control flow instructions. */
  ComparisonMode getComparisonMode() const {
    return ComparisonMode(util::bextract(m_token, 16u, 8u));
  }

  /** Queries the TexLd mode of the instruction. */
  TexLdMode getTexLdMode() const {
    return TexLdMode(util::bextract(m_token, 16u, 8u));
  }

  /** Checks whether instruction is valid */
  explicit operator bool () const {
    return m_token != DefaultInvalidToken;
  }

private:
  constexpr static uint32_t DefaultInvalidToken = ~0u;

  uint32_t m_token = DefaultInvalidToken;

  std::optional<uint8_t> m_dstOperand = { };
  std::optional<uint8_t> m_dclOperand = { };
  std::optional<uint8_t> m_predOperand = { };
  util::small_vector<uint8_t, 4u> m_srcOperands = { };
  util::small_vector<uint8_t, 4u> m_immOperands = { };

  util::small_vector<Operand, 16u> m_operands = { };

  void resetOnError();

};



class Parser {

public:
  Parser() = default;

  explicit Parser(util::ByteReader reader);

  /** Queries shader info, including the shader type and version. This
   *  is always available since it is stored at the start of the chunk. */
  ShaderInfo getShaderInfo() const {
    return m_info;
  }

  Instruction parseInstruction();

  /** Checks whether any more instructions are
   *  available to be parsed. */
  explicit operator bool () const {
    return !m_isPastEnd;
  }

private:

  util::ByteReader m_reader;

  ShaderInfo m_info = { };

  bool m_isPastEnd = false;

};

std::ostream& operator << (std::ostream& os, ShaderType type);
std::ostream& operator << (std::ostream& os, const ShaderInfo& shaderInfo);

}
