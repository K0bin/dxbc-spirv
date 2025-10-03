#include "sm3_parser.h"

#include "../util/util_log.h"

namespace dxbc_spv::sm3 {

/* If the operands differ between shading model versions, use the latest ones. */
static const std::array<InstructionLayout, 100> g_instructionLayouts = {{
  /* Nop */
  { },
  /* Mov */
  { {{
    { OperandKind::eDstReg, ir::ScalarType::eF32 },
    { OperandKind::eSrcReg, ir::ScalarType::eF32 },
  }} },
  /* Add */
  { {{
    { OperandKind::eDstReg, ir::ScalarType::eF32 },
    { OperandKind::eSrcReg, ir::ScalarType::eF32 },
    { OperandKind::eSrcReg, ir::ScalarType::eF32 },
  }} },
  /* Sub */
  { {{
    { OperandKind::eDstReg, ir::ScalarType::eF32 },
    { OperandKind::eSrcReg, ir::ScalarType::eF32 },
    { OperandKind::eSrcReg, ir::ScalarType::eF32 },
  }} },
 /* Mad */
  { {{
    { OperandKind::eDstReg, ir::ScalarType::eF32 },
    { OperandKind::eSrcReg, ir::ScalarType::eF32 },
    { OperandKind::eSrcReg, ir::ScalarType::eF32 },
    { OperandKind::eSrcReg, ir::ScalarType::eF32 },
  }} },
  /* Mul */
  { {{
    { OperandKind::eDstReg, ir::ScalarType::eF32 },
    { OperandKind::eSrcReg, ir::ScalarType::eF32 },
    { OperandKind::eSrcReg, ir::ScalarType::eF32 },
  }} },
  /* Rcp */
  { {{
    { OperandKind::eDstReg, ir::ScalarType::eF32 },
    { OperandKind::eSrcReg, ir::ScalarType::eF32 },
  }} },
  /* Rsq */
  { {{
    { OperandKind::eDstReg, ir::ScalarType::eF32 },
    { OperandKind::eSrcReg, ir::ScalarType::eF32 },
  }} },
  /* Dp3 */
  { {{
    { OperandKind::eDstReg, ir::ScalarType::eF32 },
    { OperandKind::eSrcReg, ir::ScalarType::eF32 },
    { OperandKind::eSrcReg, ir::ScalarType::eF32 },
  }} },
  /* Dp4 */
  { {{
    { OperandKind::eDstReg, ir::ScalarType::eF32 },
    { OperandKind::eSrcReg, ir::ScalarType::eF32 },
    { OperandKind::eSrcReg, ir::ScalarType::eF32 },
  }} },
  /* Min */
  { {{
    { OperandKind::eDstReg, ir::ScalarType::eF32 },
    { OperandKind::eSrcReg, ir::ScalarType::eF32 },
    { OperandKind::eSrcReg, ir::ScalarType::eF32 },
  }} },
  /* Max */
  { {{
    { OperandKind::eDstReg, ir::ScalarType::eF32 },
    { OperandKind::eSrcReg, ir::ScalarType::eF32 },
    { OperandKind::eSrcReg, ir::ScalarType::eF32 },
  }} },
  /* Slt */
  { {{
    { OperandKind::eDstReg, ir::ScalarType::eF32 },
    { OperandKind::eSrcReg, ir::ScalarType::eF32 },
    { OperandKind::eSrcReg, ir::ScalarType::eF32 },
  }} },
  /* Sge */
  { {{
    { OperandKind::eDstReg, ir::ScalarType::eF32 },
    { OperandKind::eSrcReg, ir::ScalarType::eF32 },
    { OperandKind::eSrcReg, ir::ScalarType::eF32 },
  }} },
  /* Exp */
  { {{
    { OperandKind::eDstReg, ir::ScalarType::eF32 },
    { OperandKind::eSrcReg, ir::ScalarType::eF32 },
  }} },
  /* Log */
  { {{
    { OperandKind::eDstReg, ir::ScalarType::eF32 },
    { OperandKind::eSrcReg, ir::ScalarType::eF32 },
  }} },
  /* Lit */
  { {{
    { OperandKind::eDstReg, ir::ScalarType::eF32 },
    { OperandKind::eSrcReg, ir::ScalarType::eF32 },
  }} },
  /* Dst */
  { {{
    { OperandKind::eDstReg, ir::ScalarType::eF32 },
    { OperandKind::eSrcReg, ir::ScalarType::eF32 },
    { OperandKind::eSrcReg, ir::ScalarType::eF32 },
  }} },
  /* Lrp */
  { {{
    { OperandKind::eDstReg, ir::ScalarType::eF32 },
    { OperandKind::eSrcReg, ir::ScalarType::eF32 },
    { OperandKind::eSrcReg, ir::ScalarType::eF32 },
    { OperandKind::eSrcReg, ir::ScalarType::eF32 },
  }} },
  /* Frc */
  { {{
    { OperandKind::eDstReg, ir::ScalarType::eF32 },
    { OperandKind::eSrcReg, ir::ScalarType::eF32 },
  }} },
  /* M4x4 */
  { {{
    { OperandKind::eDstReg, ir::ScalarType::eF32 },
    { OperandKind::eSrcReg, ir::ScalarType::eF32 },
    { OperandKind::eSrcReg, ir::ScalarType::eF32 },
  }} },
  /* M4x3 */
  { {{
    { OperandKind::eDstReg, ir::ScalarType::eF32 },
    { OperandKind::eSrcReg, ir::ScalarType::eF32 },
    { OperandKind::eSrcReg, ir::ScalarType::eF32 },
  }} },
  /* M3x4 */
  { {{
    { OperandKind::eDstReg, ir::ScalarType::eF32 },
    { OperandKind::eSrcReg, ir::ScalarType::eF32 },
    { OperandKind::eSrcReg, ir::ScalarType::eF32 },
  }} },
  /* M3x3 */
  { {{
    { OperandKind::eDstReg, ir::ScalarType::eF32 },
    { OperandKind::eSrcReg, ir::ScalarType::eF32 },
    { OperandKind::eSrcReg, ir::ScalarType::eF32 },
  }} },
  /* M3x2 */
  { {{
    { OperandKind::eDstReg, ir::ScalarType::eF32 },
    { OperandKind::eSrcReg, ir::ScalarType::eF32 },
    { OperandKind::eSrcReg, ir::ScalarType::eF32 },
  }} },
  /* Call */
  { {{
    { OperandKind::eSrcReg, ir::ScalarType::eUnknown },
  }} },
  /* CallNz */
  { {{
    { OperandKind::eSrcReg, ir::ScalarType::eUnknown },
    { OperandKind::eSrcReg, ir::ScalarType::eBool },
  }} },
  /* Loop */
  { {{
    { OperandKind::eDstReg, ir::ScalarType::eI32 },
    { OperandKind::eSrcReg, ir::ScalarType::eI32 },
  }} },
  /* Ret */
{ },
  /* EndLoop */
{ },
  /* Label */
  { {{
    { OperandKind::eDstReg, ir::ScalarType::eUnknown },
  }} },
  /* Dcl */
  { {{
    { OperandKind::eDclReg, ir::ScalarType::eUnknown },
    { OperandKind::eDstReg, ir::ScalarType::eUnknown },
  }} },
  /* Pow */
  { {{
    { OperandKind::eDstReg, ir::ScalarType::eF32 },
    { OperandKind::eSrcReg, ir::ScalarType::eF32 },
    { OperandKind::eSrcReg, ir::ScalarType::eF32 },
  }} },
  /* Crs */
  { {{
    { OperandKind::eDstReg, ir::ScalarType::eF32 },
    { OperandKind::eSrcReg, ir::ScalarType::eF32 },
    { OperandKind::eSrcReg, ir::ScalarType::eF32 },
  }} },
  /* Sgn */
  { {{
    { OperandKind::eDstReg, ir::ScalarType::eF32 },
    { OperandKind::eSrcReg, ir::ScalarType::eF32 },
    { OperandKind::eSrcReg, ir::ScalarType::eF32 },
    { OperandKind::eSrcReg, ir::ScalarType::eF32 },
  }} },
  /* Abs */
  { {{
    { OperandKind::eDstReg, ir::ScalarType::eF32 },
    { OperandKind::eSrcReg, ir::ScalarType::eF32 },
  }} },
  /* Nrm */
  { {{
    { OperandKind::eDstReg, ir::ScalarType::eF32 },
    { OperandKind::eSrcReg, ir::ScalarType::eF32 },
  }} },
  /* SinCos */
  { {{
    { OperandKind::eDstReg, ir::ScalarType::eF32 },
    { OperandKind::eSrcReg, ir::ScalarType::eF32 },
  }} },
  /* Rep */
  { {{
    { OperandKind::eSrcReg, ir::ScalarType::eI32 },
  }} },
  /* EndRep */
{ },
  /* If */
{ {{
  { OperandKind::eSrcReg, ir::ScalarType::eBool },
  }} },
  /* IfC */
  { {{
    { OperandKind::eSrcReg, ir::ScalarType::eF32 },
    { OperandKind::eSrcReg, ir::ScalarType::eF32 },
  }} },
  /* Else */
  { },
  /* EndIf */
  { },
  /* Break */
  { },
  /* BreakC */
  { {{
    { OperandKind::eSrcReg, ir::ScalarType::eF32 },
    { OperandKind::eSrcReg, ir::ScalarType::eF32 },
  }} },
  /* Mova */
  { {{
    { OperandKind::eDstReg, ir::ScalarType::eI32 },
    { OperandKind::eSrcReg, ir::ScalarType::eUnknown },
  }} },
  /* DefB */
  { {{
    { OperandKind::eDstReg, ir::ScalarType::eBool },
    { OperandKind::eImm32, ir::ScalarType::eBool },
  }} },
  /* DefI */
  { {{
    { OperandKind::eDstReg, ir::ScalarType::eI32 },
    { OperandKind::eImm32, ir::ScalarType::eI32 },
  }} },

  /* TexCrd. Same opcode as the SM<1.4 instruction 'texcoord'. */
  { {{
    { OperandKind::eDstReg, ir::ScalarType::eF32 },
    { OperandKind::eSrcReg, ir::ScalarType::eF32 },
  }} },
  /* TexKill */
  { {{
    { OperandKind::eDstReg, ir::ScalarType::eF32 },
  }} },
  /* TexLd. Same opcode as the SM<1.4 instruction 'tex'. */
  { {{
    { OperandKind::eDstReg, ir::ScalarType::eF32 },
    { OperandKind::eSrcReg, ir::ScalarType::eF32 }, // Only on SM2+
    { OperandKind::eSrcReg, ir::ScalarType::eSampler }, // Only on SM2+
  }} },
  /* TexBem */
  { {{
    { OperandKind::eDstReg, ir::ScalarType::eF32 },
    { OperandKind::eSrcReg, ir::ScalarType::eF32 },
  }} },
  /* TexBemL */
  { {{
    { OperandKind::eDstReg, ir::ScalarType::eF32 },
    { OperandKind::eSrcReg, ir::ScalarType::eF32 },
  }} },
  /* TexReg2Ar */
  { {{
    { OperandKind::eDstReg, ir::ScalarType::eF32 },
    { OperandKind::eSrcReg, ir::ScalarType::eF32 },
  }} },
  /* TexReg2Gb */
  { {{
    { OperandKind::eDstReg, ir::ScalarType::eF32 },
    { OperandKind::eSrcReg, ir::ScalarType::eF32 },
  }} },
  /* TexM3x2Pad */
  { {{
    { OperandKind::eDstReg, ir::ScalarType::eF32 },
    { OperandKind::eSrcReg, ir::ScalarType::eF32 },
  }} },
  /* TexM3x2Tex */
  { {{
    { OperandKind::eDstReg, ir::ScalarType::eF32 },
    { OperandKind::eSrcReg, ir::ScalarType::eF32 },
  }} },
  /* TexM3x3Pad */
  { {{
    { OperandKind::eDstReg, ir::ScalarType::eF32 },
    { OperandKind::eSrcReg, ir::ScalarType::eF32 },
  }} },
  /* TexM3x3Tex */
  { {{
    { OperandKind::eDstReg, ir::ScalarType::eF32 },
    { OperandKind::eSrcReg, ir::ScalarType::eF32 },
  }} },
  /* Reserved0 */
{ },
  /* TexM3x3Spec */
  { {{
    { OperandKind::eDstReg, ir::ScalarType::eF32 },
    { OperandKind::eSrcReg, ir::ScalarType::eF32 },
    { OperandKind::eSrcReg, ir::ScalarType::eF32 },
  }} },
  /* TexM3x3VSpec */
  { {{
    { OperandKind::eDstReg, ir::ScalarType::eF32 },
    { OperandKind::eSrcReg, ir::ScalarType::eF32 },
  }} },
  /* ExpP */
  { {{
    { OperandKind::eDstReg, ir::ScalarType::eF32 },
    { OperandKind::eSrcReg, ir::ScalarType::eF32 },
  }} },
  /* LogP */
  { {{
    { OperandKind::eDstReg, ir::ScalarType::eF32 },
    { OperandKind::eSrcReg, ir::ScalarType::eF32 },
  }} },
  /* Cnd */
  { {{
    { OperandKind::eDstReg, ir::ScalarType::eF32 },
    { OperandKind::eSrcReg, ir::ScalarType::eF32 },
    { OperandKind::eSrcReg, ir::ScalarType::eF32 },
    { OperandKind::eSrcReg, ir::ScalarType::eF32 },
  }} },
  /* Def */
  { {{
    { OperandKind::eDstReg, ir::ScalarType::eF32 },
    { OperandKind::eImm32, ir::ScalarType::eF32 },
  }} },
  /* TexReg2Rgb */
  { {{
    { OperandKind::eDstReg, ir::ScalarType::eF32 },
    { OperandKind::eSrcReg, ir::ScalarType::eF32 },
  }} },
  /* TexDp3Tex */
  { {{
    { OperandKind::eDstReg, ir::ScalarType::eF32 },
    { OperandKind::eSrcReg, ir::ScalarType::eF32 },
  }} },
  /* TexM3x2Depth */
  { {{
    { OperandKind::eDstReg, ir::ScalarType::eF32 },
    { OperandKind::eSrcReg, ir::ScalarType::eF32 },
  }} },
  /* TexDp3 */
  { {{
    { OperandKind::eDstReg, ir::ScalarType::eF32 },
    { OperandKind::eSrcReg, ir::ScalarType::eF32 },
  }} },
  /* TexM3x3 */
  { {{
    { OperandKind::eDstReg, ir::ScalarType::eF32 },
    { OperandKind::eSrcReg, ir::ScalarType::eF32 },
  }} },
  /* TexDepth */
  { {{
    { OperandKind::eDstReg, ir::ScalarType::eF32 },
    { OperandKind::eSrcReg, ir::ScalarType::eF32 },
  }} },
  /* Cmp */
  { {{
    { OperandKind::eDstReg, ir::ScalarType::eBool },
    { OperandKind::eSrcReg, ir::ScalarType::eF32 },
    { OperandKind::eSrcReg, ir::ScalarType::eF32 },
    { OperandKind::eSrcReg, ir::ScalarType::eF32 },
  }} },
  /* Bem */
  { {{
    { OperandKind::eDstReg, ir::ScalarType::eF32 },
    { OperandKind::eSrcReg, ir::ScalarType::eF32 },
    { OperandKind::eSrcReg, ir::ScalarType::eF32 },
  }} },
  /* Dp2Add */
  { {{
    { OperandKind::eDstReg, ir::ScalarType::eF32 },
    { OperandKind::eSrcReg, ir::ScalarType::eF32 },
    { OperandKind::eSrcReg, ir::ScalarType::eF32 },
    { OperandKind::eSrcReg, ir::ScalarType::eF32 },
  }} },
  /* DsX */
  { {{
    { OperandKind::eDstReg, ir::ScalarType::eF32 },
    { OperandKind::eSrcReg, ir::ScalarType::eF32 },
  }} },
  /* DsY */
  { {{
    { OperandKind::eDstReg, ir::ScalarType::eF32 },
    { OperandKind::eSrcReg, ir::ScalarType::eF32 },
  }} },
  /* TexLdd */
  { {{
    { OperandKind::eDstReg, ir::ScalarType::eF32 },
    { OperandKind::eSrcReg, ir::ScalarType::eF32 },
    { OperandKind::eSrcReg, ir::ScalarType::eF32 },
    { OperandKind::eSrcReg, ir::ScalarType::eF32 },
    { OperandKind::eSrcReg, ir::ScalarType::eF32 },
  }} },
  /* SetP */
  { {{
    { OperandKind::eDstReg, ir::ScalarType::eBool },
    { OperandKind::eSrcReg, ir::ScalarType::eF32 },
    { OperandKind::eSrcReg, ir::ScalarType::eF32 },
  }} },
  /* TexLdl */
  { {{
    { OperandKind::eDstReg, ir::ScalarType::eF32 },
    { OperandKind::eSrcReg, ir::ScalarType::eF32 },
    { OperandKind::eSrcReg, ir::ScalarType::eF32 },
  }} },
  /* BreakP */
  { {{
    { OperandKind::eDstReg, ir::ScalarType::eBool },
  }} },
  /* Phase */
  { },
  /* Comment */
  { },
  /* End */
  { }
}};


const InstructionLayout* getInstructionLayout(OpCode op) {
  auto index = uint32_t(op);

  if (index >= uint32_t(OpCode::ePhase)) {
    index -= uint32_t(OpCode::ePhase);
    index += uint32_t(OpCode::eBreakP) + 1u;
  }
  if (index >= uint32_t(OpCode::eTexCrd)) {
    index -= uint32_t(OpCode::eTexCrd);
    index += uint32_t(OpCode::eDefI) + 1u;
  }

  return index < g_instructionLayouts.size()
    ? &g_instructionLayouts[index]
    : nullptr;
}


ShaderInfo::ShaderInfo(util::ByteReader& reader) {
  if (!reader.read(m_token))
    resetOnError();
}


bool ShaderInfo::write(util::ByteWriter& writer) const {
  return writer.write(m_token);
}


void ShaderInfo::resetOnError() {
  *this = ShaderInfo();
}




Operand::Operand(util::ByteReader& reader, const OperandInfo& info, Instruction& op, const ShaderInfo& shaderInfo)
  : Operand(info, RegisterType::eConst) {
  if (!reader.read(m_token)) {
    Logger::err("Failed to read operand token");
    resetOnError();
    return;
  }

  if (info.kind == OperandKind::eImm32) {
    ComponentCount dwordCount = op.getDst().getComponentCount(shaderInfo);
    m_imm[0] = m_token;
    for (uint32_t i = 1; i < uint32_t(dwordCount); i++) {
      if (!reader.read(m_imm[i])) {
        Logger::err("Failed to read immediate value");
        resetOnError();
        return;
      }
    }
    return;
  }

  if ((info.kind == OperandKind::eSrcReg || info.kind == OperandKind::eDstReg) && hasRelativeAddressing()) {
    dxbc_spv_assert(info.kind == OperandKind::eDstReg || info.kind == OperandKind::eSrcReg);

    OperandInfo indexInfo = { };
    indexInfo.kind = OperandKind::eRelAddr;
    Operand relAddrOperand;
    if (hasExtraRelativeAddressingToken(info.kind, shaderInfo)) {
      // VS SM3 supports using the following registers as indices:
      // - a0 the dedicated address register, integer
      // - aL: the loop counter register, integer
      // - cN: one of the float constant registers, float
      // - oN: one of the output registers, float
      // Everything else only supports a subset of this.
      indexInfo.type = ir::ScalarType::eUnknown;
      relAddrOperand = Operand(reader, indexInfo, op, shaderInfo);
      dxbc_spv_assert(!relAddrOperand.hasRelativeAddressing());
      dxbc_spv_assert(relAddrOperand.getModifier() == OperandModifier::None);
    } else {
      // Always use a0
      indexInfo.type = ir::ScalarType::eU32;
      relAddrOperand = Operand(info, RegisterType::eAddr);
    }

    if (!relAddrOperand) {
      Logger::err("Failed to read relative addressing token");
      resetOnError();
      return;
    }

    m_relAddr = op.addOperand(relAddrOperand);
  }
}


void Operand::resetOnError() {
  m_token = DefaultInvalidToken;
}


Instruction::Instruction(util::ByteReader& reader, const ShaderInfo& info) {
  if (!reader.read(m_token))
    return;

  /* Determine operand layout based on the shader
   * model and opcode, and parse the operands. */
  auto layout = getLayout(info);

  /* Determine operand layout based on the shader
   * model and opcode, and parse the operands. */
  uint32_t tokenCount = getOperandTokenCount(info, layout);

  /* Get reader sub-range for the exact number of tokens required */
  auto byteSize = tokenCount * sizeof(uint32_t);
  auto tokenReader = reader.getRangeRelative(0u, byteSize);

  /* Advance base reader to the next instruction. */
  reader.skip(byteSize);

  for (uint32_t i = 0u; i < layout.operands.size(); i++) {
    const auto& operandInfo = layout.operands[i];
    Operand operand(tokenReader, operandInfo, *this, info);

    if (!operand) {
      resetOnError();
      return;
    }

    addOperand(operand);

    if (i == 0u && isPredicated()) {
      OperandInfo predInfo = { };
      predInfo.kind = OperandKind::ePred;
      predInfo.type = ir::ScalarType::eBool;

      Operand predOperand(tokenReader, predInfo, *this, info);

      if (!predOperand) {
        Logger::err("Failed to read predicate token");
        resetOnError();
        return;
      }

      addOperand(predOperand);
    }
  }

  if (getOpCode() == OpCode::eComment) {
    m_comment.emplace(static_cast<const char*>(tokenReader.getData(0u)), tokenReader.getRemaining());
  }

  dxbc_spv_assert(getOpCode() == OpCode::eComment || tokenReader.getRemaining() == 0u);
}


uint32_t Instruction::addOperand(const Operand& operand) {
  uint8_t index = uint8_t(m_operands.size());
  m_operands.push_back(operand);

  switch (operand.getInfo().kind) {
    case OperandKind::eSrcReg:  m_srcOperands.push_back(index); break;
    case OperandKind::eDstReg:
      dxbc_spv_assert(!m_dstOperand.has_value());
      m_dstOperand = index;
      break;
    case OperandKind::eDclReg:
      dxbc_spv_assert(!m_dclOperand.has_value());
      m_dclOperand = index;
      break;
    case OperandKind::eImm32:   m_immOperands.push_back(index); break;
    case OperandKind::eNone:    dxbc_spv_unreachable();
    case OperandKind::ePred:
      dxbc_spv_assert(!m_predOperand.has_value());
      m_predOperand = index;
      break;
    case OperandKind::eRelAddr: break;
  }

  return index;
}


uint32_t Instruction::getOperandTokenCount(const ShaderInfo& info, const InstructionLayout& layout) const {
  OpCode opcode = getOpCode();

  if (opcode == OpCode::eComment)
    return util::bextract(m_token, 16, 15);

  if (opcode == OpCode::eEnd)
    return 0;

  // SM2.0 and above has the length of the op in instruction count baked into it.
  // SM1.4 and below have fixed lengths and run off expectation.
  // Phase does not respect the following rules.
  if (opcode != OpCode::ePhase) {
    if (info.getVersion().first >= 2) {
      return util::bextract(m_token, 24, 4);
    } else {
      // SM1.4 barely supports relative addressing and when relative addressing is used,
      // it always uses the single RelAddr register anyway without further specifying it.
      return layout.operands.size();
    }
  }
  return 0;
}


InstructionLayout Instruction::getLayout(const ShaderInfo& info) const {
  auto layout = getInstructionLayout(getOpCode());

  if (!layout) {
    Logger::err("No layout known for opcode: ", getOpCode());
    return InstructionLayout();
  }

  /* Adjust operand counts for resource declarations */
  auto result = *layout;
  auto [major, minor] = info.getVersion();

  if (getOpCode() == OpCode::eSinCos && major <= 2u) {
    // Shader Model 2 SinCos has two additional src registers
    // that need to have the value of specific constants
    // for some reason.
    result.operands.push_back({ OperandKind::eSrcReg, ir::ScalarType::eF32 });
    result.operands.push_back({ OperandKind::eSrcReg, ir::ScalarType::eF32 });
  }

  if (getOpCode() == OpCode::eTexLd && major < 2u) {
    // TexLd/Tex (same opcode)
    result.operands.pop_back();
    result.operands.pop_back();
    result.operands.pop_back();
    if (minor < 4u) {
      // Tex (SM <1.4) only has the dst register.
      // This destination register has to be a texture register
      // and will contain the texture data afterward.
      // The index of it also determines the texture that will be sampled.
      result.operands.push_back({ OperandKind::eDstReg, ir::ScalarType::eF32 });
    } else if (minor == 4u) {
      // TexLd (SM 1.4) has separate dst/src registers.
      // Dst needs to be a temporary register and the index of it also determines the texture
      // that will be sampled.
      // Src provides the texture coordinates. It can be a texcoord register or a temporary register.
      result.operands.push_back({ OperandKind::eDstReg, ir::ScalarType::eF32 });
      result.operands.push_back({ OperandKind::eSrcReg, ir::ScalarType::eF32 });
    }
  }

  if (getOpCode() == OpCode::eTexCrd && major == 1u && minor < 4u) {
      // TexCrd/TexCoord (same opcode) are only available in SM1.
      // SM2+ can just access texcoord registers directly.
      // TexCoord (SM <1.4) does not take a separate source register.
      // The destination register has to be a texture register
      // and will contain the texture coord afterward.
      result.operands.pop_back();
  }

  if (getOpCode() == OpCode::eMov && major >= 3) {
    // Shader Model <2 doesn't have the mova instruction to move
    // a value to the address register. So the destination
    // *can* have an integer type.
    dxbc_spv_assert(!result.operands.empty());
    dxbc_spv_assert(result.operands[0].kind == OperandKind::eDstReg);
    result.operands[0].type = ir::ScalarType::eUnknown;
  }

  return result;
}


void Instruction::resetOnError() {
  m_token = DefaultInvalidToken;
}




Parser::Parser(util::ByteReader reader) {
  m_info   = ShaderInfo(reader);
  m_reader = util::ByteReader(reader);
}


Instruction Parser::parseInstruction() {
  dxbc_spv_assert(!m_isPastEnd);
  Instruction instruction = Instruction(m_reader, m_info);
  if (instruction.getOpCode() == OpCode::eEnd) {
    m_isPastEnd = true;
  }
  return instruction;
}




std::ostream& operator << (std::ostream& os, ShaderType type) {
  switch (type) {
    case ShaderType::eVertex: os << "vs";  break;
    case ShaderType::ePixel:  os << "ps";   break;
    default:                  os << "unknown"; break;
  }
  return os;
}


std::ostream& operator << (std::ostream& os, const ShaderInfo& shaderInfo) {
  return os << shaderInfo.getType() << "_" << shaderInfo.getVersion().first << "_" << shaderInfo.getVersion().second;
}

}
