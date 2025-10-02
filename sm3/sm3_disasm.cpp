#include "sm3_disasm.h"

#include <iostream>
#include <iomanip>
#include <sstream>

#include "sm3_parser.h"

namespace dxbc_spv::sm3 {

void Disassembler::disassembleOp(std::ostream& stream, const Instruction& op, const ShaderInfo& info) {
  if (opEndsNestedBlock(op))
    decrementIndentation();

  emitLineNumber(stream);
  emitIndentation(stream);

  disassembleOpcodeToken(stream, op);

  auto layout = op.getLayout(m_info);

  uint32_t nSrc = 0u;
  uint32_t nImm = 0u;

  bool first = true;
  bool hadDclBefore = false;

  for (const auto& operand : layout.operands) {
    bool inBounds = false;
    bool isFirst = std::exchange(first, false);
    bool hasDclBefore = std::exchange(hadDclBefore, operand.kind == OperandKind::eDclReg);

    if (operand.kind != OperandKind::eDclReg) {
      if (isFirst || hasDclBefore) {
        stream << " ";
      } else {
        stream << ", ";
      }
    }

    switch (operand.kind) {
      case OperandKind::eDstReg:
        if ((inBounds = op.hasDst()))
          disassembleOperand(stream, op, op.getDst(), info);
        break;

      case OperandKind::eSrcReg:
        if ((inBounds = (nSrc < op.getSrcCount())))
          disassembleOperand(stream, op, op.getSrc(nSrc++), info);
        break;

      case OperandKind::eDclReg:
        if ((inBounds = op.hasDcl()))
          disassembleDeclaration(stream, op, op.getDcl());
        break;

      case OperandKind::eImm32:
        if ((inBounds = (nImm < op.getImmCount()))) {
          disassembleImmediate(stream, op.getImm(nImm));

          nImm++;
        }
        break;

      case OperandKind::ePred:
        if ((inBounds = op.hasPred()))
          disassembleOperand(stream, op, op.getPred(), info);
      break;

      default:
        break;
    }

    if (!inBounds)
      stream << "(undefined)";
  }

  if (opBeginsNestedBlock(op))
    incrementIndentation();
}


std::string Disassembler::disassembleOp(const Instruction& op, const ShaderInfo& info) {
  std::stringstream str;
  disassembleOp(str, op, info);
  return str.str();
}


void Disassembler::disassembleOpcodeToken(std::ostream& stream, const Instruction& op) const {
  stream << op.getOpCode();

  // TODO: Coissue
}


void Disassembler::disassembleOperand(std::ostream& stream, const Instruction& op, const Operand& arg, const ShaderInfo& info) const {
  if (op.getOpCode() == OpCode::eDcl) {
    disassembleRegisterType(stream, arg.getRegisterType(), info);
    disassembleRegisterAddressing(stream, op, arg, info);
    return;
  }

  /* Handle modifier */
  auto modifier = arg.getModifier();

  std::string suffix;

  switch (modifier) {
    case Modifier::Neg:
      stream << "-";
      break;

    case Modifier::Bias:
      stream << "(";
      suffix = " - 0.5";
      break;

    case Modifier::BiasNeg:
      stream << "-(";
      suffix = " - 0.5)";
      break;

    case Modifier::Sign:
      stream << "fma(";
      suffix = ", 2.0f, -1.0f)";
      break;

    case Modifier::SignNeg:
      stream << "-fma(";
      suffix = ", 2.0f, -1.0f)";
      break;

    case Modifier::Comp:
      stream << "(1 - ";
      suffix = ")";
      break;

    case Modifier::X2:
      stream << "(";
      suffix = " * 2)";
      break;

    case Modifier::X2Neg:
      stream << "-(";
      suffix = " * 2)";
      break;

    case Modifier::Dz:
      stream << "(";
      suffix = ".z)";
      break;

    case Modifier::Dw:
      stream << "(";
      suffix = ".w)";
      break;

    case Modifier::Abs:
      stream << "abs(";
      suffix = ")";
      break;

    case Modifier::AbsNeg:
      stream << "-abs(";
      suffix = ")";
      break;

    case Modifier::Not:
      stream << "!";
      break;

    default: break;
  }

  disassembleRegisterType(stream, arg.getRegisterType(), info);
  disassembleRegisterAddressing(stream, op, arg, info);
  if (arg.getComponentCount(info) == ComponentCount::e4Component) {
    switch (arg.getSelectionMode()) {
      case SelectionMode::eMask:    stream << "." << arg.getWriteMask(); break;
      case SelectionMode::eSwizzle: stream << "." << arg.getSwizzle(); break;
      case SelectionMode::eSelect1: stream << "." << arg.getSwizzle().x(); break;
    }
  }

  if (modifier == Modifier::Dz || modifier == Modifier::Dw) {
    stream << " / ";
    disassembleRegisterType(stream, arg.getRegisterType(), info);
    disassembleRegisterAddressing(stream, op, arg, info);
    if (arg.getComponentCount(info) == ComponentCount::e4Component) {
      switch (arg.getSelectionMode()) {
        case SelectionMode::eMask:    stream << "." << arg.getWriteMask(); break;
        case SelectionMode::eSwizzle: stream << "." << arg.getSwizzle(); break;
        case SelectionMode::eSelect1: stream << "." << arg.getSwizzle().x(); break;
      }
    }
  }

  stream << suffix;
}

void Disassembler::disassembleRegisterAddressing(std::ostream& stream, const Instruction& op, const Operand& arg, const ShaderInfo& info) const {
  if (arg.getRegisterType() == RegisterType::eMiscType) {
    if (arg.getIndex() == uint32_t(MiscTypeIndex::eMiscTypeFace)) {
      stream << "vFace";
    } else if (arg.getIndex() == uint32_t(MiscTypeIndex::eMiscTypePosition)) {
      stream << "vPosition";
    } else {
      stream << "(unhandled misc register index " << arg.getIndex() << ")";
    }
  } else if (arg.getRegisterType() != RegisterType::eLoop) {
    if (arg.hasRelativeIndexing()) {
      stream << "[";
      if (arg.getIndex() != 0u) {
        stream << arg.getIndex();
        stream << " + ";
      }
      disassembleOperand(stream, op, op.getRawOperand(arg.getRelAddrOperand()), info);
      stream << "]";
    } else {
      stream << arg.getIndex();
    }
  }
}

void Disassembler::disassembleRegisterType(std::ostream& stream, RegisterType registerType, const ShaderInfo& info) const {
  switch (registerType) {
    case RegisterType::eTemp:  stream << "r"; break;
    case RegisterType::eInput: stream << "v"; break;
    case RegisterType::eConst: stream << "c"; break;
    case RegisterType::eAddr:
    // case RegisterType::eTexture: Same value
        stream << (info.getType() == ShaderType::eVertex ? "a" : "t");
    break;
    case RegisterType::eRasterizerOut: stream << "oPos"; break;
    case RegisterType::eAttributeOut: stream << "o"; break;
    case RegisterType::eTexCoordOut:
    // case RegisterType::eOutput: Same value.
      if (info.getVersion().first == 3) {
        stream << "o";
      } else {
        stream << "oT";
      }
      break;
    case RegisterType::eConstBool: stream << "b"; break;
    case RegisterType::eLoop: stream << "aL"; break;
    case RegisterType::eMiscType:
      // Handled when printing the register index
      break;
    case RegisterType::ePredicate: stream << "p"; break;
    case RegisterType::ePixelTexCoord: stream << "t"; break;
    case RegisterType::eConstInt: stream << "i"; break;
    case RegisterType::eColorOut: stream << "oC"; break;
    case RegisterType::eDepthOut: stream << "oDepth"; break;
    case RegisterType::eSampler:  stream << "s"; break;

    case RegisterType::eConst2:
    case RegisterType::eConst3:
    case RegisterType::eConst4:
    case RegisterType::eTempFloat16:
    case RegisterType::eLabel:
    default:
      stream << "(unhandled register type " << uint32_t(registerType) << ")";
      break;
  }
}


void Disassembler::disassembleDeclaration(std::ostream& stream, const Instruction& op, const Operand& operand) const {
  if (op.getDst().getRegisterType() == RegisterType::eSampler) {
    switch (operand.getTextureType()) {
      case TextureType::eTexture2D:   stream << "_2d";   break;
      case TextureType::eTextureCube: stream << "_cube"; break;
      case TextureType::eTexture3D:   stream << "_3d";   break;
    }
    return;
  }

  auto registerType = operand.getRegisterType();
  if (registerType == RegisterType::eOutput
    || registerType == RegisterType::eInput) {
    switch (operand.getUsage()) {
      case Usage::ePosition:
        stream << "_position" << operand.getUsageIndex();
        break;
      case Usage::eBlendWeight:
        stream << "_blendweight" << operand.getUsageIndex();
        break;
      case Usage::eBlendIndices:
        stream << "_blendindices" << operand.getUsageIndex();
        break;
      case Usage::eNormal:
        stream << "_normal" << operand.getUsageIndex();
        break;
      case Usage::ePointSize:
        stream << "_pointSize";
        break;
      case Usage::eTexCoord:
        stream << "_texcoord" << operand.getUsageIndex();
        break;
      case Usage::eTangent:
        stream << "_tangent";
        break;
      case Usage::eBinormal:
        stream << "_binormal";
        break;
      case Usage::eTessFactor:
        stream << "_tessfactor";
        break;
      case Usage::ePositionT:
        stream << "_positiont";
        break;
      case Usage::eColor:
        stream << "_color" << operand.getUsageIndex();
        break;
      case Usage::eFog:
        stream << "_fog";
        break;
      case Usage::eDepth:
        stream << "_depth";
        break;
      case Usage::eSample:
        stream << "_sample" << operand.getUsageIndex();
        break;
      default:
        stream << "_unknown" << operand.getUsageIndex();
    }
  }
}


void Disassembler::disassembleImmediate(std::ostream& stream, const Operand& arg) const {
  /* Determine number of components based on the operand token */
  uint32_t componentCount = arg.getComponentCount(m_info) == ComponentCount::e1Component ? 1u : 4u;

  if (componentCount > 1u)
    stream << '(';

  for (uint32_t i = 0u; i < componentCount; i++) {
    auto type = arg.getInfo().type;

    if (i)
      stream << ", ";

    /* Resolve ambiguous types based on context */
    if (type == ir::ScalarType::eUnknown) {
      auto kind = std::fpclassify(arg.getImmediate<float>(i));

      type = (kind == FP_INFINITE || kind == FP_NORMAL)
        ? ir::ScalarType::eF32
        : ir::ScalarType::eI32;
    }

    switch (type) {
      case ir::ScalarType::eBool:
      case ir::ScalarType::eI32: {
        auto si = arg.getImmediate<int32_t>(i);

        if (std::abs(si) >= 0x100000)
          stream << "0x" << std::hex << std::setw(8u) << std::setfill('0') << uint32_t(si);
        else
          stream << si;
      } break;

      case ir::ScalarType::eI64: {
        auto si = arg.getImmediate<int64_t>(i);

        if (std::abs(si) >= 0x100000)
          stream << "0x" << std::hex << std::setw(16u) << std::setfill('0') << uint64_t(si);
        else
          stream << si;
      } break;

      case ir::ScalarType::eU32: {
        auto ui = arg.getImmediate<uint32_t>(i);

        if (ui >= 0x100000u)
          stream << "0x" << std::hex << std::setw(8u) << std::setfill('0') << ui;
        else
          stream << ui;
      } break;

      case ir::ScalarType::eU64: {
        auto ui = arg.getImmediate<uint64_t>(i);

        if (ui >= 0x100000u)
          stream << "0x" << std::hex << std::setw(16u) << std::setfill('0') << ui;
        else
          stream << ui;
      } break;

      case ir::ScalarType::eF32: {
        auto f = arg.getImmediate<float>(i);

        if (std::isnan(f))
          stream << "0x" << std::hex << arg.getImmediate<uint32_t>(i);
        else
          stream << std::fixed << std::setw(8u) << f << "f";
      } break;

      case ir::ScalarType::eF64: {
        auto f = arg.getImmediate<double>(i);

        if (std::isnan(f))
          stream << "0x" << std::hex << arg.getImmediate<uint64_t>(i) << std::dec;
        else
          stream << std::fixed << std::setw(8u) << f;
      } break;

      default:
        stream << "(unhandled scalar type " << type << ") " << arg.getImmediate<uint32_t>(i);
        break;
    }

    /* Apparently there is no way to reset everything */
    stream << std::setfill(' ') << std::setw(0u) << std::dec;
  }

  if (componentCount > 1u)
    stream << ')';
}


void Disassembler::emitLineNumber(std::ostream& stream) {
  if (!m_options.lineNumbers)
    return;

  stream << std::setw(6u) << std::setfill(' ') << (++m_lineNumber) << ": "
         << std::setw(0u);
}


void Disassembler::emitIndentation(std::ostream& stream) const {
  if (!m_options.indent)
    return;

  for (uint32_t i = 0u; i < 2u * m_indentDepth; i++)
    stream << ' ';
}


void Disassembler::incrementIndentation() {
  m_indentDepth++;
}


void Disassembler::decrementIndentation() {
  if (m_indentDepth)
    m_indentDepth--;
}


  bool Disassembler::opBeginsNestedBlock(const Instruction& op) {
  auto opCode = op.getOpCode();

  return opCode == OpCode::eIf ||
         opCode == OpCode::eElse ||
         opCode == OpCode::eLoop ||
         opCode == OpCode::eRep;
}


  bool Disassembler::opEndsNestedBlock(const Instruction& op) {
  auto opCode = op.getOpCode();

  return opCode == OpCode::eElse ||
         opCode == OpCode::eEndIf ||
         opCode == OpCode::eEndLoop ||
         opCode == OpCode::eEndRep;
}

}
