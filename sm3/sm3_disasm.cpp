#include "sm3_disasm.h"

#include <iostream>
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

  for (const auto& operand : layout.operands) {
    bool inBounds = false;

    stream << (std::exchange(first, false) ? " " : ", ");

    switch (operand.kind) {
      case OperandKind::eDstReg:
        if ((inBounds = op.hasDst()))
          disassembleOperand(stream, op, op.getDst(), info);
        break;

      case OperandKind::eSrcReg:
        if ((inBounds = (nSrc < op.getSrcCount())))
          disassembleOperand(stream, op, op.getSrc(nSrc++), info);
        break;

      case OperandKind::eImm32:
        if ((inBounds = (nImm < op.getImmCount()))) {
          if (!disassembleEnumOperand(stream, op, nImm))
            disassembleOperand(stream, op, op.getImm(nImm), info);

          nImm++;
        }
        break;

      case OperandKind::ePred:
        if ((inBounds = op.hasPred()))
          disassembleOperand(stream, op, op.getPred(), info);
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

  if (modifier == Modifier::Dz || modifier == Modifier::Dw) {
    stream << " / ";
    disassembleRegisterType(stream, arg.getRegisterType(), info);
    disassembleRegisterAddressing(stream, op, arg, info);
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
    case RegisterType::eInput: stream << "i"; break;
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

}
