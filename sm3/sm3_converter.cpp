#include "sm3_converter.h"

#include "sm3_disasm.h"

namespace dxbc_spv::sm3 {

Converter::Converter(util::ByteReader code, const Options &options)
: m_code(code)
, m_options(options) {

}

Converter::~Converter() {

}

bool Converter::convertShader(ir::Builder& builder) {
  if (!initParser(m_parser, m_code))
    return false;

  auto shaderType = m_parser.getShaderInfo().getType();

  initialize(builder, shaderType);

  while (m_parser) {
    Instruction op = m_parser.parseInstruction();

    if (!op || !convertInstruction(builder, op))
      return false;
  }

  return finalize(builder, shaderType);
}


bool Converter::convertInstruction(ir::Builder& builder, const Instruction& op) {
  auto opCode = op.getOpCode();

  /* Increment instruction counter for debug purposes */
  m_instructionCount += 1u;

  switch (opCode) {
    case OpCode::eNop:
      return true;

    case OpCode::eDcl:
      return m_ioMap.handleDclIoVar(builder, op, m_parser.getShaderInfo());
  }

  return logOpError(op, "Unhandled opcode.");
}


bool Converter::initialize(ir::Builder& builder, ShaderType shaderType) {
  /* A valid debug namee is required for the main function */
  m_entryPoint.mainFunc = builder.add(ir::Op::Function(ir::ScalarType::eVoid));
  builder.add(ir::Op::FunctionEnd());
  builder.add(ir::Op::DebugName(m_entryPoint.mainFunc, "main"));

  /* Emit entry point instruction as the first instruction of the
   * shader. This is technically not needed, but makes things more
   * readable. */
  auto stage = resolveShaderStage(shaderType);

  auto entryPointOp = ir::Op::EntryPoint(m_entryPoint.mainFunc, stage);

  m_entryPoint.def = builder.addAfter(ir::SsaDef(), std::move(entryPointOp));

  /* Need to emit the shader name regardless of debug names as well */
  if (m_options.name)
    builder.add(ir::Op::DebugName(m_entryPoint.def, m_options.name));

  /* Set cursor to main function so that instructions will be emitted
   * in the correct location */
  builder.setCursor(m_entryPoint.mainFunc);
  return true;
}


bool Converter::finalize(ir::Builder& builder, ShaderType shaderType) {
  return true;
}


bool Converter::initParser(Parser& parser, util::ByteReader reader) {
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


void Converter::logOp(LogLevel severity, const Instruction& op) const {
  Disassembler::Options options = { };
  options.indent = false;
  options.lineNumbers = false;

  Disassembler disasm(options, m_parser.getShaderInfo());
  auto instruction = disasm.disassembleOp(op);

  Logger::log(severity, "Line ", m_instructionCount, ": ", instruction);
}



std::string Converter::makeRegisterDebugName(RegisterType type, uint32_t index, WriteMask mask) const {
  auto stage = m_parser.getShaderInfo().getType();

  std::stringstream name;

  switch (type) {
    case RegisterType::eTemp:               name << "r" << index << (mask ? "_" : "") << mask; break;
    case RegisterType::eInput:              name << "v" << index << (mask ? "_" : "") << mask; break;
    case RegisterType::eOutput:
    // case RegisterType::eOutput: Same value.
      if (m_parser.getShaderInfo().getVersion().first == 3) {
        name << "o" << index << (mask ? "_" : "") << mask;
      } else {
        name << "oT" << index << (mask ? "_" : "") << mask;
      }
      break;
    case RegisterType::eSampler:            name << "s" << index; break;
    case RegisterType::eLabel:              name << "l" << index; break;
    case RegisterType::eConst:              name << "c" << index << (mask ? "_" : "") << mask; break;
    case RegisterType::eAddr:
    // case RegisterType::eTexture: Same value
      name << (stage == ShaderType::eVertex ? "a" : "t");
      break;
    case RegisterType::eRasterizerOut:      name << "oPos" << (mask ? "_" : "") << mask; break;
    case RegisterType::eAttributeOut:       name << "o" << index << (mask ? "_" : "") << mask; break;
    case RegisterType::eConstInt:           name << "i" << index << (mask ? "_" : "") << mask; break;
    case RegisterType::eColorOut:           name << "oC" << index << (mask ? "_" : "") << mask; break;
    case RegisterType::eDepthOut:           name << "oDepth"; break;
    case RegisterType::eConst2:             name << "c" << index << (mask ? "_" : "") << mask; break;
    case RegisterType::eConst3:             name << "c" << index << (mask ? "_" : "") << mask; break;
    case RegisterType::eConst4:             name << "c" << index << (mask ? "_" : "") << mask; break;
    case RegisterType::eConstBool:          name << "b" << index; break;
    case RegisterType::eLoop:               name << "aL"; break;
    case RegisterType::eTempFloat16:        name << "half" << index << (mask ? "_" : "") << mask; break;
    case RegisterType::ePredicate:          name << "p"; break;
    case RegisterType::ePixelTexCoord:      name << "t" << index << (mask ? "_" : "") << mask; break;
    case RegisterType::eMiscType:
      if (index == uint32_t(MiscTypeIndex::eMiscTypeFace)) {
        name << "vFace";
      } else if (index == uint32_t(MiscTypeIndex::eMiscTypeFace)) {
        name << "vPosition" << (mask ? "_" : "") << mask;
      } else {
        name << "misc" << index << (mask ? "_" : "") << mask;
      }
      break;

    default: name << "reg_" << uint32_t(type) << "_" << index << (mask ? "_" : "") << mask;
  }

  return name.str();
}

}
