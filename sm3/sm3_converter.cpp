#include "sm3_converter.h"

#include "sm3_disasm.h"

namespace dxbc_spv::sm3 {

/* Types in SM3
 * Integer:
 *   - Instructions:
 *     - loop and rep instructions: Both of those load it straight from a constant integer register.
 *   - Registers:
 *     - Constant integer: Only used for loop and rep. Can NOT be used to write the address register or for relative addressing.
 *     - Address register (a0): Only written to with mova (or mov on SM1.1) which takes in a float and has a specific rounding mode.
 *                              Used for relative addressing, can't be read directly.
 *     - Loop counter register (aL): Used to hold the loop index. Automatically populated in loops, can't be written to.
 *
 * Boolean:
 *   - Instructions:
 *     - if bool: Loads straight from a constant boolean register.
 *     - if pred: Loads predicate register.
 *     - callnz: Loads straight from a constant boolean register.
 *     - callnz pred: Loads predicate register.
 *     - break_comp: Loads straight from a constant boolean register.
 *     - breakp pred: Loads predicate register.
 *   - Registers:
 *     - pred: Can't be read directly. Can only be used with if pred or to make operations conditional with predication.
 *             Can only be written to with setp which compares two float registers.
 *             Can be altered with NOT modifier before application.
 *
 * Float:
 *   Everything else. Has partial precision flag in Dst operand.
 */

Converter::Converter(util::ByteReader code, IoSemanticMap& semanticMap, const Options &options)
: m_code(code)
, m_options(options)
, m_semanticMap(semanticMap)
, m_regFile(*this)
, m_ioMap(*this) {

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
      return m_ioMap.handleDclIoVar(builder, op);

    default:
      break;
  }

  return logOpError(op, "Unhandled opcode.");
}


bool Converter::initialize(ir::Builder& builder, ShaderType shaderType) {
  m_ioMap.initialize(builder);
  m_regFile.initialize(builder);

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
  m_ioMap.finalize(builder);

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


ir::SsaDef Converter::applySrcModifiers(ir::Builder& builder, ir::SsaDef def, const Instruction& instruction, const Operand& operand, WriteMask mask) {
  auto modifiedDef = def;

  const auto& op = builder.getOp(def);
  auto type = op.getType().getBaseType(0u);
  bool isUnknown = type.isUnknownType();
  bool partialPrecision = instruction.getDst().isPartialPrecision();

  if (!type.isFloatType()) {
    type = ir::BasicType(partialPrecision ? ir::ScalarType::eMinF16 : ir::ScalarType::eF32, type.getVectorSize());
    modifiedDef = builder.add(ir::Op::ConsumeAs(type, modifiedDef));
  }

  auto mod = operand.getModifier();
  switch (mod) {
    case OperandModifier::eAbs: // abs(r)
    case OperandModifier::eAbsNeg: // -abs(r)
      modifiedDef = builder.add(ir::Op::FAbs(type, modifiedDef));
      if (mod == OperandModifier::eAbsNeg) {
        modifiedDef = builder.add(ir::Op::FNeg(type, modifiedDef));
      }
      break;

    case OperandModifier::eBias: // r - 0.5
    case OperandModifier::eBiasNeg: { // -(r - 0.5)
      auto halfConstOp = ir::Op(ir::OpCode::eConstant, type);
      for (uint32_t i = 0u; i < type.getVectorSize(); i++) {
        halfConstOp.addOperand(0.5f);
      }
      ir::SsaDef halfConst = builder.add(halfConstOp);
      modifiedDef = builder.add(ir::Op::FSub(type, modifiedDef, halfConst));
      if (mod == OperandModifier::eBiasNeg)
        modifiedDef = builder.add(ir::Op::FNeg(type, modifiedDef));
    } break;

    case OperandModifier::eSign: // fma(r, 2.0, -1.0)
    case OperandModifier::eSignNeg: { // -fma(r, 2.0, -1.0)
      auto twoConstOp = ir::Op(ir::OpCode::eConstant, type);
      auto minusOneConstOp = ir::Op(ir::OpCode::eConstant, type);
      for (uint32_t i = 0u; i < type.getVectorSize(); i++) {
        twoConstOp.addOperand(2.0f);
        minusOneConstOp.addOperand(-1.0f);
      }
      auto twoConst = builder.add(twoConstOp);
      auto minusOneConst = builder.add(minusOneConstOp);
      modifiedDef = builder.add(ir::Op::FMad(type, modifiedDef, twoConst, minusOneConst));
      if (mod == OperandModifier::eSignNeg)
        modifiedDef = builder.add(ir::Op::FNeg(type, modifiedDef));
    } break;

    case OperandModifier::eComp: { // 1.0 - r
      auto oneConstOp = ir::Op(ir::OpCode::eConstant, type);
      for (uint32_t i = 0u; i < type.getVectorSize(); i++) {
        oneConstOp.addOperand(1.0f);
      }
      ir::SsaDef oneConst = builder.add(oneConstOp);
      modifiedDef = builder.add(ir::Op::FSub(type, oneConst, modifiedDef));
    } break;

    case OperandModifier::eX2: // r * 2.0
    case OperandModifier::eX2Neg: { // -(r * 2.0)
      auto twoConstOp = ir::Op(ir::OpCode::eConstant, type);
      for (uint32_t i = 0u; i < type.getVectorSize(); i++) {
          twoConstOp.addOperand(2.0f);
      }
      ir::SsaDef twoConst = builder.add(twoConstOp);
      modifiedDef = builder.add(ir::Op::FMul(type, modifiedDef, twoConst));
      if (mod == OperandModifier::eX2Neg) {
        modifiedDef = builder.add(ir::Op::FAbs(type, modifiedDef));
      }
    } break;

    case OperandModifier::eDz:
    case OperandModifier::eDw: {
      // The Dz and Dw modifiers can only be applied to SM1.4 TexLd & TexCrd instructions.
      // Both of those only accept a texture coord register as argument and that is always
      // a float vec4.
      // Adjust the index to the compacted vector.
      uint32_t fullVec4ComponentIndex = mod == OperandModifier::eDz ? 2u : 3u;
      uint32_t componentIndex = 0u;
      for (auto c : mask) {
        if (util::componentFromBit(c) == Component(fullVec4ComponentIndex))
          break;

        componentIndex++;
      }

      auto indexConst = builder.add(ir::Op::Constant(componentIndex));
      auto zComp = builder.add(ir::Op::CompositeExtract(type.getBaseType(), modifiedDef, indexConst));
      modifiedDef = builder.add(ir::Op::FDiv(type, modifiedDef, zComp));
    } break;

    case OperandModifier::eNeg: {
      modifiedDef = builder.add(ir::Op::FNeg(type, modifiedDef));
    } break;

    case OperandModifier::eNot: {
      // TODO: Move this out of here because it can only be used with the predicate register.
      // The NOT modifier can only be used with the predicate register which always stores a boolean.
      dxbc_spv_assert(type.isBoolType());
      modifiedDef = builder.add(ir::Op::BNot(type, modifiedDef));
    }

    case OperandModifier::eNone:
      break;

    default:
      Logger::log(LogLevel::eError, "Unknown source register modifier: ", uint32_t(mod));
      break;
  }

  if (isUnknown) {
    type = ir::BasicType(ir::ScalarType::eUnknown, type.getVectorSize());
    modifiedDef = builder.add(ir::Op::ConsumeAs(type, modifiedDef));
  }

  return modifiedDef;
}


ir::SsaDef Converter::loadSrc(ir::Builder& builder, const Instruction& op, const Operand& operand, WriteMask mask, ir::ScalarType type) {
  auto loadDef = ir::SsaDef();

  switch (operand.getRegisterType()) {
    case RegisterType::eInput:
    case RegisterType::ePixelTexCoord:
    case RegisterType::eMiscType:
      loadDef = m_ioMap.emitLoad(builder, op, operand, mask, type);
      break;

    case RegisterType::eAddr:
    // case RegisterType::eTexture: Same Value
      if (m_parser.getShaderInfo().getType() == ShaderType::eVertex)
        loadDef = m_regFile.emitLoad(builder, operand, mask, type); // RegisterType::eAddr
      else
        loadDef = m_ioMap.emitLoad(builder, op, operand, mask, type); // RegisterType::eTexture
      break;

    case RegisterType::eTemp:
    case RegisterType::eLoop:
    case RegisterType::ePredicate:
      loadDef = m_regFile.emitLoad(builder, operand, mask, type);
      break;


    case RegisterType::eConst:
    case RegisterType::eConst2:
    case RegisterType::eConst3:
    case RegisterType::eConst4:
    case RegisterType::eConstInt:
    case RegisterType::eConstBool:
      logOpError(op, "Shader constants are not implemented yet.");
      break;

    default:
      break;
  }

  if (!loadDef) {
    auto name = makeRegisterDebugName(operand.getRegisterType(), 0u, WriteMask());
    logOpError(op, "Failed to load operand: ", name);
    return loadDef;
  }

  return loadDef;
}


ir::SsaDef Converter::loadSrcModified(ir::Builder& builder, const Instruction& op, const Operand& operand, WriteMask mask, ir::ScalarType type) {
  WriteMask originalMask = mask;
  // If the modifier divides by one of the components, that component needs to be loaded.
  // TODO: Dz & Dw need to get applied before the swizzle!
  if (operand.getModifier() == OperandModifier::eDz) {
    mask |= ComponentBit::eZ;
  } else if (operand.getModifier() == OperandModifier::eDw) {
    mask |= ComponentBit::eW;
  }

  auto value = loadSrc(builder, op, operand, mask, type);
  auto modified = applySrcModifiers(builder, value, op, operand, mask);

  if (originalMask != mask) {
    // Remove the component that was only added for the modifier.
    std::array<ir::SsaDef, 4u> components = { };
    uint32_t componentIndexDst = 0u;
    uint32_t componentIndexSrc = 0u;
    for (auto c : mask) {
      if (originalMask & c) {
        components[componentIndexDst] = builder.add(ir::Op::CompositeExtract(type, modified, builder.add(ir::Op::Constant(componentIndexSrc))));
        componentIndexDst++;
      }
      componentIndexSrc++;
    }
    modified = buildVector(builder, type, componentIndexDst, components.data());
  }

  return modified;
}


ir::SsaDef Converter::broadcastScalar(ir::Builder& builder, ir::SsaDef def, WriteMask mask) {
  if (mask == mask.first())
    return def;

  /* Determine vector type */
  auto type = builder.getOp(def).getType().getBaseType(0u);
  dxbc_spv_assert(type.isScalar());

  type = makeVectorType(type.getBaseType(), mask);

  if (type.isScalar())
    return def;

  /* Create vector */
  ir::Op op(ir::OpCode::eCompositeConstruct, type);

  for (uint32_t i = 0u; i < type.getVectorSize(); i++)
    op.addOperand(def);

  return builder.add(std::move(op));
}


ir::SsaDef Converter::swizzleVector(ir::Builder& builder, ir::SsaDef value, Swizzle swizzle, WriteMask writeMask) {
  const auto& valueOp = builder.getOp(value);
  dxbc_spv_assert(valueOp.getType().isBasicType());

  auto type = valueOp.getType().getBaseType(0u);

  if (type.isScalar())
    return broadcastScalar(builder, value, writeMask);

  /* Extract components one by one and then re-assemble vector */
  util::small_vector<ir::SsaDef, 4u> components;

  for (auto c : writeMask) {
    uint32_t componentIndex = uint8_t(swizzle.map(c));

    components.push_back(builder.add(ir::Op::CompositeExtract(type.getBaseType(),
      value, builder.makeConstant(componentIndex))));
  }

  return buildVector(builder, type.getBaseType(), components.size(), components.data());
}


ir::SsaDef Converter::composite(ir::Builder& builder, ir::BasicType type,
  const ir::SsaDef* components, Swizzle swizzle, WriteMask mask) {
  /* Apply swizzle and mask and get components in the right order. */
  std::array<ir::SsaDef, 4u> scalars = { };

  uint32_t index = 0u;

  for (auto c : mask) {
    auto scalar = components[uint8_t(swizzle.map(c))];
    scalars[index++] = scalar;

    dxbc_spv_assert(scalar);
  }

  /* Component count must match, or be exactly 1 so that
   * we can broadcast a single component. */
  dxbc_spv_assert(index == type.getVectorSize() || index == 1u);

  if (type.isScalar())
    return scalars.at(0u);

  /* Build actual composite op */
  ir::Op op(ir::OpCode::eCompositeConstruct, type);

  for (uint32_t i = 0u; i < type.getVectorSize(); i++)
    op.addOperand(scalars.at(std::min(i, index - 1u)));

  return builder.add(std::move(op));
}


ir::SsaDef Converter::buildVector(ir::Builder& builder, ir::ScalarType scalarType, size_t count, const ir::SsaDef* scalars) {
  if (!count)
    return ir::SsaDef();

  if (count == 1u)
    return scalars[0u];

  ir::BasicType type(scalarType, count);

  ir::Op op(ir::OpCode::eCompositeConstruct, type);

  for (uint32_t i = 0u; i < type.getVectorSize(); i++)
    op.addOperand(scalars[i]);

  return builder.add(std::move(op));
}


ir::SsaDef Converter::extractFromVector(ir::Builder& builder, ir::SsaDef def, uint32_t component) {
  const auto& op = builder.getOp(def);

  if (op.getType().isScalarType())
    return def;

  if (op.getOpCode() == ir::OpCode::eCompositeConstruct)
    return ir::SsaDef(op.getOperand(component));

  return builder.add(ir::Op::CompositeExtract(op.getType().getSubType(component), def, builder.makeConstant(component)));
}


template<typename T>
ir::SsaDef Converter::makeTypedConstant(ir::Builder& builder, ir::BasicType type, T value) {
  ir::Op op(ir::OpCode::eConstant, type);

  ir::Operand scalar = [type, value] {
    switch (type.getBaseType()) {
      case ir::ScalarType::eBool: return ir::Operand(bool(value));
      case ir::ScalarType::eU8:   return ir::Operand(uint8_t(value));
      case ir::ScalarType::eU16:  return ir::Operand(uint16_t(value));
      case ir::ScalarType::eMinU16:
      case ir::ScalarType::eU32:  return ir::Operand(uint32_t(value));
      case ir::ScalarType::eU64:  return ir::Operand(uint64_t(value));
      case ir::ScalarType::eI8:   return ir::Operand(int8_t(value));
      case ir::ScalarType::eI16:  return ir::Operand(int16_t(value));
      case ir::ScalarType::eMinI16:
      case ir::ScalarType::eI32:  return ir::Operand(int32_t(value));
      case ir::ScalarType::eI64:  return ir::Operand(int64_t(value));
      case ir::ScalarType::eF16:  return ir::Operand(util::float16_t(value));
      case ir::ScalarType::eMinF16:
      case ir::ScalarType::eF32:  return ir::Operand(float(value));
      case ir::ScalarType::eF64:  return ir::Operand(double(value));
      default: break;
    }

    dxbc_spv_unreachable();
    return ir::Operand();
  } ();

  for (uint32_t i = 0u; i < type.getVectorSize(); i++)
    op.addOperand(scalar);

  return builder.add(std::move(op));
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
  auto shaderInfo = m_parser.getShaderInfo();

  std::stringstream name;
  name << UnambiguousRegisterType { type, shaderInfo.getType(), shaderInfo.getVersion().first };

  const ConstantInfo* constantInfo = m_ctab.findConstantInfo(type, index);
  if (constantInfo != nullptr && m_options.includeDebugNames) {
    name << "_" << constantInfo->name;
    if (constantInfo->count > 1u) {
      name << index - constantInfo->index;
    }
  } else {
    if (type == RegisterType::eMiscType) {
      name << MiscTypeIndex(index);
    } else if (type == RegisterType::eRasterizerOut) {
      name << RasterizerOutIndex(index);
    } else if (type != RegisterType::eLoop) {
      name << index;
    }
    if (mask) {
      name << "_" << mask;
    }
  }

  return name.str();
}

}
