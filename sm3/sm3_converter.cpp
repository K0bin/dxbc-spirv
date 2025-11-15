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

Converter::Converter(util::ByteReader code,
  IoSemanticMap& semanticMap,
  SpecializationConstantLayout& specConstantsLayout,
  const Options &options)
: m_code(code)
, m_options(options)
, m_semanticMap(semanticMap)
, m_regFile(*this)
, m_ioMap(*this)
, m_resources(*this)
, m_specConstants(specConstantsLayout) {

}

Converter::~Converter() {

}

bool Converter::convertShader(ir::Builder& builder) {
  if (!initParser(m_parser, m_code))
    return false;

  auto shaderType = getShaderInfo().getType();

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
    case OpCode::eComment:
    case OpCode::eNop:
    case OpCode::eReserved0:
    case OpCode::ePhase:
    case OpCode::eEnd:
      return true;

    case OpCode::eDef:
    case OpCode::eDefI:
    case OpCode::eDefB:
      return true;

    case OpCode::eDcl:
      return handleDcl(builder, op);

    case OpCode::eMov:
    case OpCode::eMova:
      return handleMov(builder, op);

    case OpCode::eAdd:
    case OpCode::eSub:
    case OpCode::eExp:
    case OpCode::eFrc:
    case OpCode::eLog:
    case OpCode::eMax:
    case OpCode::eMin:
    case OpCode::eMul:
    case OpCode::eRcp:
    case OpCode::eRsq:
      return handleArithmetic(builder, op);

    case OpCode::eMad:
      return handleMad(builder, op);

    case OpCode::eDp2Add:
    case OpCode::eDp3:
    case OpCode::eDp4:
      return handleDot(builder, op);

    case OpCode::eSlt:
    case OpCode::eSge:
      return handleCompare(builder, op);

    case OpCode::eLit:
      return handleLit(builder, op);

    case OpCode::eM4x4:
    case OpCode::eM4x3:
    case OpCode::eM3x4:
    case OpCode::eM3x3:
    case OpCode::eM3x2:
      return handleMatrixArithmetic(builder, op);

    case OpCode::eTexM3x2Pad:
    case OpCode::eTexM3x3Pad:
      // We don't need to do anything here, these are just padding instructions
      return true;

    case OpCode::eTexCrd:
      return handleTexCoord(builder, op);

    case OpCode::eTexKill:
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
    case OpCode::eTexDepth:
    case OpCode::eTexLdd:
    case OpCode::eTexLdl:
      break;

    case OpCode::eDst:
    case OpCode::eLrp:
    case OpCode::eCall:
    case OpCode::eCallNz:
    case OpCode::eLoop:
    case OpCode::eRet:
    case OpCode::eEndLoop:
    case OpCode::eLabel:
    case OpCode::ePow:
    case OpCode::eCrs:
    case OpCode::eSgn:
    case OpCode::eAbs:
    case OpCode::eNrm:
    case OpCode::eSinCos:
    case OpCode::eRep:
    case OpCode::eEndRep:
    case OpCode::eIf:
    case OpCode::eIfc:
    case OpCode::eElse:
    case OpCode::eEndIf:
    case OpCode::eBreak:
    case OpCode::eBreakC:
    case OpCode::eExpP:
    case OpCode::eLogP:
    case OpCode::eCnd:
    case OpCode::eCmp:
    case OpCode::eBem:
    case OpCode::eDsX:
    case OpCode::eDsY:
    case OpCode::eSetP:
    case OpCode::eBreakP:
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


bool Converter::handleDcl(ir::Builder& builder, const Instruction& op) {
  auto dst = op.getDst();
  switch (dst.getRegisterType()) {
    case RegisterType::eSampler:
      return m_resources.handleDclSampler(builder, op);

    case RegisterType::eAttributeOut:
    case RegisterType::eOutput:
    case RegisterType::eInput:
    case RegisterType::eTexture:
    case RegisterType::ePixelTexCoord:
    case RegisterType::eMiscType:
    case RegisterType::eColorOut:
      return m_ioMap.handleDclIoVar(builder, op);

    default:
      dxbc_spv_unreachable();
      return false;
  }
}


bool Converter::handleMov(ir::Builder& builder, const Instruction& op) {
  /* Mov always moves data from a float register to another float register.
   * There's just one exception: mova moves data from a float register to an address register
   * and rounds the float (RTN) in the process.
   * On SM1.1 mova doesn't exist and the regular mov has that responsibility. */

  const auto& dst = op.getDst();
  const auto& src = op.getSrc(0u);

  dxbc_spv_assert(op.getSrcCount() == 1u);
  dxbc_spv_assert(op.hasDst());

  WriteMask writeMask = dst.getWriteMask(getShaderInfo());

  /* Even when writing the address register, we need to load it as a float to round properly. */
  auto type = dst.isPartialPrecision() ? ir::ScalarType::eMinF16 : ir::ScalarType::eF32;

  auto value = loadSrcModified(builder, op, src, writeMask, type);

  if (!value)
    return false;

  /* Mova writes to the address register. On <= SM2.1 mov *can* write to the address register. */
  if (dst.getRegisterType() == RegisterType::eAddr) {
    std::array<ir::SsaDef, 4u> components = { };
    for (auto c : writeMask) {
      auto componentIndex = uint8_t(util::componentFromBit(c));
      auto componentConstant = builder.makeConstant(componentIndex);
      auto scalarValue = builder.add(ir::Op::CompositeExtract(type, value, componentConstant));

      ir::SsaDef roundedValue;
      if (getShaderInfo().getVersion().first < 2 && getShaderInfo().getVersion().second < 2)
        roundedValue = builder.add(ir::Op::FRound(type, scalarValue, ir::RoundMode::eZero));
      else
        roundedValue = builder.add(ir::Op::FRound(type, scalarValue, ir::RoundMode::eNearestEven));

      components[componentIndex] = builder.add(ir::Op::Cast(ir::ScalarType::eI32, roundedValue));
    }
    value = buildVector(builder, ir::ScalarType::eI32, util::popcnt(uint8_t(writeMask)), components.data());
  }

  return storeDstModifiedPredicated(builder, op, dst, writeMask, value);
}


bool Converter::handleArithmetic(ir::Builder& builder, const Instruction& op) {
  /* All instructions handled here will operate on float vectors of any kind. */
  auto opCode = op.getOpCode();

  dxbc_spv_assert(op.getSrcCount());
  dxbc_spv_assert(op.hasDst());

  /* Instruction type */
  const auto& dst = op.getDst();

  WriteMask writeMask = dst.getWriteMask(getShaderInfo());

  auto scalarType = dst.isPartialPrecision() ? ir::ScalarType::eMinF16 : ir::ScalarType::eF32;
  auto vectorType = makeVectorType(scalarType, writeMask);

  /* Load source operands */
  util::small_vector<ir::SsaDef, 2u> src;

  for (uint32_t i = 0u; i < op.getSrcCount(); i++) {
    auto value = loadSrcModified(builder, op, op.getSrc(i), writeMask, scalarType);

    if (!value)
      return false;

    src.push_back(value);
  }

  ir::Op result = [opCode, vectorType, &src] {
    switch (opCode) {
      case OpCode::eAdd:        return ir::Op::FAdd(vectorType, src.at(0u), src.at(1u));
      case OpCode::eSub:        return ir::Op::FSub(vectorType, src.at(0u), src.at(1u));
      case OpCode::eExp:        return ir::Op::FExp2(vectorType, src.at(0u));
      case OpCode::eFrc:        return ir::Op::FFract(vectorType, src.at(0u));
      case OpCode::eLog:        return ir::Op::FLog2(vectorType, src.at(0u));
      case OpCode::eMax:        return ir::Op::FMax(vectorType, src.at(0u), src.at(1u));
      case OpCode::eMin:        return ir::Op::FMin(vectorType, src.at(0u), src.at(1u));
      case OpCode::eMul:        return ir::Op::FMulLegacy(vectorType, src.at(0u), src.at(1u));
      case OpCode::eRcp:        return ir::Op::FRcp(vectorType, src.at(0u));
      case OpCode::eRsq:        return ir::Op::FRsq(vectorType, src.at(0u));
      default: break;
    }

    dxbc_spv_unreachable();
    return ir::Op();
  } ();

  return storeDstModifiedPredicated(builder, op, dst, writeMask, builder.add(std::move(result)));
}


bool Converter::handleMad(ir::Builder& builder, const Instruction& op) {
  /* All instructions handled here will operate on float vectors of any kind. */
  auto opCode = op.getOpCode();

  dxbc_spv_assert(op.getSrcCount() == 3);
  dxbc_spv_assert(op.hasDst());

  /* Instruction type */
  const auto& dst = op.getDst();

  WriteMask writeMask = dst.getWriteMask(getShaderInfo());

  auto scalarType = dst.isPartialPrecision() ? ir::ScalarType::eMinF16 : ir::ScalarType::eF32;
  auto vectorType = makeVectorType(scalarType, writeMask);

  /* Load source operands */
  auto src0 = loadSrcModified(builder, op, op.getSrc(0u), writeMask, scalarType);
  auto src1 = loadSrcModified(builder, op, op.getSrc(1u), writeMask, scalarType);
  auto src2 = loadSrcModified(builder, op, op.getSrc(2u), writeMask, scalarType);

  auto result = ir::Op::FMadLegacy(vectorType, src0, src1, src2);

  return storeDstModifiedPredicated(builder, op, dst, writeMask, builder.add(std::move(result)));
}


bool Converter::handleDot(ir::Builder& builder, const Instruction& op) {
  /* Dp2/3/4 take two vector operands, produce a scalar, and replicate
   * that in all components included in the destination write mask.
   * Dp2Add takes a third vector operand and adds it.
   * (dst0) Result
   * (src0) First vector
   * (src1) Second vector */
  auto opCode = op.getOpCode();

  dxbc_spv_assert(op.getSrcCount() == 2u);
  dxbc_spv_assert(op.hasDst());

  /* The opcode determines which source components to read,
   * since the write mask can be literally anything. */
  auto readMask = [opCode] {
    switch (opCode) {
    case OpCode::eDp2Add: return util::makeWriteMaskForComponents(2u);
    case OpCode::eDp3: return util::makeWriteMaskForComponents(3u);
    case OpCode::eDp4: return util::makeWriteMaskForComponents(4u);
    default: break;
    }

    dxbc_spv_unreachable();
    return WriteMask();
  } ();

  /* Load source vectors and pass them to the internal dot instruction as they are */
  const auto& dst = op.getDst();

  auto scalarType = dst.isPartialPrecision() ? ir::ScalarType::eMinF16 : ir::ScalarType::eF32;

  auto vectorA = loadSrcModified(builder, op, op.getSrc(0u), readMask, scalarType);
  auto vectorB = loadSrcModified(builder, op, op.getSrc(1u), readMask, scalarType);

  auto result = builder.add(ir::Op::FDotLegacy(scalarType, vectorA, vectorB));

  if (opCode == OpCode::eDp2Add) {
    auto summandC = loadSrcModified(builder, op, op.getSrc(2u), WriteMask(ComponentBit::eX), scalarType);
    result = builder.add(ir::Op::FAdd(scalarType, result, summandC));
  }

  WriteMask writeMask = dst.getWriteMask(getShaderInfo());
  result = broadcastScalar(builder, result, writeMask);
  return storeDstModifiedPredicated(builder, op, dst, writeMask, result);
}


bool Converter::handleCompare(ir::Builder& builder, const Instruction& op) {
  /* All instructions handled here will operate on float vectors of any kind. */
  auto opCode = op.getOpCode();

  dxbc_spv_assert(op.getSrcCount() == 2u);
  dxbc_spv_assert(op.hasDst());

  /* Instruction type */
  const auto& dst = op.getDst();

  WriteMask writeMask = dst.getWriteMask(getShaderInfo());

  auto scalarType = dst.isPartialPrecision() ? ir::ScalarType::eMinF16 : ir::ScalarType::eF32;
  auto boolType = makeVectorType(ir::ScalarType::eBool, writeMask);

  /* Load source operands */
  auto src0 = loadSrcModified(builder, op, op.getSrc(0u), writeMask, scalarType);
  auto src1 = loadSrcModified(builder, op, op.getSrc(1u), writeMask, scalarType);
  if (!src0 || !src1)
    return false;

  ir::Op result;
  if (opCode == OpCode::eSlt)
    result = ir::Op::FLt(boolType, src0, src1);
  else
    result = ir::Op::FGe(boolType, src0, src1);

  return storeDstModifiedPredicated(builder, op, dst, writeMask, builder.add(std::move(result)));
}


bool Converter::handleMatrixArithmetic(ir::Builder& builder, const Instruction& op) {
  /* All instructions handled here will operate on float vectors of any kind. */
  auto opCode = op.getOpCode();

  dxbc_spv_assert(op.getSrcCount() == 2);
  dxbc_spv_assert(op.hasDst());

  uint32_t rowCount;
  uint32_t columnCount;

  switch (opCode) {
    case OpCode::eM3x2:
      columnCount = 3u;
      rowCount = 2u;
      break;
    case OpCode::eM3x3:
      columnCount = 3u;
      rowCount = 3u;
      break;
    case OpCode::eM3x4:
      columnCount = 3u;
      rowCount = 4u;
      break;
    case OpCode::eM4x3:
      columnCount = 4u;
      rowCount = 3u;
      break;
    case OpCode::eM4x4:
      columnCount = 4u;
      rowCount = 4u;
      break;
    default:
      columnCount = 0u;
      rowCount = 0u;
      dxbc_spv_unreachable();
      break;
  }

  /* Instruction type */
  const auto& dst = op.getDst();

  WriteMask writeMask = dst.getWriteMask(getShaderInfo());

  /* Fix the write mask if it writes more components than the matrix has rows. */
  WriteMask maxWriteMask = WriteMask((1u << rowCount) - 1u);
  writeMask &= maxWriteMask;

  auto scalarType = dst.isPartialPrecision() ? ir::ScalarType::eMinF16 : ir::ScalarType::eF32;

  /* Build a write mask that determines how many components we'll load. */
  WriteMask srcMask = WriteMask((1u << columnCount) - 1u);

  /* Load source operands */
  auto src0 = loadSrcModified(builder, op, op.getSrc(0u), srcMask, scalarType);
  Operand src1Operand = op.getSrc(2u);

  std::array<ir::SsaDef, 4u> components = { };
  for (uint32_t i = 0u; i < rowCount; i++) {
    /* Load matrix column */
    auto src1iOperand = src1Operand;
    src1iOperand.setIndex(src1iOperand.getIndex() + i);
    auto src1 = loadSrcModified(builder, op, src1iOperand, srcMask, scalarType);

    /* Calculate vector component */
    components[i] = builder.add(ir::Op::FDotLegacy(scalarType, src0, src1));
  }

  auto result = buildVector(builder, scalarType, rowCount, components.data());
  return storeDstModifiedPredicated(builder, op, dst, writeMask, result);
}


bool Converter::handleLit(ir::Builder& builder, const Instruction& op) {
  const auto& dst = op.getDst();
  WriteMask writeMask = dst.getWriteMask(getShaderInfo());
  auto scalarType = dst.isPartialPrecision() ? ir::ScalarType::eMinF16 : ir::ScalarType::eF32;
  auto src = loadSrcModified(builder, op, op.getSrc(0u), writeMask, scalarType);

  auto xConst = builder.makeConstant(0u);
  auto yConst = builder.makeConstant(1u);
  auto wConst = builder.makeConstant(3u);

  auto srcX = builder.add(ir::Op::CompositeExtract(scalarType, src, xConst));
  auto srcY = builder.add(ir::Op::CompositeExtract(scalarType, src, yConst));
  auto srcW = builder.add(ir::Op::CompositeExtract(scalarType, src, wConst));

  auto power = builder.add(ir::Op::FClamp(scalarType, srcW,
    builder.makeConstant(-127.9961f), builder.makeConstant(127.9961f)));

  auto zeroFConst = builder.makeConstant(0.0f);
  auto oneFConst = builder.makeConstant(1.0f);

  util::small_vector<ir::SsaDef, 4u> components;
  if (writeMask & ComponentBit::eX)
    components.push_back(oneFConst);
  if (writeMask & ComponentBit::eY)
    components.push_back(builder.add(ir::Op::FMax(scalarType, srcX, zeroFConst)));
  if (writeMask & ComponentBit::eZ)
    components.push_back(builder.add(ir::Op::FPow(scalarType, builder.add(ir::Op::FMax(scalarType, srcX, zeroFConst)), power)));
  if (writeMask & ComponentBit::eW)
    components.push_back(oneFConst);

  auto zTestX = builder.add(ir::Op::FGe(ir::ScalarType::eBool, srcX,zeroFConst));
  auto zTestY = builder.add(ir::Op::FGe(ir::ScalarType::eBool, srcY, zeroFConst));
  auto zTest = builder.add(ir::Op::BAnd(ir::ScalarType::eBool, zTestX, zTestY));

  if (components.size() > 2u)
    components[2u] = builder.add(ir::Op::Select(scalarType, zTest, components[2u], zeroFConst));

  auto result = buildVector(builder, scalarType, util::popcnt(uint8_t(writeMask)), components.data());
  return storeDstModifiedPredicated(builder, op, dst, writeMask, result);
}


bool Converter::handleTexCoord(ir::Builder &builder, const Instruction &op) {
  const auto& dst = op.getDst();
  WriteMask writeMask = dst.getWriteMask(getShaderInfo());
  auto scalarType = dst.isPartialPrecision() ? ir::ScalarType::eMinF16 : ir::ScalarType::eF32;

  if (getShaderInfo().getVersion().first >= 2 || getShaderInfo().getVersion().second >= 4) {
    /* TexCrd (SM 1.4) */
    auto src = loadSrcModified(builder, op, op.getSrc(0u), writeMask, scalarType);
    return storeDstModifiedPredicated(builder, op, dst, writeMask, src);

  } else {
    /* TexCoord (SM 1.1 - 1.3) */
    ir::BasicType vectorType = makeVectorType(scalarType, writeMask);
    auto src = loadSrc(builder, op, op.getSrc(0u), writeMask, op.getSrc(0u).getSwizzle(getShaderInfo()), scalarType);

    /* Saturate */
    src = builder.add(ir::Op::FClamp(
      vectorType,
      src,
      makeTypedConstant(builder, vectorType, 0.0f),
      makeTypedConstant(builder, vectorType, 1.0f)
    ));

    /* w = 1.0 */
    if (writeMask & ComponentBit::eW)
      src = builder.add(ir::Op::CompositeInsert(vectorType, src, builder.makeConstant(3u), builder.makeConstant(1.0f)));

    return storeDstModifiedPredicated(builder, op, dst, writeMask, src);
  }
}


  bool Converter::handleSample(ir::Builder& builder, const Instruction& op) {
  const auto& dst = op.getDst();
  const auto& texture = op.getSrc(0u);
  auto resourceInfo = m_resources.getResourceInfo(op.getSrc(0u));
  if (resourceInfo == nullptr) {
    dxbc_spv_assert(getShaderInfo().getVersion().first < 2u);
    resourceInfo = m_resources.dclSamplerAndAllTextureTypes(builder, texture.getIndex());
  }
  dxbc_spv_assert(resourceInfo != nullptr);
  auto samplerInfo = m_resources.emitDescriptorLoad(builder, resourceInfo, std::nullopt);

  if (getShaderInfo().getVersion().first >= 2) {
    auto specConstTextureType = specConstTextureTypeFromResourceKind(resourceInfo->kind);
    auto textureInfo = m_resources.emitDescriptorLoad(builder, resourceInfo, std::make_optional(specConstTextureType));
  } else {
    auto texture2DInfo   = m_resources.emitDescriptorLoad(builder, resourceInfo, std::make_optional(SpecConstTextureType::eTexture2D));
    auto textureCubeInfo = m_resources.emitDescriptorLoad(builder, resourceInfo, std::make_optional(SpecConstTextureType::eTextureCube));
    auto texture3DInfo   = m_resources.emitDescriptorLoad(builder, resourceInfo, std::make_optional(SpecConstTextureType::eTexture3D));
  }
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
      uint32_t fullVec4ComponentIndex = mod == OperandModifier::eDz ? 2u : 3u;
      uint32_t componentIndex = 0u;
      for (auto c : mask) {
        if (util::componentFromBit(c) == Component(fullVec4ComponentIndex))
          break;

        componentIndex++;
      }

      auto indexConst = builder.makeConstant(componentIndex);
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


ir::SsaDef Converter::applyDstModifiers(ir::Builder& builder, ir::SsaDef def, const Instruction& instruction, const Operand& operand, WriteMask writeMask) {
  ir::Op op = builder.getOp(def);
  auto type = op.getType().getBaseType(0u);
  int8_t shift = operand.getShift();

  /* Handle unknown type */
  if (type.isUnknownType() && (shift != 0 || operand.isSaturated())) {
    type = makeVectorType(ir::ScalarType::eF32, writeMask);
    def = builder.add(ir::Op::ConsumeAs(type, def));
  }

  /* Apply shift */
  if (shift != 0) {
    if (!type.isFloatType()) {
      logOpMessage(LogLevel::eWarn, instruction, "Shift applied to a non-float result.");
    }

    float shiftAmount = shift < 0
            ? 1.0f / (1 << -shift)
            : float(1 << shift);

    def = builder.add(ir::Op::FMulLegacy(type, def, makeTypedConstant(builder, type, shiftAmount)));
  }

  /* Saturate dst */
  if (operand.isSaturated()) {
    if (!type.isFloatType()) {
      logOpMessage(LogLevel::eWarn, instruction, "Saturation applied to a non-float result.");
    }

    def = builder.add(ir::Op::FClamp(type, def,
      makeTypedConstant(builder, type, 0.0f),
      makeTypedConstant(builder, type, 1.0f)));
  }

  return def;
}


ir::SsaDef Converter::loadSrc(ir::Builder& builder, const Instruction& op, const Operand& operand, WriteMask mask, Swizzle swizzle, ir::ScalarType type) {
  auto loadDef = ir::SsaDef();

  switch (operand.getRegisterType()) {
    case RegisterType::eInput:
    case RegisterType::ePixelTexCoord:
    case RegisterType::eMiscType:
      loadDef = m_ioMap.emitLoad(builder, op, operand, mask, swizzle, type);
      break;

    case RegisterType::eAddr:
    // case RegisterType::eTexture: Same Value
      if (getShaderInfo().getType() == ShaderType::eVertex)
        loadDef = m_regFile.emitLoad(builder, operand, mask, type); // RegisterType::eAddr
      else
        loadDef = m_ioMap.emitLoad(builder, op, operand, mask, swizzle, type); // RegisterType::eTexture
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
  Swizzle swizzle = operand.getSwizzle(getShaderInfo());
  Swizzle originalSwizzle = swizzle;
  WriteMask originalMask = mask;
  // If the modifier divides by one of the components, that component needs to be loaded.

  // Dz & Dw need to get applied before the swizzle!
  // So if those are used, we load the whole vector and swizzle afterward.
  bool hasPreSwizzleModifier = operand.getModifier() == OperandModifier::eDz || operand.getModifier() == OperandModifier::eDw;
  if (hasPreSwizzleModifier) {
    mask = WriteMask(ComponentBit::eAll);
    swizzle = Swizzle::identity();
  }

  auto value = loadSrc(builder, op, operand, mask, swizzle, type);
  auto modified = applySrcModifiers(builder, value, op, operand, mask);

  if (hasPreSwizzleModifier) {
    modified = swizzleVector(builder, modified, originalSwizzle, originalMask);
  }

  return modified;
}


bool Converter::storeDst(ir::Builder& builder, const Instruction& op, const Operand& operand, WriteMask writeMask, ir::SsaDef value) {
  switch (operand.getRegisterType()) {
    case RegisterType::eTemp:
      return m_regFile.emitStore(builder, operand, writeMask, value);

    case RegisterType::eOutput:
    case RegisterType::eRasterizerOut:
    case RegisterType::eAttributeOut:
    case RegisterType::eColorOut:
    case RegisterType::eDepthOut:
      return m_ioMap.emitStore(builder, op, operand, writeMask, value);

    default: {
      auto name = makeRegisterDebugName(operand.getRegisterType(), 0u, writeMask);
      logOpError(op, "Unhandled destination operand: ", name);
    } return false;
  }
}


bool Converter::storeDstModifiedPredicated(ir::Builder& builder, const Instruction& op, const Operand& operand, WriteMask writeMask, ir::SsaDef value) {
  value = applyDstModifiers(builder, value, op, operand, writeMask);
  return storeDst(builder, op, operand, writeMask, value);
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

  Disassembler disasm(options, getShaderInfo());
  auto instruction = disasm.disassembleOp(op, m_ctab);

  Logger::log(severity, "Line ", m_instructionCount, ": ", instruction);
}


std::string Converter::makeRegisterDebugName(RegisterType type, uint32_t index, WriteMask mask) const {
  auto shaderInfo = getShaderInfo();

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
