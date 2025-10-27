#include "sm3_registers.h"

#include "sm3_converter.h"

namespace dxbc_spv::sm3 {

RegisterFile::RegisterFile(Converter &converter)
: m_converter(converter) {
}

RegisterFile::~RegisterFile() {

}

void RegisterFile::initialize(ir::Builder& builder) {
  uint32_t addressRegisterComponents = m_converter.getShaderInfo().getVersion().first >= 2u ? 4u : 1u;
  for (uint32_t i = 0u; i < addressRegisterComponents; i++) {
    m_a0Reg[i] = builder.add(ir::Op::DclTmp(ir::ScalarType::eI32, m_converter.getEntryPoint()));
  }

  m_aLReg = builder.add(ir::Op::DclTmp(ir::ScalarType::eI32, m_converter.getEntryPoint()));

  m_pReg = builder.add(ir::Op::DclTmp(ir::ScalarType::eBool, m_converter.getEntryPoint()));
}

ir::SsaDef RegisterFile::getOrDeclareTemp(ir::Builder& builder, uint32_t index, Component component) {
  uint32_t tempIndex = 4u * index + uint8_t(component);

  if (tempIndex >= m_rRegs.size())
    m_rRegs.resize(tempIndex + 1);

  if (!m_rRegs[tempIndex])
    m_rRegs[tempIndex] = builder.add(ir::Op::DclTmp(ir::ScalarType::eUnknown, m_converter.getEntryPoint()));

  return m_rRegs[tempIndex];
}

ir::SsaDef RegisterFile::emitLoad(
  ir::Builder& builder,
  const Operand& operand,
  WriteMask componentMask,
  ir::ScalarType type
) {
  auto swizzle = operand.getSwizzle(m_converter.getShaderInfo());
  auto returnType = m_converter.makeVectorType(type, componentMask);

  auto regIndex = operand.getIndex();

  std::array<ir::SsaDef, 4u> components = { };

  ir::SsaDef scalar;
  for (auto c : swizzle.getReadMask(componentMask)) {
    auto component = componentFromBit(c);

    switch (operand.getRegisterType()) {
      case RegisterType::eTemp: {
        auto tmpReg = getOrDeclareTemp(builder, regIndex, component);
        scalar = builder.add(ir::Op::TmpLoad(ir::ScalarType::eUnknown, tmpReg));

        /* Convert to requested type */
        if (type != ir::ScalarType::eUnknown)
          scalar = builder.add(ir::Op::ConsumeAs(type, scalar));
      } break;

      case RegisterType::eAddr: {
        dxbc_spv_assert(m_converter.getShaderInfo().getVersion().first >= 2u
            || c == ComponentBit::eX);
        dxbc_spv_assert(type == ir::ScalarType::eI32);
        dxbc_spv_assert(m_a0Reg[uint8_t(component)]);
        scalar = builder.add(ir::Op::TmpLoad(ir::ScalarType::eI32, m_a0Reg[regIndex]));
      } break;

      case RegisterType::eLoop: {
        dxbc_spv_assert(c == ComponentBit::eX);
        dxbc_spv_assert(type == ir::ScalarType::eI32);
        scalar = builder.add(ir::Op::TmpLoad(ir::ScalarType::eI32, m_aLReg));
      } break;

      case RegisterType::ePredicate: {
        dxbc_spv_assert(c == ComponentBit::eX);
        dxbc_spv_assert(type == ir::ScalarType::eBool);
        scalar = builder.add(ir::Op::TmpLoad(ir::ScalarType::eBool, m_pReg));
      } break;

      case RegisterType::eConst:
      case RegisterType::eConst2:
      case RegisterType::eConst3:
      case RegisterType::eConst4:
        dxbc_spv_assert(type == ir::ScalarType::eF32);
        break;

      case RegisterType::eConstBool:
        dxbc_spv_assert(type == ir::ScalarType::eBool);
        break;

      case RegisterType::eConstInt:
        dxbc_spv_assert(type == ir::ScalarType::eI32);
        break;

      default:
        dxbc_spv_unreachable();
        return ir::SsaDef();
    }


    components[uint8_t(component)] = scalar;
  }

  return m_converter.composite(builder, returnType, components.data(), swizzle, componentMask);
}


bool RegisterFile::emitStore(
          ir::Builder&            builder,
    const Operand&                operand,
          ir::SsaDef              value) {
  auto writeMask = operand.getWriteMask(m_converter.getShaderInfo());
  const auto& valueDef = builder.getOp(value);
  auto valueType = valueDef.getType().getBaseType(0u);

  auto regIndex = operand.getIndex();

  uint32_t componentIndex = 0u;

  for (auto c : writeMask) {
    auto component = componentFromBit(c);

    /* Extract scalar and 'convert' to unknown type */
    auto scalar = m_converter.extractFromVector(builder, value, componentIndex++);

    ir::SsaDef reg;
    switch (operand.getRegisterType()) {
      case RegisterType::eTemp: {
        reg = getOrDeclareTemp(builder, regIndex, component);

        if (!valueType.isUnknownType())
          scalar = builder.add(ir::Op::ConsumeAs(ir::ScalarType::eUnknown, scalar));
      } break;

      case RegisterType::eAddr: {
        dxbc_spv_assert(m_converter.getShaderInfo().getVersion().first >= 2u || c == ComponentBit::eX);

        if (!valueType.isIntType()) {
          // a0 can be written to using the mova instruction on SM2+
          // or the mov instruction on SM1.1.

          if (valueType.isUnknownType()) {
            scalar = builder.add(ir::Op::ConsumeAs(ir::ScalarType::eF32, scalar));
          }
          scalar = builder.add(ir::Op::FRound(ir::ScalarType::eF32, scalar, ir::RoundMode::eNearestEven));
          scalar = builder.add(ir::Op::ConvertFtoI(ir::ScalarType::eI32, scalar));
        }
        reg = m_a0Reg[uint8_t(component)];
      } break;
    }

    builder.add(ir::Op::TmpStore(reg, scalar));
  }

  return true;
}


}
