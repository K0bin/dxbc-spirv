#include "sm3_registers.h"

#include "sm3_converter.h"

namespace dxbc_spv::sm3 {

RegisterFile::RegisterFile(Converter &converter)
: m_converter(converter) {
}

RegisterFile::~RegisterFile() {

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

  for (auto c : swizzle.getReadMask(componentMask)) {
    auto component = componentFromBit(c);

    auto tmpReg = getOrDeclareTemp(builder, regIndex, component);
    ir::SsaDef scalar = builder.add(ir::Op::TmpLoad(ir::ScalarType::eUnknown, tmpReg));

    /* Convert to requested type */
    if (type != ir::ScalarType::eUnknown)
      scalar = builder.add(ir::Op::ConsumeAs(type, scalar));

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

    if (!valueType.isUnknownType())
      scalar = builder.add(ir::Op::ConsumeAs(ir::ScalarType::eUnknown, scalar));

    auto tmpReg = getOrDeclareTemp(builder, regIndex, component);
    builder.add(ir::Op::TmpStore(tmpReg, scalar));
  }

  return true;
}


}
