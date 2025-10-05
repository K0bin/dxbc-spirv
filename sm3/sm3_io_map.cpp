#include <algorithm>

#include "sm3_converter.h"
#include "sm3_io_map.h"

#include "../util/util_log.h"

namespace dxbc_spv::sm3 {

IoMap::IoMap(Converter& converter)
: m_converter(converter) {

}


IoMap::~IoMap() {

}

bool IoMap::handleDclIoVar(Builder& builder, const Instruction& op) {
  const auto& dst = op.getDst();
  bool isInput = dst.getRegisterType() == RegisterType::eInput;

  auto opCode = isInput
    ? ir::OpCode::eDclInput
    : ir::OpCode::eDclOutput;

  WriteMask componentMask = dst.getWriteMask();
  while (componentMask) {
    WriteMask nextMask = util::extractConsecutiveComponents(componentMask);

    ir::Type type(ir::ScalarType::eF32, util::popcnt(uint8_t(nextMask)));

    auto declaration = ir::Op(opCode, type)
      .addOperand(m_converter.getEntryPoint())
      .addOperand(dst.getIndex())
      .addOperand(util::tzcnt(uint8_t(nextMask)));

    auto& mapping = m_variables.emplace_back();
    mapping.regType = dst.getRegisterType();
    mapping.regIndex = dst.getIndex();
    mapping.componentMask = nextMask;
    mapping.baseType = declaration.getType();
    mapping.baseDef = builder.add(std::move(declaration));

    componentMask -= nextMask;
  }
}


void IoMap::emitDebugName(ir::Builder& builder, ir::SsaDef def, RegisterType type, uint32_t index, WriteMask mask, const Operand& dclOperand) const {
  if (!m_converter.m_options.includeDebugNames)
    return;

  std::string name = m_converter.makeRegisterDebugName(type, index, mask);

  if (sigEntry) {
    name = sigEntry->getSemanticName();

    if (sigEntry->getSemanticIndex())
      name += std::to_string(sigEntry->getSemanticIndex());
  }

  builder.add(ir::Op::DebugName(def, name.c_str()));
}

}
