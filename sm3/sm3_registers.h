#pragma once

#include "sm3_parser.h"
#include "sm3_types.h"

#include "../ir/ir_builder.h"

#include "../util/util_small_vector.h"
#include "../util/util_swizzle.h"

namespace dxbc_spv::sm3 {

class Converter;

class RegisterFile {

public:

  explicit RegisterFile(Converter& converter);

  ~RegisterFile();

  void initialize(ir::Builder& builder);

  /** Loads temporary register. */
  ir::SsaDef emitLoad(
          ir::Builder&            builder,
    const Operand&                operand,
          WriteMask               componentMask,
          ir::ScalarType          type);

  /** Stores temporary register. */
  bool emitStore(
          ir::Builder&            builder,
    const Operand&                operand,
          ir::SsaDef              value);

private:

  ir::SsaDef getOrDeclareTemp(ir::Builder& builder, uint32_t index, Component component);

  Converter& m_converter;

  // Temporary registers
  util::small_vector<ir::SsaDef, 32u * 4u> m_rRegs = { };

  // Address register
  std::array<ir::SsaDef, 4u> m_a0Reg = { };

  // Loop counter register
  ir::SsaDef m_aLReg = { };

  // Predicate register
  ir::SsaDef m_pReg = { };

};

}
