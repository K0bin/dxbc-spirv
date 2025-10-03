#pragma once

#include "dxbc_parser.h"
#include "dxbc_types.h"

#include "../ir/ir_builder.h"

#include "../util/util_small_vector.h"

namespace dxbc_spv::dxbc {

class Converter;

/** Temporary register file.
 *
 * Implements temp array declarations (x#), as well as
 * loads and stores to x# and r# registers. */
class RegisterFile {
  constexpr static uint32_t MaxTgsmSize = 32768u;
public:

  explicit RegisterFile(Converter& converter);

  ~RegisterFile();

  /** Handles hull shader phase. Each phase has its local set of
   *  temporary registers, so we need to discard them. */
  void handleHsPhase();

  /** Declares temporary array. */
  bool handleDclIndexableTemp(ir::Builder& builder, const Instruction& op);

  /** Declares shared memory. */
  bool handleDclTgsmRaw(ir::Builder& builder, const Instruction& op);
  bool handleDclTgsmStructured(ir::Builder& builder, const Instruction& op);

  /** Declares a function body for use with interfaces */
  bool handleDclFunctionBody(
          ir::Builder&            builder,
    const Instruction&            op);

  /* Reserves and declares function for label. Labels occur last in the
   * DXBC binary, so their declarations are not known at the time the
   * calls are made from the entry point function. */
  ir::SsaDef getFunctionForLabel(
          ir::Builder&            builder,
    const Instruction&            op,
    const Operand&                operand);

  /** Loads temporary register. */
  ir::SsaDef emitLoad(
          ir::Builder&            builder,
    const Instruction&            op,
    const Operand&                operand,
          WriteMask               componentMask,
          ir::ScalarType          type);

  /** Stores temporary register. */
  bool emitStore(
          ir::Builder&            builder,
    const Instruction&            op,
    const Operand&                operand,
          ir::SsaDef              value);

  /** Loads data from shared memoory. */
  ir::SsaDef emitTgsmLoad(
          ir::Builder&            builder,
    const Instruction&            op,
    const Operand&                operand,
          ir::SsaDef              elementIndex,
          ir::SsaDef              elementOffset,
          WriteMask               componentMask,
          ir::ScalarType          scalarType);

  /** Stores data to shared memoory. */
  bool emitTgsmStore(
          ir::Builder&            builder,
    const Instruction&            op,
    const Operand&                operand,
          ir::SsaDef              elementIndex,
          ir::SsaDef              elementOffset,
          ir::SsaDef              data);

  /** Emits address calculation for TGSM atomics */
  std::pair<ir::SsaDef, ir::SsaDef> computeTgsmAddress(
          ir::Builder&            builder,
    const Instruction&            op,
    const Operand&                operand,
    const Operand&                address);

private:

  Converter& m_converter;

  util::small_vector<ir::SsaDef, 256u> m_rRegs;
  util::small_vector<ir::SsaDef,  16u> m_xRegs;
  util::small_vector<ir::SsaDef,  16u> m_gRegs;
  util::small_vector<ir::SsaDef,  16u> m_labels;
  util::small_vector<ir::SsaDef,  16u> m_functionBodies;

  ir::SsaDef loadArrayIndex(ir::Builder& builder, const Instruction& op, const Operand& operand);

  ir::SsaDef getOrDeclareTemp(ir::Builder& builder, uint32_t index, Component component);

  ir::SsaDef getIndexableTemp(uint32_t index);

  ir::SsaDef getTgsmRegister(const Instruction& op, const Operand& operand);

  bool declareLds(ir::Builder& builder, const Instruction& op, const Operand& operand, const ir::Type& type);

  ir::SsaDef declareEmptyFunction(ir::Builder& builder, const Operand& operand);

};

}
