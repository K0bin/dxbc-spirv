#pragma once

#include "sm3_container.h"
#include "sm3_parser.h"
#include "sm3_signature.h"
#include "sm3_types.h"

#include "../ir/ir_builder.h"

#include "../util/util_small_vector.h"

namespace dxbc_spv::sm3 {

class Converter;

/** I/O variable mapping entry. Note that each I/O variable may have
 *  multiple mappings, e.g. if a built-in output is mirrored to a
 *  regular I/O variable, or if an input is part of an index range. */
struct IoVarInfo {
  RegisterType regType = RegisterType::eTemp;

  uint32_t regIndex = 0u;

  /* Component write mask to match, if applicable. */
  WriteMask componentMask = { };

  /* Type of the underlying variable. Will generally match the declared
   * type of the base definition, unless that is a function. */
  ir::Type baseType = { };

  /* Variable definition. May be an input, output, control point input,
   * control point output, scratch, or temporary variable, depending on
   * various factors. For indexable outputs, this may be a function. */
  ir::SsaDef baseDef = { };

  /* Checks whether the variable matches the given conditions */
  bool matches(RegisterType type, uint32_t index, WriteMask mask) const {
    return type == regType && (mask & componentMask) && index == regIndex;
  }
};

/** I/O register map.
 *
 * This helper class resolves the complexities around declaring,
 * mapping and accessing input and output registers with the help
 * of I/O signatures. */
class IoMap {
  constexpr static uint32_t MaxIoArraySize = 32u;

  using IoVarList = util::small_vector<IoVarInfo, 32u>;
public:

  explicit IoMap(Converter& converter);

  ~IoMap();

  /** Handles an input or output declaration of any kind. If possible, this uses
   *  the signature to determine the correct layout for the declaration. */
  bool handleDclIoVar(ir::Builder& builder, const Instruction& op);

  /** Loads an input or output value and returns a scalar or vector containing
   *  one element for each component in the component mask. Applies swizzles,
   *  but does not support modifiers in any capacity.
   *
   *  Uses the converter's functionality to process relative indices as necessary.
   *  The register index in particular must be immediate only, unless an index
   *  range is declared for the register in question.
   *
   *  Returns a \c null def on error. */
  ir::SsaDef emitLoad(
          ir::Builder&            builder,
    const Instruction&            op,
    const Operand&                operand,
          WriteMask               componentMask,
          ir::ScalarType          type);

  /** Stores a scalar or vector value to an output variable. The component
   *  type is ignored, but the component count must match that of the
   *  operand's write mask exactly.
   *
   *  Uses the converter's functionality to process relative indices as necessary.
   *  Indexing rules are identical to those for inputs.
   *
   *  Returns \c false on error. */
  bool emitStore(
          ir::Builder&            builder,
    const Instruction&            op,
    const Operand&                operand,
          ir::SsaDef              value);

private:

  Converter&      m_converter;
  ShaderType      m_shaderType = { };

  IoVarList       m_variables;

  void emitDebugName(ir::Builder& builder, ir::SsaDef def, RegisterType type, uint32_t index, WriteMask mask, const Operand& dclOperand) const;

};

}
