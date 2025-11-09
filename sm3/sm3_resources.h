#pragma once

#include <cstdint>

#include "sm3_parser.h"
#include "sm3_types.h"

#include "../ir/ir.h"
#include "../ir/ir_builder.h"

namespace dxbc_spv::sm3 {

class Converter;

  /** Resource info */
struct ResourceInfo {
  /* Register / Resource type */
  RegisterType regType = { };

  /* Register index */
  uint32_t regIndex = 0u;

  /* Declared register range. For textures and when not using the constant table,
   * the count will always be 1. */
  uint32_t regCount = 0u;

  /* Resource kind being declared */
  ir::ResourceKind kind = { };

  /* Declared data type of the resource.
   *
   * - For constant buffers, this is a vec4 array fo an unknown type.
   * - For raw buffers, this is a plain unbounded array of unknown
   *   type, and will likely be mapped to u32 down the line.
   * - For structured buffers, this is an unbounded array of a sized
   *   array of unknown type, which may be promoted to a structure.
   * - For typed buffers and images, this is a scalar type that matches
   *   the returned scalar type of any sample or read operations. */
  ir::Type type = { };

  /* Declarations for the resource itself. */
  ir::SsaDef resourceDef = { };
};


/** Retrieved typed resource parameters */
struct ResourceProperties {
  /* Resource kind */
  ir::ResourceKind kind = { };

  /* Scalar sampled type */
  ir::ScalarType type = { };

  /* Loaded descriptor */
  ir::SsaDef descriptor = { };
};


/** Resource look-up structure */
struct ResourceKey {
  RegisterType  regType  = { };
  uint32_t      regIndex = 0u;

  bool operator == (const ResourceKey& other) const { return regType == other.regType && regIndex == other.regIndex; }
  bool operator != (const ResourceKey& other) const { return regType != other.regType || regIndex != other.regIndex; }
  bool operator <  (const ResourceKey& other) const { return regType < other.regType || (regType == other.regType && regIndex <  other.regIndex); }
  bool operator <= (const ResourceKey& other) const { return regType < other.regType || (regType == other.regType && regIndex <= other.regIndex); }
  bool operator >  (const ResourceKey& other) const { return regType > other.regType || (regType == other.regType && regIndex >  other.regIndex); }
  bool operator >= (const ResourceKey& other) const { return regType > other.regType || (regType == other.regType && regIndex >= other.regIndex); }
};


/** Resource variable map. Handles both texture declaration and access.
 *  Also takes care of textures getting accessed without getting declared first
 *  in SM1. On top of that it handles constant registers. */
class ResourceMap {
  public:

    explicit ResourceMap(Converter& converter);

    ~ResourceMap();

    /** Loads a resource or sampler descriptor and retrieves basic
     *  properties required to perform any operations on typed resources. */
    ResourceProperties emitDescriptorLoad(
            ir::Builder&            builder,
      const Instruction&            op,
      const Operand&                operand);

    /** Loads data from a constant buffer using one or more BufferLoad
     *  instruction. If possible this will emit a vectorized load. */
    ir::SsaDef emitConstantLoad(
            ir::Builder&            builder,
      const Instruction&            op,
      const Operand&                operand,
            WriteMask               componentMask,
            ir::ScalarType          scalarType);

  private:

  Converter& m_converter;

  std::pair<ir::SsaDef, const ResourceInfo*> loadDescriptor(
          ir::Builder&            builder,
    const Instruction&            op,
    const Operand&                operand);

  void emitDebugName(
          ir::Builder&            builder,
    const ResourceInfo*           info);

};

}
