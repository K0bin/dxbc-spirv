#pragma once

#include <cstdint>

#include "sm3_parser.h"
#include "sm3_types.h"

#include "../ir/ir.h"
#include "../ir/ir_builder.h"

namespace dxbc_spv::sm3 {

class Converter;

enum class SpecConstTextureType : uint32_t {
  eTexture2D   = 0u,
  eTextureCube = 1u,
  eTexture3D   = 2u,
};

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

  /* Declared data type of the resource. */
  ir::Type type = { };

  /* Declarations for the resource itself. */
  /* Primary resource. For sampler register this is the sampler. */
  ir::SsaDef resourceDef = { };
  /* Additional resources. For sampler registers, these are one (SM2+) or more (SM1) textures. */
  std::array<ir::SsaDef, 3u> additionalResourceDefs = { };
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
            ir::Builder&                        builder,
      const ResourceInfo*                       resourceInfo,
            std::optional<SpecConstTextureType> specConstTextureType);

  const ResourceInfo* getResourceInfo(const Operand& operand);

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

  util::small_vector<ResourceInfo, 256u> m_resources;

  void emitDebugName(
          ir::Builder&            builder,
    const ResourceInfo*           info);

  bool matchesResource(
    const Operand&                   operand,
    const ResourceInfo&              info) const;

};


inline ir::ResourceKind resourceKindFromTextureType(TextureType textureType) {
  switch (textureType) {
    case TextureType::eTexture2D:
      return ir::ResourceKind::eImage2D;
    case TextureType::eTextureCube:
      return ir::ResourceKind::eImageCube;
    case TextureType::eTexture3D:
      return ir::ResourceKind::eImage3D;
  }
  return ir::ResourceKind::eBufferRaw;
}

}
