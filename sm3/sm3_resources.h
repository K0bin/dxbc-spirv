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


enum class SamplingConfigBit : uint8_t {
  eExplicitLod         = 1u,
  eLodBias             = 2u,
  eExplicitDerivatives = 3u,

  eFlagEnum            = 0u,
};

using SamplingConfig = util::Flags<SamplingConfigBit>;


struct SamplerRegister {
  /** Register index */
  uint32_t regIndex = 0u;

  /** Declaration of the texture
   * One for each texture type on SM1 and only one on SM2+ */
  std::array<ir::SsaDef, 3u> textureDefs = { };

  /** The type of the texture. This is only set on SM2+ as there are no dcl_samplerType instructions
   * on SM1. This texture type represents the index of the one valid `textureDef` on SM2. */
  std::optional<SpecConstTextureType> textureType = std::nullopt;

  /** Declaration of the sampler */
  ir::SsaDef samplerDef = { };

  /** Sampling functions. Will be populated lazily.
   * A SamplingFunctionConfigBit bitmask makes up the index into this array.
   * Each function takes in an F32 vec4 for the texCoords and some
   * will take additional arguments for LODs and/or derivatives depending
   * on the flags. */
  std::array<ir::SsaDef, 6u> samplingFunctions = { };
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


/** Resource variable map. Handles both texture declaration and access.
 *  Also takes care of textures getting accessed without getting declared first
 *  in SM1. On top of that it handles constant registers. */
class ResourceMap {
public:

    explicit ResourceMap(Converter& converter);

    ~ResourceMap();

    /** Loads a resource or sampler descriptor and retrieves basic
     *  properties required to perform any operations on typed resources. */
    ir::SsaDef emitSample(
            ir::Builder& builder,
            uint32_t     samplerIndex,
            ir::SsaDef   texCoord,
            bool         project,
            bool         controlProjectWithSpecConst,
            ir::SsaDef   lod,
            ir::SsaDef   lodBias,
            ir::SsaDef   dx,
            ir::SsaDef   dy);

  /** Loads data from a constant buffer using one or more BufferLoad
   *  instruction. If possible this will emit a vectorized load. */
  ir::SsaDef emitConstantLoad(
          ir::Builder&            builder,
    const Instruction&            op,
    const Operand&                operand,
          WriteMask               componentMask,
          ir::ScalarType          scalarType);

  /** Handles Dcl instructions on SM 2+ with Sampler as the register type. */
  bool handleDclSampler(ir::Builder& builder, const Instruction& op);

  bool dclSamplerAndAllTextureTypes(ir::Builder& builder, uint32_t samplerIndex);

private:

  Converter& m_converter;

  std::array<SamplerRegister, 32> m_samplers;

  ir::SsaDef dclSampler(ir::Builder& builder, uint32_t samplerIndex);

  ir::SsaDef dclTexture(ir::Builder& builder, SpecConstTextureType textureType, uint32_t samplerIndex);

  ir::SsaDef emitSampleImageFunction(
    ir::Builder& builder,
    uint32_t samplerIndex,
    SamplingConfig config
  );

  ir::SsaDef emitSampleColorOrDref(
    ir::Builder& builder,
    ir::SsaDef texCoord,
    SpecConstTextureType textureType,
    uint32_t samplerIndex,
    ir::SsaDef descriptor,
    ir::SsaDef sampler,
    ir::SsaDef lod,
    ir::SsaDef lodBias,
    ir::SsaDef dx,
    ir::SsaDef dy
  );

  ir::SsaDef emitSampleColorImageType(
    ir::Builder& builder,
    ir::SsaDef texCoord,
    SpecConstTextureType textureType,
    uint32_t samplerIndex,
    ir::SsaDef descriptor,
    ir::SsaDef sampler,
    ir::SsaDef lod,
    ir::SsaDef lodBias,
    ir::SsaDef dx,
    ir::SsaDef dy
  );

  ir::SsaDef emitSampleDref(
    ir::Builder& builder,
    ir::SsaDef texCoord,
    SpecConstTextureType textureType,
    ir::SsaDef descriptor,
    ir::SsaDef sampler,
    ir::SsaDef lod,
    ir::SsaDef lodBias,
    ir::SsaDef dx,
    ir::SsaDef dy
  );

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

inline SpecConstTextureType specConstTextureTypeFromTextureType(TextureType textureType) {
  return SpecConstTextureType(uint32_t(textureType) - uint32_t(TextureType::eTexture2D));
}

inline TextureType textureTypeFromSpecConstTextureType(SpecConstTextureType specConstTextureType) {
  return TextureType(uint32_t(specConstTextureType) + uint32_t(TextureType::eTexture2D));
}

inline SpecConstTextureType specConstTextureTypeFromResourceKind(ir::ResourceKind resourceKind) {
  switch (resourceKind) {
    case ir::ResourceKind::eImage2D:
      return SpecConstTextureType::eTexture2D;
    case ir::ResourceKind::eImage3D:
      return SpecConstTextureType::eTexture3D;
    case ir::ResourceKind::eImageCube:
      return SpecConstTextureType::eTextureCube;
    default:
      dxbc_spv_unreachable();
      return SpecConstTextureType::eTexture2D;
  }
}

}
