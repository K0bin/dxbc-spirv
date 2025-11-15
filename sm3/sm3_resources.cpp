#include "sm3_resources.h"

#include "sm3_converter.h"

#include "../util/util_log.h"

namespace dxbc_spv::sm3 {

ResourceProperties ResourceMap::emitDescriptorLoad(
            ir::Builder&                        builder,
      const ResourceInfo*                       resourceInfo,
            std::optional<SpecConstTextureType> specConstTextureType) {

  if (!resourceInfo)
    return ResourceProperties();

  /* Retrieve resource definition. */
  auto baseDef = resourceInfo->resourceDef;

  ir::ScalarType descriptorType = ir::ScalarType::eUnknown;
  switch (resourceInfo->regType) {
    case RegisterType::eSampler:
      descriptorType = ir::ScalarType::eSampler;

      if (specConstTextureType.has_value()) {
        /* If a texture type is provided as a parameter,
         * load the requested texture descriptor instead of the sampler. */
        baseDef = resourceInfo->additionalResourceDefs[uint32_t(specConstTextureType.value())];
        descriptorType = ir::ScalarType::eSrv;
      }
      break;

    default:
      dxbc_spv_unreachable();
      return ResourceProperties();
  }

  if (!baseDef)
    return ResourceProperties();

  ir::SsaDef descriptorIndex = builder.makeConstant(0u);
  auto descriptor = builder.add(ir::Op::DescriptorLoad(descriptorType, baseDef, descriptorIndex));

  /* Populate ResourceProperties */
  ResourceProperties result = { };

  if (resourceInfo->regType != RegisterType::eSampler) {
    result.kind = resourceInfo->kind;

    if (result.kind != ir::ResourceKind::eBufferRaw &&
        result.kind != ir::ResourceKind::eBufferStructured)
      result.type = resourceInfo->type.getBaseType(0u).getBaseType();
  }

  result.descriptor = descriptor;
  return result;
}


const ResourceInfo* ResourceMap::getResourceInfo(const Operand& operand) {
  for (auto& e : m_resources) {
    if (matchesResource(operand, e))
      return &e;
  }

  auto name = m_converter.makeRegisterDebugName(operand.getRegisterType(), operand.getIndex(), WriteMask());
  Logger::err("Resource ", name, " not declared.");
  return nullptr;
}


void ResourceMap::emitDebugName(ir::Builder& builder, const ResourceInfo* info) {
  if (m_converter.m_options.includeDebugNames) {
    auto name = m_converter.makeRegisterDebugName(info->regType, info->regIndex, WriteMask());
    builder.add(ir::Op::DebugName(info->resourceDef, name.c_str()));
  }
}


bool ResourceMap::matchesResource(
    const Operand&                   operand,
    const ResourceInfo&              info) const {
  if (info.regType != operand.getRegisterType())
    return false;

  if (!operand.hasRelativeAddressing() && info.regIndex != operand.getIndex())
    return false;

  if (info.regIndex < operand.getIndex() || info.regIndex + info.regCount >= operand.getIndex())
    return false;

  if (info.regType == RegisterType::eSampler) {
    // RegisterType Sampler + No Texture type => Sampler
    // RegisterType Sampler + Texture type => Texture
    if (!textureType.has_value() && !info.kind.has_value()) {
      return true;
    } else if (textureType.has_value()) {
      auto resourceType = resourceKindFromTextureType(textureType.value());
      return info.kind == resourceType;
    } else {
      return false;
    }
  } else {
    return true;
  }
}

}
