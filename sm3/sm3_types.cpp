#include "sm3_types.h"

namespace dxbc_spv::sm3 {

  std::ostream& operator << (std::ostream& os, OpCode op) {
    switch (op) {
      case OpCode::eNop:          return os << "Nop";
      case OpCode::eMov:          return os << "Mov";
      case OpCode::eAdd:          return os << "Add";
      case OpCode::eSub:          return os << "Sub";
      case OpCode::eMad:          return os << "Mad";
      case OpCode::eMul:          return os << "Mul";
      case OpCode::eRcp:          return os << "Rcp";
      case OpCode::eRsq:          return os << "Rsq";
      case OpCode::eDp3:          return os << "Dp3";
      case OpCode::eDp4:          return os << "Dp4";
      case OpCode::eMin:          return os << "Min";
      case OpCode::eMax:          return os << "Max";
      case OpCode::eSlt:          return os << "Slt";
      case OpCode::eSge:          return os << "Sge";
      case OpCode::eExp:          return os << "Exp";
      case OpCode::eLog:          return os << "Log";
      case OpCode::eLit:          return os << "Lit";
      case OpCode::eDst:          return os << "Dst";
      case OpCode::eLrp:          return os << "Lrp";
      case OpCode::eFrc:          return os << "Frc";
      case OpCode::eM4x4:         return os << "M4x4";
      case OpCode::eM4x3:         return os << "M4x3";
      case OpCode::eM3x4:         return os << "M3x4";
      case OpCode::eM3x3:         return os << "M3x3";
      case OpCode::eM3x2:         return os << "M3x2";
      case OpCode::eCall:         return os << "Call";
      case OpCode::eCallNz:       return os << "CallNz";
      case OpCode::eLoop:         return os << "Loop";
      case OpCode::eRet:          return os << "Ret";
      case OpCode::eEndLoop:      return os << "EndLoop";
      case OpCode::eLabel:        return os << "Label";
      case OpCode::eDcl:          return os << "Dcl";
      case OpCode::ePow:          return os << "Pow";
      case OpCode::eCrs:          return os << "Crs";
      case OpCode::eSgn:          return os << "Sgn";
      case OpCode::eAbs:          return os << "Abs";
      case OpCode::eNrm:          return os << "Nrm";
      case OpCode::eSinCos:       return os << "SinCos";
      case OpCode::eRep:          return os << "Rep";
      case OpCode::eEndRep:       return os << "EndRep";
      case OpCode::eIf:           return os << "If";
      case OpCode::eIfc:          return os << "Ifc";
      case OpCode::eElse:         return os << "Else";
      case OpCode::eEndIf:        return os << "EndIf";
      case OpCode::eBreak:        return os << "Break";
      case OpCode::eBreakC:       return os << "BreakC";
      case OpCode::eMova:         return os << "Mova";
      case OpCode::eDefB:         return os << "DefB";
      case OpCode::eDefI:         return os << "DefI";

      case OpCode::eTexCoord:     return os << "TexCoord";
      case OpCode::eTexKill:      return os << "TexKill";
      case OpCode::eTex:          return os << "Tex";
      case OpCode::eTexBem:       return os << "TexBem";
      case OpCode::eTexBemL:      return os << "TexBemL";
      case OpCode::eTexReg2Ar:    return os << "TexReg2Ar";
      case OpCode::eTexReg2Gb:    return os << "TexReg2Gb";
      case OpCode::eTexM3x2Pad:   return os << "TexM3x2Pad";
      case OpCode::eTexM3x2Tex:   return os << "TexM3x2Tex";
      case OpCode::eTexM3x3Pad:   return os << "TexM3x3Pad";
      case OpCode::eTexM3x3Tex:   return os << "TexM3x3Tex";
      case OpCode::eReserved0:    return os << "Reserved0";
      case OpCode::eTexM3x3Spec:  return os << "TexM3x3Spec";
      case OpCode::eTexM3x3VSpec: return os << "TexM3x3VSpec";
      case OpCode::eExpP:         return os << "ExpP";
      case OpCode::eLogP:         return os << "LogP";
      case OpCode::eCnd:          return os << "Cnd";
      case OpCode::eDef:          return os << "Def";
      case OpCode::eTexReg2Rgb:   return os << "TexReg2Rgb";
      case OpCode::eTexDp3Tex:    return os << "TexDp3Tex";
      case OpCode::eTexM3x2Depth: return os << "TexM3x2Depth";
      case OpCode::eTexDp3:       return os << "TexDp3";
      case OpCode::eTexM3x3:      return os << "TexM3x3";
      case OpCode::eTexDepth:     return os << "TexDepth";
      case OpCode::eCmp:          return os << "Cmp";
      case OpCode::eBem:          return os << "Bem";
      case OpCode::eDp2Add:       return os << "Dp2Add";
      case OpCode::eDsX:          return os << "DsX";
      case OpCode::eDsY:          return os << "DsY";
      case OpCode::eTexLdd:       return os << "TexLdd";
      case OpCode::eSetP:         return os << "SetP";
      case OpCode::eTexLdl:       return os << "TexLdl";
      case OpCode::eBreakP:       return os << "BreakP";

      case OpCode::ePhase:        return os << "Phase";
      case OpCode::eComment:      return os << "Comment";
      case OpCode::eEnd:          return os << "End";
    }

    return os << "Opcode(" << uint32_t(op) << ")";
  }

  std::ostream& operator << (std::ostream& os, Usage usage) {
    switch (usage) {
      case Usage::ePosition:     return os << "Position";
      case Usage::eBlendWeight:  return os << "BlendWeight";
      case Usage::eBlendIndices: return os << "BlendIndices";
      case Usage::eNormal:       return os << "Normal";
      case Usage::ePointSize:    return os << "PointSize";
      case Usage::eTexCoord:     return os << "TexCoord";
      case Usage::eTangent:      return os << "Tangent";
      case Usage::eBinormal:     return os << "Binormal";
      case Usage::eTessFactor:   return os << "TessFactor";
      case Usage::ePositionT:    return os << "PositionT";
      case Usage::eColor:        return os << "Color";
      case Usage::eFog:          return os << "Fog";
      case Usage::eDepth:        return os << "Depth";
      case Usage::eSample:       return os << "Sample";
    }

    return os << "Usage(" << uint32_t(usage) << ")";
  }

  std::ostream& operator << (std::ostream& os, TextureType textureType) {
    switch (textureType) {
      case TextureType::eTexture2D:   return os << "Texture2D";
      case TextureType::eTextureCube: return os << "TextureCube";
      case TextureType::eTexture3D:   return os << "Texture3D";
    }

    return os << "TextureType(" << uint32_t(textureType) << ")";
  }

  std::ostream& operator << (std::ostream& os, RasterizerOutIndex outIndex) {
    switch (outIndex) {
      case eRasterOutPosition:  return os << "RasterizerOutPosition";
      case eRasterOutFog:       return os << "RasterizerOutFog";
      case eRasterOutPointSize: return os << "RasterizerOutPointSize";
    }

    return os << "RasterizerOutIndex(" << uint32_t(outIndex) << ")";
  }

  std::ostream& operator << (std::ostream& os, MiscTypeIndex miscTypeIndex) {
    switch (miscTypeIndex) {
      case eMiscTypePosition: return os << "MiscTypePosition";
      case eMiscTypeFace:     return os << "MiscTypeFace";
    }

    return os << "MiscTypeIndex(" << uint32_t(miscTypeIndex) << ")";
  }

}
