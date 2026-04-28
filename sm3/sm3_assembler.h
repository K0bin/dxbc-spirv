#pragma once

#include <fstream>
#include <sstream>
#include <string>

#include "sm3_types.h"
#include "sm3_parser.h"
#include "util/util_log.h"
#include "util/util_str.h"
#include "util/util_bit.h"

namespace dxbc_spv::sm3 {

struct SrcRegister {
  Swizzle swizzle;
  OperandModifier modifier;
};

struct DstRegister {
  WriteMask writeMask;
};

struct OpCodeWithModifier {
  OpCode opCode;

  bool partialPrecision;
  bool saturated;
  bool centroid;
  int shift;

  union {
    Semantic semantic;
    ComparisonMode comparisonMode;
    TextureType textureType;
  };
};

struct Register {
  RegisterType type;
  uint32_t index;
  union {
    SrcRegister src;
    DstRegister dst;
  };
};

class Assembler {

private:

  void parseFile(const std::string& path);

  static bool parseShaderType(const std::string& str, ShaderType& shaderType) {
    if (util::compareCaseInsensitive(str.c_str(), "vs")) {
      shaderType = ShaderType::eVertex;
      return true;
    }

    if (util::compareCaseInsensitive(str.c_str(), "ps")) {
      shaderType = ShaderType::ePixel;
      return true;
    }

    return false;
  }

  ShaderInfo parseHeader(std::string_view line);

  sm3::Instruction parseInstructionLine(std::string_view line);

  OpCodeWithModifier parseOpCode(std::string_view str);

  sm3::Operand parseOperand(std::string_view str);

  sm3::RegisterType parseRegisterType(std::string_view str);

  OpCodeWithModifier parseOpCodeWithModifier(std::string_view str);

};

}
