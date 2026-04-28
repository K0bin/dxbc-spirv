#include "sm3_assembler.h"

#include "../util/util_log.h"
#include "../util/util_str.h"

namespace dxbc_spv::sm3 {

  void Assembler::parseFile(const std::string& path) {
    ShaderInfo info;
    std::vector<sm3::Instruction> instructions;

    std::ifstream stream(path, std::ios_base::in);

    std::string line;

    if (!std::getline(stream, line)) {
      Logger::err("Failed to read file.");
      return;
    }

    info = parseHeader(line);
    if (!info) {
      return;
    }

    while (std::getline(stream, line)) {
      size_t start = util::skipWhitespace(line, 0);
      std::string lineNoWhitespace = line.substr(start);

      if (lineNoWhitespace.empty())
        continue;

      sm3::Instruction instruction = parseInstructionLine(line);
      if (!instruction) {
        Logger::err("Failed to read file.");
        return;
      }
    }
  }

  ShaderInfo Assembler::parseHeader(std::string_view line) {
    uint32_t versionMajor = 0;
    uint32_t versionMinor = 0;
    ShaderType shaderType = ShaderType::eVertex;

    std::stringstream str;
    uint32_t element = 0u;
    for (size_t n = 0; n < line.size(); n++) {
      if (line[n] == '_' || line[n] == ':' || n == line.size() - 1) {
        switch (element) {
          case 0:
            if (!parseShaderType(str.str(), shaderType)) {
              Logger::err("Invalid shader type.");
              return ShaderInfo();
            }
            break;

          case 1:
            versionMajor = std::stoi(str.str(), nullptr, 10);
            break;

          case 2:
            versionMinor = std::stoi(str.str(), nullptr, 10);
            break;

          default:
            Logger::err("Invalid header");
            return ShaderInfo();
        }

        str.clear();
        element++;
        continue;
      }

      str << line[n];
    }

    return ShaderInfo(shaderType, versionMajor, versionMinor);
  }

  sm3::Instruction Assembler::parseInstructionLine(std::string_view line) {
    bool coissued = false;
    bool predicated = false;
    SrcRegister predicate = { };
    OpCodeWithModifier opCode = { };
    Register dst = { };
    std::array<Register, 3u> src = { };

    std::stringstream word;
    uint32_t wordIndex = 0u;
    for (size_t n = 0; n < line.size(); n++) {
      if (util::isWhitespace(line[n]) || n == line.size() - 1) {
        n = util::skipWhitespace(line, n + 1);

        switch (wordIndex) {
          case 0:
            /* Coissued instructions have a `+` as the first word. */
            if (line[n] == '+') {
              coissued = true;
              word.clear();
              continue;
            }

            /* Predicates look like this: `(!p0.x)` */
            if (n != 0u && line[n - 1u] == ')') {
              predicated = true;
              //parsePredicate
              word.clear();
              continue;
            }

            /* Otherwise the first word is `opcode_modifier_modifier` */
            opCode = parseOpCode(word.str());
            if (opCode == OpCode::eNop) {
              Logger::err("Failed to parse instruction");
              return sm3::Instruction();
            }
            break;
        }

        wordIndex++;
        word.clear();
        continue;
      }

      /* Add non-whitespace character to current word. */
      word << line[n];
    }
  }

  OpCodeWithModifier Assembler::parseOpCode(std::string_view str) {
    OpCodeWithModifier result = { };

    const char* partStart = str.data();
    uint32_t partLength = 0u;
    uint32_t partIndex = 0u;
    for (uint32_t i = 0u; i < str.size(); i++) {
      if (str[i] == '_' || (str[i] >= '0' && str[i] <= '9')) {
        auto part = std::string_view(partStart, partLength);
        if (partIndex == 0u) {
          /* First part of the string (up to the first _) is the opcode */
          std::stringstream opCodeStr;
          for (uint32_t j = 1u; j < uint32_t(OpCode::eBreakP); j++) {
            opCodeStr << OpCode(j);

            if (util::compareCaseInsensitiveViews(opCodeStr.str(), part)) {
              result.opCode = OpCode(j);
              break;
            }

            opCodeStr.clear();
          }
        } else {
          /* Parts after that are modifiers, comparison modes, texture types or semantics */
          if (util::compareCaseInsensitiveViews("pp", part)) {
            result.partialPrecision = true;
          } else if (util::compareCaseInsensitiveViews("centroid", part)) {
            result.centroid = true;
          } else if (util::compareCaseInsensitiveViews("sat", part)) {
            result.saturated = true;
          } else if (util::compareCaseInsensitiveViews("specular", part)) {
            result.semantic.usage = SemanticUsage::eColor;
          } else {
            bool found = false;
            std::stringstream enumStr;

            for (uint32_t j = 0u; j < uint32_t(ComparisonMode::eAlways) && !found; j++) {
              enumStr << ComparisonMode(j);

              if (util::compareCaseInsensitiveViews(enumStr.str(), part)) {
                result.comparisonMode = ComparisonMode(j);
                found = true;
                break;
              }

              enumStr.clear();
            }

            for (uint32_t j = 0u; j < uint32_t(SemanticUsage::eSample) && !found; j++) {
              enumStr << SemanticUsage(j);

              if (util::compareCaseInsensitiveViews(enumStr.str(), part)) {
                result.semantic.usage = SemanticUsage(j);
                found = true;
                break;
              }

              enumStr.clear();
            }

            for (uint32_t j = uint32_t(TextureType::eTexture2D); j < uint32_t(TextureType::eTexture3D) && !found; j++) {
              enumStr << TextureType(j);

              if (util::compareCaseInsensitiveViews(enumStr.str(), part)) {
                result.semantic.usage = SemanticUsage(j);
                found = true;
                break;
              }

              enumStr.clear();
            }
          }
        }

        partIndex++;
        partStart += partLength + 1u;
        partLength = 0u;
      } else {
        partLength++;
      }
    }

    return result;
  }


  sm3::Operand Assembler::parseOperand(std::string_view str) {


    return Operand();
  }

  sm3::RegisterType Assembler::parseRegisterType(std::string_view str) {
    if (util::compareCaseInsensitiveViews(str, "r"))
      return RegisterType::eTemp;

    if (util::compareCaseInsensitiveViews(str, "oC"))
      return RegisterType::eColorOut;

    if (util::compareCaseInsensitiveViews(str, "c"))
      return RegisterType::eConst;

    if (util::compareCaseInsensitiveViews(str, "v"))
      return RegisterType::ePixelTexCoord;

    if (util::compareCaseInsensitiveViews(str, "s"))
      return RegisterType::eSampler;

    if (util::compareCaseInsensitiveViews(str, "vPos"))
      return RegisterType::eMiscType;

    return RegisterType::eTemp;
  }

}
