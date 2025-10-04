#pragma once

#include "sm3_parser.h"

#include "../ir/ir_builder.h"

#include "../util/util_byte_stream.h"
#include "../util/util_log.h"

namespace dxbc_spv::sm3 {

enum class FloatEmulation {
  eDisabled,
  eFast,
  eStrict,
};

/** Shader converter from SM3 DXBC to custom IR.
 *
 * The generated IR will contain temporaries rather than pure SSA form,
 * scoped control rather than structured control flow, min-precision or
 * unknown types, and instructions that cannot be lowered directly. As
 * such, the IR will require further processing. */
class Converter {
public:

  struct Options {
    /** Shader name. If non-null, this will be set as the entry point
     *  name, which is interpreted as the overall name of the shader. */
    const char* name = nullptr;

    FloatEmulation floatEmulation;
  };

  Converter(util::ByteReader code, const Options& options);

  ~Converter();

  /** Creates internal IR from SM3 DXBC shader. If an error occurs, this function
   *  will return false and log messages to the thread-local logger. */
  bool convertShader(ir::Builder& builder);

private:

  util::ByteReader m_code;
  Parser           m_parser;
  Options          m_options;

  uint32_t m_instructionCount = 0u;

  /* Entry point definition and function definitions. */
  struct {
    ir::SsaDef def;

    ir::SsaDef mainFunc;
  } m_entryPoint;

  bool convertInstruction(ir::Builder& builder, const Instruction& op);

  bool initialize(ir::Builder& builder, ShaderType shaderType);

  bool finalize(ir::Builder& builder, ShaderType shaderType);

  bool initParser(Parser& parser, util::ByteReader reader);

  ir::SsaDef getEntryPoint() const {
    return m_entryPoint.def;
  }

  void logOp(LogLevel severity, const Instruction& op) const;

  template<typename... Args>
  bool logOpMessage(LogLevel severity, const Instruction& op, const Args&... args) const {
    logOp(severity, op);
    Logger::log(severity, args...);
    return false;
  }

  template<typename... Args>
  bool logOpError(const Instruction& op, const Args&... args) const {
    return logOpMessage(LogLevel::eError, op, args...);
  }

};

}
