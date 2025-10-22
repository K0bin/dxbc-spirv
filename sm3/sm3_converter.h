#pragma once

#include "sm3_parser.h"
#include "sm3_semantic_map.h"
#include "sm3_io_map.h"
#include "sm3_registers.h"

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
  friend IoMap;
  friend RegisterFile;

public:

  struct Options {
    /** Shader name. If non-null, this will be set as the entry point
     *  name, which is interpreted as the overall name of the shader. */
    const char* name = nullptr;
    /** Whether to emit any debug names besides the shader name. This
     *  includes resources, scratch and shared variables, as well as
     *  semantic names for I/O variables. */
    bool includeDebugNames = false;

    FloatEmulation floatEmulation;
  };

  Converter(util::ByteReader code, IoSemanticMap& semanticMap, const Options& options);

  ~Converter();

  /** Creates internal IR from SM3 DXBC shader. If an error occurs, this function
   *  will return false and log messages to the thread-local logger. */
  bool convertShader(ir::Builder& builder);

  IoSemanticMap& getSemanticMap() const {
    return m_semanticMap;
  }

private:

  util::ByteReader m_code;
  Options          m_options;

  ConstantTable    m_ctab = { };

  IoSemanticMap&   m_semanticMap;

  RegisterFile     m_regFile;
  IoMap            m_ioMap;
  Parser           m_parser;

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

  ShaderInfo getShaderInfo() const {
    return m_parser.getShaderInfo();
  }

  ir::SsaDef broadcastScalar(ir::Builder& builder, ir::SsaDef def, WriteMask mask);

  ir::SsaDef swizzleVector(ir::Builder& builder, ir::SsaDef value, Swizzle swizzle, WriteMask writeMask);

  ir::SsaDef composite(ir::Builder& builder, ir::BasicType type,
    const ir::SsaDef* components, Swizzle swizzle, WriteMask mask);

  ir::SsaDef buildVector(ir::Builder& builder, ir::ScalarType scalarType, size_t count, const ir::SsaDef* scalars);

  ir::SsaDef extractFromVector(ir::Builder& builder, ir::SsaDef def, uint32_t component);

  template<typename T>
  ir::SsaDef makeTypedConstant(ir::Builder& builder, ir::BasicType type, T value);

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

  static ir::BasicType makeVectorType(ir::ScalarType type, WriteMask mask) {
    return ir::BasicType(type, util::popcnt(uint8_t(mask)));
  }

  std::string makeRegisterDebugName(RegisterType type, uint32_t index, WriteMask mask) const;

};

}
