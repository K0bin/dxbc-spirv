#pragma once

#include "../ir/ir_builder.h"
#include "../util/util_byte_stream.h"

namespace dxbc_spv::sm3 {

/** Shader converter from SM3 DXBC to custom IR.
 *
 * The generated IR will contain temporaries rather than pure SSA form,
 * scoped control rather than structured control flow, min-precision or
 * unknown types, and instructions that cannot be lowered directly. As
 * such, the IR will require further processing. */
class Converter {
public:

  struct Options {

  };

  Converter(util::ByteReader code, const Options& options);

  ~Converter();

  /** Creates internal IR from SM3 DXBC shader. If an error occurs, this function
   *  will return false and log messages to the thread-local logger. */
  bool convertShader(ir::Builder& builder);
};

}
