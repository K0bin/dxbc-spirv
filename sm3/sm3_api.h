#pragma once

#include <optional>

#include "sm3_converter.h"

#include "../ir/ir_builder.h"

namespace dxbc_spv::sm3 {

/** Compilation and lowering options for SM3 DXBC. */
struct CompileOptions {
  /* DXBC SM3 conversion options. Includes the shader name,
   * which should always be provided. */
  Converter::Options convertOptions = { };
};

std::optional<ir::Builder> compileShaderToIr(const void* data, const CompileOptions& options);

}
