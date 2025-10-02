#include "sm3_api.h"

#include <limits>

namespace dxbc_spv::sm3 {

std::optional<ir::Builder> compileShaderToIr(const void *data, const CompileOptions &options) {
  ir::Builder builder = { };

  const util::ByteReader reader(data, std::numeric_limits<size_t>::max());
  Converter converter(reader, options.convertOptions);
  return std::optional<ir::Builder>();
}

}
