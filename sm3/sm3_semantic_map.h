#pragma once

#include "sm3_types.h"

namespace dxbc_spv::sm3 {

class IoSemanticMap {

public:

  virtual uint32_t getIoLocation(Semantic semantic) = 0;

};

}
