#include <fstream>
#include <iomanip>
#include <iostream>
#include <sstream>
#include <vector>

#include "../sm3/sm3_disasm.h"
#include "../sm3/sm3_parser.h"

using namespace dxbc_spv;


bool printCode(util::ByteReader reader) {
  if (!reader) {
    std::cout << "(no code)" << std::endl;
    return true;
  }

  sm3::Parser parser(reader);

  sm3::Disassembler::Options options = { };
  options.lineNumbers = true;
  options.indent = true;

  sm3::Disassembler disasm(options, parser.getShaderInfo());

  std::cout << parser.getShaderInfo() << ":" << std::endl;

  while (parser) {
    auto op = parser.parseInstruction();

    if (!op) {
      std::cerr << "Failed to parse instruction" << std::endl;
      return false;
    }

    disasm.disassembleOp(std::cout, op, parser.getShaderInfo());
    std::cout << std::endl;
  }

  return true;
}


bool disassembleShader(util::ByteReader reader) {
  util::ByteReader infoReader(reader);
  sm3::ShaderInfo info(infoReader);

  if (!info) {
    std::cout << "Failed to parse shader info" << std::endl;
    std::cout << "Read token: " << info.getToken() << std::endl;
    return false;
  }

  std::cout << info << std::endl;

  return printCode(reader);
}


void printHelp(const char* appName) {
  std::cerr << "Usage: " << appName << " file.dxso" << std::endl;
}


int main(int argc, char** argv) {
  if (argc < 2) {
    printHelp(argv[0u]);
    return 1;
  }

  const char* fileName = argv[1u];

  std::ifstream file(fileName, std::ios_base::in | std::ios_base::binary);

  if (!file.is_open()) {
    std::cerr << "Failed to open file: " << fileName << std::endl;
    return 1;
  }

  file.seekg(0u, std::ios_base::end);
  std::vector<char> data(file.tellg());
  file.seekg(0u, std::ios_base::beg);
  file.read(data.data(), data.size());

  if (!disassembleShader(util::ByteReader(data.data(), data.size())))
    return 1;

  return 0;
}
