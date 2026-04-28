#pragma once

#include <string>
#include <vector>

namespace dxbc_spv::util {

  static bool isWhitespace(char ch) {
    return ch == ' ' || ch == '\x9' || ch == '\r';
  }

  static size_t skipWhitespace(std::string_view line, size_t n) {
    while (n < line.size() && isWhitespace(line[n]))
      n += 1;
    return n;
  }

  /**
   * \brief Split string at one or more delimiters characters
   *
   * \param [in] string String to split
   * \param [in] delims Delimiter characters
   * \returns Vector of substring views
  */
  inline std::vector<std::string_view> split(std::string_view string, std::string_view delims = " ") {
    std::vector<std::string_view> tokens;

    for (size_t start = 0; start < string.size(); ) {
      // Find first delimiter
      const auto end = string.find_first_of(delims, start);

      // Add non-empty tokens
      if (start != end)
        tokens.emplace_back(string.substr(start, end-start));

      // Break at the end of string
      if (end == std::string_view::npos)
        break;

      start = end + 1;
    }
    return tokens;
  }

  /** Converts ASCII string to lower case */
  inline std::string tolower(std::string str) {
    for (size_t i = 0u; i < str.size(); i++) {
      if (str[i] >= 'A' && str[i] <= 'Z')
        str[i] += 'a' - 'A';
    }

    return str;
  }

}
