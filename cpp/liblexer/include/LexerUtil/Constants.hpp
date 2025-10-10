/// @file Constants.hpp
/// @brief Contains project-wide constants

#pragma once

#include <limits>
#include <cstddef>
#include <unordered_set>

/// @brief The alphabet used by the project. Includes printable ASCII 
///        characters, EOF, tab, and newline.
const std::unordered_set<char> ALPHABET = []()
{
    std::unordered_set<char> ret{ };
    for (int c = 32; c < 128; ++c)
    {
        //DBG << (char) c << std::endl;
        ret.insert((char)c);
    }
    ret.insert('\t');
    ret.insert('\n');
    
    return ret;
}();

/// @brief state not having a rule associated with it
constexpr size_t NO_CASE_TAG = std::numeric_limits<std::size_t>::max();

/// @brief invalid state index, used for no transition / placeholder
constexpr size_t INVALID_STATE_INDEX = std::numeric_limits<std::size_t>::max();

/// @brief epsilon character used by nfas
constexpr char EPSILON = '\0';