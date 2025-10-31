/// @file Constants.hpp
/// @brief Contains project-wide constants
/// @todo make this a std::range

#pragma once

#include <limits>
#include <cstddef>
#include <unordered_set>

/// @brief alphabet of this library. Note that '\0' is not included, nore 127
const std::unordered_set<char> ALPHABET = []() {
    std::unordered_set<char> ret; 
    for (int i = 1; i <127; ++i) ret.insert(char(i));
    return ret;
}();


//// @brief size of the alphabet of this library
const size_t ALPHABET_SIZE = ALPHABET.size();

/// @brief state not having a rule associated with it
constexpr size_t NO_CASE_TAG = std::numeric_limits<std::size_t>::max();

/// @brief invalid state index, used for no transition / placeholder
constexpr size_t INVALID_STATE_INDEX = std::numeric_limits<std::size_t>::max();

/// @brief epsilon character used by nfas
constexpr char EPSILON = '\0';