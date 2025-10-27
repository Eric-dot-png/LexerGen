/// @file Constants.hpp
/// @brief Contains project-wide constants
/// @todo make this a std::range

#pragma once

#include <limits>
#include <cstddef>
#include <unordered_set>
#include <ranges>

/// @brief alphabet of this library. Note that '\0' is not included, nore 127
constexpr std::ranges::view auto ALPHABET = std::views::iota(char(1), char(127));

//// @brief size of the alphabet of this library
constexpr size_t ALPHABET_SIZE = std::ranges::distance(ALPHABET);

/// @brief state not having a rule associated with it
constexpr size_t NO_CASE_TAG = std::numeric_limits<std::size_t>::max();

/// @brief invalid state index, used for no transition / placeholder
constexpr size_t INVALID_STATE_INDEX = std::numeric_limits<std::size_t>::max();

/// @brief epsilon character used by nfas
constexpr char EPSILON = '\0';