/// @file ruleCase.hpp
/// @brief Provides the definition for Pattern and it's utilities

#pragma once

#include <cstddef>
#include <string>
#include <optional>

/// @brief struct to represent a pattern 
struct RuleCase
{
    /// @brief Type enum to represent possible pattern types
    enum class Pattern_t : size_t
    {
        REGEX       = 0, // regular expression
        STRING      = 1, // literal string
        NONE        = 2, // no-match 
        END_OF_FILE = 3  // end of file 
    };

    std::string patternData; ///< data that the pattern holds (the pattern)
    Pattern_t patternType; ///< type of the pattern (Regex, string, eof , ... ) 
    std::string matchAlias; ///< opt alias for matched input
    std::string actionCode; ///< code to perform when the pattern is matched 

};