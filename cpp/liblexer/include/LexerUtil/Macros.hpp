/// @file Macros.hpp
/// @brief Common macros used across the project
/// @note This file should NOT be included in header files to avoid macro pollution

#pragma once

#include "NullStream.hpp"

#include <iostream>
#include <stdexcept>
#include <format>
#include <string_view>

/// @brief Function to throw a structured, formatted error
/// @param file the file the error was thrown in
/// @param line the line number of the file where the error was thrown
/// @param message the message associated with the error
inline void ThrowFormattedError(const char * const file,
    size_t line, std::string_view message)
{
    throw std::invalid_argument(std::format("Error: {}:{} - {}", file, line, message));
}

/// @brief Inline function to throw an error if a condition is not true
/// @param cond the condition to test
/// @param file the string of the file number called in
/// @param line the line number in the file called in 
/// @param message the message of the error 
inline void ThrowIFNTrue(bool cond, const char * const file, 
    size_t line, std::string_view message)
{
    if (!cond) 
    {
        ThrowFormattedError(file, line, message);
    }
}

/// @brief macro to call ThrowFormattedError with file and line params already
///        populated. This is used to ALWAYS throw an error thats formatted.
#define THROW_ERR(xMsg) ThrowFormattedError(__FILE__, __LINE__, xMsg);

/// @brief macro to call the ThrowIFNTrue method with file and line number already 
///        populated. This is used to check conditions at the start of a code block
#define EXPECTS_THROW(xCond, xMsg) ThrowIFNTrue(xCond, __FILE__, __LINE__, xMsg);

/// @brief macro to call the ThrowIFNTrue method with file and line number already 
///        populated. This is used to check conditions at the end of a code block
#define ENSURES_THROW(xCond, xMsg) ThrowIFNTrue(xCond, __FILE__, __LINE__, xMsg);

#define UNREACHABLE() __builtin_unreachable()

/// @brief Debug output stream. In release mode, this outputs to a null stream. 
///        In debug mode, this outputs to DBG.
#ifdef DEBUG_MODE
#warning "Debug mode enabled"
#define DBG std::cout
#else
#define DBG nullout
#endif