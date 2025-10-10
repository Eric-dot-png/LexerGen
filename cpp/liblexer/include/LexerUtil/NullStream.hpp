/// @file NullStream.hpp
/// @brief Defines a null output stream for discarding output (release mode)

#pragma once

#include <ostream>

/// @brief A stream buffer that discards all output
struct NullBuffer : std::streambuf
{
    int overflow(int c) override { return c; }
};

/// @brief An output stream that discards all output
struct NullStream : std::ostream
{
    NullStream() : std::ostream(&m_sb) {}
private:
    NullBuffer m_sb;
};

/// @brief An instance of NullStream 
inline NullStream nullout;