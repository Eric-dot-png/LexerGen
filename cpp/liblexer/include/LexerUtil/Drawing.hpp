/// @file Drawing.hpp
/// @brief Utility functions for drawing lexer structures

#pragma once

#include "LexerUtil/Constants.hpp"
#include "LexerUtil/Misc.hpp"
#include "LexerUtil/Macros.hpp"
#include "LexerUtil/Macros.hpp"

#include <fstream>

template <typename SM_t>
static void DrawStateMachine(const SM_t& sm, const char * const outFilePath)
{
    std::fstream file(outFilePath, std::ios::out);
    ENSURES_THROW(file.is_open(), std::format("Could not open file '{}'", outFilePath));

    std::unordered_map<size_t, std::unordered_map<size_t, std::string>> labelMap;
    
    file << "digraph StateMachine {\n"
        << "    rankdir=LR;\n"
        << "    hiddenStart[shape=point, width=0, label=\"\"];\n"
        << std::format("    hiddenStart -> q{};\n", sm.Start());

    for (const auto& state : sm.States())
    {
        std::string shape = (state.caseTag != NO_CASE_TAG ? "doublecircle" : "circle");
        file << std::format("    q{} [shape={}, label=\"q{}\"];\n", state.index, shape, state.index);
    }

    for (const auto& state : sm.States())
    {
        labelMap[state.index];
        for (const auto& [symbol, result] : state.transitions)
        {
            labelMap[state.index][result] += Escaped(symbol);
        }
        for (const auto& [result, label] : labelMap[state.index])
        {
            file << std::format("    q{} -> q{} [label=\"{}\"];\n", state.index, result, label);
        }
    }

    file << "}\n";
}