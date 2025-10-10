/// @file DFA.hpp
/// @brief Provides the declarations for the DFA class

#pragma once

#include <cstddef>
#include <unordered_map>
#include <unordered_set>
#include <vector>

struct NFA;

/// @brief DFA class
class DFA
{
public:
    struct State
    {
        size_t index;
        size_t caseTag;
        std::unordered_map<char, size_t> transitions;
    };

    DFA(const NFA& nfa);

    size_t Start() const;
    size_t Dead() const;
    const std::vector<State>& States() const;

    static void Minimize(DFA& dfa);

private:
    DFA();
    static void Powerset(const NFA& nfa, DFA& dfa);
    
    size_t start_; ///< starting state index
    size_t deadState_; ///< dead state index
    size_t numCases_; ///< number of cases in this dfa
    std::vector<State> states_; ///< state vector
};
