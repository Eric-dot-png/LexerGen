/// @file NFA.hpp
/// @brief Provides defnitions for NFA

#pragma once

#include <vector>
#include <cstddef>
#include <unordered_set>

/// @brief Represents a Non-deterministic Finite Automaton (NFA)
struct NFA
{
    /// @brief  Represents a transition from one state to another
    struct Transition
    {
        char symbol; ///< The input symbol for the transition
        size_t to; ///< The index of the result state
    };

    /// @brief Represents a state in the NFA
    struct State
    {
        const size_t index; ///< The index of the state
        const size_t caseTag; ///< The case tag associated with the state (if any)
        std::vector<Transition> transitions; ///< the transitions from this state
    };

    size_t start; ///< The index of the start state
    std::unordered_set<size_t> accept; ///< The indices of the accept states
    std::vector<State> states; ///< The states of the NFA
    size_t numCases; ///< the number of cases in this NFA

    const std::unordered_set<size_t> & Accepting() const { return accept; }
    const std::vector<State>& States() const { return states; }
    size_t Start() const { return start; }
};