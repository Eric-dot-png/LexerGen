/// @file stubs.cpp
/// @brief entry point for ocaml interface
/// @brief in this file, cpp vars are written in caml case, and ocaml vals are in snake case

#include "liblexer/include/RuleCase.hpp"
#include "liblexer/include/NFABuilder.hpp"
#include "liblexer/include/DFA.hpp"
#include "liblexer/include/LexerUtil/Drawing.hpp"
#include "liblexer/include/LexerUtil/Constants.hpp"

#include <caml/mlvalues.h>
#include <caml/memory.h>
#include <caml/alloc.h>
#include <caml/custom.h>
#include <caml/fail.h>

#include <limits>
#include <iostream>
#include <iomanip>
#include <vector>

/// @brief the smallest char value [inclusive]
constexpr size_t CHAR_MIN = std::numeric_limits<unsigned char>::min();

/// @brief the largest char value [inclusive]
constexpr size_t CHAR_MAX = std::numeric_limits<char>::max();

/// @brief api method to process a rule from ocaml
/// @param ruleCases the list of rule cases (pattern, type, alias, action)
/// @return ocaml tuple of 
///         `(start state, dead state, number of states, transition table, case tag table)`. 
///         the transition table is an ocaml array representing the transition table. 
///         `table[s][char] = s'` dead state does not have any outgoing transitions.
///         the case tag table is an ocaml array representing the case tag for each state.
///         `case_table[state_tag] = case_tag` where `case_tag` is the case number associated with `state` 
///         if any, or `NO_CASE_TAG = -1` if no rule is associated with the state
extern "C" CAMLprim value processRule(value rule_cases)
{
    CAMLparam1(rule_cases);
    
    /// gather up rule cases, and format them into c++ structures
    ///
    CAMLlocal1(case_tuple);
    std::vector<RuleCase> ruleCases;
    while (rule_cases != Val_int(0))
    {
        case_tuple = Field(rule_cases, 0);
        const char * const patternData = String_val(Field(case_tuple, 0));
        RuleCase::Pattern_t patternType = \ 
            static_cast<RuleCase::Pattern_t>(Int_val(Field(case_tuple, 1)));
        const char * const matchAlias = String_val(Field(case_tuple, 2));
        const char * const actionCode = String_val(Field(case_tuple, 3));

        ruleCases.emplace_back(patternData, patternType, matchAlias, actionCode);
        rule_cases = Field(rule_cases, 1);
    }
    const size_t NUM_CASES = ruleCases.size();

    /// use the rule cases the build an nfa, and then a dfa (unminimized : TODO)
    ///
    DFA lexDfa(NFABuilder::Build(ruleCases));

    const size_t NUM_STATES = lexDfa.States().size();

    /// debug : draw the dfa
    /// 
    DrawStateMachine(lexDfa, "output/dfa.dot");

    /// convert the dfa's cpp transition table into an ocaml value and store the case tags
    /// as a seperate array. Initialize the seperate array to NO_CASE_TAG values.
    /// 
    CAMLlocal3(transition_table, state_transitions, case_table);
    transition_table = caml_alloc(NUM_STATES, 0);
    case_table = caml_alloc(NUM_STATES, 0);
    
    for (size_t case_index = 0; case_index < NUM_CASES; ++case_index)
    {
        Store_field(case_table, case_index, Val_int((int) NO_CASE_TAG));
    }

    for (const DFA::State& state : lexDfa.States())
    {
        Store_field(case_table, state.index, Val_int(state.caseTag));
        state_transitions = caml_alloc(CHAR_MAX - CHAR_MIN + 1, 0);
        for (int symbol = CHAR_MIN; symbol <= CHAR_MAX; ++symbol)
        {
            size_t toState = (ALPHABET.contains((char)symbol) ? 
                state.transitions.at((char)symbol) : lexDfa.Dead());
            Store_field(state_transitions, (char)symbol , Val_int( (int) toState) );
        }
        Store_field(transition_table, (int) state.index, state_transitions);
    }

    /// create and return tuple with start state, dead state, number of states, and 
    /// transition table
    ///
    CAMLlocal1(ret);
    ret = caml_alloc_tuple(5);
    Store_field(ret, 0, Val_int(lexDfa.Start()));
    Store_field(ret, 1, Val_int(lexDfa.Dead()));
    Store_field(ret, 2, Val_int(NUM_STATES));
    Store_field(ret, 3, transition_table);
    Store_field(ret, 4, case_table);
    CAMLreturn(ret);
}


/// @brief function to get the alphabet used in the lexer
/// @return the alphabet as an ocaml array of chars
extern "C" CAMLprim value getAlphabet()
{
    CAMLparam0();
    CAMLlocal1(ret);
    ret = caml_alloc(ALPHABET.size(), 0);
    size_t symbol = 0;
    for (char c : ALPHABET)
    {
        Store_field(ret, symbol++, Val_long(c));
    }
    CAMLreturn(ret);
}