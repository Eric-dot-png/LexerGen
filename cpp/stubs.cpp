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

#include <iostream>
#include <iomanip>
#include <vector>

/// @brief api method to process a rule from ocaml
/// @param ruleCases the list of rule cases (pattern, type, alias, action)
/// @return ocaml tuple of `(start state, dead state, number of states, transition table)`. 
///         the transition table is an ocaml array representing the transition table. 
///         `table[s][char] = s'` dead state does not have any outgoing transitions 
extern "C" CAMLprim value processRule(value rule_cases)
{
    CAMLparam1(rule_cases);

    /// gather up rule cases, and format them into c++ structures
    ///
    std::vector<RuleCase> ruleCases;
    while (rule_cases != Val_int(0))
    {
        value case_tuple = Field(rule_cases, 0);
        const char * const patternData = String_val(Field(case_tuple, 0));
        RuleCase::Pattern_t patternType = \ 
            static_cast<RuleCase::Pattern_t>(Int_val(Field(case_tuple, 1)));
        const char * const matchAlias = String_val(Field(case_tuple, 2));
        const char * const actionCode = String_val(Field(case_tuple, 3));

        std::cout << "Read pattern type " << (size_t)patternType << " : " << patternData 
            << std::endl; 
                
        ruleCases.emplace_back(patternData, patternType, matchAlias, actionCode);
        rule_cases = Field(rule_cases, 1);
    }
    
    /// use the rule cases the build an nfa, and then a dfa (unminimized : TODO)
    ///
    DFA m(NFABuilder::Build(ruleCases));
    std::cout << "DFA has " << m.States().size() << " states." << std::endl;
    std::cout << "DFA start state is " << m.Start() << std::endl;

    /// debug : draw the dfa
    /// 
    DrawStateMachine(m, "output/dfa.dot");

    /// convert the dfa's cpp transition table into an ocaml value
    ///
    value transition_table = caml_alloc(m.States().size(), 0);
    for (const DFA::State& state : m.States())
    {
        value state_transitions = caml_alloc(ALPHABET.size(), 0);
        for (const auto& [symbol, to] : state.transitions)
        {
            Store_field(state_transitions, symbol, Val_int(to));
        }
        Store_field(transition_table, state.index, state_transitions);
    }

    /// create and return tuple with start state, dead state, number of states, and 
    /// transition table
    ///
    value ret = caml_alloc(4, 0);
    Store_field(ret, 0, Val_int(m.Start()));
    Store_field(ret, 1, Val_int(m.Dead()));
    Store_field(ret, 2, Val_int(m.States().size()));
    Store_field(ret, 3, transition_table);
    CAMLreturn(ret);
}

/// @brief function to get the alphabet used in the lexer
/// @return the alphabet as an ocaml array of chars
extern "C" CAMLprim value getAlphabet()
{
    CAMLparam0();
    value ret = caml_alloc(ALPHABET.size(), 0);
    size_t i = 0;
    for (char c : ALPHABET)
    {
        Store_field(ret, i++, Val_long(c));
    }
    CAMLreturn(ret);
}