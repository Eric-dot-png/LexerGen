/// @file stubs.cpp
/// @brief entry point for ocaml interface

#include "liblexer/include/NFABuilder.hpp"
#include "liblexer/include/DFA.hpp"
#include "liblexer/include/Regex.hpp"
#include "liblexer/include/LexerUtil/Constants.hpp"
#include "liblexer/include/LexerUtil/Drawing.hpp"

#include <caml/mlvalues.h>
#include <caml/memory.h>
#include <caml/alloc.h>
#include <caml/custom.h>

#include <unordered_map>
#include <functional>
#include <string>
#include <vector>
#include <iostream>

/// ---------------------------------------------------------------------------
/// Interface Constants 
/// ---------------------------------------------------------------------------


/** @brief Enum for Ocaml Tags (see MyParsing.mli - flat_regex)
 *  @note Ocaml types inside a recursive are tagged starting with 0 in order
 *        of their declaration. This tag is exposed to c such that we can
 *        determine the type's tag and operate with that assumption. 
 */
enum class OcamlReTags : size_t
{
    FCHAR = 0, 
    FSTRING,
    FCHAR_RANGE,
    START_OPS, 
    FUNION = START_OPS,
    FCAT,
    FSTAR, 
    SIZE // total number of ocaml tags in this enum 
};


/** @brief Function type alias for converting ocaml re symbol to cpp re symbol 
 */
using ConvertFunction_t = std::function<Regex::Flat::Symbol(value)>;


/// ---------------------------------------------------------------------------
/// Non Ocaml Accessable Function Prototypes
/// ---------------------------------------------------------------------------

OcamlReTags DeduceTag(value ocaml_re_sym);

Regex::Flat::Type MakeCppRe(value ocaml_re_pair, 
    std::vector<std::string>& stringBuffer);

Regex::Flat::Char_t MakeChar(value ocaml_symbol);

Regex::Flat::Literal_t MakeLiteral(value ocaml_symbol, 
    std::vector<std::string>& stringBuffer);

Regex::Flat::Charset_t MakeCharset(value ocaml_symbol);

template <typename OP> OP MakeFlatOp([[maybe_unused]] value ocaml_symbol);


/// ---------------------------------------------------------------------------
/// Ocaml Accessable Functions 
/// ---------------------------------------------------------------------------


extern "C"
{
    /** @brief Function to make a lexer dfa from a set of inputted RE postorder
     *         trees flattened as a list. 
     * 
     *  @param ocaml_re_list list of postorder RE passed from ocaml
     *  @param ocaml_list_Len length of the re_list passed from ocaml
     * 
     *  @return ocaml tuple of: 
     *          1. transition table ([state index][symbol] -> result index)
     *          2. case tag table ([state index] -> state tag)
     *          3. start state index
     *          4. dead state index
     *          5. size of dfa (num states)
     */
    CAMLprim value MakeDFA(value ocaml_re_list, value ocaml_list_len, 
        value ocaml_num_str)
    {
        CAMLparam2(ocaml_re_list, ocaml_list_len);
        CAMLlocal1(ocaml_re);

        // allocate a vector to hold the res from ocaml
        const size_t LEN_RE_LIST = size_t(Val_int(ocaml_list_len));
        std::vector<Regex::Flat::Type> cppREs; 
        cppREs.reserve(static_cast<size_t>(LEN_RE_LIST));

        // allocate a vector of strings used as literals
        const size_t NUM_STRINGS = size_t(Val_int(ocaml_num_str));
        std::vector<std::string> stringBuffer;
        stringBuffer.reserve(NUM_STRINGS);

        // loop over the ocaml_re_list and convert the re to
        // cpp versions of the res
        while (ocaml_re_list != Val_emptylist) // while list != []
        {
            ocaml_re = Field(ocaml_re_list, 0);
            cppREs.emplace_back(MakeCppRe(ocaml_re, stringBuffer));
            ocaml_re_list = Field(ocaml_re_list, 1); // advance head
        }

        for (const Regex::Flat::Type& re : cppREs)
        {
            std::cout << re << std::endl;
        }

        // use the library to build the dfa
        const NFA n(NFABuilder::Build<Regex::ItOrder::POST>(cppREs));
        const DFA m( n );
        const size_t NUM_STATES = m.States().size();

        DrawStateMachine(n, "output/nfa.dot");
        DrawStateMachine(m, "output/dfa.dot");

        std::cout << "Start: " << m.Start() << '\n'
            << "Dead: " << m.Dead() << std::endl
            << "ttable: " << std::endl;

        for (const DFA::State& state : m.States())
        {
            std::cout << state.index << " : ";
            std::cout << m.Dead() << ", ";
            for (char sym : ALPHABET)
            {
                std::cout << state.transitions.at(sym) << ", ";
            }
            std::cout << m.Dead() << std::endl;
        }

        std::cout << "ctable: " << std::endl;
        for (const DFA::State& state : m.States())
        {
            std::cout << state.index << " : " << state.caseTag << std::endl;
        }

        // now convert the dfa to ocaml-friendly types
        CAMLlocal3(ocaml_ttable, ocaml_state_ttable, ocaml_ctable);
        ocaml_ttable = caml_alloc(NUM_STATES, 0);
        ocaml_ctable = caml_alloc(NUM_STATES, 0);
        
        // initialize case table with empty cases
        for (size_t case_index = 0; case_index < LEN_RE_LIST; ++case_index)
        {
            Store_field(ocaml_ctable, int(case_index), 
                Val_int((int) NO_CASE_TAG));
        }

        // iterate over the state vector updating case and transition tables
        for (const DFA::State& state : m.States())
        {
            // allocate space for an individual state's transition table
            ocaml_state_ttable = caml_alloc(ALPHABET_SIZE+2, 0);
            
            // include symbols not included in alphabet, but in ascii 
            Store_field(ocaml_state_ttable, 0, Val_int(m.Dead()));
            
            // iterate over the alphabet and get the state's result transition
            // over the symbol
            for (char sym : ALPHABET)
            {
                Store_field(ocaml_state_ttable, int(sym), 
                    Val_int(state.transitions.at(sym))); 
            }
            // store the individual state's ttable in the master ttable 
            Store_field(ocaml_ttable, int(state.index), ocaml_state_ttable);
            
            Store_field(ocaml_state_ttable, 127, Val_int(m.Dead()));

            // update the case table with the state and the state's tag
            Store_field(ocaml_ctable, state.index, Val_int(state.caseTag));
        }

        // create and return tuple with start state, dead state, number of 
        // states, and transition table
        CAMLlocal1(ret);
        ret = caml_alloc_tuple(5);
        Store_field(ret, 0, ocaml_ttable);
        Store_field(ret, 1, ocaml_ctable);
        Store_field(ret, 2, Val_int(m.Start()));
        Store_field(ret, 3, Val_int(m.Dead()));
        Store_field(ret, 4, Val_int(NUM_STATES));
        CAMLreturn(ret);
    }
}


/// ---------------------------------------------------------------------------
/// Non Ocaml Accessable Function Definitions  
/// ---------------------------------------------------------------------------


/** @brief Function to convert an ocaml regex to a cpp regex
 * 
 *  @param ocaml_re_pair ocaml tuple of a postorder re (as a list) and size
 * 
 *  @return Constructed cpp re
 */
Regex::Flat::Type MakeCppRe(value ocaml_re_pair, 
    std::vector<std::string>& stringBuffer)
{
    CAMLparam0();
    CAMLlocal2(ocaml_re, ocaml_re_sym);

    // define the static conversion function map
    static std::unordered_map<OcamlReTags, ConvertFunction_t> convMap = 
    {
        {OcamlReTags::FCHAR, MakeChar},
        {OcamlReTags::FCHAR_RANGE, MakeCharset},
        {OcamlReTags::FCAT, MakeFlatOp<Regex::Flat::Concat_t>},
        {OcamlReTags::FUNION, MakeFlatOp<Regex::Flat::Union_t>},
        {OcamlReTags::FSTAR, MakeFlatOp<Regex::Flat::KleeneStar_t>}  
    };

    // get the ocaml re instance and it's size
    ocaml_re = Field(ocaml_re_pair, 0);
    size_t THIS_RE_SIZE = size_t(Int_val(Field(ocaml_re_pair, 1)));
    
    // create the cpp re instance 
    Regex::Flat::Type cppRe; 
    cppRe.reserve(static_cast<size_t>(THIS_RE_SIZE));

    // now that we've created the string buffer for this function, add the
    // string conversion function (which depends on the buffer)
    convMap.emplace(
        OcamlReTags::FSTRING, 
        [&stringBuffer](value v){ return MakeLiteral(v, stringBuffer); }
    );
    
    // process all symbols, converting them into cpp re symbols
    while (ocaml_re != Val_emptylist)
    {
        // get the current symbol and deduce it's tag
        ocaml_re_sym = Field(ocaml_re, 0);
        OcamlReTags tag = DeduceTag(ocaml_re_sym);
        
        // add the converted re sym to the cppRE
        cppRe.emplace_back( convMap[tag](ocaml_re_sym) );

        // advance the head
        ocaml_re = Field(ocaml_re, 1);
    }

    // now cleanup. Important to remove the MakeLiteral function ptr we 
    // added earlier, as the conv map is static. 
    convMap.erase(OcamlReTags::FSTRING);

    return cppRe;
}


/** @brief Function to make a cpp char type from an ocaml symbol.
 * 
 *  @param ocaml_symbol ocaml symbol
 * 
 *  @return Constructed cpp re char type 
 */
inline Regex::Flat::Char_t MakeChar(value ocaml_symbol)
{
    char c = static_cast<char>(Int_val(Field(ocaml_symbol, 0)));
    return Regex::Flat::Char_t{ c };
}


/** @brief Function to make a cpp re literal (string) type from an ocaml symbol.
 * 
 *  @param ocaml_symbol ocaml symbol
 *  @param stringBuffer cpp memory buffer for strings. Warning: If a pushback 
 *                   requires the buf vector to be resized, it will cause the 
 *                   cpp re literal's view of the string to be invalid, causing 
 *                   mem/seg fault. Allocate buffer vector properly.
 *  
 *  @return Constructed cpp re literal type
 */
inline Regex::Flat::Literal_t MakeLiteral(value ocaml_symbol, 
    std::vector<std::string>& stringBuffer)
{
    const char * const pStr = String_val(Field(ocaml_symbol, 0));
    stringBuffer.emplace_back(pStr);
    return Regex::Flat::Literal_t{ std::string_view ( stringBuffer.back() ) };
}


/** @brief Function to make a cpp re charset type from an ocaml symbol.
 * 
 *  @param ocaml_symbol ocaml symbol
 *  
 *  @return Constructed cpp re charset type
 */
inline Regex::Flat::Charset_t MakeCharset(value ocaml_symbol)
{
    char lo = static_cast<char>(Int_val(Field(ocaml_symbol, 0)));
    char hi = static_cast<char>(Int_val(Field(ocaml_symbol, 1)));
    // todo : bool inv = static_cast<bool>(Bool_val(Field(ocaml_symbol, 2)));

    return Regex::Flat::Charset_t{ lo, hi, false };
}


/** @brief Function to make a cpp <OP> type from an ocaml symbol.
 * 
 *  @tparam OP the operator type
 * 
 *  @param ocaml_symbol ocaml symbol, which is unused as flat operators are 
 *                      empty and don't require space
 *  
 *  @return constructed re <op> type
 */
template <typename OP>
OP MakeFlatOp([[maybe_unused]] value ocaml_symbol)
{
    return OP{ };
}

OcamlReTags DeduceTag(value ocaml_re_sym)
{
    if (Is_block(ocaml_re_sym))
    {
        return static_cast<OcamlReTags>(Tag_val(ocaml_re_sym));
    }
    else
    {
        static constexpr size_t OFFSET = size_t(OcamlReTags::START_OPS);
        return static_cast<OcamlReTags>(Long_val(ocaml_re_sym) + OFFSET);
    }
}