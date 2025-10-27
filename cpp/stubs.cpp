/// @file stubs.cpp
/// @brief entry point for ocaml interface
/// @brief in this file, cpp vars are written in caml case, and ocaml vals are in snake case

#include "liblexer/include/NFABuilder.hpp"
#include "liblexer/include/DFA.hpp"
#include "liblexer/include/Regex.hpp"

#include <caml/mlvalues.h>
#include <caml/memory.h>
#include <caml/alloc.h>
#include <caml/custom.h>

#include <unordered_map>
#include <functional>
#include <string>
#include <vector>

/// -----------------------------------------------------------------------
/// Interface Constants 
/// -----------------------------------------------------------------------


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
    FUNION,
    FCAT,
    FSTAR, 
    SIZE // total number of ocaml tags in this enum 
};


/** @brief Function type alias for converting ocaml re symbol to cpp re symbol 
 */
using ConvertFunction_t = std::function<Regex::Flat::Symbol(value)>;


/// -----------------------------------------------------------------------
/// Non Ocaml Accessable Function Prototypes
/// -----------------------------------------------------------------------


Regex::Flat::Type MakeCppRe(value ocaml_re_pair);
Regex::Flat::Char_t MakeChar(value ocaml_symbol);
Regex::Flat::Literal_t MakeLiteral(value ocaml_symbol, 
        std::vector<std::string>& stringBuf);
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
    CAMLprim value MakeDFA(value ocaml_re_list, value ocaml_list_len)
    {
        CAMLparam2(ocaml_re_list, ocaml_list_len);
        CAMLlocal1(ocaml_re);

        // allocate a vector to hold the res from ocaml
        int lenList = Val_long(ocaml_list_len);
        std::vector<Regex::Flat::Type> cppREs; 
        cppREs.reserve(static_cast<size_t>(lenList));

        // loop over the ocaml_re_list and convert the re to
        // cpp versions of the res
        while (ocaml_re_list != Val_int(0)) // while list != []
        {
            ocaml_re = Field(ocaml_re_list, 0);
            cppREs.emplace_back(MakeCppRe(ocaml_re));
            ocaml_re_list = Field(ocaml_re_list, 1); // advance head
        }
    }

}


/// -----------------------------------------------------------------------
/// Non Ocaml Accessable Function Definitions  
/// -----------------------------------------------------------------------


/** @brief Function to convert an ocaml regex to a cpp regex
 * 
 *  @param ocaml_re_pair ocaml tuple of a postorder re (as a list) and size
 * 
 *  @return Constructed cpp re
 */
Regex::Flat::Type MakeCppRe(value ocaml_re_pair)
{
    // ocaml var declares
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

    ocaml_re = Field(ocaml_re_pair, 0);
    int reSize = Int_val(Field(ocaml_re_pair, 1));

    // create the cpp re instance 
    Regex::Flat::Type cppRe; 
    cppRe.reserve(static_cast<size_t>(reSize));

    // create a buffer for strings in ocaml. Must do this because 
    // std::string_view does not work with ocaml strings (cant deduce size)
    std::vector<std::string> stringBuffer;
    stringBuffer.reserve(static_cast<size_t>(reSize));

    // now that we've created the string buffer for this function, add the
    // string conversion function (which depends on the buffer)
    convMap.emplace(
        OcamlReTags::FSTRING, 
        [&stringBuffer](value v){ return MakeLiteral(v, stringBuffer); }
    );
    
    // process all symbols, converting them into cpp re symbols
    while (ocaml_re != Int_val(0)) // while ocaml_re != []
    {
        // get the current symbol and deduce it's tag
        ocaml_re_sym = Field(ocaml_re, 0);
        OcamlReTags tag = static_cast<OcamlReTags>(Tag_val(ocaml_re_sym));
        
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
 *  @param stringBuf cpp memory buffer for strings. Warning: If a pushback 
 *                   requires the buf vector to be resized, it will cause the 
 *                   cpp re literal's view of the string to be invalid, causing 
 *                   mem/seg fault. Allocate buffer vector properly.
 *  
 *  @return Constructed cpp re literal type
 */
inline Regex::Flat::Literal_t MakeLiteral(value ocaml_symbol, 
    std::vector<std::string>& stringBuf)
{
    const char * const pStr = String_val(Field(ocaml_symbol, 0));
    stringBuf.emplace_back(pStr);
    
    return Regex::Flat::Literal_t{ std::string_view ( stringBuf.back() ) };
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
    bool inv = static_cast<bool>(Bool_val(Field(ocaml_symbol, 2)));

    return Regex::Flat::Charset_t{ lo, hi, inv };
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