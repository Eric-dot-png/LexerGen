/// @file PreProcessor.hpp
/// @brief Header of PreProcessor

#pragma once

#include <string>
#include <cstdint>
#include <vector>

struct RuleCase;

/// @brief static class to process regular expressions into NFAs
class PreProcessor
{
public:
    /// -----------------------------------------------------------------------
    /// Explicitly delete constructors, destructor and operator=
    /// -----------------------------------------------------------------------

    PreProcessor() = delete;
    ~PreProcessor() = delete;
    PreProcessor(const PreProcessor&) = delete;
    PreProcessor(const PreProcessor&&) = delete;
    PreProcessor& operator=(const PreProcessor&) = delete;
    PreProcessor& operator=(const PreProcessor&&) = delete;

    /// -----------------------------------------------------------------------
    /// Public api methods
    /// -----------------------------------------------------------------------

    /// @brief method to preprocess a pattern
    /// @param pattern the pattern to modify
    static void PreProcess(RuleCase& pattern);

    /// @brief method to preprocess a vector of patterns
    /// @param patterns the patterns to pre-process
    static void PreProcess(std::vector<RuleCase>& patterns);

    /// @brief enum class representing the publicly available operators, where
    ///        their values represent their operator precidence (other than LPAREN RPAREN)
    enum class Operator_t : uint32_t
    {
        UNION = 0,
        CONCAT = 1,
        KSTAR = 2,
        KPLUS = 3, 
        OPTIONAL = 4,
        LPAREN = 5,
        RPAREN = 6
    };

    static bool IsOperator(char c);
    static Operator_t OperatorOf(char c);
    static uint32_t PriorityOf(Operator_t op);
    static bool isBinary(Operator_t op);

private:

    /// -----------------------------------------------------------------------
    /// Pre-Processing functions
    /// -----------------------------------------------------------------------

    /// @brief function to process a pattern's range operators
    /// @param pattern the pattern to modify
    static void UnifyRanges(std::string& pattern);

    /// @brief function to insert concatination operators 
    /// @param pattern the pattern to modify
    static void InsertConcats(std::string& pattern);

    /// @brief method to convert a regex pattern to RPN
    /// @param pattern the pattern to change
    static void makeRPN(std::string& pattern);

    /// -----------------------------------------------------------------------
    /// Pre-Processing Utility Functions / Enums
    /// -----------------------------------------------------------------------

    /// @brief enum class to represent decoded operators (as they appear)
    enum class OpDecoded : char 
    {
        UNION     = '|', 
        CONCAT    = '.', 
        KLEENE    = '*', 
        PLUS      = '+',
        OPTIONAL  = '?', 
        LPAREN    = '(', 
        RPAREN    = ')', 
        LBRACE    = '[',
        RBRACE    = ']', 
        INVERT    = '^', 
        RANGE_MID = '-'
    };

    /// @brief Decode a character, treating it as a literal no matter what
    /// @param c the character to decode
    /// @return the decoded character
    static char Decode(char c);

    /// @brief enum class to represent encoded operators 
    enum class OpEncoded : char
    {
        UNION     = 0x01, 
        CONCAT    = 0x02, 
        KLEENE    = 0x03, 
        PLUS      = 0x04, 
        OPTIONAL  = 0x05, 
        LPAREN    = 0x06, 
        RPAREN    = 0x07, 
        LBRACE    = 0x08,
        RBRACE    = 0x11, 
        INVERT    = 0x12, 
        RANGE_MID = 0X13
    };
    
    /// @brief Encode the operators in the pattern to their encoded values
    /// @param op the operator to encode
    /// @return the encoded operator
    static OpEncoded EncodeOp(OpDecoded op);

    /// @brief encode a pattern's operators to their encoded values
    /// @param pattern the pattern to encode
    static void Encode(std::string& pattern);

    /// @brief enum class to represent different "classes" of characeters
    enum class SymbolClass : uint32_t
    {
        LITERAL, 
        BINARY_OP, 
        UNARY_OP, 
        LPAREN,
        RPAREN, 
        RANGE_OP
    };
    
    /// @brief method to return the type of a character in a regex
    /// @tparam which operator type to use to check. 
    ///         Instantiated explicitly with encoded and decoded op enum types
    /// @param c the character to check
    /// @return the type of the character
    template <typename Operator_t>
    static SymbolClass GetType(char c);

    /// -----------------------------------------------------------------------
    /// Pre-Processing functions
    /// -----------------------------------------------------------------------

    static void PrintRegex(std::ostream& os, std::string_view pattern);
    static std::string RegexStr(std::string_view pattern);
};