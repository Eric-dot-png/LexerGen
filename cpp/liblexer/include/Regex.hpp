/// @file Regex.hpp
/// @brief Provides Flattened Regex structure definitions and 
///        utility functions used for postorder traversal

#pragma once

#include <string_view>
#include <variant>
#include <vector>
#include <ostream>
#include <unordered_set>

namespace Regex
{
    /// @brief type of iteration over regex containers
    enum class ItOrder : size_t
    {
        PRE = 0, /// pre-order 
        IN = 1, /// in-order
        POST = 2 /// post-order (fastest)
    };

    /** @brief NS for Flattened regex types. These types are not recursive 
     *         (non-ast), and are meant for postorder traversal
     */
    namespace Flat
    {
        /// 
        /// Terminal FlatRegex Types
        ///

        struct Char_t { char value; };
        struct Literal_t { std::string_view value; };
        struct Charset_t { 
            Charset_t(char lo, char hi, bool inv) 
                : chars({}), inverted(inv) 
            {
                for (int i = int(lo); i <= int(hi); ++i)
                    chars.insert(char(i));
            }
            Charset_t(const std::unordered_set<char>& set={}, bool inv=false)
                : chars(set), inverted(inv)
            { }

            std::unordered_set<char> chars; bool inverted; 
        
        };

        ///
        /// Non-Terminal FlatRegex Types
        ///
        
        struct Union_t { };
        struct Concat_t { };
        struct KleeneStar_t{ };

        ///
        /// Flat Regex symbol type 
        ///
        using Symbol = std::variant<Char_t,Literal_t,Charset_t,Union_t,
            Concat_t,KleeneStar_t>;

        ///
        /// Flat regex expression type
        ///
        using Type = std::vector<Symbol>;
    };
};


/** @brief Stream operator to write the string representation of a flat re 
 *         symbol to an ostream inheritant.  
 *  @return ostream&
 */
std::ostream& operator<<(std::ostream&, const Regex::Flat::Symbol&);

/** @brief Stream operato to write the string representation of a flat re
 *         type to an ostream inheritant.
 */
std::ostream& operator<<(std::ostream&, const Regex::Flat::Type&);