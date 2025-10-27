/// @file NFABuilder.hpp
/// @brief NFA builder class

#include "NFA.hpp"
#include "Regex.hpp"

#include <cstddef>
#include <vector>
#include <stack>

struct RuleCase;

class NFABuilder
{
public:
    /// -----------------------------------------------------------------------
    /// Explicitly delete constructors, destructor and operator=
    /// -----------------------------------------------------------------------
    NFABuilder() = delete;
    ~NFABuilder() = delete;
    NFABuilder(const NFABuilder&) = delete;
    NFABuilder(const NFABuilder&&) = delete;
    NFABuilder& operator=(const NFABuilder&) = delete;
    NFABuilder& operator=(const NFABuilder&&) = delete;

    /// -----------------------------------------------------------------------
    /// Public api methods 
    /// -----------------------------------------------------------------------

    /// @brief method to construct an nfa from a flat regex type 
    /// @tparam It the iteration type of the flat regex
    /// @param expr expressions to build the nfa from
    /// @return the constructed NFA from the regex
    template <Regex::ItOrder It>
    static NFA Build(const std::vector<Regex::Flat::Type>& exprs);

private:
    /// @brief struct representing a fragment. a fragment has an index, but
    ///        has transitions that are waiting to be filled
    struct Fragment
    {
        /// @brief struct representing a fragment's hole (to be patched)
        struct Hole
        {
            size_t holeIndex; ///< index of the state needing a transition
            char tVal; ///< known transition value from holeIndex 
        };
        size_t startIndex; ///< index of the starting state in this fragment 
        std::vector<Hole> holes; ///< transitions to be patched
    };

    /// @brief Method to build a nearly completed (untagged) NFA fragment from an RE
    /// @tparam It the iteration method 
    /// @param[in] expr the expression to build from
    /// @param[out] states the vector of states belonging to the nfa
    /// @return the nearly completed NFA fragment to be finalized.
    template <Regex::ItOrder It>
    static Fragment BuildFragment(const Regex::Flat::Type& expr, 
        std::vector<NFA::State>& states);

    
    /// @brief method to patch a fragment's holes (in the state vector)
    /// @param holes the vector of holes to patch
    /// @param patchIndex the index of the state to patch the holes with
    /// @param nfaStates the vector of nfa states
    static void PatchHoles(const std::vector<Fragment::Hole>& holes,
        size_t patchIndex, std::vector<NFA::State>& nfaStates);

    /// @brief method to create a new state with a case tag and add it to
    ///        the state vector
    /// @param nfaStates the state vector to add the new state to
    /// @param caseNo the case number to tag the state with
    /// @param estTCount the estimated number of transitions out of this state
    /// @return the index of the new state added (previous size of the array)
    static size_t NewState(std::vector<NFA::State>& nfaStates, size_t caseNo, size_t estTCount);

    /// @brief method to create a new state without a case tag and add it to
    ///        the nfa state vector
    /// @param nfaStates the state vector to add the new state to
    /// @param estTCount the case number to 
    /// @return the index of the new state added (previous size of the array)
    static size_t NewState(std::vector<NFA::State>& nfaStates, size_t estTCount);

    /// @brief method to create a character fragment
    /// @param a the transition (character) value
    /// @param nfaStates the state vector of the current nfa
    /// @return the constructed fragment
    static Fragment MakeChar(char a, std::vector<NFA::State>& nfaStates);

    /// @brief method to create a set/range of characters to transition from
    /// @param lo the low end of the range
    /// @param hi the hi end (inclusive) of the range
    /// @param inverted if the range is to be inverted (\Sigma - S)
    /// @param nfaStates the nfa states
    /// @return the constructed fragment 
    static Fragment MakeCharset(char lo, char hi, bool inverted, std::vector<NFA::State>& nfaStates);

    /// @brief method to create a literal/string fragment
    /// @param string the string to create the fragment from 
    /// @param nfaStates the nfa states
    /// @return the constructed fragment 
    static Fragment MakeLiteral(std::string_view string, std::vector<NFA::State>& nfaStates);

    /// @brief method to apply a concatination operator to two fragments
    /// @param left the left fragment 
    /// @param right the right fragment
    /// @param nfaStates
    /// @return the concatination of left->right
    static Fragment ApplyCat(const Fragment& left, const Fragment& right,
        std::vector<NFA::State>& nfaStates);

    /// @brief method to apply a union operator to two fragments
    /// @param left the left fragment
    /// @param right the right fragment
    /// @param nfaStates the vector of nfa states
    /// @return the constructed fragment
    static Fragment ApplyUnion(const Fragment& left, const Fragment& right,
        std::vector<NFA::State>& nfaStates);
    
    /// @brief method to apply a kleene star operator to a fragment
    /// @param fragment the fragment
    /// @param nfaStates the vector of nfa states
    /// @return the constructed fragment
    static Fragment ApplyKStar(const Fragment& fragment, 
        std::vector<NFA::State>& nfaStates);

    /// @brief method to apply a kleene plus operator to a fragment
    /// @param fragment the fragment
    /// @param nfaStates the vector of nfa states
    /// @return the constructed fragment
    static Fragment ApplyKPlus(const Fragment& fragment,
        std::vector<NFA::State>& nfaStates);
    
    static Fragment ApplyKOpt(const Fragment& fragment,
        std::vector<NFA::State>& nfaStates);

    static size_t ConcludeCase(size_t ruleNo, Fragment& ruleFragment, 
        std::vector<NFA::State>& nfaStates, 
        std::unordered_set<size_t>& nfaAccepting);

    /// -----------------------------------------------------------------------
    ///  Debug functions (to be removed)
    /// -----------------------------------------------------------------------
    static void Debug(const Fragment& frag);

};