/// file : stubs.cpp
/// brief : entry point for ocaml interface

#include "RuleCase.hpp"

#include <caml/mlvalues.h>
#include <caml/memory.h>
#include <caml/alloc.h>
#include <caml/custom.h>
#include <caml/fail.h>

#include <iostream>
#include <vector>
#include <string_view>

extern "C" 
{
    CAMLprim value processRule(value ruleName, value ruleCases)
    {
        CAMLparam2(ruleName, ruleCases);

        std::vector<RuleCase> rcs;

        while (ruleCases != Val_int(0)) // while ruleCases != []
        {
            value tuple = Field(ruleCases, 0);

            const char * const patternData = String_val(Field(tuple, 0));
            RuleCase::Pattern_t patternType = static_cast<RuleCase::Pattern_t>(Field(tuple, 1));
            const char * const matchAlias = String_val(Field(tuple, 2));
            const char * const actionCode = String_val(Field(tuple, 3));

            rcs.emplace_back(patternData, patternType, matchAlias, actionCode);

            ruleCases = Field(ruleCases, 1);
        }

        for (const RuleCase& rc : rcs)
        {
            std::cout << rc.patternData << std::endl;
        }


        CAMLreturn(Val_unit);
    }
}