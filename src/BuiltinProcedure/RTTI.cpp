// SPDX-License-Identifier: MIT

#include "Interface/Scope.hpp"
#include "Interface/Value.hpp"
#include <fmt/format.h>

namespace schemepp {

#define PREFIX "Builtin.BaseLibrary.RTTI."

#define BUILTIN_RTTI_DEFINE(CLASS, TYPE)                                                               \
    class CLASS final : public Procedure {                                                             \
    public:                                                                                            \
        void printValue(std::ostream& stream) const override {                                         \
            stream << PREFIX #CLASS;                                                                   \
        }                                                                                              \
        Result<Ref<Value>> apply(EvaluateContext&, const std::vector<Ref<Value>>& operands) override { \
            if(operands.size() != 1)                                                                   \
                return Result<Ref<Value>>{ Error{                                                      \
                    fmt::format("Wrong operands count. Expect 1, but got {}.", operands.size()) } };   \
                                                                                                       \
            return Result{ constantBoolean(operands[0]->type() == ValueType::TYPE) };                  \
        }                                                                                              \
    }

    BUILTIN_RTTI_DEFINE(IsInteger, integer);
    BUILTIN_RTTI_DEFINE(IsByteVector, byteVector);
    BUILTIN_RTTI_DEFINE(IsBoolean, boolean);
    BUILTIN_RTTI_DEFINE(IsVector, vector);
    BUILTIN_RTTI_DEFINE(IsString, string);
    BUILTIN_RTTI_DEFINE(IsCharacter, character);
    BUILTIN_RTTI_DEFINE(IsComplex, complex);
    BUILTIN_RTTI_DEFINE(IsSymbol, symbol);
    BUILTIN_RTTI_DEFINE(IsList, list);
    BUILTIN_RTTI_DEFINE(IsPair, pair);

    class IsNumber final : public Procedure {
    public:
        void printValue(std::ostream& stream) const override {
            stream << PREFIX "IsNumber";
        }
        Result<Ref<Value>> apply(EvaluateContext&, const std::vector<Ref<Value>>& operands) override {
            if(operands.size() != 1)
                return Result<Ref<Value>>{ Error{ fmt::format("Wrong operands count. Expect 1, but got {}.", operands.size()) } };

            const auto type = operands[0]->type();
            const auto res = type == ValueType::complex || type == ValueType::integer || type == ValueType::real;

            return Result{ constantBoolean(res) };
        }
    };

    class IsRational final : public Procedure {
    public:
        void printValue(std::ostream& stream) const override {
            stream << PREFIX "IsRational";
        }
        Result<Ref<Value>> apply(EvaluateContext&, const std::vector<Ref<Value>>& operands) override {
            if(operands.size() != 1)
                return Result<Ref<Value>>{ Error{ fmt::format("Wrong operands count. Expect 1, but got {}.", operands.size()) } };

            const auto type = operands[0]->type();
            const auto res = type == ValueType::integer || type == ValueType::real;

            return Result{ constantBoolean(res) };
        }
    };

#undef BUILTIN_RTTI_DEFINE

    void initializeBuiltinRTTIProcedure(Scope& scope) {
#define ADD_BUILTIN_PROCEDURE(NAME, CLASS) scope.insert(NAME, makeRefCount<CLASS>())  // NOLINT(cppcoreguidelines-macro-usage)

        ADD_BUILTIN_PROCEDURE("integer?", IsInteger);
        ADD_BUILTIN_PROCEDURE("real?", IsRational);
        ADD_BUILTIN_PROCEDURE("bytevector?", IsByteVector);
        ADD_BUILTIN_PROCEDURE("boolean?", IsBoolean);
        ADD_BUILTIN_PROCEDURE("vector?", IsVector);
        ADD_BUILTIN_PROCEDURE("string?", IsString);
        ADD_BUILTIN_PROCEDURE("char?", IsCharacter);
        ADD_BUILTIN_PROCEDURE("number?", IsNumber);
        ADD_BUILTIN_PROCEDURE("rational?", IsRational);
        ADD_BUILTIN_PROCEDURE("complex?", IsComplex);
        ADD_BUILTIN_PROCEDURE("symbol?", IsSymbol);
        ADD_BUILTIN_PROCEDURE("list?", IsList);
        ADD_BUILTIN_PROCEDURE("pair?", IsPair);

#undef ADD_BUILTIN_PROCEDURE
    }
}  // namespace schemepp
