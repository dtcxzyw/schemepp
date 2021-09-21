// SPDX-License-Identifier: MIT

#include "Interface/Scope.hpp"
#include "Interface/Value.hpp"
#include <fmt/format.h>

namespace schemepp {

#define PREFIX "Builtin.BaseLibrary.RTTI."

    class RTTIBase : public Procedure {
    public:
        virtual bool checkType(ValueType type) const noexcept = 0;
        Ref<Value> apply(EvaluateContext& ctx, const std::vector<Ref<Value>>& operands) final {
            if(operands.size() != 1)
                throwWrongOperandCountError(ctx, 1 << 1, operands.size());

            const auto type = operands[0]->type();
            return constantBoolean(checkType(type));
        }
    };

#define BUILTIN_RTTI_DEFINE(CLASS, TYPE)                               \
    class CLASS final : public RTTIBase {                              \
    public:                                                            \
        void printValue(std::ostream& stream) const override {         \
            stream << PREFIX #CLASS;                                   \
        }                                                              \
        bool checkType(const ValueType type) const noexcept override { \
            return type == ValueType::TYPE;                            \
        }                                                              \
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

#undef BUILTIN_RTTI_DEFINE

    class IsNumber final : public RTTIBase {
    public:
        void printValue(std::ostream& stream) const override {
            stream << PREFIX "IsNumber";
        }
        bool checkType(const ValueType type) const noexcept override {
            return type == ValueType::integer || type == ValueType::complex || type == ValueType::real;
        }
    };

    class IsRational final : public RTTIBase {
    public:
        void printValue(std::ostream& stream) const override {
            stream << PREFIX "IsRational";
        }
        bool checkType(ValueType type) const noexcept override {
            return type == ValueType::integer || type == ValueType::real;
        }
    };

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
