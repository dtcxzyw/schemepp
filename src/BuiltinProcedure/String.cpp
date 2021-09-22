// SPDX-License-Identifier: MIT

#include "Interface/Scope.hpp"
#include "Interface/Value.hpp"
#include <fmt/format.h>
#include <utf8cpp/utf8.h>

namespace schemepp {
#define PREFIX "<builtin procedure> Builtin.BaseLibrary.String."

    class StringConstructor final : public Procedure {
    public:
        void printValue(std::ostream& stream) const override {
            stream << PREFIX "StringConstructor";
        }
        Ref<Value> apply(EvaluateContext&, const std::vector<Ref<Value>>& operands) override {
            std::string str;
            const std::back_insert_iterator iter{ str };

            for(auto&& operand : operands) {
                utf8::append(asCharacter(operand), iter);
            }

            return constantString(std::move(str));
        }
    };

    class StringRef final : public Procedure {
    public:
        void printValue(std::ostream& stream) const override {
            stream << PREFIX "StringRef";
        }
        Ref<Value> apply(EvaluateContext& ctx, const std::vector<Ref<Value>>& operands) override {
            if(operands.size() != 2)
                throwWrongOperandCountError(ctx, 1 << 2, operands.size());

            auto& str = asString(operands[0]);
            auto offset = asInteger(operands[1]);

            if(offset < 0)
                throwDomainError();

            auto iter = str.begin();
            uint32_t cp = 0;
            do {
                cp = utf8::next(iter, str.end());
            } while(offset--);
            return constantCharacter(cp);
        }
    };

    class StringConcat final : public Procedure {
    public:
        void printValue(std::ostream& stream) const override {
            stream << PREFIX "StringConcat";
        }
        Ref<Value> apply(EvaluateContext&, const std::vector<Ref<Value>>& operands) override {
            std::string str;
            for(auto&& operand : operands)
                str += asString(operand);
            return constantString(std::move(str));
        }
    };

    class StringBuilder final : public Procedure {
    public:
        void printValue(std::ostream& stream) const override {
            stream << PREFIX "StringBuilder";
        }
        Ref<Value> apply(EvaluateContext& ctx, const std::vector<Ref<Value>>& operands) override {
            if(operands.size() != 1 && operands.size() != 2)
                throwWrongOperandCountError(ctx, (1 << 1) | (1 << 2), operands.size());

            auto count = asInteger(operands[0]);
            if(count <= 0)
                throwDomainError();

            const uint32_t val = operands.size() == 2 ? asCharacter(operands[1]) : '\0';

            std::string base;
            const std::back_insert_iterator iter{ base };
            utf8::append(val, iter);

            std::string str;
            str.reserve(base.size() * count);
            while(count--) {
                str += base;
            }

            return constantString(std::move(str));
        }
    };

    void initializeBuiltinStringProcedure(Scope& scope) {
#define ADD_BUILTIN_PROCEDURE(NAME, CLASS) scope.insert(#NAME, makeRefCount<CLASS>())  // NOLINT(cppcoreguidelines-macro-usage)
        // clang-format off
        ADD_BUILTIN_PROCEDURE(string, StringConstructor);
        ADD_BUILTIN_PROCEDURE(string-ref, StringRef);
        ADD_BUILTIN_PROCEDURE(string-append, StringConcat);
        ADD_BUILTIN_PROCEDURE(make-string,StringBuilder);
        // clang-format on
#undef ADD_BUILTIN_PROCEDURE
    }
}  // namespace schemepp
