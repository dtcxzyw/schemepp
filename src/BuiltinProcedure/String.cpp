// SPDX-License-Identifier: MIT

#include "Interface/Scope.hpp"
#include "Interface/Value.hpp"
#include <fmt/format.h>
#include <utf8cpp/utf8.h>

namespace schemepp {
#define PREFIX "Builtin.BaseLibrary.String."

#define BUILTIN_CHAR_COMPARER(NAME, OP)                                        \
    template <bool CaseInensitive>                                             \
    struct Char##NAME final {                                                  \
        static constexpr auto name() {                                         \
            return PREFIX #NAME;                                               \
        }                                                                      \
        static constexpr auto config() {                                       \
            return CaseInensitive ? "CaseInsensitive" : "";                    \
        }                                                                      \
        static bool compare(const uint32_t lhs, const uint32_t rhs) noexcept { \
            if constexpr(CaseInensitive) {                                     \
                if(std::isalpha(lhs) && std::isalpha(rhs)) {                   \
                    return std::tolower(lhs) OP std::tolower(rhs);             \
                }                                                              \
            }                                                                  \
            return lhs OP rhs;                                                 \
        }                                                                      \
    }

    BUILTIN_CHAR_COMPARER(LessThan, <);
    BUILTIN_CHAR_COMPARER(GreaterThan, >);
    BUILTIN_CHAR_COMPARER(LessEq, <=);
    BUILTIN_CHAR_COMPARER(GreaterEq, >=);
    BUILTIN_CHAR_COMPARER(Equal, ==);

#undef BUILTIN_CHAR_COMPARER

    template <typename Comparer>
    class CharCompare final : public Procedure {
    public:
        void printValue(std::ostream& stream) const override {
            stream << Comparer::name() << Comparer::config();
        }
        Ref<Value> apply(EvaluateContext& ctx, const std::vector<Ref<Value>>& operands) override {
            if(operands.empty())
                throwNoOperandError(ctx);

            uint32_t last = asCharacter(operands[0]);

            for(size_t i = 1; i < operands.size(); ++i) {
                uint32_t cur = asCharacter(operands[i]);
                if(!Comparer::compare(last, cur)) {
                    return constantBoolean(false);
                }
                last = cur;
            }

            return constantBoolean(true);
        }
    };

    template <bool Direction>
    class CharCast final : public Procedure {
    public:
        void printValue(std::ostream& stream) const override {
            stream << PREFIX << (Direction ? "UpCase" : "DownCase");
        }
        Ref<Value> apply(EvaluateContext& ctx, const std::vector<Ref<Value>>& operands) override {
            if(operands.size() != 1)
                throwWrongOperandCountError(ctx, 1 << 1, operands.size());

            uint32_t val = asCharacter(operands[0]);

            if(std::isalpha(val)) {
                val = (Direction ? std::toupper : std::tolower)(val);
            }

            return constantCharacter(val);
        }
    };

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

            uint32_t val = operands.size() == 2 ? asCharacter(operands[1]) : '\0';

            std::string base;
            std::back_insert_iterator iter{ base };
            utf8::append(val, iter);

            std::string str;
            str.reserve(base.size() * count);
            while(count--) {
                str += base;
            }

            return constantString(std::move(str));
        }
    };

    template <typename Comparer>
    class StringCompare final : public Procedure {
        static const std::string& toString(const Ref<Value>& val) {
            return dynamic_cast<const StringValue*>(val.get())->ref();
        }

    public:
        void printValue(std::ostream& stream) const override {
            stream << Comparer::name() << Comparer::config();
        }
        Ref<Value> apply(EvaluateContext& ctx, const std::vector<Ref<Value>>& operands) override {
            if(operands.empty())
                throwNoOperandError(ctx);

            auto last = std::ref(asString(operands[0]));

            for(size_t i = 1; i < operands.size(); ++i) {
                auto cur = std::ref(asString(operands[i]));
                if(!Comparer::compare(last.get(), cur.get())) {
                    return constantBoolean(false);
                }
                last = cur;
            }

            return constantBoolean(true);
        }
    };

    void initializeBuiltinStringProcedure(Scope& scope) {
#define ADD_BUILTIN_PROCEDURE(NAME, CLASS) scope.insert(#NAME, makeRefCount<CLASS>())  // NOLINT(cppcoreguidelines-macro-usage)
        // clang-format off
        ADD_BUILTIN_PROCEDURE(char<?, CharCompare<CharLessThan<false>>);
        ADD_BUILTIN_PROCEDURE(char>?, CharCompare<CharGreaterThan<false>>);
        ADD_BUILTIN_PROCEDURE(char>=?, CharCompare<CharGreaterEq<false>>);
        ADD_BUILTIN_PROCEDURE(char<=?, CharCompare<CharLessEq<false>>);
        ADD_BUILTIN_PROCEDURE(char=?, CharCompare<CharEqual<false>>);

        ADD_BUILTIN_PROCEDURE(char-ci<?, CharCompare<CharLessThan<true>>);
        ADD_BUILTIN_PROCEDURE(char-ci>?, CharCompare<CharGreaterThan<true>>);
        ADD_BUILTIN_PROCEDURE(char-ci>=?, CharCompare<CharGreaterEq<true>>);
        ADD_BUILTIN_PROCEDURE(char-ci<=?, CharCompare<CharLessEq<true>>);
        ADD_BUILTIN_PROCEDURE(char-ci=?, CharCompare<CharEqual<true>>);
        
        ADD_BUILTIN_PROCEDURE(char-downcase, CharCast<false>);
        ADD_BUILTIN_PROCEDURE(char-upcase, CharCast<true>);

        ADD_BUILTIN_PROCEDURE(string, StringConstructor);
        ADD_BUILTIN_PROCEDURE(string-ref, StringRef);
        ADD_BUILTIN_PROCEDURE(string-append, StringConcat);
        ADD_BUILTIN_PROCEDURE(make-string,StringBuilder);
        // clang-format on
#undef ADD_BUILTIN_PROCEDURE
    }
}  // namespace schemepp
