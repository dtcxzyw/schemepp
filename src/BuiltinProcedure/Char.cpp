// SPDX-License-Identifier: MIT

#include "Interface/Scope.hpp"
#include "Interface/Value.hpp"
#include <fmt/format.h>
#include <utf8cpp/utf8.h>

namespace schemepp {
#define PREFIX "<builtin procedure> Builtin.CharLibrary."

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

    void initializeBuiltinCharProcedure(Scope& scope) {
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
        // clang-format on
#undef ADD_BUILTIN_PROCEDURE
    }
}  // namespace schemepp
