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

    static uint32_t toChar(const Ref<Value>& val) {
        return dynamic_cast<const CharacterValue*>(val.get())->value();
    }

    template <typename Comparer>
    class CharCompare final : public Procedure {
    public:
        void printValue(std::ostream& stream) const override {
            stream << Comparer::name() << Comparer::config();
        }
        Result<Ref<Value>> apply(EvaluateContext&, const std::vector<Ref<Value>>& operands) override {
            if(operands.empty())
                return Result<Ref<Value>>{ Error{ "No operand" } };

            for(auto&& operand : operands) {
                if(operand->type() != ValueType::character)
                    return Result<Ref<Value>>{ Error{ "Unsupported type" } };
            }

            bool res = true;
            for(size_t i = 1; i < operands.size(); ++i)
                if(!Comparer::compare(toChar(operands[i - 1]), toChar(operands[i]))) {
                    res = false;
                    break;
                }

            return Result{ constantBoolean(res) };
        }
    };

    template <bool Direction>
    class CharCast final : public Procedure {
    public:
        void printValue(std::ostream& stream) const override {
            stream << PREFIX << (Direction ? "UpCase" : "DownCase");
        }
        Result<Ref<Value>> apply(EvaluateContext&, const std::vector<Ref<Value>>& operands) override {
            if(operands.size() != 1)
                return Result<Ref<Value>>{ Error{
                    Error{ fmt::format("Wrong operands count. Expect 1, but got {}.", operands.size()) } } };

            auto& operand = operands[0];
            if(operand->type() != ValueType::character)
                return Result<Ref<Value>>{ Error{ "Unsupported type" } };

            uint32_t val = toChar(operand);

            if(std::isalpha(val)) {
                val = (Direction ? std::toupper : std::tolower)(val);
            }

            return Result{ constantCharacter(val) };
        }
    };

    class StringConstructor final : public Procedure {
    public:
        void printValue(std::ostream& stream) const override {
            stream << PREFIX "StringConstructor";
        }
        Result<Ref<Value>> apply(EvaluateContext&, const std::vector<Ref<Value>>& operands) override {
            std::string str;
            const std::back_insert_iterator iter{ str };

            for(auto&& operand : operands) {
                if(operand->type() != ValueType::character)
                    return Result<Ref<Value>>{ Error{ "Unsupported type" } };

                utf8::append(dynamic_cast<const CharacterValue*>(operand.get())->value(), iter);
            }

            return Result{ constantString(std::move(str)) };
        }
    };

    class StringRef final : public Procedure {
    public:
        void printValue(std::ostream& stream) const override {
            stream << PREFIX "StringRef";
        }
        Result<Ref<Value>> apply(EvaluateContext&, const std::vector<Ref<Value>>& operands) override {
            if(operands.size() != 2)
                return Result<Ref<Value>>{ Error{
                    Error{ fmt::format("Wrong operands count. Expect 2, but got {}.", operands.size()) } } };

            if(operands[0]->type() != ValueType::string || operands[1]->type() != ValueType::integer) {
                return Result<Ref<Value>>{ Error{ "Unsupported type" } };
            }

            auto& str = dynamic_cast<const StringValue*>(operands[0].get())->value();
            auto offset = dynamic_cast<const IntegerValue*>(operands[1].get())->value();

            if(offset < 0)
                return Result<Ref<Value>>{ Error{ "Wrong offset" } };

            try {
                auto iter = str.begin();
                uint32_t cp = 0;
                do {
                    cp = utf8::next(iter, str.end());
                } while(offset--);
                return Result{ constantCharacter(cp) };
            } catch(...) {
                return Result<Ref<Value>>{ Error{ "Out of bound" } };
            }
        }
    };

    class StringConcat final : public Procedure {
    public:
        void printValue(std::ostream& stream) const override {
            stream << PREFIX "StringConcat";
        }
        Result<Ref<Value>> apply(EvaluateContext&, const std::vector<Ref<Value>>& operands) override {
            std::string str;

            for(auto&& operand : operands) {
                if(operand->type() != ValueType::string)
                    return Result<Ref<Value>>{ Error{ "Unsupported type" } };

                str += dynamic_cast<const StringValue*>(operand.get())->value();
            }

            return Result{ constantString(std::move(str)) };
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
        Result<Ref<Value>> apply(EvaluateContext&, const std::vector<Ref<Value>>& operands) override {
            if(operands.empty())
                return Result<Ref<Value>>{ Error{ "No operand" } };

            for(auto&& operand : operands) {
                if(operand->type() != ValueType::string)
                    return Result<Ref<Value>>{ Error{ "Unsupported type" } };
            }

            bool res = true;
            for(size_t i = 1; i < operands.size(); ++i)
                if(!Comparer::compare(toString(operands[i - 1]), toString(operands[i]))) {
                    res = false;
                    break;
                }

            return Result{ constantBoolean(res) };
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
        // clang-format on
#undef ADD_BUILTIN_PROCEDURE
    }
}  // namespace schemepp
