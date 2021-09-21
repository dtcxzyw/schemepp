// SPDX-License-Identifier: MIT

#include "Interface/Scope.hpp"
#include "Interface/Value.hpp"
#include <fmt/format.h>

namespace schemepp {
    // TODO: rational
#define PREFIX "Builtin.BaseLibrary.Arithmetic."

    // TODO: range check
    static Real toRealUnsafe(const Ref<Value>& value) {
        if(value->type() == ValueType::real)
            return dynamic_cast<const RealValue*>(value.get())->value();
        return static_cast<Real>(dynamic_cast<const IntegerValue*>(value.get())->value());
    }

    static Integer getIntegerUnsafe(const Ref<Value>& value) {
        return dynamic_cast<const IntegerValue*>(value.get())->value();
    }

    class Sum final : public Procedure {
    public:
        void printValue(std::ostream& stream) const override {
            stream << PREFIX "Sum";
        }
        Result<Ref<Value>> apply(EvaluateContext&, const std::vector<Ref<Value>>& operands) override {
            if(operands.empty())
                return Result<Ref<Value>>{ Error{ "No operand" } };

            bool useReal = false;
            for(auto&& operand : operands) {
                switch(operand->type()) {  // NOLINT(clang-diagnostic-switch-enum)
                    case ValueType::integer:
                        break;
                    case ValueType::real:
                        useReal = true;
                        break;
                    default:
                        return Result<Ref<Value>>{ Error{ "Unsupported type" } };
                }
            }

            if(useReal) {
                Real res = 0.0;

                for(auto&& operand : operands)
                    res += toRealUnsafe(operand);

                return Result{ constantReal(res) };
            }

            {
                Integer res = 0;

                for(auto&& operand : operands) {
                    res += getIntegerUnsafe(operand);
                }
                return Result{ constantInteger(res) };
            }
        }
    };

    class Diff final : public Procedure {
    public:
        void printValue(std::ostream& stream) const override {
            stream << PREFIX "Diff";
        }
        Result<Ref<Value>> apply(EvaluateContext&, const std::vector<Ref<Value>>& operands) override {
            if(operands.empty())
                return Result<Ref<Value>>{ Error{ "No operand" } };

            bool useReal = false;
            for(auto&& operand : operands) {
                switch(operand->type()) {  // NOLINT(clang-diagnostic-switch-enum)
                    case ValueType::integer:
                        break;
                    case ValueType::real:
                        useReal = true;
                        break;
                    default:
                        return Result<Ref<Value>>{ Error{ "Unsupported type" } };
                }
            }

            if(useReal) {
                bool first = true;
                Real res = 0.0;

                for(auto&& operand : operands)
                    if(first) {
                        res += toRealUnsafe(operand);
                        first = false;
                    } else {
                        res -= toRealUnsafe(operand);
                    }

                return Result{ constantReal(res) };
            }

            {
                bool first = true;
                Integer res = 0;

                for(auto&& operand : operands)
                    if(first) {
                        res += getIntegerUnsafe(operand);
                        first = false;
                    } else {
                        res -= getIntegerUnsafe(operand);
                    }

                return Result{ constantInteger(res) };
            }
        }
    };

    class Mul final : public Procedure {
    public:
        void printValue(std::ostream& stream) const override {
            stream << PREFIX "Mul";
        }
        Result<Ref<Value>> apply(EvaluateContext&, const std::vector<Ref<Value>>& operands) override {
            if(operands.empty())
                return Result<Ref<Value>>{ Error{ "No operand" } };

            bool useReal = false;
            for(auto&& operand : operands) {
                switch(operand->type()) {  // NOLINT(clang-diagnostic-switch-enum)
                    case ValueType::integer:
                        break;
                    case ValueType::real:
                        useReal = true;
                        break;
                    default:
                        return Result<Ref<Value>>{ Error{ "Unsupported type" } };
                }
            }

            if(useReal) {
                Real res = 1.0;

                for(auto&& operand : operands)
                    res *= toRealUnsafe(operand);

                return Result{ constantReal(res) };
            }

            {
                Integer res = 1;

                for(auto&& operand : operands) {
                    res *= getIntegerUnsafe(operand);
                }
                return Result{ constantInteger(res) };
            }
        }
    };

    class Div final : public Procedure {
    public:
        void printValue(std::ostream& stream) const override {
            stream << PREFIX "Div";
        }
        Result<Ref<Value>> apply(EvaluateContext&, const std::vector<Ref<Value>>& operands) override {
            if(operands.size() != 2)
                return Result<Ref<Value>>{ Error{ fmt::format("Wrong operands count. Expect 2, but got {}.", operands.size()) } };

            bool useReal = false;
            for(auto&& operand : operands) {
                switch(operand->type()) {  // NOLINT(clang-diagnostic-switch-enum)
                    case ValueType::integer:
                        break;
                    case ValueType::real:
                        useReal = true;
                        break;
                    default:
                        return Result<Ref<Value>>{ Error{ "Unsupported type" } };
                }
            }

            // TODO: handle divided by zero

            if(useReal)
                return Result{ constantReal(toRealUnsafe(operands[0]) / toRealUnsafe(operands[1])) };

            const auto lhs = getIntegerUnsafe(operands[0]);
            const auto rhs = getIntegerUnsafe(operands[1]);

            if(lhs % rhs == 0)
                return Result{ constantInteger(lhs / rhs) };
            return Result{ constantReal(static_cast<Real>(lhs) / static_cast<Real>(rhs)) };
        }
    };

#define BUILTIN_COMPARER(NAME, OP)                   \
    template <typename T>                            \
    struct NAME final {                              \
        static constexpr auto name() noexcept {      \
            return PREFIX #NAME;                     \
        }                                            \
        static bool compare(T lhs, T rhs) noexcept { \
            return lhs OP rhs;                       \
        }                                            \
    }

    BUILTIN_COMPARER(LessThan, <);
    BUILTIN_COMPARER(GreaterThan, >);
    BUILTIN_COMPARER(LessEq, <=);
    BUILTIN_COMPARER(GreaterEq, >=);
    BUILTIN_COMPARER(Equal, ==);  // NOLINT(clang-diagnostic-float-equal)

#undef BUILTIN_COMPARER

    template <template <typename T> class Comparer>
    class Compare final : public Procedure {
    public:
        void printValue(std::ostream& stream) const override {
            stream << Comparer<Integer>::name();
        }
        Result<Ref<Value>> apply(EvaluateContext&, const std::vector<Ref<Value>>& operands) override {
            if(operands.empty())
                return Result<Ref<Value>>{ Error{ "No operand" } };

            bool useReal = false;
            for(auto&& operand : operands) {
                switch(operand->type()) {  // NOLINT(clang-diagnostic-switch-enum)
                    case ValueType::integer:
                        break;
                    case ValueType::real:
                        useReal = true;
                        break;
                    default:
                        return Result<Ref<Value>>{ Error{ "Unsupported type" } };
                }
            }

            bool res = true;
            if(useReal) {
                for(size_t i = 1; i < operands.size(); ++i)
                    if(!Comparer<Real>::compare(toRealUnsafe(operands[i - 1]), toRealUnsafe(operands[i]))) {
                        res = false;
                        break;
                    }
            } else {
                for(size_t i = 1; i < operands.size(); ++i)
                    if(!Comparer<Integer>::compare(getIntegerUnsafe(operands[i - 1]), getIntegerUnsafe(operands[i]))) {
                        res = false;
                        break;
                    }
            }

            return Result{ constantBoolean(res) };
        }
    };

    class Not final : public Procedure {
    public:
        void printValue(std::ostream& stream) const override {
            stream << PREFIX "Not";
        }
        Result<Ref<Value>> apply(EvaluateContext&, const std::vector<Ref<Value>>& operands) override {
            if(operands.size() != 1)
                return Result<Ref<Value>>{ Error{ fmt::format("Wrong operands count. Expect 1, but got {}.", operands.size()) } };

            return Result{ constantBoolean(!toBoolean(operands[0])) };
        }
    };

    class GeneticEqual final : public Procedure {
    public:
        void printValue(std::ostream& stream) const override {
            stream << PREFIX "Equal";
        }
        Result<Ref<Value>> apply(EvaluateContext&, const std::vector<Ref<Value>>& operands) override {
            if(operands.empty())
                return Result<Ref<Value>>{ Error{ "No operand" } };

            const auto type = operands.front()->type();
            for(size_t i = 1; i < operands.size(); ++i) {
                if(!operands[i]->equal(operands[0]))
                    return Result{ constantBoolean(false) };
            }

            return Result{ constantBoolean(true) };
        }
    };

    class Exponent final : public Procedure {
    public:
        void printValue(std::ostream& stream) const override {
            stream << PREFIX "Exponent";
        }
        Result<Ref<Value>> apply(EvaluateContext&, const std::vector<Ref<Value>>& operands) override {
            if(operands.size() != 2)
                return Result<Ref<Value>>{ Error{ fmt::format("Wrong operands count. Expect 2, but got {}.", operands.size()) } };

            bool useReal = false;
            for(auto&& operand : operands) {
                switch(operand->type()) {  // NOLINT(clang-diagnostic-switch-enum)
                    case ValueType::integer:
                        break;
                    case ValueType::real:
                        useReal = true;
                        break;
                    default:
                        return Result<Ref<Value>>{ Error{ "Unsupported type" } };
                }
            }

            // TODO: handle domain

            if(useReal)
                return Result{ constantReal(std::pow(toRealUnsafe(operands[0]), toRealUnsafe(operands[1]))) };

            Integer x = getIntegerUnsafe(operands[0]), y = getIntegerUnsafe(operands[1]), ans = 1;

            while(y) {
                if(y & 1)
                    ans *= x;
                x *= x;
                y >>= 1;
            }

            return Result{ constantInteger(ans) };
        }
    };

    void initializeBuiltinArithmeticProcedure(Scope& scope) {
#define ADD_BUILTIN_PROCEDURE(NAME, CLASS) scope.insert(#NAME, makeRefCount<CLASS>())  // NOLINT(cppcoreguidelines-macro-usage)
        ADD_BUILTIN_PROCEDURE(+, Sum);
        ADD_BUILTIN_PROCEDURE(-, Diff);
        ADD_BUILTIN_PROCEDURE(*, Mul);
        ADD_BUILTIN_PROCEDURE(/, Div);

        ADD_BUILTIN_PROCEDURE(<, Compare<LessThan>);
        ADD_BUILTIN_PROCEDURE(>, Compare<GreaterThan>);
        ADD_BUILTIN_PROCEDURE(<=, Compare<LessEq>);
        ADD_BUILTIN_PROCEDURE(>=, Compare<GreaterEq>);
        ADD_BUILTIN_PROCEDURE(=, Compare<Equal>);

        ADD_BUILTIN_PROCEDURE(eqv?, GeneticEqual);

        ADD_BUILTIN_PROCEDURE(not, Not);

        ADD_BUILTIN_PROCEDURE(expt, Exponent);

#undef ADD_BUILTIN_PROCEDURE
    }

}  // namespace schemepp
