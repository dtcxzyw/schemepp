// SPDX-License-Identifier: MIT

#include "Interface/Error.hpp"
#include "Interface/Scope.hpp"
#include "Interface/Value.hpp"
#include <fmt/format.h>

namespace schemepp {
    // TODO: rational
#define PREFIX "Builtin.BaseLibrary.Arithmetic."

    // TODO: range check
    static Number liftUpImpl(const Integer val, const size_t idx) {
        if(idx == 0)
            return val;
        if(idx == 1)
            return static_cast<Real>(val);
        if(idx == 2)
            return static_cast<Complex>(static_cast<Real>(val));
        throwInternalError();
    }
    static Number liftUpImpl(const Real val, const size_t idx) {
        if(idx == 1)
            return val;
        if(idx == 2)
            return static_cast<Complex>(val);
        throwInternalError();
    }
    static Number liftUpImpl(const Complex val, const size_t idx) {
        if(idx == 2)
            return val;
        throwInternalError();
    }
    static Number liftUp(const Number& x, const size_t idx) {
        return std::visit([idx](auto val) { return liftUpImpl(val, idx); }, x);
    }
    template <template <typename T> class Operator, typename Ret = Number>
    auto applyBinaryOp(Number a, Number b) -> Ret {
        const auto dst = std::max(a.index(), b.index());
        a = liftUp(a, dst);
        b = liftUp(b, dst);
        if(dst == 0)
            return Operator<Integer>()(std::get<0>(a), std::get<0>(b));
        if(dst == 1)
            return Operator<Real>()(std::get<1>(a), std::get<1>(b));
        if(dst == 2)
            return Operator<Complex>()(std::get<2>(a), std::get<2>(b));
        throwInternalError();
    }

    class Sum final : public Procedure {
    public:
        void printValue(std::ostream& stream) const override {
            stream << PREFIX "Sum";
        }
        Ref<Value> apply(EvaluateContext& ctx, const std::vector<Ref<Value>>& operands) override {
            if(operands.empty())
                throwNoOperandError(ctx);

            Number res = asNumber(operands[0]);
            for(size_t i = 1; i < operands.size(); ++i)
                res = applyBinaryOp<std::plus>(res, asNumber(operands[i]));

            return constantNumber(res);
        }
    };

    class Diff final : public Procedure {
    public:
        void printValue(std::ostream& stream) const override {
            stream << PREFIX "Diff";
        }
        Ref<Value> apply(EvaluateContext& ctx, const std::vector<Ref<Value>>& operands) override {
            if(operands.empty())
                throwNoOperandError(ctx);

            Number res = asNumber(operands[0]);

            for(size_t i = 1; i < operands.size(); ++i)
                res = applyBinaryOp<std::minus>(res, asNumber(operands[i]));

            return constantNumber(res);
        }
    };

    class Mul final : public Procedure {
    public:
        void printValue(std::ostream& stream) const override {
            stream << PREFIX "Mul";
        }
        Ref<Value> apply(EvaluateContext& ctx, const std::vector<Ref<Value>>& operands) override {
            if(operands.empty())
                throwNoOperandError(ctx);

            Number res = asNumber(operands[0]);
            for(size_t i = 1; i < operands.size(); ++i)
                res = applyBinaryOp<std::multiplies>(res, asNumber(operands[i]));

            return constantNumber(res);
        }
    };

    template <typename T>
    struct Divide final {
        T operator()(T lhs, T rhs) {
            return lhs / rhs;
        }
    };

    template <>
    struct Divide<Integer> final {
        Number operator()(const Integer lhs, const Integer rhs) const {
            if(rhs == 0)
                throwDividedByZeroError();
            if(lhs % rhs == 0)
                return lhs / rhs;
            return static_cast<Real>(lhs) / static_cast<Real>(rhs);
        }
    };

    class Div final : public Procedure {
    public:
        void printValue(std::ostream& stream) const override {
            stream << PREFIX "Div";
        }
        Ref<Value> apply(EvaluateContext& ctx, const std::vector<Ref<Value>>& operands) override {
            if(operands.size() != 2)
                throwWrongOperandCountError(ctx, 1 << 2, operands.size());

            return constantNumber(applyBinaryOp<Divide>(asNumber(operands[0]), asNumber(operands[1])));
        }
    };

#define BUILTIN_COMPARER(NAME, OP)                                 \
    template <typename T>                                          \
    struct NAME final {                                            \
        static constexpr auto name() noexcept {                    \
            return PREFIX #NAME;                                   \
        }                                                          \
        bool operator()(T lhs, T rhs) const noexcept {             \
            return lhs OP rhs;                                     \
        }                                                          \
    };                                                             \
                                                                   \
    template <>                                                    \
    struct NAME<Complex> final {                                   \
        static constexpr auto name() noexcept {                    \
            return PREFIX #NAME;                                   \
        }                                                          \
        bool operator()(Complex lhs, Complex rhs) const noexcept { \
            throwInternalError();                                  \
            return false;                                          \
        }                                                          \
    }

    BUILTIN_COMPARER(LessThan, <);
    BUILTIN_COMPARER(GreaterThan, >);
    BUILTIN_COMPARER(LessEq, <=);
    BUILTIN_COMPARER(GreaterEq, >=);

    template <typename T>
    struct Equal final {
        static constexpr auto name() noexcept {
            return PREFIX "Equal";
        }
        bool operator()(T lhs, T rhs) const noexcept {
            return lhs == rhs;
        }
    };

#undef BUILTIN_COMPARER

    template <template <typename T> class Comparer>
    class Compare final : public Procedure {
    public:
        void printValue(std::ostream& stream) const override {
            stream << Comparer<Integer>::name();
        }
        Ref<Value> apply(EvaluateContext& ctx, const std::vector<Ref<Value>>& operands) override {
            if(operands.empty())
                throwNoOperandError(ctx);

            Number last = asNumber(operands[0]);
            for(size_t i = 1; i < operands.size(); ++i) {
                Number cur = asNumber(operands[i]);

                if(!applyBinaryOp<Comparer, bool>(last, cur))
                    return constantBoolean(false);

                last = cur;
            }

            return constantBoolean(true);
        }
    };

    class Not final : public Procedure {
    public:
        void printValue(std::ostream& stream) const override {
            stream << PREFIX "Not";
        }
        Ref<Value> apply(EvaluateContext& ctx, const std::vector<Ref<Value>>& operands) override {
            if(operands.size() != 1)
                throwWrongOperandCountError(ctx, 1 << 1, operands.size());

            return constantBoolean(!asBoolean(operands[0]));
        }
    };

    class GeneticEqual final : public Procedure {
    public:
        void printValue(std::ostream& stream) const override {
            stream << PREFIX "Equal";
        }
        Ref<Value> apply(EvaluateContext& ctx, const std::vector<Ref<Value>>& operands) override {
            if(operands.empty())
                throwNoOperandError(ctx);

            for(size_t i = 1; i < operands.size(); ++i) {
                if(!operands[i]->equal(operands[0]))
                    return constantBoolean(false);
            }

            return constantBoolean(true);
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

#undef ADD_BUILTIN_PROCEDURE
    }

}  // namespace schemepp
