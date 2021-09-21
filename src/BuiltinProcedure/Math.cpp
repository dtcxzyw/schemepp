// SPDX-License-Identifier: MIT

#include "Interface/Scope.hpp"
#include "Interface/Value.hpp"
#include <fmt/format.h>

namespace schemepp {
#define PREFIX "Builtin.BaseLibrary.Math."

    class Exponent final : public Procedure {
        template <typename T>
        static Number eval(T x, Integer y) {
            if(y < 0)
                return eval(x, static_cast<Real>(y));
            if(y == 0)
                return static_cast<T>(1);
            if(y == 1)
                return x;
            // TODO: handle domain
            T ans = static_cast<T>(1);
            while(y) {
                if(y & 1)
                    ans *= x;
                x *= x;
                y >>= 1;
            }
            return ans;
        }
        static Number eval(const Integer x, const Real y) {
            if(x < 0)
                throwDomainError();
            return std::pow(x, y);
        }
        static Number eval(const Real x, const Real y) {
            if(x < 0.0)
                throwDomainError();
            return std::pow(x, y);
        }
        static Number eval(const Complex x, const Real y) {
            return std::pow(x, y);
        }
        static Number eval(const Complex x, const Complex y) {
            return std::pow(x, y);
        }

    public:
        void printValue(std::ostream& stream) const override {
            stream << PREFIX "Exponent";
        }
        Ref<Value> apply(EvaluateContext& ctx, const std::vector<Ref<Value>>& operands) override {
            if(operands.size() != 2)
                throwWrongOperandCountError(ctx, 1 << 2, operands.size());

            Number a = asNumber(operands[0]);
            Number b = asNumber(operands[0]);

            return constantNumber(std::visit([](auto x, auto y) { return eval(x, y); }, a, b));
        }
    };

    void initializeBuiltinMathProcedure(Scope& scope) {
#define ADD_BUILTIN_PROCEDURE(NAME, CLASS) scope.insert(#NAME, makeRefCount<CLASS>())  // NOLINT(cppcoreguidelines-macro-usage)

        ADD_BUILTIN_PROCEDURE(expt, Exponent);

#undef ADD_BUILTIN_PROCEDURE
    }
}  // namespace schemepp
