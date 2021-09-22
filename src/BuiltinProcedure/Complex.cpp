// SPDX-License-Identifier: MIT

#include "Interface/Scope.hpp"
#include "Interface/Value.hpp"

namespace schemepp {
#define PREFIX "<builtin procedure> Builtin.ComplexLibrary."

    class ComplexAccessBase : public Procedure {
        virtual Ref<Value> eval(Complex val) const = 0;

    public:
        Ref<Value> apply(EvaluateContext& ctx, const std::vector<Ref<Value>>& operands) override {
            if(operands.size() != 1)
                throwWrongOperandCountError(ctx, 1 << 1, operands.size());
            return eval(asComplex(operands[0]));
        }
    };
    class ComplexBuildBase : public Procedure {
        virtual Complex eval(Real arg0, Real arg1) const = 0;

    public:
        Ref<Value> apply(EvaluateContext& ctx, const std::vector<Ref<Value>>& operands) override {
            if(operands.size() != 2)
                throwWrongOperandCountError(ctx, 1 << 2, operands.size());
            return constantComplex(eval(asReal(operands[0]), asReal(operands[1])));
        }
    };

    class Angle final : public ComplexAccessBase {
        void printValue(std::ostream& stream) const override {
            stream << PREFIX "Angle";
        }
        Ref<Value> eval(const Complex val) const override {
            return constantReal(std::arg(val));
        }
    };
    class ImagPart final : public ComplexAccessBase {
        void printValue(std::ostream& stream) const override {
            stream << PREFIX "ImagPart";
        }
        Ref<Value> eval(const Complex val) const override {
            return constantReal(std::imag(val));
        }
    };
    class RealPart final : public ComplexAccessBase {
        void printValue(std::ostream& stream) const override {
            stream << PREFIX "RealPart";
        }
        Ref<Value> eval(const Complex val) const override {
            return constantReal(std::real(val));
        }
    };
    class Magnitude final : public ComplexAccessBase {
        void printValue(std::ostream& stream) const override {
            stream << PREFIX "Magnitude";
        }
        Ref<Value> eval(const Complex val) const override {
            return constantReal(std::norm(val));
        }
    };
    class MakePolar final : public ComplexBuildBase {
        void printValue(std::ostream& stream) const override {
            stream << PREFIX "MakePolar";
        }
        Complex eval(const Real arg0, const Real arg1) const override {
            return std::polar(arg0, arg1);
        }
    };
    class MakeRectangular final : public ComplexBuildBase {
        void printValue(std::ostream& stream) const override {
            stream << PREFIX "MakeRectangular";
        }
        Complex eval(const Real arg0, const Real arg1) const override {
            return { arg0, arg1 };
        }
    };

    void initializeBuiltinComplexProcedure(Scope& scope) {
#define ADD_BUILTIN_PROCEDURE(NAME, CLASS) scope.insert(NAME, makeRefCount<CLASS>())  // NOLINT(cppcoreguidelines-macro-usage)

        ADD_BUILTIN_PROCEDURE("angle", Angle);
        ADD_BUILTIN_PROCEDURE("imag-part", ImagPart);
        ADD_BUILTIN_PROCEDURE("magnitude", Magnitude);
        ADD_BUILTIN_PROCEDURE("make-polar", MakePolar);
        ADD_BUILTIN_PROCEDURE("make-rectangular", MakeRectangular);
        ADD_BUILTIN_PROCEDURE("real-part", RealPart);

#undef ADD_BUILTIN_PROCEDURE
    }

}  // namespace schemepp
