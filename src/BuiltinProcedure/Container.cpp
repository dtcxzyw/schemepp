// SPDX-License-Identifier: MIT

#include "Interface/Scope.hpp"
#include "Interface/Value.hpp"

namespace schemepp {
#define PREFIX "<builtin procedure> Builtin.BaseLibrary.Container."

    class VectorConstructor final : public Procedure {
    public:
        void printValue(std::ostream& stream) const override {
            stream << PREFIX "VectorConstructor";
        }
        Ref<Value> apply(EvaluateContext&, const std::vector<Ref<Value>>& operands) override {
            return makeVector(operands);
        }
    };

    class VectorBuilder final : public Procedure {
    public:
        void printValue(std::ostream& stream) const override {
            stream << PREFIX "VectorBuilder";
        }
        Ref<Value> apply(EvaluateContext& ctx, const std::vector<Ref<Value>>& operands) override {
            if(operands.size() != 1 && operands.size() != 2)
                throwWrongOperandCountError(ctx, (1 << 1) | (1 << 2), operands.size());

            const auto count = asInteger(operands[0]);
            if(count <= 0)
                throwDomainError();

            if(operands.size() == 2)
                return makeVector(std::vector(count, operands[1]));
            return makeVector(std::vector(count, constantBoolean(false)));
        }
    };

    class VectorRef final : public Procedure {
    public:
        void printValue(std::ostream& stream) const override {
            stream << PREFIX "VectorRef";
        }
        Ref<Value> apply(EvaluateContext& ctx, const std::vector<Ref<Value>>& operands) override {
            if(operands.size() != 2)
                throwWrongOperandCountError(ctx, 1 << 2, operands.size());

            auto& vec = asVector(operands[0]);
            const auto offset = asInteger(operands[1]);

            if(offset < 0)
                throwDomainError();
            if(offset >= static_cast<Integer>(operands.size()))
                throwOutOfBoundError(ctx, operands.size(), offset);

            return vec[offset];
        }
    };

    class VectorModifier final : public Procedure {
    public:
        void printValue(std::ostream& stream) const override {
            stream << PREFIX "VectorModifier";
        }
        Ref<Value> apply(EvaluateContext& ctx, const std::vector<Ref<Value>>& operands) override {
            if(operands.size() != 3)
                throwWrongOperandCountError(ctx, 1 << 3, operands.size());

            auto& vec = asVector(operands[0]);
            const auto offset = asInteger(operands[1]);

            if(offset < 0)
                throwDomainError();
            if(offset >= static_cast<Integer>(operands.size()))
                throwOutOfBoundError(ctx, operands.size(), offset);

            const_cast<std::vector<Ref<Value>>&>(vec)[offset] = operands[2];

            return operands[0];
        }
    };

    class PairConcat final : public Procedure {
    public:
        void printValue(std::ostream& stream) const override {
            stream << PREFIX "PairConcat";
        }
        Ref<Value> apply(EvaluateContext& ctx, const std::vector<Ref<Value>>& operands) override {
            if(operands.size() != 2)
                throwWrongOperandCountError(ctx, 1 << 2, operands.size());

            return makePair(operands[0], operands[1]);
        }
    };

    template <bool Position>
    class PairAccess final : public Procedure {
    public:
        void printValue(std::ostream& stream) const override {
            stream << PREFIX "PairAccess" << (Position ? "Car" : "Cdr");
        }
        Ref<Value> apply(EvaluateContext& ctx, const std::vector<Ref<Value>>& operands) override {
            if(operands.size() != 1)
                throwWrongOperandCountError(ctx, 1 << 1, operands.size());

            const auto& [car, cdr] = asPair(operands[0]);
            return Position ? car : cdr;
        }
    };

    template <bool Position>
    class PairModifier final : public Procedure {
    public:
        void printValue(std::ostream& stream) const override {
            stream << PREFIX "PairModifier" << (Position ? "Car" : "Cdr");
        }
        Ref<Value> apply(EvaluateContext& ctx, const std::vector<Ref<Value>>& operands) override {
            if(operands.size() != 2)
                throwWrongOperandCountError(ctx, 1 << 2, operands.size());

            const auto& [car, cdr] = asPair(operands[0]);
            const_cast<Ref<Value>&>(Position ? car : cdr) = operands[1];
            return Unspecified::value();
        }
    };

    class ListConstructor final : public Procedure {
    public:
        void printValue(std::ostream& stream) const override {
            stream << PREFIX "ListConstructor";
        }
        Ref<Value> apply(EvaluateContext&, const std::vector<Ref<Value>>& operands) override {
            return makeList({ operands.cbegin(), operands.cend() });
        }
    };

    class ListRef final : public Procedure {
    public:
        void printValue(std::ostream& stream) const override {
            stream << PREFIX "ListRef";
        }
        Ref<Value> apply(EvaluateContext& ctx, const std::vector<Ref<Value>>& operands) override {
            if(operands.size() != 2)
                throwWrongOperandCountError(ctx, 1 << 2, operands.size());

            auto& list = asList(operands[0]);
            const auto offset = asInteger(operands[1]);

            if(offset < 0)
                throwDomainError();
            if(offset >= static_cast<Integer>(operands.size()))
                throwOutOfBoundError(ctx, operands.size(), offset);

            auto iter = list.cbegin();
            std::advance(iter, offset);
            return *iter;
        }
    };

    class ListTailRef final : public Procedure {
    public:
        void printValue(std::ostream& stream) const override {
            stream << PREFIX "ListTailRef";
        }
        Ref<Value> apply(EvaluateContext& ctx, const std::vector<Ref<Value>>& operands) override {
            if(operands.size() != 2)
                throwWrongOperandCountError(ctx, 1 << 2, operands.size());

            auto& list = asList(operands[0]);
            const auto offset = asInteger(operands[1]);

            if(offset < 0)
                throwDomainError();
            if(offset > static_cast<Integer>(operands.size()))
                throwOutOfBoundError(ctx, operands.size(), offset);

            auto iter = list.cbegin();
            std::advance(iter, offset);
            return makeList({ iter, list.cend() });
        }
    };

    void initializeBuiltinContainerProcedure(Scope& scope) {
#define ADD_BUILTIN_PROCEDURE(NAME, CLASS) scope.insert(NAME, makeRefCount<CLASS>())  // NOLINT(cppcoreguidelines-macro-usage)

        ADD_BUILTIN_PROCEDURE("vector", VectorConstructor);
        ADD_BUILTIN_PROCEDURE("make-vector", VectorBuilder);
        ADD_BUILTIN_PROCEDURE("vector-ref", VectorRef);
        ADD_BUILTIN_PROCEDURE("vector-set!", VectorModifier);
        ADD_BUILTIN_PROCEDURE("cons", PairConcat);
        ADD_BUILTIN_PROCEDURE("car", PairAccess<true>);
        ADD_BUILTIN_PROCEDURE("cdr", PairAccess<false>);
        ADD_BUILTIN_PROCEDURE("set-car!", PairModifier<true>);
        ADD_BUILTIN_PROCEDURE("set-cdr!", PairModifier<false>);
        ADD_BUILTIN_PROCEDURE("list", ListConstructor);
        ADD_BUILTIN_PROCEDURE("list-ref", ListRef);
        ADD_BUILTIN_PROCEDURE("list-tail", ListTailRef);

#undef ADD_BUILTIN_PROCEDURE
    }

}  // namespace schemepp
