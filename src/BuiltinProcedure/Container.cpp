// SPDX-License-Identifier: MIT

#include "Interface/Error.hpp"
#include "Interface/Scope.hpp"
#include "Interface/Value.hpp"

namespace schemepp {
#define PREFIX "<builtin procedure> Builtin.BaseLibrary.Container."

    class VectorConstructor final : public Procedure {
    public:
        void printValue(std::ostream& stream) const override {
            stream << PREFIX "VectorConstructor";
        }
        Ref<Value> apply(EvaluateContext&, const std::vector<Ref<Value>>& operands) const override {
            return makeVector(operands);
        }
    };

    class VectorBuilder final : public Procedure {
    public:
        void printValue(std::ostream& stream) const override {
            stream << PREFIX "VectorBuilder";
        }
        Ref<Value> apply(EvaluateContext& ctx, const std::vector<Ref<Value>>& operands) const override {
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
        Ref<Value> apply(EvaluateContext& ctx, const std::vector<Ref<Value>>& operands) const override {
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
        Ref<Value> apply(EvaluateContext& ctx, const std::vector<Ref<Value>>& operands) const override {
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
        Ref<Value> apply(EvaluateContext& ctx, const std::vector<Ref<Value>>& operands) const override {
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
        Ref<Value> apply(EvaluateContext& ctx, const std::vector<Ref<Value>>& operands) const override {
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
        Ref<Value> apply(EvaluateContext& ctx, const std::vector<Ref<Value>>& operands) const override {
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
        Ref<Value> apply(EvaluateContext&, const std::vector<Ref<Value>>& operands) const override {
            return makeList({ operands.cbegin(), operands.cend() });
        }
    };

    class ListRef final : public Procedure {
    public:
        void printValue(std::ostream& stream) const override {
            stream << PREFIX "ListRef";
        }
        Ref<Value> apply(EvaluateContext& ctx, const std::vector<Ref<Value>>& operands) const override {
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
        Ref<Value> apply(EvaluateContext& ctx, const std::vector<Ref<Value>>& operands) const override {
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

    class ApplyList final : public Procedure {
    public:
        void printValue(std::ostream& stream) const override {
            stream << PREFIX "ApplyList";
        }
        Ref<Value> apply(EvaluateContext& ctx, const std::vector<Ref<Value>>& operands) const override {
            if(operands.size() < 2)
                throw Error{ "At lease two arguments are required." };
            if(operands[0]->type() != ValueType::procedure)
                throwMismatchedOperandTypeError(ctx, 0, ValueType::procedure, operands[0]->type());
            std::vector<Ref<Value>> ops{ operands.cbegin() + 1, operands.cend() - 1 };
            auto& list = asList(operands.back());
            ops.insert(ops.cend(), list.cbegin(), list.cend());
            return dynamic_cast<const Procedure*>(operands[0].get())->apply(ctx, ops);
        }
    };

    template <bool DiscardReturnValue>
    class MapList final : public Procedure {
    public:
        void printValue(std::ostream& stream) const override {
            stream << (DiscardReturnValue ? (PREFIX "ForEach") : (PREFIX "Map"));
        }
        Ref<Value> apply(EvaluateContext& ctx, const std::vector<Ref<Value>>& operands) const override {
            if(operands.size() < 2)
                throw Error{ "At lease two arguments are required." };
            if(operands[0]->type() != ValueType::procedure)
                throwMismatchedOperandTypeError(ctx, 0, ValueType::procedure, operands[0]->type());
            const auto procedure = dynamic_cast<const Procedure*>(operands[0].get());
            std::vector<std::reference_wrapper<const std::list<Ref<Value>>>> lists;
            lists.reserve(operands.size() - 1);
            for(size_t i = 1; i < operands.size(); ++i)
                lists.push_back(std::ref(asList(operands[i])));

            const size_t ops = lists[0].get().size();
            std::vector<std::list<Ref<Value>>::const_iterator> iterators;
            iterators.reserve(lists.size());
            for(auto& list : lists) {
                iterators.push_back(list.get().cbegin());
                if(ops != list.get().size()) {
                    throw Error{ "Mismatched operands count" };
                }
            }

            std::list<Ref<Value>> res;

            for(size_t i = 0; i < ops; ++i) {
                std::vector<Ref<Value>> subOperands;
                subOperands.reserve(lists.size());
                for(auto& iterator : iterators) {
                    subOperands.push_back(*iterator);
                    ++iterator;
                }
                auto value = procedure->apply(ctx, subOperands);
                if(!DiscardReturnValue)
                    res.push_back(std::move(value));
            }

            return res.empty() ? constantBoolean(true) : makeList(std::move(res));
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
        ADD_BUILTIN_PROCEDURE("apply", ApplyList);
        ADD_BUILTIN_PROCEDURE("map", MapList<false>);
        ADD_BUILTIN_PROCEDURE("for-each", MapList<true>);

#undef ADD_BUILTIN_PROCEDURE
    }

}  // namespace schemepp
