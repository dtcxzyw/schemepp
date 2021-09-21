// SPDX-License-Identifier: MIT

#include "Interface/Error.hpp"
#include "Interface/EvaluateContext.hpp"
#include "Interface/Scope.hpp"
#include "Interface/Value.hpp"
#include <fmt/format.h>

namespace schemepp {
#define PREFIX "Builtin.WriteLibrary."

    class WriteBase : public Procedure {
    public:
        virtual void apply(const Ref<Value>& val, std::ostream& stream) = 0;

    private:
        Ref<Value> apply(const Ref<Value>& val, const Port* port) {
            if(const auto outputStream = port->output()) {
                auto&& stream = outputStream.value().get();
                apply(val, stream);
                return constantBoolean(static_cast<bool>(stream));
            }
            throw Error{ "Expect output port." };
        }

    public:
        Ref<Value> apply(EvaluateContext& ctx, const std::vector<Ref<Value>>& operands) override {
            if(operands.size() == 2) {
                if(operands[1]->type() != ValueType::port)
                    throwMismatchedOperandTypeError(ctx, 1, ValueType::port, operands[1]->type());
                return apply(operands[0], dynamic_cast<const Port*>(operands[1].get()));
            }
            if(operands.size() == 1) {
                return apply(operands[0], ctx.currentOutputPort.get());
            }
            throwWrongOperandCountError(ctx, (1 << 1) | (1 << 2), operands.size());
        }
    };

    class Display final : public WriteBase {
    public:
        void printValue(std::ostream& stream) const override {
            stream << PREFIX "Display";
        }
        void apply(const Ref<Value>& val, std::ostream& stream) override {
            val->printValue(stream);
        }
    };

    class Newline final : public Procedure {
        static Ref<Value> apply(const Port* port) {
            if(const auto outputStream = port->output()) {
                auto&& stream = outputStream.value().get();
                stream << '\n';
                return constantBoolean(static_cast<bool>(stream));
            }
            throw Error{ "Expect output port." };
        }

    public:
        void printValue(std::ostream& stream) const override {
            stream << PREFIX "Newline";
        }
        Ref<Value> apply(EvaluateContext& ctx, const std::vector<Ref<Value>>& operands) override {
            if(operands.size() == 1) {
                if(operands[1]->type() != ValueType::port)
                    throwMismatchedOperandTypeError(ctx, 1, ValueType::port, operands[1]->type());
                return apply(dynamic_cast<const Port*>(operands[1].get()));
            }
            if(operands.empty()) {
                return apply(ctx.currentOutputPort.get());
            }
            throwWrongOperandCountError(ctx, (1 << 0) | (1 << 1), operands.size());
        }
    };

    void initializeBuiltinWriteProcedure(Scope& scope) {
#define ADD_BUILTIN_PROCEDURE(NAME, CLASS) scope.insert(NAME, makeRefCount<CLASS>())  // NOLINT(cppcoreguidelines-macro-usage)

        ADD_BUILTIN_PROCEDURE("display", Display);
        ADD_BUILTIN_PROCEDURE("newline", Newline);

#undef ADD_BUILTIN_PROCEDURE
    }
}  // namespace schemepp
