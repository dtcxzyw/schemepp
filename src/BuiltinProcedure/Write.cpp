// SPDX-License-Identifier: MIT

#include "Interface/AST.hpp"
#include "Interface/Scope.hpp"
#include "Interface/Value.hpp"
#include <fmt/format.h>

namespace schemepp {
#define PREFIX "Builtin.WriteLibrary."

    class WriteBase : public Procedure {
    public:
        virtual void apply(const Ref<Value>& val, std::ostream& stream) = 0;

    private:
        Result<Ref<Value>> apply(const Ref<Value>& val, const Port* port) {
            if(const auto outputStream = port->output()) {
                auto&& stream = outputStream.value().get();
                apply(val, stream);
                return Result{ constantBoolean(static_cast<bool>(stream)) };
            }
            return Result<Ref<Value>>{ Error{ "Expect output port." } };
        }

    public:
        Result<Ref<Value>> apply(EvaluateContext& ctx, const std::vector<Ref<Value>>& operands) override {
            if(operands.size() == 2) {
                if(operands[1]->type() != ValueType::port)
                    return Result<Ref<Value>>{ Error{ "Expect port operands for second argument." } };
                return apply(operands[0], dynamic_cast<const Port*>(operands[1].get()));
            }
            if(operands.size() == 1) {
                return apply(operands[0], ctx.currentOutputPort.get());
            }
            return Result<Ref<Value>>{ Error{
                fmt::format("Wrong operands count. Expect 1 or 2, but got {}.", operands.size()) } };
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

        static Result<Ref<Value>> apply(const Port* port) {
            if(const auto outputStream = port->output()) {
                auto&& stream = outputStream.value().get();
                stream << '\n';
                return Result{ constantBoolean(static_cast<bool>(stream)) };
            }
            return Result<Ref<Value>>{ Error{ "Expect output port." } };
        }

    public:
        void printValue(std::ostream& stream) const override {
            stream << PREFIX "Newline";
        }
        Result<Ref<Value>> apply(EvaluateContext& ctx, const std::vector<Ref<Value>>& operands) override {
            if(operands.size() == 1) {
                if(operands[1]->type() != ValueType::port)
                    return Result<Ref<Value>>{ Error{ "Expect port operands for second argument." } };
                return apply(dynamic_cast<const Port*>(operands[1].get()));
            }
            if(operands.empty()) {
                return apply(ctx.currentOutputPort.get());
            }
            return Result<Ref<Value>>{ Error{
                fmt::format("Wrong operands count. Expect 0 or 1, but got {}.", operands.size()) } };
        }
    };

    void initializeBuiltinWriteProcedure(Scope& scope) {
#define ADD_BUILTIN_PROCEDURE(NAME, CLASS) scope.insert(NAME, makeRefCount<CLASS>())  // NOLINT(cppcoreguidelines-macro-usage)

        ADD_BUILTIN_PROCEDURE("display", Display);
        ADD_BUILTIN_PROCEDURE("newline", Newline);

#undef ADD_BUILTIN_PROCEDURE
    }
}  // namespace schemepp
