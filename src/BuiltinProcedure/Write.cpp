// SPDX-License-Identifier: MIT

#include "Interface/Error.hpp"
#include "Interface/EvaluateContext.hpp"
#include "Interface/Scope.hpp"
#include "Interface/Value.hpp"
#include <fmt/format.h>
#include <fstream>
#include <utf8cpp/utf8.h>

namespace schemepp {
#define PREFIX "<builtin procedure> Builtin.WriteLibrary."

    class WriteBase : public Procedure {
    public:
        virtual void apply(const Ref<Value>& val, std::ostream& stream) const = 0;

    private:
        Ref<Value> applyValue(const Ref<Value>& val, std::ostream& stream) const {
            apply(val, stream);
            return constantBoolean(static_cast<bool>(stream));
        }

    public:
        Ref<Value> apply(EvaluateContext& ctx, const std::vector<Ref<Value>>& operands) const override {
            if(operands.size() == 2) {
                return applyValue(operands[0], asOutputPort(operands[1]));
            }
            if(operands.size() == 1) {
                return applyValue(operands[0], ctx.currentOutputPort->output());
            }
            throwWrongOperandCountError(ctx, (1 << 1) | (1 << 2), operands.size());
        }
    };

    class Display final : public WriteBase {
    public:
        void printValue(std::ostream& stream) const override {
            stream << PREFIX "Display";
        }
        void apply(const Ref<Value>& val, std::ostream& stream) const override {
            val->printValue(stream);
        }
    };

    class Newline final : public Procedure {
        static Ref<Value> apply(std::ostream& stream) {
            stream << '\n';
            return constantBoolean(static_cast<bool>(stream));
        }

    public:
        void printValue(std::ostream& stream) const override {
            stream << PREFIX "Newline";
        }
        Ref<Value> apply(EvaluateContext& ctx, const std::vector<Ref<Value>>& operands) const override {
            if(operands.size() == 1) {
                return apply(asOutputPort(operands[0]));
            }
            if(operands.empty()) {
                return apply(ctx.currentOutputPort->output());
            }
            throwWrongOperandCountError(ctx, (1 << 0) | (1 << 1), operands.size());
        }
    };

    class CloseOutputPort final : public Procedure {
    public:
        void printValue(std::ostream& stream) const override {
            stream << PREFIX "CloseOutputPort";
        }
        Ref<Value> apply(EvaluateContext& ctx, const std::vector<Ref<Value>>& operands) const override {
            if(operands.size() == 1) {
                auto&& stream = asOutputPort(operands[0]);

                if(const auto file = dynamic_cast<std::ofstream*>(&stream)) {
                    file->close();
                    return constantBoolean(true);
                }
                throw Error{ "Expect closable output port." };
            }
            throwWrongOperandCountError(ctx, 1 << 1, operands.size());
        }
    };

    class CurrentOutputPort final : public Procedure {
    public:
        void printValue(std::ostream& stream) const override {
            stream << PREFIX "CurrentOutputPort";
        }
        Ref<Value> apply(EvaluateContext& ctx, const std::vector<Ref<Value>>& operands) const override {
            if(operands.empty()) {
                return ctx.currentOutputPort;
            }
            throwWrongOperandCountError(ctx, 1 << 0, operands.size());
        }
    };

    class WriteChar final : public WriteBase {
    public:
        void printValue(std::ostream& stream) const override {
            stream << PREFIX "WriteChar";
        }
        void apply(const Ref<Value>& val, std::ostream& stream) const override {
            std::string res;
            const std::back_insert_iterator iter{ res };
            utf8::append(asCharacter(val), iter);
            stream << res;
        }
    };

    class Write final : public WriteBase {
    public:
        void printValue(std::ostream& stream) const override {
            stream << PREFIX "Write";
        }
        void apply(const Ref<Value>& val, std::ostream& stream) const override {
            throwNotImplementedError();
        }
    };

    std::ostream& asOutputPort(const Ref<Value>& value) {
        if(value->type() == ValueType::outputPort) {
            return dynamic_cast<const OutputPort*>(value.get())->output();
        }
        throwMismatchedOperandTypeError(ValueType::outputPort, value->type());
    }

    void initializeBuiltinWriteProcedure(Scope& scope) {
#define ADD_BUILTIN_PROCEDURE(NAME, CLASS) scope.insert(NAME, makeRefCount<CLASS>())  // NOLINT(cppcoreguidelines-macro-usage)

        ADD_BUILTIN_PROCEDURE("display", Display);
        ADD_BUILTIN_PROCEDURE("newline", Newline);
        ADD_BUILTIN_PROCEDURE("current-output-port", CurrentOutputPort);
        ADD_BUILTIN_PROCEDURE("write-char", WriteChar);
        ADD_BUILTIN_PROCEDURE("write", Write);
        ADD_BUILTIN_PROCEDURE("close-output-port", CloseOutputPort);

#undef ADD_BUILTIN_PROCEDURE
    }
}  // namespace schemepp
