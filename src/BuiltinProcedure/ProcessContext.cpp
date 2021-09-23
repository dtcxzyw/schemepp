// SPDX-License-Identifier: MIT

#include "Interface/Scope.hpp"
#include "Interface/Value.hpp"

namespace schemepp {
#define PREFIX "<builtin procedure> Builtin.ProcessContextLibrary."

    class CommandLine final : public Procedure {
    public:
        void printValue(std::ostream& stream) const override {
            stream << PREFIX "CommandLine";
        }
        Ref<Value> apply(EvaluateContext& ctx, const std::vector<Ref<Value>>& operands) const override {
            if(!operands.empty())
                throwWrongOperandCountError(ctx, 1 << 0, operands.size());
            std::string res;
            for(int i = 0; i < __argc; ++i) {
                if(i != 0)
                    res += ' ';
                res += __argv[i];
            }
            return constantString(std::move(res));
        }
    };

    class Exit final : public Procedure {
    public:
        void printValue(std::ostream& stream) const override {
            stream << PREFIX "Exit";
        }
        [[noreturn]] Ref<Value> apply(EvaluateContext& ctx, const std::vector<Ref<Value>>& operands) const override {
            if(operands.size() > 1)
                throwWrongOperandCountError(ctx, (1 << 1) | (1 << 0), operands.size());
            std::exit(operands.empty() ? 0 : static_cast<int>(asInteger(operands[0])));  // NOLINT(concurrency-mt-unsafe)
        }
    };

    class EmergencyExit final : public Procedure {
    public:
        void printValue(std::ostream& stream) const override {
            stream << PREFIX "EmergencyExit";
        }
        [[noreturn]] Ref<Value> apply(EvaluateContext& ctx, const std::vector<Ref<Value>>& operands) const override {
            if(operands.size() > 1)
                throwWrongOperandCountError(ctx, (1 << 1) | (1 << 0), operands.size());
            std::quick_exit(operands.empty() ? 0 : static_cast<int>(asInteger(operands[0])));  // NOLINT(concurrency-mt-unsafe)
        }
    };

    class GetEnvVar final : public Procedure {
    public:
        void printValue(std::ostream& stream) const override {
            stream << PREFIX "GetEnvVar";
        }
        Ref<Value> apply(EvaluateContext& ctx, const std::vector<Ref<Value>>& operands) const override {
            if(operands.size() != 1)
                throwWrongOperandCountError(ctx, 1 << 1, operands.size());
            char buf[4096];
            size_t count;
            getenv_s(&count, buf, asString(operands[0]).c_str());
            return constantString({ buf, count });
        }
    };

    class ListEnvVar final : public Procedure {
    public:
        void printValue(std::ostream& stream) const override {
            stream << PREFIX "ListEnvVar";
        }
        Ref<Value> apply(EvaluateContext& ctx, const std::vector<Ref<Value>>& operands) const override {
            if(!operands.empty())
                throwWrongOperandCountError(ctx, 1 << 0, operands.size());
            auto env = _environ;
            std::list<Ref<Value>> res;
            while(*env) {
                std::string_view kv{ *env };
                const auto split = kv.find_first_of('=');
                res.push_back(makeList(
                    { constantString(std::string{ kv.substr(0, split) }), constantString(std::string{ kv.substr(split + 1) }) }));
                ++env;
            }
            return makeList(std::move(res));
        }
    };

    void initializeBuiltinProcessContextProcedure(Scope& scope) {
#define ADD_BUILTIN_PROCEDURE(NAME, CLASS) scope.insert(NAME, makeRefCount<CLASS>())  // NOLINT(cppcoreguidelines-macro-usage)

        ADD_BUILTIN_PROCEDURE("command-line", CommandLine);
        ADD_BUILTIN_PROCEDURE("exit", Exit);
        ADD_BUILTIN_PROCEDURE("emergency-exit", EmergencyExit);
        ADD_BUILTIN_PROCEDURE("get-environment-variable", GetEnvVar);
        ADD_BUILTIN_PROCEDURE("get-environment-variables", ListEnvVar);

#undef ADD_BUILTIN_PROCEDURE
    }
}  // namespace schemepp
