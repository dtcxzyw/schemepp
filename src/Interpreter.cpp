// SPDX-License-Identifier: MIT

#include "Interface/Interpreter.hpp"
#include "Interface/AST.hpp"
#include "Interface/BuiltinLibraries.hpp"
#include "Interface/EvaluateContext.hpp"
#include "Interface/Scope.hpp"
#include <csignal>
#include <rang.hpp>
#include <sstream>

namespace schemepp {

    [[noreturn]] static void fatal(const std::string_view message) {
        std::cerr << rang::fg::red << message << rang::fg::reset << std::endl;
        std::quick_exit(EXIT_FAILURE);
    }

    class StandardInput final : public Port {
    public:
        void printValue(std::ostream& stream) const override {
            stream << "Builtin.StandardInput";
        }
        std::optional<std::reference_wrapper<std::ostream>> output() const override {
            return std::nullopt;
        }
        std::optional<std::reference_wrapper<std::istream>> input() const override {
            return std::ref(std::cin);
        }
    };

    class StandardOutput final : public Port {
    public:
        void printValue(std::ostream& stream) const override {
            stream << "Builtin.StandardOutput";
        }
        std::optional<std::reference_wrapper<std::ostream>> output() const override {
            return std::ref(std::cout);
        }
        std::optional<std::reference_wrapper<std::istream>> input() const override {
            return std::nullopt;
        }
    };

    class InterpreterImpl final : public Interpreter {
        EvaluateContext mContext;

    public:
        InterpreterImpl() : mContext{ {}, makeRefCount<StandardInput>(), makeRefCount<StandardOutput>() } {
            // handle other control-flows
            using namespace std::string_view_literals;
            std::set_terminate([] { fatal("[FATAL] Terminated"sv); });
            std::set_new_handler([] {
                // TODO: full GC
                fatal("[FATAL] Out Of Memory"sv);
            });
            signal(SIGSEGV, [](int) { fatal("[FATAL] Segmentation Fault"sv); });
            signal(SIGINT, [](int) { fatal("[FATAL] Interrupted by User"sv); });
            signal(SIGABRT, [](int) { fatal("[FATAL] Aborted"sv); });
            signal(SIGFPE, [](int) { fatal("[FATAL] Arithmetic Exception"sv); });
            // add builtin procedures
            initializeBuiltinArithmeticProcedure(mContext.scope);
            initializeBuiltinRTTIProcedure(mContext.scope);
            initializeBuiltinWriteProcedure(mContext.scope);
            initializeBuiltinStringProcedure(mContext.scope);
            initializeBuiltinMathProcedure(mContext.scope);
        }

        [[nodiscard]] std::string execute(const std::string_view statement) override {
            const auto root = parse(statement);
            const auto res = root->evaluate(mContext);
            std::stringstream ss;
            res->printValue(ss);
            return ss.str();
        }
    };

    [[nodiscard]] Ref<Interpreter> createInterpreter() {
        return makeRefCount<InterpreterImpl>();
    }
}  // namespace schemepp
