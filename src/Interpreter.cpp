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

    class StandardInput final : public InputPort {
    public:
        void printValue(std::ostream& stream) const override {
            stream << "Builtin.StandardInput";
        }
        std::istream& input() const override {
            return std::cin;
        }
    };

    class StandardOutput final : public OutputPort {
    public:
        void printValue(std::ostream& stream) const override {
            stream << "Builtin.StandardOutput";
        }
        std::ostream& output() const override {
            return std::cout;
        }
    };

    class InterpreterImpl final : public Interpreter {
        Scope mGlobalScope;
        EvaluateContext mContext;

    public:
        explicit InterpreterImpl(GlobalSettings settings)
            : mGlobalScope{ nullptr }, mContext{ std::ref(mGlobalScope), makeRefCount<StandardInput>(),
                                                 makeRefCount<StandardOutput>(), std::move(settings) } {
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
            initializeBuiltinArithmeticProcedure(mGlobalScope);
            initializeBuiltinRTTIProcedure(mGlobalScope);
            initializeBuiltinWriteProcedure(mGlobalScope);
            initializeBuiltinStringProcedure(mGlobalScope);
            initializeBuiltinMathProcedure(mGlobalScope);
            initializeBuiltinFileProcedure(mGlobalScope);
            initializeBuiltinComplexProcedure(mGlobalScope);
            initializeBuiltinCharProcedure(mGlobalScope);
            initializeBuiltinProcessContextProcedure(mGlobalScope);
            initializeBuiltinContainerProcedure(mGlobalScope);
        }

        [[nodiscard]] std::string execute(const std::string_view statement) override {
            const auto root = parse(statement, mContext.settings.printAST);
            const auto res = root->evaluate(mContext);
            std::stringstream ss;
            res->printValue(ss);
            return ss.str();
        }
    };

    [[nodiscard]] Ref<Interpreter> createInterpreter(GlobalSettings settings) {
        return makeRefCount<InterpreterImpl>(std::move(settings));
    }
}  // namespace schemepp
