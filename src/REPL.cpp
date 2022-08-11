// SPDX-License-Identifier: MIT

#include "Interface/EvaluateContext.hpp"
#include "Interface/Interpreter.hpp"
#include <cstdlib>
#include <gflags/gflags.h>
#include <iostream>
#include <rang.hpp>
#include <string>

#if defined(SCHEMEPP_DEBUG)
static constexpr bool printASTDefault = true;
#else
static constexpr bool printASTDefault = false;
#endif

DEFINE_bool(printAST, printASTDefault, "printAST");
DEFINE_string(extensions, "", "extensions");

static void about() {
    std::cout << "schemepp [built " __DATE__ " at " __TIME__ "]" << std::endl;
    std::cout << "Copyright (c) 2021 Yingwei Zheng" << std::endl;
    std::cout << "The source code to schemepp is covered by the MIT License." << std::endl;
    std::cout << "See the file LICENSE for the conditions of the license." << std::endl;
}

static std::string input() {
    int64_t count = 0;
    std::string res;
    bool first = true;
    do {
        if(first) {
            std::cout << rang::fg::green << ">>> " << rang::fg::reset << std::flush;
            first = false;
        } else {
            std::cout << rang::fg::green << "---   " << rang::fg::reset << std::flush;
        }
        std::string statement;
        std::getline(std::cin, statement);
        count += std::count(statement.cbegin(), statement.cend(), '(') -
            std::count(statement.cbegin(), statement.cend(), ')');  // TODO: escape
        res += statement;
    } while(count > 0);
    return res;
}

int main(int argc, char** argv) {
    using namespace schemepp;
    try {
        gflags::ParseCommandLineFlags(&argc, &argv, true);

        GlobalSettings settings;
        settings.printAST = FLAGS_printAST;
        constexpr auto splitExtensions = [](std::string str) {
            std::unordered_set<std::string> res;
            while(true) {
                const auto pos = str.find(',');
                if(pos == std::string::npos) {
                    if(!str.empty())
                        res.insert(str);
                    break;
                }
                res.insert(str.substr(0, pos));
                str = str.substr(pos + 1);
            }

            return res;
        };
        settings.builtinExtensions = splitExtensions(FLAGS_extensions);

        const auto interpreter = createInterpreter(std::move(settings));

        // REPL mode
        about();

        while(std::cin) {
            const auto statement = input();
            if(std::all_of(statement.cbegin(), statement.cend(), [](const char ch) { return !std::isgraph(ch); }))
                continue;
            try {
                if(const auto str = interpreter->execute(statement); !str.empty())
                    std::cout << rang::fg::magenta << "=> " << rang::fg::reset << str << std::endl;
            } catch(std::exception& error) {
                std::cerr << rang::fg::red << error.what() << rang::fg::reset << std::endl;
            }
        }

        return EXIT_SUCCESS;

        /*
        if(argc == 2) {
            // direct execute mode
            using namespace std::string_literals;
            auto res = interpreter->execute("(load "s + argv[1] + ")");
            return EXIT_SUCCESS;
        }
        */

        /*
        {
            about();
            std::cerr << rang::fg::red << "[FATAL] Unrecognized Command" << rang::fg::reset << std::endl;
            return EXIT_FAILURE;
        }
        */
    } catch(std::exception& error) {
        std::cerr << rang::fg::red << "[FATAL] " << error.what() << rang::fg::reset << std::endl;
        return EXIT_FAILURE;
    } catch(...) {
        std::cerr << rang::fg::red << "[FATAL] Unknown Error" << rang::fg::reset << std::endl;
        return EXIT_FAILURE;
    }
}
