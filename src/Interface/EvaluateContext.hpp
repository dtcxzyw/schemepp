// SPDX-License-Identifier: MIT

#pragma once
#include "Scope.hpp"
#include "Value.hpp"
#include <unordered_set>

namespace schemepp {

    struct GlobalSettings final {
        bool printAST;
        std::unordered_set<std::string> builtinExtensions;
        bool enableCache;
    };

    struct EvaluateContext final {
        std::reference_wrapper<Scope> scope;
        Ref<InputPort> currentInputPort;
        Ref<OutputPort> currentOutputPort;
        GlobalSettings settings;
    };

}  // namespace schemepp
