// SPDX-License-Identifier: MIT

#pragma once
#include "Scope.hpp"
#include "Value.hpp"

namespace schemepp {

    struct GlobalSettings final {
        bool printAST;
        std::vector<std::string> builtinExtensions;
        bool enableCache;
    };

    struct EvaluateContext final {
        Scope scope;
        Ref<Port> currentInputPort;
        Ref<Port> currentOutputPort;
        GlobalSettings settings;
    };

}  // namespace schemepp
