// SPDX-License-Identifier: MIT

#pragma once
#include "Common.hpp"
#include <string_view>

namespace schemepp {
    struct GlobalSettings;

    class Interpreter : public RefCountBase {
    public:
        [[nodiscard]] virtual std::string execute(std::string_view statement) = 0;
    };

    [[nodiscard]] Ref<Interpreter> createInterpreter(GlobalSettings settings);
}  // namespace schemepp
