// SPDX-License-Identifier: MIT

#pragma once
#include "Common.hpp"
#include "Result.hpp"
#include <string_view>

namespace schemepp {
    class Interpreter : public RefCountBase {
    public:
        [[nodiscard]] virtual Result<std::string> execute(std::string_view statement) = 0;
    };

    [[nodiscard]] Ref<Interpreter> createInterpreter();
}  // namespace schemepp
