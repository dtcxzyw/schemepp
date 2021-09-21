// SPDX-License-Identifier: MIT

#pragma once
#include <iostream>
#include <string>

namespace schemepp {
    struct Error final {
        std::string errorMessage;
    };

    std::ostream& operator<<(std::ostream& stream, const Error& error);
}  // namespace schemepp
