// SPDX-License-Identifier: MIT

#include "Interface/Error.hpp"

namespace schemepp {

    std::ostream& operator<<(std::ostream& stream, const Error& error) {
        return stream << error.errorMessage;
    }

}  // namespace schemepp
