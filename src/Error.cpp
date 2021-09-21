// SPDX-License-Identifier: MIT

#include "Interface/Error.hpp"

namespace schemepp {
    void throwInternalError() {
        throw Error{ "Internal error" };
    }
    void throwNotImplementedError() {
        throw Error{ "Not implemented" };
    }

}  // namespace schemepp
