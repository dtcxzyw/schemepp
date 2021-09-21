// SPDX-License-Identifier: MIT

#include "Interface/Value.hpp"

namespace std {
    bool operator==(const schemepp::Ref<schemepp::Value>& lhs, const schemepp::Ref<schemepp::Value>& rhs) {
        return lhs->equal(rhs);
    }
}  // namespace std
