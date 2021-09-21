// SPDX-License-Identifier: MIT

#include "Interface/Value.hpp"
#include "Interface/Error.hpp"
#include <fmt/format.h>

namespace std {
    bool operator==(const schemepp::Ref<schemepp::Value>& lhs, const schemepp::Ref<schemepp::Value>& rhs) {
        return lhs->equal(rhs);
    }

}  // namespace std

namespace schemepp {
    // TODO: format error message & call stack
    [[noreturn]] void throwNoOperandError(EvaluateContext& ctx) {
        throw Error{ "No operand" };
    }
    [[noreturn]] void throwWrongOperandCountError(EvaluateContext& ctx, const uint32_t expect, const size_t passed) {
        throw Error{ fmt::format("Wrong operand count. Expect {}, but got {}", expect, passed) };
    }
    [[noreturn]] void throwMismatchedOperandTypeError(EvaluateContext& ctx, uint32_t idx, ValueType expect, ValueType passed) {
        throw Error{ "Unsupported type" };
    }
    [[noreturn]] void throwMismatchedOperandTypeError(ValueType expect, ValueType passed) {
        throw Error{ "Unsupported type" };
    }
    [[noreturn]] void throwDomainError() {
        throw Error{ "out of domain" };
    }
    [[noreturn]] void throwOutOfBoundError(EvaluateContext& ctx, Integer bound, Integer access) {
        throw Error{ "out of bound" };
    }
    [[noreturn]] void throwOutOfRangeError(EvaluateContext& ctx) {
        throw Error{ "out of range" };
    }
    [[noreturn]] void throwDividedByZeroError() {
        throw Error{ "divided by zero" };
    }
}  // namespace schemepp
