// SPDX-License-Identifier: MIT

#pragma once
#include "Error.hpp"
#include <variant>

namespace schemepp {

    template <typename T>
    class Result final {
        std::variant<T, Error> mValue;

    public:
        explicit Result(T value) : mValue{ std::move(value) } {}
        explicit Result(Error error) : mValue{ std::move(error) } {}

        [[nodiscard]] Error& error() noexcept {  // NOLINT(bugprone-exception-escape)
            return std::get<Error>(mValue);
        }
        [[nodiscard]] T& get() noexcept {  // NOLINT(bugprone-exception-escape)
            return std::get<T>(mValue);
        }
        [[nodiscard]] explicit operator bool() const noexcept {
            return mValue.index() == 0;
        }
    };

}  // namespace schemepp
