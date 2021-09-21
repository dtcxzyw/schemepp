// SPDX-License-Identifier: MIT

#pragma once
#include <iostream>
#include <string>

namespace schemepp {
    class Error final : public std::exception {
        std::string mErrorMessage;

    public:
        explicit Error(std::string errorMessage) : mErrorMessage{ std::move(errorMessage) } {}

        [[nodiscard]] char const* what() const override {
            return mErrorMessage.c_str();
        }
    };
    
    [[noreturn]] void throwNotImplementedError();
    [[noreturn]] void throwInternalError();

}  // namespace schemepp
