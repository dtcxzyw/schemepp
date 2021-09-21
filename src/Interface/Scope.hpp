// SPDX-License-Identifier: MIT

#pragma once
#include "Value.hpp"
#include <unordered_map>

namespace schemepp {
    class Scope final {
        std::unordered_map<std::string, Ref<Value>> mSymbols;

    public:
        Ref<Value> insert(std::string symbol, Ref<Value> value);
        Ref<Value> assign(const std::string& symbol, Ref<Value> value);
        [[nodiscard]] Ref<Value> lookup(const std::string& symbol) const;
    };
}  // namespace schemepp
