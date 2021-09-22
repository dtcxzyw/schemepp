// SPDX-License-Identifier: MIT

#pragma once
#include "Value.hpp"
#include <unordered_map>

namespace schemepp {
    class ScopeGuard final {
        Scope& mScope;
        std::vector<std::pair<std::string, Ref<Value>>> mOldSymbols;

    public:
        ScopeGuard(Scope& scope, std::vector<std::pair<std::string, Ref<Value>>> oldSymbols);
        ScopeGuard(ScopeGuard&&) = default;
        ScopeGuard(const ScopeGuard&) = delete;
        ScopeGuard& operator=(const ScopeGuard&) = delete;
        ScopeGuard& operator=(ScopeGuard&&) = delete;
        ~ScopeGuard();
    };

    class Scope final {
        std::unordered_map<std::string, Ref<Value>> mSymbols;

    public:
        Ref<Value> insert(std::string symbol, Ref<Value> value);
        Ref<Value> assign(const std::string& symbol, Ref<Value> value);
        [[nodiscard]] Ref<Value> lookup(const std::string& symbol) const;
        ScopeGuard bind(const std::vector<std::string>& symbols, const std::vector<Ref<Value>>& operands);
        void recover(std::vector<std::pair<std::string, Ref<Value>>> oldSymbols);
    };
}  // namespace schemepp
