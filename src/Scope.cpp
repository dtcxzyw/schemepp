// SPDX-License-Identifier: MIT

#include "Interface/Scope.hpp"
#include "Interface/Error.hpp"
#include <fmt/format.h>

namespace schemepp {
    Ref<Value> Scope::insert(std::string symbol, Ref<Value> value) {
        if(mSymbols.insert(std::make_pair(symbol, value)).second) {
            return value;
        }
        throw Error{ fmt::format("Redefinition of symbol \"{}\"", std::move(symbol)) };
    }
    Ref<Value> Scope::assign(const std::string& symbol, Ref<Value> value) {
        if(const auto iter = mSymbols.find(symbol); iter != mSymbols.cend()) {
            iter->second = value;
            return value;
        }
        throw Error{ fmt::format("Undefined symbol \"{}\"", symbol) };
    }
    Ref<Value> Scope::lookup(const std::string& symbol) const {
        if(const auto iter = mSymbols.find(symbol); iter != mSymbols.cend())
            return iter->second;
        throw Error{ fmt::format("Undefined symbol \"{}\"", symbol) };
    }

    ScopeGuard::ScopeGuard(Scope& scope, std::vector<std::pair<std::string, Ref<Value>>> oldSymbols)
        : mScope{ scope }, mOldSymbols{ std::move(oldSymbols) } {}

    ScopeGuard::~ScopeGuard() {
        mScope.recover(std::move(mOldSymbols));
    }

    ScopeGuard Scope::bind(const std::vector<std::string>& symbols, const std::vector<Ref<Value>>& operands) {
        std::vector<std::pair<std::string, Ref<Value>>> oldSymbols;
        oldSymbols.reserve(mSymbols.size());

        for(size_t i = 0; i < symbols.size(); ++i) {
            auto& key = symbols[i];
            auto value = operands[i];
            auto& ref = mSymbols[key];
            std::swap(ref, value);
            oldSymbols.emplace_back(key, std::move(value));
        }

        return { *this, std::move(oldSymbols) };
    }

    void Scope::recover(std::vector<std::pair<std::string, Ref<Value>>> oldSymbols) {
        for(auto& [k, v] : oldSymbols) {
            if(v)
                mSymbols[k] = std::move(v);
            else
                mSymbols.erase(k);
        }
    }

}  // namespace schemepp
