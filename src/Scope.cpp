// SPDX-License-Identifier: MIT

#include "Interface/Scope.hpp"
#include <fmt/format.h>

namespace schemepp {
    Result<Ref<Value>> Scope::insert(std::string symbol, Ref<Value> value) {
        if(mSymbols.insert(std::make_pair(std::move(symbol), value)).second) {
            return Result{ std::move(value) };
        }
        return Result<Ref<Value>>{ Error{ fmt::format("Redefinition of symbol \"{}\"", symbol) } };
    }
    Result<Ref<Value>> Scope::assign(const std::string& symbol, Ref<Value> value) {
        if(const auto iter = mSymbols.find(symbol); iter != mSymbols.cend()) {
            iter->second = value;
            return Result{ std::move(value) };
        }
        return Result<Ref<Value>>{ Error{ fmt::format("Undefined symbol \"{}\"", symbol) } };
    }
    Result<Ref<Value>> Scope::lookup(const std::string& symbol) const {
        if(const auto iter = mSymbols.find(symbol); iter != mSymbols.cend())
            return Result{ iter->second };
        return Result<Ref<Value>>{ Error{ fmt::format("Undefined symbol \"{}\"", symbol) } };
    }

}  // namespace schemepp
