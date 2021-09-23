// SPDX-License-Identifier: MIT

#include "Interface/Scope.hpp"
#include "Interface/Error.hpp"
#include <fmt/format.h>

namespace schemepp {
    Ref<Value> Scope::insert(const std::string& symbol, Ref<Value> value) {
        if(mSymbols.insert(std::make_pair(symbol, value)).second) {
            return value;
        }
        throw Error{ fmt::format("Redefinition of symbol \"{}\"", symbol) };
    }
    Ref<Value> Scope::assign(const std::string& symbol, Ref<Value> value) {
        if(!mLookupBarrier) {
            if(const auto iter = mSymbols.find(symbol); iter != mSymbols.cend()) {
                iter->second = value;
                return value;
            }
        }
        if(mParent)
            return mParent->assign(symbol, std::move(value));
        throw Error{ fmt::format("Undefined symbol \"{}\"", symbol) };
    }
    Ref<Value> Scope::lookup(const std::string& symbol) const {
        if(!mLookupBarrier)
            if(const auto iter = mSymbols.find(symbol); iter != mSymbols.cend())
                return iter->second;
        if(mParent)
            return mParent->lookup(symbol);
        throw Error{ fmt::format("Undefined symbol \"{}\"", symbol) };
    }

}  // namespace schemepp
