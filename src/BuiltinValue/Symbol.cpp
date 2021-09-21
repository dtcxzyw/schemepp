// SPDX-License-Identifier: MIT

#include "Interface/Value.hpp"
#include <utf8cpp/utf8.h>
#include <utility>

namespace schemepp {
    static bool compareStringInsensitive(const std::string& lhs, const std::string& rhs) {
        if(lhs.size() != rhs.size())
            return false;
        auto iterA = lhs.cbegin(), iterB = rhs.cbegin();
        while(iterA != lhs.cend() && iterB != rhs.cend()) {
            const auto cpA = static_cast<int32_t>(utf8::next(iterA, lhs.cend()));
            const auto cpB = static_cast<int32_t>(utf8::next(iterB, rhs.cend()));
            if(cpA == cpB)
                continue;
            if(std::isalpha(cpA) && std::isalpha(cpB)) {
                if(std::tolower(cpA) != std::tolower(cpB))
                    return false;
            } else
                return false;
        }
        return iterA == lhs.cend() && iterB == rhs.cend();
    }

    class SymbolImpl final : public Value {
        std::string mVal;

    public:
        explicit SymbolImpl(std::string val) : mVal{ std::move(val) } {}
        void printValue(std::ostream& stream) const override {
            stream << mVal;
        }
        ValueType type() const noexcept override {
            return ValueType::symbol;
        }
        bool equal(const Ref<Value>& rhs) const noexcept override {
            if(this == rhs.get())
                return true;
            if(rhs->type() != ValueType::symbol)
                return false;

            return compareStringInsensitive(dynamic_cast<const SymbolImpl*>(rhs.get())->mVal, mVal);
        }
    };

    Ref<Value> makeSymbol(std::string val) {
        return makeRefCount<SymbolImpl>(std::move(val));
    }
}  // namespace schemepp
