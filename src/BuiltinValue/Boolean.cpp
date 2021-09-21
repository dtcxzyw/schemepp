// SPDX-License-Identifier: MIT

#include "Interface/Value.hpp"

namespace schemepp {
    class BooleanImpl final : public BooleanValue {
        bool mVal;

    public:
        explicit BooleanImpl(const bool val) : mVal{ val } {}
        void printValue(std::ostream& stream) const override {
            stream << (mVal ? "#t" : "#f");
        }
        const bool& ref() const noexcept override {
            return mVal;
        }
    };

    Ref<Value> constantBoolean(const bool val) {
        return makeRefCount<BooleanImpl>(val);
    }

    bool asBoolean(const Ref<Value>& value) {
        return (value->type() != ValueType::boolean) || dynamic_cast<const BooleanValue*>(value.get())->value();
    }
}  // namespace schemepp
