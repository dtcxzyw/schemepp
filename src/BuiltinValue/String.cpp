// SPDX-License-Identifier: MIT

#include "Interface/Value.hpp"
#include <fmt/format.h>

namespace schemepp {
    class StringImpl final : public StringValue {
        std::string mVal;

    public:
        explicit StringImpl(std::string val) : mVal{ std::move(val) } {}
        void printValue(std::ostream& stream) const override {
            stream << '\"' << mVal << '\"';
        }
        const std::string& ref() const noexcept override {
            return mVal;
        }
    };

    Ref<Value> constantString(std::string val) {
        return makeRefCount<StringImpl>(std::move(val));
    }

    const std::string& asString(const Ref<Value>& value) {
        if(value->type() == ValueType::string) {
            return dynamic_cast<const StringValue*>(value.get())->ref();
        }
        throwMismatchedOperandTypeError(ValueType::string, value->type());
    }
}  // namespace schemepp
