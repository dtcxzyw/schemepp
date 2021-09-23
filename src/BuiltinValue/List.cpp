// SPDX-License-Identifier: MIT

#include "Interface/Value.hpp"

namespace schemepp {
    class ListImpl final : public ListValue {
        std::list<Ref<Value>> mValues;

    public:
        explicit ListImpl(std::list<Ref<Value>> values) : mValues{ std::move(values) } {}
        void printValue(std::ostream& stream) const override {
            stream << '(';
            for(auto&& child : mValues) {
                child->printValue(stream);
                if(&child != &mValues.back())
                    stream << ' ';
            }
            stream << ')';
        }
        const std::list<Ref<Value>>& ref() const noexcept override {
            return mValues;
        }
    };

    Ref<Value> makeList(std::list<Ref<Value>> values) {
        return makeRefCount<ListImpl>(std::move(values));
    }

    const std::list<Ref<Value>>& asList(const Ref<Value>& value) {
        if(value->type() == ValueType::list) {
            return dynamic_cast<const ListValue*>(value.get())->ref();
        }
        throwMismatchedOperandTypeError(ValueType::list, value->type());
    }
}  // namespace schemepp
