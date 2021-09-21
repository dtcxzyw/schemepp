// SPDX-License-Identifier: MIT

#include "Interface/Value.hpp"

namespace schemepp {
    class ListImpl final : public ListValue {
        std::list<Ref<Value>> mValues;

    public:
        explicit ListImpl(std::list<Ref<Value>> values) : mValues{ std::move(values) } {}
        void printValue(std::ostream& stream) const override {
            stream << "[ ";
            for(auto&& child : mValues) {
                child->printValue(stream);
                stream << " ";
            }
            stream << "]";
        }
        const std::list<Ref<Value>>& ref() const noexcept override {
            return mValues;
        }
    };

    Ref<Value> makeList(std::list<Ref<Value>> values) {
        return makeRefCount<ListImpl>(std::move(values));
    }
}  // namespace schemepp
