// SPDX-License-Identifier: MIT

#include "Interface/Value.hpp"

namespace schemepp {
    class VectorImpl final : public VectorValue {
        std::vector<Ref<Value>> mValues;

    public:
        explicit VectorImpl(std::vector<Ref<Value>> values) : mValues{ std::move(values) } {}
        void printValue(std::ostream& stream) const override {
            stream << "[ ";
            for(auto&& child : mValues) {
                child->printValue(stream);
                stream << " ";
            }
            stream << "]";
        }
        const std::vector<Ref<Value>>& ref() const noexcept override {
            return mValues;
        }
    };

    Ref<Value> makeVector(std::vector<Ref<Value>> values) {
        return makeRefCount<VectorImpl>(std::move(values));
    }
}  // namespace schemepp
