// SPDX-License-Identifier: MIT

#include "Interface/Value.hpp"
#include <fmt/format.h>
#include <fmt/ostream.h>
#include <fmt/ranges.h>

namespace schemepp {

    class ByteVectorImpl final : public ByteVectorValue {
        std::vector<uint8_t> mVal;

    public:
        explicit ByteVectorImpl(std::vector<uint8_t> val) : mVal{ std::move(val) } {}
        void printValue(std::ostream& stream) const override {
            fmt::print(stream, "{:02X}", mVal);
        }
        const std::vector<uint8_t>& ref() const noexcept override {
            return mVal;
        }
    };

    Ref<Value> constantByteVector(std::vector<uint8_t> val) {
        return makeRefCount<ByteVectorImpl>(std::move(val));
    }
}  // namespace schemepp
