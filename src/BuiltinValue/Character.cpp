// SPDX-License-Identifier: MIT

#include "Interface/Value.hpp"
#include <fmt/format.h>
#include <fmt/ostream.h>

namespace schemepp {
    class CharacterImpl final : public CharacterValue {
        uint32_t mVal;

    public:
        explicit CharacterImpl(const uint32_t val) : mVal{ val } {}
        void printValue(std::ostream& stream) const override {
            if(std::isgraph(static_cast<int>(mVal)))
                fmt::print(stream, "#\\{}", static_cast<char>(mVal));
            else if(mVal < 65536U)
                fmt::print(stream, "U+{:04X}", mVal);
            else
                fmt::print(stream, "U+{:06X}", mVal);
        }
        const uint32_t& ref() const noexcept override {
            return mVal;
        }
    };

    Ref<Value> constantCharacter(const uint32_t val) {
        return makeRefCount<CharacterImpl>(val);
    }
}  // namespace schemepp
