// SPDX-License-Identifier: MIT

#pragma once
#include "Common.hpp"
#include "Value.hpp"
#include <complex>
#include <list>
#include <optional>
#include <variant>
#include <vector>

namespace schemepp {
    class Value;
}

namespace std {
    bool operator==(const schemepp::Ref<schemepp::Value>& lhs, const schemepp::Ref<schemepp::Value>& rhs);
}

namespace schemepp {
    class Scope;
    struct EvaluateContext;

    enum class ValueType : uint32_t {
        integer = 1 << 0,
        real = 1 << 1,
        complex = 1 << 2,
        boolean = 1 << 3,
        byteVector = 1 << 4,
        character = 1 << 5,
        list = 1 << 6,
        pair = 1 << 7,
        string = 1 << 8,
        vector = 1 << 9,
        procedure = 1 << 10,
        port = 1 << 11,
        symbol = 1 << 12
    };

    constexpr ValueType operator|(const ValueType lhs, const ValueType rhs) {
        return static_cast<ValueType>(static_cast<uint32_t>(lhs) | static_cast<uint32_t>(rhs));
    }

    class Value : public RefCountBase {
    public:
        virtual void printValue(std::ostream& stream) const = 0;
        [[nodiscard]] virtual ValueType type() const noexcept = 0;
        [[nodiscard]] virtual bool equal(const Ref<Value>& rhs) const noexcept {
            return this == rhs.get();
        }
    };

    Ref<Value> constantBoolean(bool val);
    Ref<Value> constantByteVector(std::vector<uint8_t> val);
    Ref<Value> constantCharacter(uint32_t val);

    using Integer = int64_t;
    using Real = double;
    using Complex = std::complex<Real>;

    Ref<Value> constantInteger(Integer val);
    Ref<Value> constantReal(Real val);
    Ref<Value> constantComplex(Complex val);

    Ref<Value> constantString(std::string val);

    Ref<Value> makeVector(std::vector<Ref<Value>> values);
    Ref<Value> makeList(std::list<Ref<Value>> values);
    Ref<Value> makeSymbol(std::string val);

    class Procedure : public Value {
    public:
        [[nodiscard]] ValueType type() const noexcept final {
            return ValueType::procedure;
        }

        [[nodiscard]] virtual Ref<Value> apply(EvaluateContext& ctx, const std::vector<Ref<Value>>& operands) = 0;
    };

    class Port : public Value {
    public:
        [[nodiscard]] ValueType type() const noexcept final {
            return ValueType::port;
        }

        [[nodiscard]] virtual std::optional<std::reference_wrapper<std::istream>> input() const = 0;
        [[nodiscard]] virtual std::optional<std::reference_wrapper<std::ostream>> output() const = 0;
    };

#define BUILTIN_VALUE_DEFINE(NAME, NATIVE, TYPE)                                   \
    class NAME : public Value {                                                    \
    public:                                                                        \
        ValueType type() const noexcept final {                                    \
            return ValueType::TYPE;                                                \
        }                                                                          \
        [[nodiscard]] virtual const NATIVE& ref() const noexcept = 0;              \
        [[nodiscard]] const NATIVE& value() const noexcept {                       \
            return ref();                                                          \
        }                                                                          \
        [[nodiscard]] NATIVE& ref() noexcept {                                     \
            return const_cast<NATIVE&>(const_cast<const NAME*>(this)->ref());      \
        }                                                                          \
        bool equal(const Ref<Value>& rhs) const noexcept final {                   \
            if(this == rhs.get())                                                  \
                return true;                                                       \
            if(ValueType::TYPE != rhs->type())                                     \
                return false;                                                      \
            return this->value() == dynamic_cast<const NAME*>(rhs.get())->value(); \
        }                                                                          \
    }

    BUILTIN_VALUE_DEFINE(BooleanValue, bool, boolean);
    BUILTIN_VALUE_DEFINE(IntegerValue, Integer, integer);
    BUILTIN_VALUE_DEFINE(RealValue, Real, real);
    BUILTIN_VALUE_DEFINE(ComplexValue, Complex, complex);
    BUILTIN_VALUE_DEFINE(StringValue, std::string, string);
    BUILTIN_VALUE_DEFINE(VectorValue, std::vector<Ref<Value>>, string);
    BUILTIN_VALUE_DEFINE(ListValue, std::list<Ref<Value>>, string);
    BUILTIN_VALUE_DEFINE(ByteVectorValue, std::vector<uint8_t>, byteVector);
    BUILTIN_VALUE_DEFINE(CharacterValue, uint32_t, character);

#undef BUILTIN_VALUE_DEFINE

    [[noreturn]] void throwNoOperandError(EvaluateContext& ctx);
    [[noreturn]] void throwWrongOperandCountError(EvaluateContext& ctx, uint32_t expect, size_t passed);
    [[noreturn]] void throwMismatchedOperandTypeError(EvaluateContext& ctx, uint32_t idx, ValueType expect, ValueType passed);
    [[noreturn]] void throwMismatchedOperandTypeError(ValueType expect, ValueType passed);
    [[noreturn]] void throwDomainError();
    [[noreturn]] void throwOutOfBoundError(EvaluateContext& ctx, Integer bound, Integer access);
    [[noreturn]] void throwOutOfRangeError(EvaluateContext& ctx);
    [[noreturn]] void throwDividedByZeroError();

    Integer asInteger(const Ref<Value>& value);
    using Number = std::variant<Integer, Real, Complex>;
    Number asNumber(const Ref<Value>& value);
    Ref<Value> constantNumber(const Number& value);
    const std::string& asString(const Ref<Value>& value);
    uint32_t asCharacter(const Ref<Value>& value);
    bool asBoolean(const Ref<Value>& value);
    const std::vector<Ref<Value>>& asVector(const Ref<Value>& value);
    const std::vector<uint8_t>& asByteVector(const Ref<Value>& value);

}  // namespace schemepp
