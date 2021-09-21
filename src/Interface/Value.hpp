// SPDX-License-Identifier: MIT

#pragma once
#include "Common.hpp"
#include "Result.hpp"

#include <complex>
#include <list>
#include <optional>
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

    enum class ValueType {
        integer,
        real,
        complex,
        boolean,
        byteVector,
        character,
        list,
        pair,
        string,
        vector,
        procedure,
        port,
        symbol
    };

    class Value : public RefCountBase {
    public:
        virtual void printValue(std::ostream& stream) const = 0;
        [[nodiscard]] virtual ValueType type() const noexcept = 0;
        [[nodiscard]] virtual bool equal(const Ref<Value>& rhs) const noexcept {
            return this == rhs.get();
        }
    };

    Ref<Value> constantBoolean(bool val);
    bool toBoolean(const Ref<Value>& condition);
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

        [[nodiscard]] virtual Result<Ref<Value>> apply(EvaluateContext& ctx, const std::vector<Ref<Value>>& operands) = 0;
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

}  // namespace schemepp
