// SPDX-License-Identifier: MIT

#include "Interface/Value.hpp"
#include <fmt/format.h>
#include <fmt/ostream.h>

namespace schemepp {
    class IntegerImpl final : public IntegerValue {
        Integer mVal;

    public:
        explicit IntegerImpl(const Integer val) : mVal{ val } {}
        void printValue(std::ostream& stream) const override {
            stream << mVal;
        }
        const Integer& ref() const noexcept override {
            return mVal;
        }
    };

    Ref<Value> constantInteger(const Integer val) {
        return makeRefCount<IntegerImpl>(val);
    }

    Integer asInteger(const Ref<Value>& value) {
        if(value->type() == ValueType::integer) {
            return dynamic_cast<const IntegerImpl*>(value.get())->ref();
        }
        throwMismatchedOperandTypeError(ValueType::integer, value->type());
    }

    class RealImpl final : public RealValue {
        Real mVal;

    public:
        explicit RealImpl(const Real val) : mVal{ val } {}
        void printValue(std::ostream& stream) const override {
            fmt::print(stream, "{}", mVal);
        }
        const Real& ref() const noexcept override {
            return mVal;
        }
    };

    Ref<Value> constantReal(const Real val) {
        return makeRefCount<RealImpl>(val);
    }

    class ComplexImpl final : public ComplexValue {
        Complex mVal;

    public:
        explicit ComplexImpl(const Complex val) : mVal{ val } {}
        void printValue(std::ostream& stream) const override {
            const auto zeroReal = std::fabs(mVal.real()) < std::numeric_limits<Real>::epsilon();
            const auto zeroImag = std::fabs(mVal.imag()) < std::numeric_limits<Real>::epsilon();
            if(zeroReal && zeroImag) {
                stream << '0';
            } else if(zeroReal)
                fmt::print(stream, "{}i", mVal.imag());
            else if(zeroImag)
                fmt::print(stream, "{}", mVal.real());
            else if(mVal.imag() > 0)
                fmt::print(stream, "{} + {}i", mVal.real(), mVal.imag());
            else
                fmt::print(stream, "{} - {}i", mVal.real(), -mVal.imag());
        }
        const Complex& ref() const noexcept override {
            return mVal;
        }
    };

    Ref<Value> constantComplex(const Complex val) {
        return makeRefCount<ComplexImpl>(val);
    }

    Number asNumber(const Ref<Value>& value) {
        if(value->type() == ValueType::integer) {
            return dynamic_cast<const IntegerImpl*>(value.get())->ref();
        }
        if(value->type() == ValueType::real) {
            return dynamic_cast<const RealImpl*>(value.get())->ref();
        }
        if(value->type() == ValueType::complex) {
            return dynamic_cast<const ComplexImpl*>(value.get())->ref();
        }
        throwMismatchedOperandTypeError(ValueType::integer | ValueType::real | ValueType::complex, value->type());
    }

    Ref<Value> constantNumberImpl(const Integer value) {
        return constantInteger(value);
    }
    Ref<Value> constantNumberImpl(const Real value) {
        return constantReal(value);
    }
    Ref<Value> constantNumberImpl(const Complex value) {
        return constantComplex(value);
    }

    Ref<Value> constantNumber(const Number& value) {
        return std::visit([](auto x) { return constantNumberImpl(x); }, value);
    }
}  // namespace schemepp
