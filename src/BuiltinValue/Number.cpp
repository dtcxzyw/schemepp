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
}  // namespace schemepp
