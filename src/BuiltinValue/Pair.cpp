// SPDX-License-Identifier: MIT

#include "Interface/Value.hpp"

namespace schemepp {
    class PairImpl final : public PairValue {
        Pair mPair;

    public:
        explicit PairImpl(Ref<Value> car, Ref<Value> cdr) : mPair{ std::move(car), std::move(cdr) } {}
        void printValue(std::ostream& stream) const override {
            stream << '(';
            const Value* cur = this;

            while(true) {
                if(cur->type() == ValueType::pair) {
                    auto& [car, cdr] = dynamic_cast<const PairValue*>(cur)->value();
                    car->printValue(stream);
                    stream << ' ';

                    cur = cdr.get();
                } else {
                    stream << ". ";
                    cur->printValue(stream);
                    break;
                }
            }

            stream << ')';
        }
        const Pair& ref() const noexcept override {
            return mPair;
        }
    };

    Ref<Value> makePair(Ref<Value> car, Ref<Value> cdr) {
        return makeRefCount<PairImpl>(std::move(car), std::move(cdr));
    }

    const Pair& asPair(const Ref<Value>& value) {
        if(value->type() == ValueType::pair) {
            return dynamic_cast<const PairValue*>(value.get())->ref();
        }
        throwMismatchedOperandTypeError(ValueType::pair, value->type());
    }
}  // namespace schemepp
