// SPDX-License-Identifier: MIT

#pragma once
#include "Scope.hpp"
#include "Value.hpp"

namespace schemepp {
    class Node : public RefCountBase {
    public:
        virtual Ref<Value> evaluate(EvaluateContext& context) const = 0;
        virtual void printAST(std::ostream& stream) const = 0;
    };

    Ref<Node> parse(std::string_view statement, bool printAST);
    std::vector<Ref<Node>> parseFile(std::string_view path);

    // serialize & deserialize

}  // namespace schemepp
