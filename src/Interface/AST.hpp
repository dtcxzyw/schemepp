// SPDX-License-Identifier: MIT

#pragma once
#include "EvaluateContext.hpp"
#include "Result.hpp"
#include "Scope.hpp"
#include "Value.hpp"

namespace schemepp {

    class Node : public RefCountBase {
    public:
        virtual Result<Ref<Value>> evaluate(EvaluateContext& context) const = 0;
        virtual void printAST(std::ostream& stream) const = 0;
    };

    Result<Ref<Node>> parse(std::string_view statement);
    Result<std::vector<Ref<Node>>> parseFile(std::string_view path);

    // serialize & deserialize

}  // namespace schemepp
