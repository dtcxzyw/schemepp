// SPDX-License-Identifier: MIT

// ReSharper disable CppInconsistentNaming
// ReSharper disable CppClangTidyCppcoreguidelinesMacroUsage
#include "Interface/AST.hpp"
#include "Interface/Error.hpp"
#include "Interface/EvaluateContext.hpp"
#include <boost/fusion/include/adapt_struct.hpp>
#include <boost/spirit/home/x3.hpp>
#include <boost/spirit/home/x3/support/ast/position_tagged.hpp>
#include <boost/spirit/home/x3/support/utility/annotate_on_success.hpp>
#include <boost/spirit/home/x3/support/utility/error_reporting.hpp>
#include <charconv>

// TODO: split to different implementations

namespace schemepp {

    class Constant final : public Node {
        Ref<Value> mVal;

    public:
        explicit Constant(Ref<Value> val) : mVal{ std::move(val) } {}
        Ref<Value> evaluate(EvaluateContext&) const override {
            return mVal;
        }
        void printAST(std::ostream& stream) const override {
            stream << "(constant ";
            mVal->printValue(stream);
            stream << ')';
        }
    };

    class SymbolReference final : public Node {
        std::string mSymbol;

    public:
        explicit SymbolReference(std::string symbol) : mSymbol{ std::move(symbol) } {}
        Ref<Value> evaluate(EvaluateContext& context) const override {
            return context.scope.lookup(mSymbol);
        }
        void printAST(std::ostream& stream) const override {
            stream << "(symbol " << mSymbol << ')';
        }
    };

    template <typename DstContainer, typename SrcContainer>
    [[nodiscard]] DstContainer evaluateValues(EvaluateContext& context, const SrcContainer& src) {
        DstContainer values;
        if constexpr(std::is_same_v<DstContainer, std::vector<Ref<Value>>>)
            values.reserve(src.size());

        for(auto& child : src) {
            values.push_back(child->evaluate(context));
        }
        return values;
    }

    class VectorConstructor final : public Node {
        std::vector<Ref<Node>> mValues;

    public:
        explicit VectorConstructor(std::vector<Ref<Node>> values) : mValues{ std::move(values) } {}
        Ref<Value> evaluate(EvaluateContext& context) const override {
            return makeVector(evaluateValues<std::vector<Ref<Value>>>(context, mValues));
        }
        void printAST(std::ostream& stream) const override {
            stream << "(vector [ ";
            for(auto& child : mValues) {
                child->printAST(stream);
                stream << ' ';
            }
            stream << "] )";
        }
    };

    class ListConstructor final : public Node {
        std::vector<Ref<Node>> mValues;

    public:
        explicit ListConstructor(std::vector<Ref<Node>> values) : mValues{ std::move(values) } {}
        Ref<Value> evaluate(EvaluateContext& context) const override {
            return makeList(evaluateValues<std::list<Ref<Value>>>(context, mValues));
        }
        void printAST(std::ostream& stream) const override {
            stream << "(list [ ";
            for(auto& child : mValues) {
                child->printAST(stream);
                stream << ' ';
            }
            stream << "] )";
        }
    };

    class ProcedureCall final : public Node {
        Ref<Node> mOperator;
        std::vector<Ref<Node>> mOperands;

    public:
        explicit ProcedureCall(Ref<Node> operator_, std::vector<Ref<Node>> operands)
            : mOperator{ std::move(operator_) }, mOperands{ std::move(operands) } {}
        Ref<Value> evaluate(EvaluateContext& context) const override {
            auto operator_ = mOperator->evaluate(context);
            if(operator_->type() != ValueType::procedure)
                throwMismatchedOperandTypeError(context, 0, ValueType::procedure, operator_->type());

            auto operands = evaluateValues<std::vector<Ref<Value>>>(context, mOperands);
            return dynamic_cast<Procedure*>(operator_.get())->apply(context, operands);
        }

        void printAST(std::ostream& stream) const override {
            stream << '(';
            mOperator->printAST(stream);
            for(auto& operand : mOperands) {
                stream << ' ';
                operand->printAST(stream);
            }
            stream << ')';
        }
    };

    class Conditional final : public Node {
        Ref<Node> mCondition;
        Ref<Node> mThenPart;
        Ref<Node> mElsePart;

    public:
        explicit Conditional(Ref<Node> condition, Ref<Node> then, Ref<Node> else_)
            : mCondition{ std::move(condition) }, mThenPart{ std::move(then) }, mElsePart{ std::move(else_) } {}
        Ref<Value> evaluate(EvaluateContext& context) const override {
            if(asBoolean(mCondition->evaluate(context))) {
                return mThenPart->evaluate(context);
            }

            if(mElsePart) {
                return mElsePart->evaluate(context);
            }
            return constantBoolean(false);
        }
        void printAST(std::ostream& stream) const override {
            stream << "(if ";
            mCondition->printAST(stream);
            stream << ' ';
            mThenPart->printAST(stream);
            if(mElsePart) {
                stream << ' ';
                mElsePart->printAST(stream);
            }
            stream << ')';
        }
    };

    class Sequence final : public Node {
        std::vector<Ref<Node>> mSequence;

    public:
        explicit Sequence(std::vector<Ref<Node>> sequence) : mSequence{ std::move(sequence) } {}
        Ref<Value> evaluate(EvaluateContext& context) const override {
            return evaluateValues<std::vector<Ref<Value>>>(context, mSequence).back();
        }
        void printAST(std::ostream& stream) const override {
            stream << "(begin";
            for(auto& child : mSequence) {
                stream << ' ';
                child->printAST(stream);
            }
            stream << ')';
        }
    };

    class LogicOr final : public Node {
        std::vector<Ref<Node>> mSequence;

    public:
        explicit LogicOr(std::vector<Ref<Node>> sequence) : mSequence{ std::move(sequence) } {}
        Ref<Value> evaluate(EvaluateContext& context) const override {
            for(auto& child : mSequence) {
                if(asBoolean(child->evaluate(context)))
                    return constantBoolean(true);
            }
            return constantBoolean(false);
        }
        void printAST(std::ostream& stream) const override {
            stream << "(or";
            for(auto& child : mSequence) {
                stream << ' ';
                child->printAST(stream);
            }
            stream << ')';
        }
    };

    class LogicAnd final : public Node {
        std::vector<Ref<Node>> mSequence;

    public:
        explicit LogicAnd(std::vector<Ref<Node>> sequence) : mSequence{ std::move(sequence) } {}
        Ref<Value> evaluate(EvaluateContext& context) const override {
            for(auto& child : mSequence) {
                if(!asBoolean(child->evaluate(context)))
                    return constantBoolean(false);
            }
            return constantBoolean(true);
        }
        void printAST(std::ostream& stream) const override {
            stream << "(and";
            for(auto& child : mSequence) {
                stream << ' ';
                child->printAST(stream);
            }
            stream << ')';
        }
    };

    class Definition final : public Node {
        std::string mSymbol;
        Ref<Node> mExpr;
        bool mAssignment;

    public:
        explicit Definition(std::string symbol, Ref<Node> expr, const bool assignment)
            : mSymbol{ std::move(symbol) }, mExpr{ std::move(expr) }, mAssignment{ assignment } {}
        Ref<Value> evaluate(EvaluateContext& context) const override {
            auto val = mExpr->evaluate(context);
            if(mAssignment)
                return context.scope.assign(mSymbol, std::move(val));
            return context.scope.insert(mSymbol, std::move(val));
        }
        void printAST(std::ostream& stream) const override {
            stream << "(define " << mSymbol << ' ';
            mExpr->printAST(stream);
            stream << ')';
        }
    };

}  // namespace schemepp

namespace schemepp {
    namespace parser {
        namespace x3 = boost::spirit::x3;

        struct ParseResult final : x3::position_tagged {
            Ref<Node> root;

            ParseResult() = default;
            // ReSharper disable once CppNonExplicitConvertingConstructor
            ParseResult(Ref<Node> root) : root{ std::move(root) } {}
            ParseResult(const ParseResult&) = default;
            ParseResult(ParseResult&&) = default;
            ParseResult& operator=(const ParseResult&) = default;
            ParseResult& operator=(ParseResult&&) = default;
            ~ParseResult() = default;
        };
    }  // namespace parser
}  // namespace schemepp

BOOST_FUSION_ADAPT_STRUCT(schemepp::parser::ParseResult, root)

namespace schemepp {

    namespace x3 = boost::spirit::x3;

    namespace parser {

        struct ParseErrorHandler {
            template <typename Iterator, typename Exception, typename Context>
            x3::error_handler_result on_error(Iterator&, Iterator const&, Exception const& x, Context const& context) {
                auto& error_handler = x3::get<x3::error_handler_tag>(context).get();
                error_handler(x.where(), "Error! Expecting " + x.which() + " here:");
                return x3::error_handler_result::fail;
            }
        };

        // Please refer to https://small.r7rs.org/attachment/r7rs.pdf

#define DEFINE_X3_RULE(NAME)                                             \
    struct NAME##_class : ParseErrorHandler, x3::annotate_on_success {}; \
    x3::rule<NAME##_class, ParseResult> constexpr NAME = #NAME
#define DEFINE_X3_RULE_NONE(NAME)                                        \
    struct NAME##_class : ParseErrorHandler, x3::annotate_on_success {}; \
    x3::rule<NAME##_class, x3::unused_type> constexpr NAME = #NAME
#define DEFINE_X3_RULE_TYPE(NAME, TYPE)                                  \
    struct NAME##_class : ParseErrorHandler, x3::annotate_on_success {}; \
    x3::rule<NAME##_class, TYPE> constexpr NAME = #NAME

        DEFINE_X3_RULE(boolean);
        DEFINE_X3_RULE(number);
        DEFINE_X3_RULE(vector);
        DEFINE_X3_RULE(character);
        DEFINE_X3_RULE(string);
        DEFINE_X3_RULE(byteVector);
        DEFINE_X3_RULE(identifier);
        DEFINE_X3_RULE(expr);
        DEFINE_X3_RULE(literal);
        DEFINE_X3_RULE(selfEval);
        DEFINE_X3_RULE(quotation);
        DEFINE_X3_RULE(procedureCall);
        DEFINE_X3_RULE(lambda);
        DEFINE_X3_RULE(formal);
        DEFINE_X3_RULE(body);
        DEFINE_X3_RULE(sequence);
        DEFINE_X3_RULE(conditional);
        DEFINE_X3_RULE(assignment);
        DEFINE_X3_RULE(derived);
        DEFINE_X3_RULE(condClause);
        DEFINE_X3_RULE(bindingSpec);
        DEFINE_X3_RULE(mvBindingSpec);
        DEFINE_X3_RULE(iterSpec);
        DEFINE_X3_RULE(caseLambdaClause);
        DEFINE_X3_RULE(doResult);
        DEFINE_X3_RULE(macroUse);
        DEFINE_X3_RULE(macroBlock);
        DEFINE_X3_RULE(syntaxSpec);
        DEFINE_X3_RULE(include);
        DEFINE_X3_RULE(datum);
        DEFINE_X3_RULE(definition);
        DEFINE_X3_RULE(caseClause);
        DEFINE_X3_RULE(quasiQuotation);
        DEFINE_X3_RULE(transformerSpec);
        DEFINE_X3_RULE(simpleDatum);
        DEFINE_X3_RULE(compoundDatum);
        DEFINE_X3_RULE(list);
        DEFINE_X3_RULE(abbreviation);
        DEFINE_X3_RULE(label);
        DEFINE_X3_RULE_NONE(initial);
        DEFINE_X3_RULE_NONE(subsequent);
        DEFINE_X3_RULE_NONE(symbolElement);
        DEFINE_X3_RULE_NONE(peculiarIdentifier);
        DEFINE_X3_RULE_NONE(explicitSign);
        DEFINE_X3_RULE_NONE(signSubsequent);
        DEFINE_X3_RULE_NONE(dotSubsequent);
        DEFINE_X3_RULE(definitionFormal);
        DEFINE_X3_RULE(constructor);
        DEFINE_X3_RULE(fieldSpec);
        DEFINE_X3_RULE(labeledDatum);
        DEFINE_X3_RULE(booleanTrue);
        DEFINE_X3_RULE(booleanFalse);
        DEFINE_X3_RULE(integer);
        DEFINE_X3_RULE(real);
        DEFINE_X3_RULE(characterGraph);
        DEFINE_X3_RULE(characterUInt);
        DEFINE_X3_RULE(cond1);
        DEFINE_X3_RULE(cond2);
        DEFINE_X3_RULE(case1);
        DEFINE_X3_RULE(case2);
        DEFINE_X3_RULE(case3);
        DEFINE_X3_RULE(logicOr);
        DEFINE_X3_RULE(logicAnd);
        DEFINE_X3_RULE(when);
        DEFINE_X3_RULE(unless);
        DEFINE_X3_RULE(let1);
        DEFINE_X3_RULE(let2);
        DEFINE_X3_RULE(let3);
        DEFINE_X3_RULE(let4);
        DEFINE_X3_RULE(begin);
        DEFINE_X3_RULE(doStatement);
        DEFINE_X3_RULE(delay);
        DEFINE_X3_RULE(delayForce);
        DEFINE_X3_RULE(parameterize);
        DEFINE_X3_RULE(guard);
        DEFINE_X3_RULE(caseLambda);
        DEFINE_X3_RULE(iterSpec1);
        DEFINE_X3_RULE(iterSpec2);
        DEFINE_X3_RULE(definition1);
        DEFINE_X3_RULE(definition2);
        DEFINE_X3_RULE(definition3);
        DEFINE_X3_RULE(definition4);
        DEFINE_X3_RULE(definition5);
        DEFINE_X3_RULE(definition6);
        DEFINE_X3_RULE(condClause1);
        DEFINE_X3_RULE(condClause2);
        DEFINE_X3_RULE(condClause3);
        DEFINE_X3_RULE(caseClause1);
        DEFINE_X3_RULE(caseClause2);
        DEFINE_X3_RULE(list1);
        DEFINE_X3_RULE(list2);
        DEFINE_X3_RULE(complex);
        DEFINE_X3_RULE(rational);
        DEFINE_X3_RULE_NONE(floatingPoint);
        DEFINE_X3_RULE_TYPE(complexPart, Real);
        DEFINE_X3_RULE_TYPE(rationalPart, Integer);
        DEFINE_X3_RULE_TYPE(identifierPattern, std::string);

#undef DEFINE_X3_RULE
#undef DEFINE_X3_RULE_NONE
#undef DEFINE_X3_RULE_TYPE

        static std::vector<Ref<Node>> removePack(const std::vector<ParseResult>& nodes) {
            std::vector<Ref<Node>> res;
            res.reserve(nodes.size());
            for(const auto& node : nodes)
                res.push_back(node.root);
            return res;
        }

        // Literals

        constexpr auto buildTrue = [](auto& ctx) { x3::_val(ctx) = { makeRefCount<Constant>(constantBoolean(true)) }; };
        constexpr auto buildFalse = [](auto& ctx) { x3::_val(ctx) = { makeRefCount<Constant>(constantBoolean(false)) }; };
        const auto booleanTrue_def = (x3::lit("#true") | x3::lit("#t"))[buildTrue];
        const auto booleanFalse_def = (x3::lit("#false") | x3::lit("#f"))[buildFalse];
        constexpr auto boolean_def = booleanTrue | booleanFalse;

        // TODO: support radix 2,8,10,16
        constexpr auto parseNumber = [](const auto& range, auto& val) {
            const std::string_view str{ range.begin().operator->() + (range[0] == '+'),
                                        static_cast<size_t>(range.end() - range.begin()) };
            return static_cast<int32_t>(std::from_chars(str.data(), str.data() + str.size(), val).ec) == 0;
        };
        constexpr auto buildInteger = [](auto& ctx) {
            const auto& range = x3::_attr(ctx);
            Integer val = 0;
            if(parseNumber(range, val))
                x3::_val(ctx) = { makeRefCount<Constant>(constantInteger(val)) };
            else
                x3::_pass(ctx) = false;
        };
        const auto integer_def = x3::raw[x3::lexeme[(-explicitSign) >> x3::int64]][buildInteger];

        constexpr auto buildReal = [](auto& ctx) {
            const auto& range = x3::_attr(ctx);
            Real val = 0.0;
            if(parseNumber(range, val))
                x3::_val(ctx) = { makeRefCount<Constant>(constantReal(val)) };
            else
                x3::_pass(ctx) = false;
        };
        constexpr auto floatingPoint_def = x3::lexeme[(-explicitSign) >> (-x3::int64) >> x3::lit('.') >> x3::int64];
        const auto real_def = x3::raw[floatingPoint][buildReal];
        constexpr auto parseReal = [](auto& ctx) {
            const auto& range = x3::_attr(ctx);
            Real val = 0.0;
            if(parseNumber(range, val))
                x3::_val(ctx) = val;
            else
                x3::_pass(ctx) = false;
        };
        const auto complexPart_def = x3::raw[floatingPoint | x3::lexeme[(-explicitSign) >> x3::int64]][parseReal];
        constexpr auto buildComplex = [](auto& ctx) {
            const auto& attr = x3::_attr(ctx);
            const std::complex<Real> val{ boost::fusion::at_c<0>(attr).value_or(0.0), boost::fusion::at_c<1>(attr) };
            x3::_val(ctx) = { makeRefCount<Constant>(constantComplex(val)) };
        };
        const auto complex_def = ((-complexPart) >> complexPart >> x3::lit('i'))[buildComplex];
        constexpr auto buildRational = [](auto& ctx) {
            const auto& attr = x3::_attr(ctx);
            const auto val1 = boost::fusion::at_c<0>(attr);
            const auto val2 = boost::fusion::at_c<1>(attr);

            if(val2 != 0) {
                x3::_val(ctx) = { makeRefCount<Constant>(constantReal(static_cast<Real>(val1) / static_cast<Real>(val2))) };
            } else
                x3::_pass(ctx) = false;
        };
        constexpr auto parseInteger = [](auto& ctx) {
            const auto& range = x3::_attr(ctx);
            Integer val = 0;
            if(parseNumber(range, val))
                x3::_val(ctx) = val;
            else
                x3::_pass(ctx) = false;
        };
        const auto rationalPart_def = x3::raw[x3::lexeme[(-explicitSign) >> x3::int64]][parseInteger];
        const auto rational_def = (rationalPart >> x3::lit('/') >> rationalPart)[buildRational];
        constexpr auto number_def = complex | real | rational | integer;

        constexpr auto buildCharacterGraph = [](auto& ctx) {
            x3::_val(ctx) = { makeRefCount<Constant>(constantCharacter(static_cast<uint32_t>(x3::_attr(ctx)))) };
        };
        const auto characterGraph_def = (x3::lit("#\\") >> x3::graph)[buildCharacterGraph];

        struct CharacterName : x3::symbols<uint32_t> {
            CharacterName() {
                // ReSharper disable once CppExpressionWithoutSideEffects
                add("alarm", '\a')("backspace", '\b')(
                    "delete", 127)("escape", '\\')("newline", '\n')("null", '\0')("return", '\r')("space", ' ')("tab", '\t');
            }
        };

        const CharacterName characterNameSymbols;  // NOLINT(clang-diagnostic-exit-time-destructors)

        constexpr auto buildCharacterUInt = [](auto& ctx) {
            auto&& attr = x3::_attr(ctx);
            x3::_val(ctx) = { makeRefCount<Constant>(constantCharacter(boost::get<uint32_t>(attr))) };
        };
        const auto characterUInt_def =  // NOLINT(clang-diagnostic-exit-time-destructors)
            ((x3::lit("#\\") >> characterNameSymbols) | (x3::lit("#\\x") >> x3::hex))[buildCharacterUInt];
        constexpr auto character_def = characterUInt | characterGraph;

        // TODO: handle escape characters
        constexpr auto buildString = [](auto& ctx) {
            const auto& range = x3::_attr(ctx);
            std::string str{ range.begin() + 1, range.end() - 1 };
            x3::_val(ctx) = { makeRefCount<Constant>(constantString(std::move(str))) };
        };
        const auto string_def =
            x3::raw[x3::lexeme[x3::lit('\"') >> (*(x3::print - x3::lit('\"'))) >> x3::lit('\"')]][buildString];

        constexpr auto buildByteVector = [](auto& ctx) {
            const std::vector<uint8_t>& vec = x3::_attr(ctx);
            x3::_val(ctx) = { makeRefCount<Constant>(constantByteVector(vec)) };
        };
        const auto byteVector_def = (x3::lit("#u8") >> '(' >> (*x3::uint8) >> ')')[buildByteVector];
        constexpr auto parseSymbol = [](auto& ctx) {
            const auto& range = x3::_attr(ctx);
            x3::_val(ctx) = std::string{ range.begin(), range.end() };
        };
        const auto identifierPattern_def =
            x3::raw[x3::lexeme[((initial >> *subsequent) | ('|' >> *symbolElement >> '|') | peculiarIdentifier)]][parseSymbol];
        constexpr auto buildSymbol = [](auto& ctx) {
            x3::_val(ctx) = { makeRefCount<SymbolReference>(std::move(x3::_attr(ctx))) };
        };
        const auto identifier_def = identifierPattern[buildSymbol];
        constexpr auto peculiarIdentifier_def = explicitSign | (explicitSign >> signSubsequent >> *subsequent) |
            (explicitSign >> x3::lit('.') >> dotSubsequent >> *subsequent) | (x3::lit('.') >> dotSubsequent >> *subsequent);
        constexpr auto initial_def =
            x3::alpha | x3::lit('!') | '$' | '%' | '&' | '*' | '/' | ':' | '<' | '=' | '>' | '?' | '^' | '_' | '~';
        constexpr auto subsequent_def = initial | x3::digit | explicitSign | '.' | '@';
        constexpr auto symbolElement_def =
            (x3::char_ - (x3::lit('|') | '\\')) | (x3::lit("\\x") >> x3::hex) | "\\a" | "\\b" | "\\t" | "\\r" | "\\n" | "\\|";
        constexpr auto explicitSign_def = x3::lit('+') | '-';
        constexpr auto signSubsequent_def = initial | explicitSign | '@';
        constexpr auto dotSubsequent_def = signSubsequent | '.';

        // External Representations
        constexpr auto buildLabeledDatum = [](auto& ctx) {
            // TODO: Not implemented
            throwNotImplementedError();
        };
        const auto labeledDatum_def = (label >> '=' >> datum)[buildLabeledDatum];
        constexpr auto datum_def = simpleDatum | compoundDatum | labeledDatum | (label >> '#');
        constexpr auto simpleDatum_def = string | boolean | number | character | identifier | byteVector;
        constexpr auto compoundDatum_def = list | vector | abbreviation;

        constexpr auto buildAbbreviation = [](auto& ctx) {
            // TODO: Not implemented
            throwNotImplementedError();
        };
        const auto abbreviation_def = ((x3::lit('\'') | '`' | ',' | x3::lit(",@")) >> datum)[buildAbbreviation];
        constexpr auto buildVector = [](auto& ctx) {
            x3::_val(ctx) = { makeRefCount<VectorConstructor>(removePack(x3::_attr(ctx))) };
        };
        const auto vector_def = (x3::lit("#(") >> *datum >> ')')[buildVector];
        constexpr auto buildLabel = [](auto& ctx) {
            // TODO: Not implemented
            throwNotImplementedError();
        };
        const auto label_def = (x3::lit('#') >> x3::uint_)[buildLabel];
        constexpr auto buildList1 = [](auto& ctx) {
            x3::_val(ctx) = { makeRefCount<ListConstructor>(removePack(x3::_attr(ctx))) };
        };
        const auto list1_def = ('(' >> *datum >> ')')[buildList1];
        constexpr auto buildList2 = [](auto& ctx) {
            auto& attr = x3::_attr(ctx);
            auto res = removePack(boost::fusion::at_c<0>(attr));
            res.push_back(boost::fusion::at_c<1>(attr).root);
            x3::_val(ctx) = { makeRefCount<ListConstructor>(std::move(res)) };
        };
        const auto list2_def = ('(' >> +datum >> '.' >> datum >> ')')[buildList2];
        constexpr auto list_def = list1 | list2;

        // Expressions
        constexpr auto expr_def = literal | identifier | conditional | definition | lambda | assignment | derived |
            procedureCall | macroUse | macroBlock | include;
        constexpr auto literal_def = quotation | selfEval;
        constexpr auto selfEval_def = string | boolean | number | vector | character | byteVector;
        constexpr auto buildQuotation = [](auto& ctx) {
            const auto& attr = x3::_attr(ctx);
            x3::_val(ctx) = { makeRefCount<Constant>(makeSymbol(boost::get<std::string>(attr))) };
        };
        // const auto quotation_def = (('\'' >> datum) | ('(' >> x3::lit("quote") >> datum >> ')'))[buildQuotation]; ???
        const auto quotation_def =
            (('\'' >> identifierPattern) | ('(' >> x3::lit("quote") >> identifierPattern >> ')'))[buildQuotation];
        constexpr auto buildProcedureCall = [](auto& ctx) {
            auto&& attr = x3::_attr(ctx);
            x3::_val(ctx) = { makeRefCount<ProcedureCall>(boost::fusion::at_c<0>(attr).root,
                                                          removePack(boost::fusion::at_c<1>(attr))) };
        };
        const auto procedureCall_def = ('(' >> expr >> (*expr) >> ')')[buildProcedureCall];
        constexpr auto buildLambda = [](auto& ctx) {
            // TODO: Not implemented
            throwNotImplementedError();
        };
        const auto lambda_def = ('(' >> x3::lit("lambda") >> formal >> body >> ')')[buildLambda];
        constexpr auto buildFormal = [](auto& ctx) {
            // TODO: Not implemented
            throwNotImplementedError();
        };
        const auto formal_def =
            (('(' >> (*identifier) >> ')') | identifier | ('(' >> (+identifier) >> '.' >> identifier >> ')'))[buildFormal];
        constexpr auto buildBody = [](auto& ctx) {
            // TODO: Not implemented
            throwNotImplementedError();
        };
        const auto body_def = ((*definition) >> sequence)[buildBody];
        constexpr auto buildSequence = [](auto& ctx) { x3::_val(ctx) = { makeRefCount<Sequence>(removePack(x3::_attr(ctx))) }; };
        const auto sequence_def = (+expr)[buildSequence];
        constexpr auto buildConditional = [](auto& ctx) {
            auto&& attr = x3::_attr(ctx);
            auto&& elsePart = boost::fusion::at_c<2>(attr);

            x3::_val(ctx) = { makeRefCount<Conditional>(boost::fusion::at_c<0>(attr).root, boost::fusion::at_c<1>(attr).root,
                                                        elsePart.has_value() ? elsePart.value().root : Ref<Node>{}) };
        };
        const auto conditional_def = ('(' >> x3::lit("if") >> expr >> expr >> (-expr) >> ')')[buildConditional];
        constexpr auto buildAssignment = [](auto& ctx) {
            const auto& attr = x3::_attr(ctx);
            x3::_val(ctx) = { makeRefCount<Definition>(boost::fusion::at_c<0>(attr), boost::fusion::at_c<1>(attr).root, true) };
        };
        const auto assignment_def = ('(' >> x3::lit("set!") >> identifierPattern >> expr >> ')')[buildAssignment];
        constexpr auto buildCond1 = [](auto& ctx) {
            // TODO: Not implemented
            throwNotImplementedError();
        };
        const auto cond1_def = ('(' >> x3::lit("cond") >> (+condClause) >> ')')[buildCond1];
        constexpr auto buildCond2 = [](auto& ctx) {
            // TODO: Not implemented
            throwNotImplementedError();
        };
        const auto cond2_def =
            ('(' >> x3::lit("cond") >> (*condClause) >> '(' >> x3::lit("else") >> sequence >> ')' >> ')')[buildCond2];
        constexpr auto buildCase1 = [](auto& ctx) {
            // TODO: Not implemented
            throwNotImplementedError();
        };
        const auto case1_def = ('(' >> x3::lit("case") >> expr >> (+caseClause) >> ')')[buildCase1];
        constexpr auto buildCase2 = [](auto& ctx) {
            // TODO: Not implemented
            throwNotImplementedError();
        };
        const auto case2_def =
            ('(' >> x3::lit("case") >> expr >> (*caseClause) >> '(' >> x3::lit("else") >> sequence >> ")" >> ")")[buildCase2];
        constexpr auto buildCase3 = [](auto& ctx) {
            // TODO: Not implemented
            throwNotImplementedError();
        };
        const auto case3_def = ('(' >> x3::lit("case") >> expr >> (*caseClause) >> '(' >> x3::lit("else") >> x3::lit("=>") >>
                                expr >> ')' >> ')')[buildCase3];
        constexpr auto buildLogicOr = [](auto& ctx) { x3::_val(ctx) = { makeRefCount<LogicOr>(removePack(x3::_attr(ctx))) }; };
        const auto logicOr_def = ('(' >> x3::lit("or") >> (*expr) >> ')')[buildLogicOr];
        constexpr auto buildLogicAnd = [](auto& ctx) { x3::_val(ctx) = { makeRefCount<LogicAnd>(removePack(x3::_attr(ctx))) }; };
        const auto logicAnd_def = ('(' >> x3::lit("and") >> (*expr) >> ')')[buildLogicAnd];
        constexpr auto buildWhen = [](auto& ctx) {
            // TODO: Not implemented
            throwNotImplementedError();
        };
        const auto when_def = ('(' >> x3::lit("when") >> expr >> sequence >> ')')[buildWhen];
        constexpr auto buildUnless = [](auto& ctx) {
            // TODO: Not implemented
            throwNotImplementedError();
        };
        const auto unless_def = ('(' >> x3::lit("unless") >> expr >> sequence >> ')')[buildUnless];
        constexpr auto buildLet1 = [](auto& ctx) {
            // TODO: Not implemented
            throwNotImplementedError();
        };
        const auto let1_def = ('(' >> x3::lit("let") >> '(' >> *bindingSpec >> ')' >> body >> ')')[buildLet1];
        constexpr auto buildLet2 = [](auto& ctx) {
            // TODO: Not implemented
            throwNotImplementedError();
        };
        const auto let2_def = ('(' >> x3::lit("let") >> identifier >> '(' >> *bindingSpec >> ')' >> body >> ')')[buildLet2];
        constexpr auto buildLet3 = [](auto& ctx) {
            // TODO: Not implemented
            throwNotImplementedError();
        };
        const auto let3_def = ('(' >> (x3::lit("let*") | x3::lit("letrec") | x3::lit("letrec*")) >> '(' >> *bindingSpec >> ')' >>
                               body >> ')')[buildLet3];
        constexpr auto buildLet4 = [](auto& ctx) {
            // TODO: Not implemented
            throwNotImplementedError();
        };
        const auto let4_def =
            ('(' >> (x3::lit("let-values") | x3::lit("let*-values")) >> '(' >> *mvBindingSpec >> ')' >> body >> ')')[buildLet4];

        constexpr auto begin_def = '(' >> x3::lit("begin") >> sequence >> ')';
        constexpr auto buildDo = [](auto& ctx) {
            // TODO: Not implemented
            throwNotImplementedError();
        };
        const auto doStatement_def =
            ('(' >> x3::lit("do") >> '(' >> *iterSpec >> ')' >> '(' >> expr >> doResult >> ')' >> *expr >> ')')[buildDo];
        constexpr auto buildDelay = [](auto& ctx) {
            // TODO: Not implemented
            throwNotImplementedError();
        };
        const auto delay_def = ('(' >> x3::lit("delay") >> expr >> ')')[buildDelay];
        constexpr auto buildDelayForce = [](auto& ctx) {
            // TODO: Not implemented
            throwNotImplementedError();
        };
        const auto delayForce_def = ('(' >> x3::lit("delay-force") >> expr >> ')')[buildDelayForce];
        constexpr auto buildParameterize = [](auto& ctx) {
            // TODO: Not implemented
            throwNotImplementedError();
        };
        const auto parameterize_def =
            ('(' >> x3::lit("parameterize") >> '(' >> *('(' >> expr >> expr >> ')') >> ')' >> body >> ')')[buildParameterize];
        constexpr auto buildGuard = [](auto& ctx) {
            // TODO: Not implemented
            throwNotImplementedError();
        };
        const auto guard_def = ('(' >> x3::lit("guard") >> '(' >> identifier >> *condClause >> ')' >> body >> ')')[buildGuard];
        constexpr auto buildCaseLambda = [](auto& ctx) {
            // TODO: Not implemented
            throwNotImplementedError();
        };
        const auto caseLambda_def = ('(' >> x3::lit("case-lambda") >> *caseLambdaClause >> ')')[buildCaseLambda];
        constexpr auto derived_def = begin | cond1 | cond2 | case1 | case2 | case3 | logicOr | logicAnd | when | unless | let1 |
            let2 | let3 | let4 | doStatement | delay | delayForce | parameterize | guard | quasiQuotation | caseLambda;

        constexpr auto buildCondClause1 = [](auto& ctx) {
            // TODO: Not implemented
            throwNotImplementedError();
        };
        const auto condClause1_def = ('(' >> expr >> sequence >> ')')[buildCondClause1];
        constexpr auto buildCondClause2 = [](auto& ctx) {
            // TODO: Not implemented
            throwNotImplementedError();
        };
        const auto condClause2_def = ('(' >> expr >> ')')[buildCondClause2];
        constexpr auto buildCondClause3 = [](auto& ctx) {
            // TODO: Not implemented
            throwNotImplementedError();
        };
        const auto condClause3_def = ('(' >> expr >> x3::lit("=>") >> expr >> ')')[buildCondClause3];
        constexpr auto condClause_def = condClause1 | condClause2 | condClause3;
        constexpr auto buildCaseClause1 = [](auto& ctx) {
            // TODO: Not implemented
            throwNotImplementedError();
        };
        const auto caseClause1_def = (x3::lit('(') >> '(' >> *datum >> ')' >> sequence >> ')')[buildCaseClause1];
        constexpr auto buildCaseClause2 = [](auto& ctx) {
            // TODO: Not implemented
            throwNotImplementedError();
        };
        const auto caseClause2_def = (x3::lit('(') >> '(' >> *datum >> ')' >> x3::lit("=>") >> expr >> ')')[buildCaseClause2];
        constexpr auto caseClause_def = caseClause1 | caseClause2;
        constexpr auto buildBindingSpec = [](auto& ctx) {
            // TODO: Not implemented
            throwNotImplementedError();
        };
        const auto bindingSpec_def = ('(' >> identifier >> expr >> ')')[buildBindingSpec];
        constexpr auto buildMvBindingSpec = [](auto& ctx) {
            // TODO: Not implemented
            throwNotImplementedError();
        };
        const auto mvBindingSpec_def = ('(' >> formal >> expr >> ')')[buildMvBindingSpec];
        constexpr auto buildIterSpec1 = [](auto& ctx) {
            // TODO: Not implemented
            throwNotImplementedError();
        };
        const auto iterSpec1_def = ('(' >> identifier >> expr >> expr >> ')')[buildIterSpec1];
        constexpr auto buildIterSpec2 = [](auto& ctx) {
            // TODO: Not implemented
            throwNotImplementedError();
        };
        const auto iterSpec2_def = ('(' >> identifier >> expr >> ')')[buildIterSpec2];
        constexpr auto iterSpec_def = iterSpec1 | iterSpec2;
        constexpr auto buildCaseLambdaClause = [](auto& ctx) {
            // TODO: Not implemented
            throwNotImplementedError();
        };
        const auto caseLambdaClause_def = ('(' >> formal >> body >> ')')[buildCaseLambdaClause];
        constexpr auto buildDoResult = [](auto& ctx) {
            // TODO: Not implemented
            throwNotImplementedError();
        };
        const auto doResult_def = (-sequence)[buildDoResult];
        constexpr auto buildMacroUse = [](auto& ctx) {
            // TODO: Not implemented
            throwNotImplementedError();
        };
        const auto macroUse_def = ('(' >> identifier >> *datum >> ')')[buildMacroUse];
        constexpr auto buildMacroBlock = [](auto& ctx) {
            // TODO: Not implemented
            throwNotImplementedError();
        };
        const auto macroBlock_def = ('(' >> (x3::lit("let-syntax") | x3::lit("letrec-syntax")) >> '(' >> *syntaxSpec >> ')' >>
                                     body >> ')')[buildMacroBlock];
        constexpr auto buildSyntaxSpec = [](auto& ctx) {
            // TODO: Not implemented
            throwNotImplementedError();
        };
        const auto syntaxSpec_def = ('(' >> identifier >> transformerSpec >> ')')[buildSyntaxSpec];
        constexpr auto buildInclude = [](auto& ctx) {
            // TODO: Not implemented
            throwNotImplementedError();
        };
        const auto include_def = ('(' >> (x3::lit("include") | x3::lit("include-ci")) >> +string >> ')')[buildInclude];

        // QuasiQuotations
        constexpr auto buildQuasiQuotation = [](auto& ctx) {
            // TODO: Not implemented
            throwNotImplementedError();
        };
        const auto quasiQuotation_def = x3::lit("quasiQuotation_def")[buildQuasiQuotation];
        // Transformers
        constexpr auto buildTransformerSpec = [](auto& ctx) {
            // TODO: Not implemented
            throwNotImplementedError();
        };
        const auto transformerSpec_def = x3::lit("transformerSpec_def")[buildTransformerSpec];
        // Programs & Definitions
        constexpr auto buildDefinition1 = [](auto& ctx) {
            const auto& attr = x3::_attr(ctx);
            x3::_val(ctx) = { makeRefCount<Definition>(boost::fusion::at_c<0>(attr), boost::fusion::at_c<1>(attr).root, false) };
        };
        const auto definition1_def = ('(' >> x3::lit("define") >> identifierPattern >> expr >> ')')[buildDefinition1];
        constexpr auto buildDefinition2 = [](auto& ctx) {
            // TODO: Not implemented
            throwNotImplementedError();
        };
        const auto definition2_def =
            ('(' >> x3::lit("define") >> '(' >> identifier >> definitionFormal >> ')' >> body >> ')')[buildDefinition2];
        constexpr auto buildDefinition3 = [](auto& ctx) {
            // TODO: Not implemented
            throwNotImplementedError();
        };
        const auto definition3_def = ('(' >> x3::lit("define-syntax") >> identifier >> transformerSpec >> ')')[buildDefinition3];
        constexpr auto buildDefinition4 = [](auto& ctx) {
            // TODO: Not implemented
            throwNotImplementedError();
        };
        const auto definition4_def = ('(' >> x3::lit("define-values") >> formal >> body >> ')')[buildDefinition4];
        constexpr auto buildDefinition5 = [](auto& ctx) {
            // TODO: Not implemented
            throwNotImplementedError();
        };
        const auto definition5_def = ('(' >> x3::lit("define-record-type") >> identifier >> constructor >> identifier >>
                                      *fieldSpec >> ')')[buildDefinition5];
        constexpr auto buildDefinition6 = [](auto& ctx) {
            // TODO: Not implemented
            throwNotImplementedError();
        };
        const auto definition6_def = ('(' >> x3::lit("begin") >> *definition >> ')')[buildDefinition6];
        constexpr auto definition_def = definition1 | definition2 | definition3 | definition4 | definition5 | definition6;
        constexpr auto buildDefinitionFormal = [](auto& ctx) {
            // TODO: Not implemented
            throwNotImplementedError();
        };
        const auto definitionFormal_def = ((*identifier) | (*identifier >> '.' >> identifier))[buildDefinitionFormal];
        constexpr auto buildConstructor = [](auto& ctx) {
            // TODO: Not implemented
            throwNotImplementedError();
        };
        const auto constructor_def = ('(' >> identifier >> *identifier >> ')')[buildConstructor];
        constexpr auto buildFieldSpec = [](auto& ctx) {
            // TODO: Not implemented
            throwNotImplementedError();
        };
        const auto fieldSpec_def =
            (('(' >> identifier >> identifier >> ')') | ('(' >> identifier >> identifier >> identifier >> ')'))[buildFieldSpec];
        // Libraries

        BOOST_SPIRIT_DEFINE(boolean, number, vector, character, string, byteVector, identifier, expr, literal, selfEval,
                            quotation, procedureCall, lambda, formal, body, sequence, conditional, assignment, derived,
                            condClause, bindingSpec, mvBindingSpec, iterSpec, caseLambdaClause, doResult, macroUse, macroBlock,
                            syntaxSpec, include, datum, definition, caseClause, quasiQuotation, transformerSpec, simpleDatum,
                            compoundDatum, list, abbreviation, label, initial, subsequent, symbolElement, peculiarIdentifier,
                            complex, rational, floatingPoint, rationalPart)

        BOOST_SPIRIT_DEFINE(explicitSign, signSubsequent, dotSubsequent, definitionFormal, constructor, fieldSpec, labeledDatum,
                            booleanTrue, booleanFalse, integer, real, characterGraph, characterUInt, cond1, cond2, case1, case2,
                            case3, logicOr, logicAnd, when, unless, let1, let2, let3, let4, begin, doStatement, delay, delayForce,
                            parameterize, guard, caseLambda, iterSpec1, iterSpec2, definition1, definition2, definition3,
                            definition4, definition5, definition6, condClause1, condClause2, condClause3, caseClause1,
                            caseClause2, list1, list2, complexPart, identifierPattern)
    };  // namespace parser

    Ref<Node> parse(std::string_view statement) {
        auto iter = statement.cbegin();
        using Iterator = std::string_view::const_iterator;
        using ErrorHandler = x3::error_handler<Iterator>;

        std::stringstream errorMessage{ "Parse failed " };
        ErrorHandler errorHandler(iter, statement.cend(), errorMessage);

        // TODO: error handling
        const auto parserWithErrorHandling = x3::with<x3::error_handler_tag>(std::ref(errorHandler))[parser::expr];

        if(parser::ParseResult res; x3::phrase_parse(iter, statement.cend(), parserWithErrorHandling, x3::space, res)) {
            res.root->printAST(std::cout);

            if(iter == statement.end()) {
                std::cout << std::endl << std::endl;

                return std::move(res.root);
            }
            throw Error{ "not finished" };
        }

        throw Error{ errorMessage.str() };
    }
}  // namespace schemepp
