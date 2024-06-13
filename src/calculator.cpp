#include "calculator.h"
#include <vector>
#include <iostream>
#define _USE_MATH_DEFINES
#include <math.h>
#include "util.h"


struct Range {
    size_t start;       // raw start index
    size_t end;         // raw end index

    size_t line_start;  // index of the first character in the line
    size_t line;        // line number

    std::string HighlightPosition(const std::string& data) const {
        if(this->start < data.size() && this->end <= data.size()) {
            size_t line_end = data.size();
            for(size_t i = this->line_start; i < data.size(); ++i) {
                if(data[i] == '\n') {
                    line_end = i;
                    break;
                }
            }
            std::string pos = this->GetPositionData() + ": ";
            std::string out = pos + data.substr(this->line_start, line_end - this->line_start) + "\n"; 
            for(size_t i = 0; i < pos.size() + (start - line_start); ++i) {
                out.append(" ");
            }
            for(size_t i = 0; i < (end - start); ++i) {
                out.append("^");
            }
            return out;
        }
        // return nothing if the range is not valid
        return "";
    }
    std::string GetPositionData() const {
        return "[" + std::to_string(this->line + 1) + "," + std::to_string(this->start - this->line_start + 1) + "]";
    }
};

struct Token {
    enum Type {
        NUMBER,
        VARIABLE,
        COMMA,
        OPEN_BRACKET,
        CLOSE_BRACKET,
        OPERATOR_ADD,
        OPERATOR_SUB,
        OPERATOR_MUL,
        OPERATOR_DIV,
        // unary operator version of the sub operator
        OPERATOR_NEG, 
        OPERATOR_POW, 
        END,
        INVALID,
    };

    std::string val;
    Type type;
    Range range;
    void PrintContent() const {
        std::cout << "token[" << type << "]: " << val << "\n(start: " << range.start << ", end: " << range.end << ", line_start: " << range.line_start << ", line: " << range.line << ")" << std::endl;
    }
};
struct OperatorInfo {
    Token::Type token_type;
    size_t precedence;
};
struct FunctionInfo {
    size_t param_count;
    void(*function_pointer)();
};


struct ValueExpression {
    float val;
};
struct UnaryExpression {
    enum Operator {
        NEGATIVE,
        INVALID,
    };
    struct Expression* expr;
    Operator op;
};
struct BinaryExpression {
    enum Operator {
        ADD,
        SUB,
        MUL,
        DIV,
        POW,
        INVALID,
    };
    struct Expression* left;
    struct Expression* right;
    Operator op;
};
struct VariableExpression {
    char* variable_name;
    size_t variable_name_len;
};
struct CallExpression {
    char* function_name;
    struct Expression** params;
    size_t function_name_len;
    size_t params_count;
};
struct Expression {
    enum Type {
        VALUE,
        UNARY,
        BINARY,
        CALL,
        VARIABLE, 
        INVALID,
    };
    void FreeExpression() {
        if(this->type == Type::CALL) {
            if(this->call.function_name) {
                delete[] this->call.function_name;
                this->call.function_name = nullptr;
            }
            if(this->call.params) {
                delete[] this->call.params;
                this->call.params = nullptr;
            }
            this->type = Type::INVALID;
        }
        if(this->type == Type::VARIABLE) {
            if(this->variable.variable_name) {
                delete[] this->variable.variable_name;
                this->variable.variable_name = nullptr;
            }
        }
    }
    Expression() {
        memset(this, 0, sizeof(*this));
    }
    Expression(const Expression& expr) {
        memcpy(this, &expr, sizeof(Expression));
    }
    ~Expression() {
        this->FreeExpression();
    }
    Expression(Expression&& expr) {
        this->FreeExpression();
        memcpy(this, &expr, sizeof(*this));
        memset(&expr, 0, sizeof(*this));
    }
    union {
        ValueExpression value;
        UnaryExpression unary;
        BinaryExpression binary;
        CallExpression call;
        VariableExpression variable;
    };
    Type type;
    Range range;
};
struct ExpressionTree {
    std::vector<Expression> expression_allocator;
    Expression* root_expression;
};

struct InternalErrorInfo {
    std::string info;
    Range range;
    bool failed;
};



static constexpr OperatorInfo OPERATORS[] = {
    {Token::Type::OPERATOR_ADD, 1},
    {Token::Type::OPERATOR_SUB, 2},
    {Token::Type::OPERATOR_MUL, 3},
    {Token::Type::OPERATOR_DIV, 3},
    {Token::Type::OPERATOR_NEG, 5},
    {Token::Type::OPERATOR_POW, 4},

    // the open bracket is handled like a operator that cancels 
    // out the precedences of all operators that came before it
    {Token::Type::OPEN_BRACKET, 0},
};
static const std::unordered_map<std::string, FunctionInfo> FUNCTIONS = {
    {"sin", {1, reinterpret_cast<void(*)()>(sinf)}},
    {"cos", {1, reinterpret_cast<void(*)()>(cosf)}},
    {"tan", {1, reinterpret_cast<void(*)()>(tanf)}},
    {"sqrt", {1, reinterpret_cast<void(*)()>(sqrtf)}},
    {"pow", {2, reinterpret_cast<void(*)()>(powf)}},
    {"mod", {2, reinterpret_cast<void(*)()>(fmodf)}},
};
static const std::unordered_map<std::string, float> CONSTANTS = {
    {"pi", {static_cast<float>(M_PI)}},
    {"e", {static_cast<float>(M_E)}},
};

static bool IsBinaryOperator(Token::Type type) {
    return type == Token::Type::OPERATOR_ADD || 
        type == Token::Type::OPERATOR_SUB || 
        type == Token::Type::OPERATOR_MUL || 
        type == Token::Type::OPERATOR_POW || 
        type == Token::Type::OPERATOR_DIV;
}
static bool IsUnaryOperator(Token::Type type) {
    return type == Token::Type::OPERATOR_NEG;
}
static bool IsBracket(Token::Type type) {
    return type == Token::Type::CLOSE_BRACKET ||
        type == Token::Type::OPEN_BRACKET;
}


static ErrorData lex(const std::string& expr, std::vector<Token>& output) {
    ErrorData err = {};
    Token current_token = {std::string(), Token::INVALID, {}};
    auto add_token_with_cond = [&output, &current_token](Token::Type ignore_type) {
        if(current_token.type != Token::INVALID && current_token.type != ignore_type) {
            output.emplace_back(std::move(current_token));
            current_token.type = Token::INVALID;
            current_token.range.start = current_token.range.end;
        }
    };
    for(auto& c : expr) {
        if(std::isdigit(c) || ((current_token.type == Token::INVALID || current_token.type == Token::NUMBER) && c == '.')) {
            add_token_with_cond(Token::NUMBER);
            if(c == '.') {
                if(current_token.val.find('.') != SIZE_MAX) {
                    Range err_range = current_token.range;
                    err_range.start = err_range.end;
                    err_range.end += 1;
                    std::string err_pos = err_range.HighlightPosition(expr);

                    err.failed = true;
                    err.info = std::string("Lexer Error: Number has more than 1 decimal point in it\n") + err_pos;

                    return err;
                }
            }
            current_token.val.push_back(c);
            current_token.range.end += 1;
            current_token.type = Token::NUMBER;
        }
        else if(std::isspace(c)) {
            add_token_with_cond(Token::INVALID);
            current_token.range.start += 1;
            current_token.range.end += 1;
            if(c == '\n') {
                current_token.range.line_start = current_token.range.end;
                current_token.range.line += 1;
            }
        }
        else if(std::isprint(c)) {
            add_token_with_cond(Token::VARIABLE);
            if(c == '+') {
                add_token_with_cond(Token::INVALID);
                current_token.val = c;
                current_token.range.end += 1;
                current_token.type = Token::OPERATOR_ADD;
                add_token_with_cond(Token::INVALID);
            }
            else if(c == '-') {
                add_token_with_cond(Token::INVALID);
                Token::Type tok_type = Token::Type::OPERATOR_SUB;
                if(!output.empty()) {
                    Token::Type last_type = (output.end()-1)->type;
                    if(IsBinaryOperator(last_type) || IsUnaryOperator(last_type) || last_type == Token::Type::OPEN_BRACKET) {
                        tok_type = Token::Type::OPERATOR_NEG;
                    }
                }
                else {
                    tok_type = Token::Type::OPERATOR_NEG;
                }
                current_token.val = c;
                current_token.range.end += 1;
                current_token.type = tok_type;
                add_token_with_cond(Token::INVALID);
            }
            else if(c == '*') {
                add_token_with_cond(Token::INVALID);
                current_token.val = c;
                current_token.range.end += 1;
                current_token.type = Token::OPERATOR_MUL;
                add_token_with_cond(Token::INVALID);
            }
            else if(c == '/') {
                add_token_with_cond(Token::INVALID);
                current_token.val = c;
                current_token.range.end += 1;
                current_token.type = Token::OPERATOR_DIV;
                add_token_with_cond(Token::INVALID);
            }
            else if(c == '^') {
                add_token_with_cond(Token::INVALID);
                current_token.val = c;
                current_token.range.end += 1;
                current_token.type = Token::OPERATOR_POW;
                add_token_with_cond(Token::INVALID);
            }
            else if(c == '(') {
                add_token_with_cond(Token::INVALID);
                current_token.val = c;
                current_token.range.end += 1;
                current_token.type = Token::OPEN_BRACKET;
                add_token_with_cond(Token::INVALID);
            }
            else if(c == ')') {
                add_token_with_cond(Token::INVALID);
                current_token.val = c;
                current_token.range.end += 1;
                current_token.type = Token::CLOSE_BRACKET;
                add_token_with_cond(Token::INVALID);
            }
            else if(c == ',') {
                add_token_with_cond(Token::INVALID);
                current_token.val = c;
                current_token.range.end += 1;
                current_token.type = Token::COMMA;
                add_token_with_cond(Token::INVALID);
            }
            else {
                current_token.val.push_back(c);
                current_token.range.end += 1;
                current_token.type = Token::VARIABLE;
            }
        }
        else {
            Range err_range = current_token.range;
            err_range.start = err_range.end;
            err_range.end += 1;
            std::string err_pos = err_range.HighlightPosition(expr);

            err.failed = true;
            err.info = std::string("Lexer Error Invalid Token: ") + c + std::string("\n") + err_pos;
            return err;
        }
    }
    add_token_with_cond(Token::INVALID);
    current_token.val = "";
    current_token.type = Token::END;
    add_token_with_cond(Token::INVALID);
    return err;
}
static size_t GetOperatorPrecedence(Token::Type type) {
    for(size_t i = 0; i < ARRSIZE(OPERATORS); ++i) {
        if(OPERATORS[i].token_type == type) {
            return OPERATORS[i].precedence;
        }
    }
    return static_cast<size_t>(-1);
}
static BinaryExpression::Operator TokenToBinaryOperator(Token::Type type) {
    if(type == Token::Type::OPERATOR_ADD) {
        return BinaryExpression::Operator::ADD;
    }
    else if(type == Token::Type::OPERATOR_SUB) {
        return BinaryExpression::Operator::SUB;
    }
    else if(type == Token::Type::OPERATOR_MUL) {
        return BinaryExpression::Operator::MUL;
    }
    else if(type == Token::Type::OPERATOR_DIV) {
        return BinaryExpression::Operator::DIV;
    }
    else if(type == Token::Type::OPERATOR_POW) {
        return BinaryExpression::Operator::POW;
    }
    return BinaryExpression::Operator::INVALID;
}
static UnaryExpression::Operator TokenToUnaryOperator(Token::Type type) {
    if(type == Token::Type::OPERATOR_NEG) {
        return UnaryExpression::Operator::NEGATIVE;
    }
    return UnaryExpression::Operator::INVALID;
}


static InternalErrorInfo EvaluateExpression(Expression* expr, const VariableData* variables, float& output) {
    InternalErrorInfo err_info = {};
    if(expr->type == Expression::Type::BINARY) {
        float left = 0.0f;
        float right = 0.0f;
        err_info = EvaluateExpression(expr->binary.left, variables, left);
        if(err_info.failed) {
            return err_info;
        }
        err_info = EvaluateExpression(expr->binary.right, variables, right);
        if(err_info.failed) {
            return err_info;
        }

        float calc = 0.0f;
        switch(expr->binary.op) {
            case BinaryExpression::Operator::ADD:
                calc = left + right;
                break;
            case BinaryExpression::Operator::SUB:
                calc = left - right;
                break;
            case BinaryExpression::Operator::MUL:
                calc = left * right;
                break;
            case BinaryExpression::Operator::DIV:
                calc = left / right;
                break;
            case BinaryExpression::Operator::POW:
                calc = powf(left, right);
                break;

            default:
                break;
        };
        output = calc;
        return err_info;
    }
    else if(expr->type == Expression::Type::UNARY) {
        float value = 0.0f;
        err_info = EvaluateExpression(expr->unary.expr, variables, value);
        if(err_info.failed) {
            return err_info;
        }
        switch(expr->unary.op) {
            case UnaryExpression::Operator::NEGATIVE:
                value = -value;
                break;
            default:
                break;
        }
        output = value;
        return err_info;
    }
    else if(expr->type == Expression::Type::VALUE) {
        output = expr->value.val;
        return err_info;
    }
    else if(expr->type == Expression::Type::CALL) {
        std::string function_name(expr->call.function_name, expr->call.function_name_len);
        auto fn = FUNCTIONS.find(function_name);
        if(fn != FUNCTIONS.end()) {
            if(expr->call.params_count == fn->second.param_count) {
                std::vector<float> parameters;
                for(size_t i = 0; i < expr->call.params_count; ++i) {
                    float param_val = 0.0f;
                    err_info = EvaluateExpression(expr->call.params[i], variables, param_val);
                    if(err_info.failed) {
                        return err_info;
                    }
                    parameters.push_back(param_val);
                }
                CALL_FUNCTION_MACRO(fn->second.function_pointer, parameters, expr->call.params_count, output)
                else {
                    err_info.info = "Evaluation Error: so far there are no functions with multiple parameters... what are you doing\n";
                    err_info.range = expr->range;
                    err_info.failed = true;
                }
                return err_info;
            }
            else {
                err_info.info = "Evaluation Error: Parameter mismatch expected: " + std::to_string(fn->second.param_count) + " got: " + std::to_string(expr->call.params_count) + "\n";
                err_info.range = expr->range;
                err_info.failed = true;
                return err_info;
            }
        }
        else {
            err_info.info = "Evaluation Error: function not found\n";
            err_info.range = expr->range;
            err_info.failed = true;
            return err_info;
        }
    }
    else if(expr->type == Expression::Type::VARIABLE) {
        std::string var_name(expr->variable.variable_name, expr->variable.variable_name_len);
        if(variables) {
            const auto& var = variables->variables.find(var_name);
            if(var != variables->variables.end()) {
                output = var->second;
                return err_info;
            }
        }
        err_info.range = expr->range;
        err_info.info = "Evaluation Error: variable not found\n";
        err_info.failed = true;
        return err_info;
    }
    err_info.info = "Evaluation Error: internel error unkown expression found in expression tree\n";
    err_info.range = expr->range;
    err_info.failed = true;
    return err_info;
}

static InternalErrorInfo CreateExpressionTree(ExpressionTree& calc_data, Token* tokens, size_t count) {
    InternalErrorInfo err_info = {};

    std::vector<Expression*> solving_stack;
    solving_stack.reserve(count);

    for(size_t i = 0; i < count; ++i) {
        if(IsBinaryOperator(tokens[i].type)) {
            Expression binary = {};
            if(solving_stack.size() < 2) {
                err_info.info = "Parser Error: binary token requires 2 arguments\n";
                err_info.range = tokens[i].range;
                err_info.failed = true;
                return err_info;
            }
            Expression* right = solving_stack.at((solving_stack.size()-1));
            Expression* left = solving_stack.at((solving_stack.size()-2));
            solving_stack.pop_back();
            solving_stack.pop_back();
            binary.range = left->range;
            binary.range.end = right->range.end;

            binary.type = Expression::Type::BINARY;
            binary.binary.left = left;
            binary.binary.right = right;
            binary.binary.op = TokenToBinaryOperator(tokens[i].type);
            calc_data.expression_allocator.push_back(binary);
            solving_stack.push_back(&calc_data.expression_allocator.at(calc_data.expression_allocator.size()-1));
        }
        else if(IsUnaryOperator(tokens[i].type)) {
            Expression unary = {};
            if(solving_stack.size() < 1) {
                err_info.info = "Parser Error: unary token requires 1 argument\n";
                err_info.range = tokens[i].range;
                err_info.failed = true;
                return err_info;
            }
            Expression* expr = solving_stack.at((solving_stack.size()-1));
            solving_stack.pop_back();
            unary.type = Expression::Type::UNARY;
            unary.unary.expr = expr;
            unary.unary.op = TokenToUnaryOperator(tokens[i].type);

            unary.range = expr->range;
            unary.range.start = tokens[i].range.start;


            calc_data.expression_allocator.push_back(unary);
            solving_stack.push_back(&calc_data.expression_allocator.at(calc_data.expression_allocator.size()-1));
        }
        else if(tokens[i].type == Token::Type::VARIABLE) {
            if((i+2) < count && tokens[i+1].type == Token::Type::OPEN_BRACKET) {
                // function call
                Expression call = {};
                call.range = tokens[i].range;

                call.type = Expression::Type::CALL;
                std::vector<Expression*> params;
                call.call.function_name = new char[tokens[i].val.size()];
                call.call.function_name_len = tokens[i].val.size();
                memcpy(call.call.function_name, tokens[i].val.data(), tokens[i].val.size());

                size_t current_expr_start = i + 2;
                size_t bracket_count = 1;
                for(size_t j = i + 2; j < count; j++) {
                    if(tokens[j].type == Token::Type::COMMA) {
                        if(bracket_count == 1) {
                            if(j - current_expr_start == 0) {
                                // ERROR: comma requires some input
                                err_info.info = "Parser Error: invalid function call, no data seperated by comma\n";
                                err_info.range = tokens[j].range;
                                err_info.failed = true;
                                return err_info;
                            }
                            err_info = CreateExpressionTree(calc_data, tokens + current_expr_start, j - current_expr_start);
                            if(err_info.failed) {
                                return err_info;
                            }
                            params.push_back(calc_data.root_expression);
                            current_expr_start = j + 1;
                        }
                    }
                    else if(tokens[j].type == Token::Type::OPEN_BRACKET) {
                        bracket_count += 1;
                    }
                    else if(tokens[j].type == Token::Type::CLOSE_BRACKET) {
                        bracket_count -= 1;
                        if(bracket_count == 0) {
                            size_t diff = j - current_expr_start;
                            if(diff == 0 && (i + 2 != current_expr_start)) {
                                // ERROR: no data in the function call, and there was at least one comma preceding
                                // otherwise this behaviour would be perfectly legal
                                err_info.info = "Parser Error: invalid function call, no data seperated by comma\n";
                                err_info.range = tokens[j].range;
                                err_info.failed = true;
                                return err_info;
                            }
                            if(diff > 0) {
                                err_info = CreateExpressionTree(calc_data, tokens + current_expr_start, j - current_expr_start);
                                if(err_info.failed) {
                                    return err_info;
                                }
                                params.push_back(calc_data.root_expression);
                            }
                            i = j;

                            call.range.end = tokens[j-1].range.end;
                            break;
                        }
                    }
                }
                call.call.params = new Expression*[params.size()];
                call.call.params_count = params.size();
                for(size_t j = 0; j < params.size(); ++j) {
                    call.call.params[j] = params[j];
                }

                // IMPORTANT!: this needs to be moved in, otherwise the internal data will be deleted by the destructor
                calc_data.expression_allocator.emplace_back(std::move(call));
                solving_stack.push_back(&calc_data.expression_allocator.at(calc_data.expression_allocator.size() - 1));
            }
            else {
                auto const& c = CONSTANTS.find(tokens[i].val);
                float val = 0.0f;
                Expression expr = {};
                expr.range = tokens[i].range;
                if(c != CONSTANTS.end()) {
                    val = c->second;
                    expr.type = Expression::Type::VALUE;
                    expr.value.val = val;
                }
                else {
                    expr.type = Expression::Type::VARIABLE;
                    expr.variable.variable_name = new char[tokens[i].val.size()];
                    expr.variable.variable_name_len = tokens[i].val.size();
                    memcpy(expr.variable.variable_name, tokens[i].val.data(), tokens[i].val.size());
                }

                // IMPORTANT!: this needs to be moved in, otherwise the internal data will be deleted by the destructor
                calc_data.expression_allocator.emplace_back(std::move(expr));
                solving_stack.push_back(&calc_data.expression_allocator.at(calc_data.expression_allocator.size() - 1));
            }
        }
        else {
            Expression value = {};
            value.type = Expression::Type::VALUE;
            value.value.val = std::stof(tokens[i].val);
            value.range = tokens[i].range;

            calc_data.expression_allocator.push_back(value);
            solving_stack.push_back(&calc_data.expression_allocator.at(calc_data.expression_allocator.size() - 1));
        }
    }

    if(solving_stack.size() != 1) {
        Expression* last = *(solving_stack.end() - 1);
        err_info.info = "Parser Error: missing operator\n";
        err_info.range = last->range;
        err_info.failed = true;
        return err_info;
    }
    calc_data.root_expression = solving_stack.at(0);
    return err_info;
}
// reorder the tokens: (shuntin yard algorithm)
// into reverse polish notation
InternalErrorInfo SortTokens(std::vector<Token>& sorted, Token* tokens, size_t count) {
    InternalErrorInfo err_info = {};
    sorted.reserve(count);

    std::vector<Token> operator_stack;
    operator_stack.reserve(count);
    auto last_operator_precedence = [&operator_stack]() {
        if(operator_stack.empty()) {
            return 0uLL;
        }
        return GetOperatorPrecedence((operator_stack.end()-1)->type);
    };
    // flush until the first open bracket is hit
    auto flush_stack = [&operator_stack, &sorted]() {
        while(!operator_stack.empty()) {
            Token last = std::move(*(operator_stack.end() - 1));
            operator_stack.pop_back();
            if(last.type == Token::Type::OPEN_BRACKET) {
                break;
            }
            else {
                sorted.push_back(std::move(last));
            }
        }
    };
    for(size_t i = 0; i < count; ++i) {

        if(IsBinaryOperator(tokens[i].type) || IsUnaryOperator(tokens[i].type)) {
            const size_t last_operator = last_operator_precedence();
            const size_t precedence = GetOperatorPrecedence(tokens[i].type);
            if(last_operator > precedence) {
                flush_stack();
                operator_stack.push_back(tokens[i]);
            }
            else {
                operator_stack.push_back(tokens[i]);
            }
        }
        else if(IsBracket(tokens[i].type)) {
            if(tokens[i].type == Token::Type::OPEN_BRACKET) {
                if(i > 0 && tokens[i-1].type == Token::Type::VARIABLE) {
                    // function call
                    sorted.push_back(tokens[i]);
                    size_t bracket_count = 1;
                    size_t end_bracket_idx = 0;
                    size_t cur_param_start = i + 1;
                    for(size_t j = i + 1; j < count; ++j) {
                        if(tokens[j].type == Token::Type::OPEN_BRACKET) {
                            bracket_count += 1;
                        }
                        else if(tokens[j].type == Token::Type::CLOSE_BRACKET) {
                            bracket_count -= 1;
                            if(bracket_count == 0) {
                                end_bracket_idx = j;
                                break;
                            }
                        }
                        else if(tokens[j].type == Token::Type::COMMA) {
                            if(bracket_count == 1) {
                                std::vector<Token> param;
                                err_info = SortTokens(param, tokens + cur_param_start, j - cur_param_start + 1);
                                if(err_info.failed) {
                                    return err_info;
                                }
                                for(auto& t : param) {
                                    sorted.push_back(t);
                                }
                                sorted.push_back(tokens[j]);
                                cur_param_start = j + 1;
                            }
                        }
                    }
                    if(end_bracket_idx == 0) {
                        err_info.info = "Parser Error: function call is missing closing parentheses\n";
                        err_info.range = tokens[i].range;
                        err_info.failed = true;
                        return err_info;
                    }
                    std::vector<Token> param;
                    err_info = SortTokens(param, tokens + cur_param_start, end_bracket_idx - cur_param_start);
                    if(err_info.failed) {
                        return err_info;
                    }
                    for(auto& t : param) {
                        sorted.push_back(t);
                    }
                    sorted.push_back(tokens[end_bracket_idx]);
                    i = end_bracket_idx;
                }
                else {
                    size_t bracket_count = 1;
                    for(size_t j = i + 1; j < count; ++j) {
                        if(tokens[j].type == Token::Type::CLOSE_BRACKET) {
                            bracket_count -= 1;
                            if(bracket_count == 0) {
                                break;
                            }
                        }
                        else if(tokens[j].type == Token::Type::OPEN_BRACKET) {
                            bracket_count += 1;
                        }
                    }
                    if(bracket_count != 0) {
                        err_info.info = "Parser Error: missing closing parentheses\n";
                        err_info.range = tokens[i].range;
                        err_info.failed = true;
                        return err_info;
                    }
                    operator_stack.push_back(tokens[i]);
                }
            }
            else {
                flush_stack();
            }
        }
        else if(tokens[i].type == Token::Type::VARIABLE || tokens[i].type == Token::Type::NUMBER) {
            sorted.push_back(tokens[i]);
        }
        else if(tokens[i].type == Token::Type::END) {
            break;
        }
        else if(tokens[i].type == Token::Type::INVALID) { 
            // this should not happen,
            // just skip it for now
        }
    }
    flush_stack();


    return err_info;
}
static InternalErrorInfo ParseExpressionAndEvaluate(Token* tokens, size_t count, float& output) {
    std::vector<Token> sorted;
    InternalErrorInfo err_info = SortTokens(sorted, tokens, count);
    if(err_info.failed) {
        return err_info;
    }
    ExpressionTree calc_data = {};
    calc_data.expression_allocator.reserve(count);
    err_info = CreateExpressionTree(calc_data, sorted.data(), sorted.size());
    if(err_info.failed) {
        return err_info;
    }
    if(calc_data.root_expression) {
        err_info = EvaluateExpression(calc_data.root_expression, nullptr, output);
    }
    return err_info;
}
static InternalErrorInfo ParseExpression(Token* tokens, size_t count, struct ExpressionTree** output) {
    std::vector<Token> sorted;
    InternalErrorInfo err_info = SortTokens(sorted, tokens, count);
    if(err_info.failed) {
        return err_info;
    }
    ExpressionTree* tree = new ExpressionTree{};
    tree->expression_allocator.reserve(count);
    err_info = CreateExpressionTree(*tree, sorted.data(), sorted.size());
    if(err_info.failed) {
        delete tree;
        return err_info;
    }
    *output = tree;
    return err_info;
}
static void FillVariableInformation(Expression* expr, VariableData& out) {
    switch(expr->type) {
        case Expression::Type::VALUE:
            break;
        case Expression::Type::UNARY:
            FillVariableInformation(expr->unary.expr, out);
            break;
        case Expression::Type::BINARY:
            FillVariableInformation(expr->binary.left, out);
            FillVariableInformation(expr->binary.right, out);
            break;
        case Expression::Type::CALL:
            for(size_t i = 0; i < expr->call.params_count; ++i) {
                FillVariableInformation(expr->call.params[i], out);
            }
            break;
        case Expression::Type::VARIABLE:
            {
                std::string var_name(expr->variable.variable_name, expr->variable.variable_name_len);
                out.variables[var_name] = 0.0f;
            }
            break;

        default:
            break;
    };
}


ErrorData CalculateExpression(const std::string& expr, float& output) {
    ErrorData calc_err{};
    calc_err.failed = false;
    if(expr.size() == 0) {
        calc_err.failed = true;
        calc_err.info = "No input provided";
        return calc_err;
    }
    std::vector<Token> tokens;
    calc_err = lex(expr, tokens);
    if(calc_err.failed) {
        return calc_err;
    }
    InternalErrorInfo err_info = ParseExpressionAndEvaluate(tokens.data(), tokens.size(), output);
    if(err_info.failed) {
        calc_err.info = err_info.info + err_info.range.HighlightPosition(expr);
        calc_err.failed = true;
        return calc_err;
    }
    return calc_err;
}
ErrorData ParseFunction(const std::string& func, struct ExpressionTree** output) {
    ErrorData err_data{};
    err_data.failed = false;
    if(func.size() == 0) {
        err_data.failed = true;
        err_data.info = "No input provided";
        return err_data;
    }
    std::vector<Token> tokens;
    err_data = lex(func, tokens);
    if(!output) {
        err_data.failed = true;
        err_data.info = "No output provided";
        return err_data;
    }
    InternalErrorInfo err_info = ParseExpression(tokens.data(), tokens.size(), output);
    if(err_info.failed) {
        err_data.info = err_info.info + err_info.range.HighlightPosition(func);
        err_data.failed = true;
        return err_data;
    }
    return err_data;
}
void DeleteExpressionTree(struct ExpressionTree* tree) {
    if(tree) {
        delete tree;
    }
}
VariableData GetVariablesInExpressionTree(struct ExpressionTree* tree) {
    VariableData output = {};
    if(tree->root_expression) {
        FillVariableInformation(tree->root_expression, output);
    }
    return output;
}
ErrorData EvaluateExpressionTree(const std::string& raw_data, struct ExpressionTree* tree, const VariableData& variables, float& output) {
    ErrorData err_data{};
    if(tree->root_expression) {
        InternalErrorInfo err_info = EvaluateExpression(tree->root_expression, &variables, output);
        if(err_info.failed) {
            err_data.info = err_info.info + err_info.range.HighlightPosition(raw_data);
            err_data.failed = true;
            return err_data;
        }
    }
    return err_data;
}


