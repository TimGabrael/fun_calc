#pragma once
#include <string>
#include <unordered_map>

struct ErrorData {
    std::string info;
    bool failed;
};
struct VariableData {
    std::unordered_map<std::string, float> variables;
};

ErrorData CalculateExpression(const std::string& expr, float& output);
ErrorData ParseFunction(const std::string& func, struct ExpressionTree** output);
void DeleteExpressionTree(struct ExpressionTree* tree);

// All variables will be initialized to 0
VariableData GetVariablesInExpressionTree(struct ExpressionTree* tree);

// the raw_data is required for error information
ErrorData EvaluateExpressionTree(const std::string& raw_data, struct ExpressionTree* tree, const VariableData& variables, float& output);

ErrorData CalculateDerivative(struct ExpressionTree* tree, const std::string& diff_var, struct ExpressionTree** derivative);

std::string PrintExpressions(struct ExpressionTree* tree);


