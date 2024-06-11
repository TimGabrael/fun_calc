#pragma once
#include <string>

struct ErrorData {
    std::string info;
    bool failed;
};

ErrorData CalculateExpression(const std::string& expr, float& output);

