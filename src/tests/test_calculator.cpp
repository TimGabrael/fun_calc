#include "../calculator.h"
#include "gtest/gtest.h"

//ErrorData err = ParseFunction("2*sin(x^2)^2 - -2*cos(x^2)^2 + 2*x*x - x+1", &expr_tree);
//ErrorData err = ParseFunction("2*x^2+2*x-4*sin(x*log(x))-(2*x^2+2*x-4*sin(x*log(x)))", &expr_tree);
//ErrorData err = ParseFunction("x + a - x", &expr_tree);
//ErrorData err = ParseFunction("(a+x)*(a-x)-2*x*x", &expr_tree);
//ErrorData err = ParseFunction("3*x*x-2*x*x", &expr_tree);
//ErrorData err = ParseFunction("3*x*x-2*x*x+(x*x-3*x*x)", &expr_tree);
//ErrorData err = ParseFunction("3*x*4", &expr_tree);
//ErrorData err = ParseFunction("3*x*(1*x+2)", &expr_tree);
//ErrorData err = ParseFunction("3*x-x*2", &expr_tree);
//ErrorData err = ParseFunction("x*x-x", &expr_tree);
//ErrorData err = ParseFunction("x-x*x", &expr_tree);

TEST(ParseTests, SimpleParseTest) 
{
    ExpressionTree* expr_tree = nullptr;
    ErrorData err = ParseFunction("(x-3)*(x+2)", &expr_tree);
    EXPECT_TRUE(!err.failed && expr_tree != nullptr) << err.failed;
    DeleteExpressionTree(expr_tree);
}

TEST(SimplifyTests, DistributionRule0)
{
    ExpressionTree* expr_tree = nullptr;
    ErrorData err = ParseFunction("(x-3)*(x+2)", &expr_tree);
    EXPECT_TRUE(!err.failed && expr_tree != nullptr) << err.failed;
    ExpressionTree* simplified_expr_tree = nullptr;
    CopyExpressionTree(expr_tree, &simplified_expr_tree);
    SimplifyExpressionTree(simplified_expr_tree);
    EXPECT_NE(simplified_expr_tree, nullptr);
    DeleteExpressionTree(expr_tree);
    
}


int main() {
    ::testing::InitGoogleTest();
    return RUN_ALL_TESTS();
}
