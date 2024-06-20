#include <chrono>
#include <thread>
#include <iostream>
#include <sstream>
#include "calculator.h"
#include "raylib.h"

#include "imgui.h"
#include "imgui_internal.h"
#include "rlImGui.h"
#include "rlImGuiColors.h"
#include "imgui_impl_raylib.h"

#include "graph.h"


int main() {

    // test derivatives
    ExpressionTree* expr_tree = nullptr;
    //ErrorData err = ParseFunction("2*sin(x^2)^2 - -2*cos(x^2)^2 + 2*x*x - x+1", &expr_tree);
    //ErrorData err = ParseFunction("2*x^2+2*x-4*sin(x*log(x))-(2*x^2+2*x-4*sin(x*log(x)))", &expr_tree);
    //ErrorData err = ParseFunction("x + a - x", &expr_tree);
    //ErrorData err = ParseFunction("(a+x)*(a-x)-2*x*x", &expr_tree);
    //ErrorData err = ParseFunction("3*x*x-2*x*x", &expr_tree);
    //ErrorData err = ParseFunction("3*x*x-2*x*x+(x*x-3*x*x)", &expr_tree);
    //ErrorData err = ParseFunction("3*x*4", &expr_tree);
    ErrorData err = ParseFunction("3*x*(1+2)", &expr_tree);
    //ErrorData err = ParseFunction("3*x-x*2", &expr_tree);
    //ErrorData err = ParseFunction("x*x-x", &expr_tree);
    //ErrorData err = ParseFunction("x-x*x", &expr_tree);
    if(err.failed) {
        std::cout << err.info << std::endl;
        return 1;
    }
    ExpressionTree* simplified_expr_tree = nullptr;
    CopyExpressionTree(expr_tree, &simplified_expr_tree);
    SimplifyExpressionTree(simplified_expr_tree);

    //ExpressionTree* derivative = nullptr;
    //err = CalculateDerivative(expr_tree, "x", &derivative);
    //if(err.failed) {
    //    std::cout << err.info << std::endl;
    //    return 1;
    //}

    std::string expression_str = PrintExpressions(expr_tree);
    std::cout << expression_str << std::endl;
    std::string simpliefied_expression_str = PrintExpressions(simplified_expr_tree);
    std::cout << simpliefied_expression_str << std::endl;


    //std::string derivative_str = PrintExpressions(derivative);
    //std::cout << derivative_str << std::endl;
    //std::string simplified_derivative_str = PrintExpressions(simplified_derivative_expr_tree);
    //std::cout << simplified_derivative_str << std::endl;

    VariableData variables;
    variables.variables["x"] = 3.0f;
    variables.variables["a"] = 5.0f;
    float output = 0.0f;
    EvaluateExpressionTree("", expr_tree, variables, output);
    std::cout << "f1(x) = " << output << std::endl;
    EvaluateExpressionTree("", simplified_expr_tree, variables, output);
    std::cout << "f2(x) = " << output << std::endl;
    //EvaluateExpressionTree("", derivative, variables, output);
    //std::cout << "f(x) = " << output << std::endl;
    return 0;

    

    SetConfigFlags(FLAG_WINDOW_RESIZABLE | FLAG_WINDOW_HIGHDPI);
    InitWindow(800, 600, "calc");
    SetTargetFPS(60);
    SetExitKey(KEY_NULL);

    ImGui::CreateContext();
    ImGuiIO& io = ImGui::GetIO(); (void)io;
    io.ConfigFlags |= ImGuiConfigFlags_NavEnableKeyboard;
    io.ConfigFlags |= ImGuiConfigFlags_NavEnableGamepad;
    io.ConfigWindowsMoveFromTitleBarOnly = true;

    ImGui::StyleColorsDark();

    ImGui_ImplRaylib_Init();

    io.Fonts->AddFontDefault();

    ImFont* font = io.Fonts->AddFontFromFileTTF("../assets/MartianMono-Regular.ttf", 24.0f, NULL, io.Fonts->GetGlyphRangesDefault());
    if(font) {
        io.FontDefault = font;
    }
    Imgui_ImplRaylib_BuildFontAtlas();

    NodeEditor editor;
    while(!WindowShouldClose()) {
        ImGui_ImplRaylib_ProcessEvents();
        ImGui_ImplRaylib_NewFrame();
        ImGui::NewFrame();

        Vector2 win_size = {static_cast<float>(GetScreenWidth()), static_cast<float>(GetScreenHeight())};

        ImGui::Begin("##editor", NULL, ImGuiWindowFlags_::ImGuiWindowFlags_NoNav | ImGuiWindowFlags_::ImGuiWindowFlags_NoMove | ImGuiWindowFlags_::ImGuiWindowFlags_NoResize | ImGuiWindowFlags_::ImGuiWindowFlags_NoTitleBar);

        editor.Draw(win_size);

        ImGui::End();


        ImGui::Render();

        BeginDrawing();
        ClearBackground(DARKGRAY);

        ImGui_ImplRaylib_RenderDrawData(ImGui::GetDrawData());

        EndDrawing();
    }

    ImGui_ImplRaylib_Shutdown();
    ImGui::DestroyContext();
    CloseWindow();
    return 0;
}

