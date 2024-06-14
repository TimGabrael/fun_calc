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
    ErrorData err = ParseFunction("pi*x*x*x+cos(x^x)", &expr_tree);
    if(err.failed) {
        std::cout << err.info << std::endl;
        return 1;
    }
    ExpressionTree* derivative = nullptr;
    err = CalculateDerivative(expr_tree, "x", &derivative);
    if(err.failed) {
        std::cout << err.info << std::endl;
        return 1;
    }
    std::string expression_str = PrintExpressions(expr_tree);
    std::cout << expression_str << std::endl;
    std::string derivative_str = PrintExpressions(derivative);
    std::cout << derivative_str << std::endl;
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

