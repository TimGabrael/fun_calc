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
        editor.Draw(win_size);


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

