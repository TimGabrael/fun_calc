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


int main() {
    float output = 0.0f;
    ErrorData info = CalculateExpression("((1)*2+1)", output);
    if(!info.failed) {
        std::cout << "output: " << output << std::endl;
    }
    else {
        std::cout << info.info << std::endl;
    }

    InitWindow(800, 600, "calc");
    SetTargetFPS(60);
    SetExitKey(KEY_NULL);

    rlImGuiSetup(true);
    ImGui::GetIO().ConfigWindowsMoveFromTitleBarOnly = true;


    while(!WindowShouldClose()) {
        BeginDrawing();
        ClearBackground(DARKGRAY);

        rlImGuiBegin();

        static constexpr size_t buf_size = 256;
        static char buffer[buf_size] = {};
        static ErrorData err = {};
        static float result = 0.0f;
        ImGui::SetKeyboardFocusHere();
        if(ImGui::InputText("function", buffer, buf_size - 1, ImGuiInputTextFlags_::ImGuiInputTextFlags_EnterReturnsTrue)) {
            bool invalid = false;
            for(size_t i = 0; i < 255; ++i) {
                if(buffer[i] < 0) {
                    invalid = true;
                    break;
                }
            }
            if(!invalid) {
                err = CalculateExpression(buffer, result);
                if(!err.failed) {
                    std::cout << "output: " << result << std::endl;
                }
            }
            else {
                err.info = "contains invalid character";
                err.failed = true;
            }
        }
        if(err.failed) {
            ImGui::Text("%s", err.info.c_str());
        }
        else {
            ImGui::Text("%s", std::to_string(result).c_str());
        }

        rlImGuiEnd();
        EndDrawing();
    }

    CloseWindow();
    return 0;
}

