#include <chrono>
#include <thread>
#include <iostream>
#include <sstream>
#include "calculator.h"
#include "raylib.h"

#define RAYGUI_IMPLEMENTATION
#include "raygui.h"


                                


int main() {
    InitWindow(800, 600, "calc");
    SetTargetFPS(60);
    SetExitKey(KEY_NULL);

    float output = 0.0f;
    ErrorData info = CalculateExpression("1 - 2 + 4 * 2.2 * sin(pi * cos(pi))", output);
    if(!info.failed) {
        std::cout << "output: " << output << std::endl;
    }
    else {
        std::cout << info.info << std::endl;
    }

    bool show_message_box = false;
    while(!WindowShouldClose()) {
        BeginDrawing();
        ClearBackground(DARKGRAY);

        if(GuiButton({ 24, 24, 120, 30 }, "#191#Show Message")) {
            show_message_box = true;
        }

        if(show_message_box) {
            int result = GuiMessageBox({ 85, 70, 250, 100 }, "#191#Message Box", "Hi! This is a message!", "Nice;Cool;Wow");
            if(result >= 0) {
                show_message_box = false;
            }
        } 
        else {
            static char buffer[256] = {};
            if(GuiTextBox({85, 70, 250, 100}, buffer, 255, true)) {
                float output = 0.0f;
                ErrorData err = CalculateExpression(buffer, output);
                if(err.failed) {
                    std::cout << err.info << std::endl;
                }
                std::cout << "output: " << output << std::endl;
            }
        }

        EndDrawing();
    }

    CloseWindow();
    return 0;
}

