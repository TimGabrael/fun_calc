#include <chrono>
#include <thread>
#include <iostream>
#include <sstream>
#include "calculator.h"
#include "raylib.h"

#define RAYGUI_IMPLEMENTATION
#include "raygui.h"


                                


int main() {
    float output = 0.0f;
    ErrorData info = CalculateExpression("((1)*2+1)", output);
    if(!info.failed) {
        std::cout << "output: " << output << std::endl;
    }
    else {
        std::cout << info.info << std::endl;
    }
    return 1;

    InitWindow(800, 600, "calc");
    SetTargetFPS(60);
    SetExitKey(KEY_NULL);

    // little problem, the font is not monospace, 
    // and my error output relies on the characters neatly fitting underneath eachother
    int font_size = GuiGetStyle(DEFAULT, TEXT_SIZE);
    GuiSetStyle(DEFAULT, TEXT_SIZE, font_size * 2);


    while(!WindowShouldClose()) {
        BeginDrawing();
        ClearBackground(DARKGRAY);

        static char buffer[256] = {};
        static ErrorData err = {};
        static float result = 0.0f;
        if(GuiTextBox({150.0f, 120.0f, 250.0f, 60.0f}, buffer, 255, true)) {
            err = CalculateExpression(buffer, result);
            if(!err.failed) {
                std::cout << "output: " << result << std::endl;
            }
        }
        if(err.failed) {
            GuiDrawText(err.info.c_str(), {150.0f, 190.0f, 250.0f, 120.0f}, 0, RED);
        }
        else {
            GuiDrawText(std::to_string(result).c_str(), {410.0f, 120.0f, 250.0f, 60.0f}, 0, GREEN);
        }

        EndDrawing();
    }

    CloseWindow();
    return 0;
}

