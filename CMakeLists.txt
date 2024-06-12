cmake_minimum_required(VERSION 3.0)
project(FunCalculator)

find_package(raylib CONFIG REQUIRED)

set(IMGUI_FILES "ImGui/imconfig.h" "ImGui/imgui.cpp" "ImGui/imgui.h" "ImGui/imgui.h" "ImGui/imgui_demo.cpp" "ImGui/imgui_draw.cpp" "ImGui/imgui_internal.h" "ImGui/imgui_tables.cpp" "ImGui/imgui_widgets.cpp" "ImGui/imstb_rectpack.h" "ImGui/imstb_textedit.h" "ImGui/imstb_truetype.h")
set(RLIMGUI_FILES "rlImGui/imgui_impl_raylib.h" "rlImGui/rlImGui.h" "rlImGui/rlImGui.cpp" "rlImGui/rlImGuiColors.h")

add_executable(fun_calc src/main.cpp src/calculator.h src/calculator.cpp ${IMGUI_FILES} ${RLIMGUI_FILES})

target_include_directories(fun_calc PRIVATE rlImGui ImGui)
target_link_libraries(fun_calc PRIVATE raylib)

