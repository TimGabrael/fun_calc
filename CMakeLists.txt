cmake_minimum_required(VERSION 3.0)
project(FunCalculator)

find_package(raylib CONFIG REQUIRED)

set(IMGUI_FILES "ImGui/imconfig.h" "ImGui/imgui.cpp" "ImGui/imgui.h" "ImGui/imgui.h" "ImGui/imgui_demo.cpp" "ImGui/imgui_draw.cpp" "ImGui/imgui_internal.h" "ImGui/imgui_tables.cpp" "ImGui/imgui_widgets.cpp" "ImGui/imstb_rectpack.h" "ImGui/imstb_textedit.h" "ImGui/imstb_truetype.h" "ImGui/imgui_stdlib.h" "ImGui/imgui_stdlib.cpp")
set(RLIMGUI_FILES "rlImGui/imgui_impl_raylib.h" "rlImGui/rlImGui.h" "rlImGui/rlImGui.cpp" "rlImGui/rlImGuiColors.h")
set(NODEEDIT_FILES "imgui-node-editor/crude_json.cpp" "imgui-node-editor/crude_json.h" "imgui-node-editor/imgui_bezier_math.h" "imgui-node-editor/imgui_bezier_math.inl" "imgui-node-editor/imgui_canvas.cpp" "imgui-node-editor/imgui_canvas.h" "imgui-node-editor/imgui_extra_math.h" "imgui-node-editor/imgui_extra_math.inl" "imgui-node-editor/imgui_node_editor.cpp" "imgui-node-editor/imgui_node_editor.h" "imgui-node-editor/imgui_node_editor_api.cpp" "imgui-node-editor/imgui_node_editor_internal.h" "imgui-node-editor/imgui_node_editor_internal.inl")

add_executable(fun_calc src/main.cpp src/calculator.h src/calculator.cpp src/util.h src/util.cpp src/graph.h src/graph.cpp ${IMGUI_FILES} ${RLIMGUI_FILES} ${NODEEDIT_FILES})

target_include_directories(fun_calc PRIVATE rlImGui ImGui imgui-node-editor)
target_link_libraries(fun_calc PRIVATE raylib)

