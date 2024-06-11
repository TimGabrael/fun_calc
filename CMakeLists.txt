cmake_minimum_required(VERSION 3.0)
project(FunCalculator)

find_package(raylib CONFIG REQUIRED)
find_path(RAYGUI_INCLUDE_DIRS "raygui.h")

add_executable(fun_calc src/main.cpp src/calculator.h src/calculator.cpp)

target_link_libraries(fun_calc PRIVATE raylib)
target_include_directories(fun_calc PRIVATE ${RAYGUI_INCLUDE_DIRS})
