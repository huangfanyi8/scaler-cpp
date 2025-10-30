cmake_minimum_required(VERSION 3.20)
project(expected)

set(CMAKE_CXX_STANDARD 23)

add_library(json STATIC json.cpp)
target_include_directories(json PUBLIC ${CMAKE_CURRENT_SOURCE_DIR})
add_executable(expected main.cpp)
add_executable(test test_expected.cpp)
add_executable(json_test json_test.cpp)

