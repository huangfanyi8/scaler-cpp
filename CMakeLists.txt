cmake_minimum_required(VERSION 3.20)
project(scaler-cpp
        DESCRIPTION [=[类似与google abseil,boost,facebook folly实现自己的基础工具库,使用现代cmake(3.20)以上版本重新模块化管理项目]=]
        LANGUAGES CXX C
        VERSION 2.0.0)

set(main_directory ${CMAKE_CURRENT_LIST_DIR})
message(STATUS "Include directories: ${CMAKE_CURRENT_SOURCE_DIR}/scaler")

if (MSVC)
    add_compile_options("/Zc:__cplusplus") # 启用 MSVC 的校正
endif()

add_compile_options("$<$<CXX_COMPILER_ID:MSVC>:/Zc:__cplusplus>") # 仅 MSVC 生效
add_library(${PROJECT_NAME} INTERFACE
        scaler/type_traits/type_traits.hpp
        scaler/concepts/concepts.hpp
        scaler/concepts/iterator_concepts.hpp
        scaler/memory/pointer_traits.hpp
        scaler/iterator/iterator.hpp
        scaler/ranges/range_access.hpp
        scaler/ranges/range_algorithm.hpp
        scaler/ranges/range_concepts.hpp
        scaler/ranges/range_primitives.h
        scaler/ranges/range_view.hpp)

set_target_properties(${PROJECT_NAME} PROPERTIES CXX_STANDARD 17)
#add_subdirectory(googletest)

file(GLOB_RECURSE all_test_files
        ${main_directory}/test/*.cpp
        ${main_directory}/test/*.c
        ${main_directory}/test/*.cc
        ${main_directory}/test/*.cxx
)

foreach(test_file  ${all_test_files})
    get_filename_component(extension ${test_file} EXT)#获取后缀名
    get_filename_component(filename ${test_file} NAME_WE)#获取名字

    if(extension STREQUAL ".c")
        string(APPEND filename   "_c")
        add_executable(${filename} ${test_file})
    else (extension STREQUAL ".cpp")
        string(APPEND filename  "_cpp")
        add_executable(${filename} ${test_file})
    endif ()

    target_include_directories(${filename} PUBLIC ${CMAKE_CURRENT_SOURCE_DIR}/scaler)
    set_target_properties(${filename}
            PROPERTIES
            RUNTIME_OUTPUT_DIRECTORY ${main_directory}/bin
            CXX_STANDARD 17)
endforeach ()

add_library(single_span INTERFACE scaler/single_include/span.hpp)

target_compile_features(single_span INTERFACE cxx_std_17)
