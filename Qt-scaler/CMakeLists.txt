        cmake_minimum_required(VERSION 3.20)
        project(Qt-scaler)

        set(CMAKE_CXX_STANDARD 17)
        set(CMAKE_PREFIX_PATH  "D:/Qt/6.9.1/mingw_64")

        set(CMAKE_AUTOMOC  ON)
        set(CMAKE_AUTORCC ON)
        set(CMAKE_AUTOUIC ON)

        find_package(Qt6 COMPONENTS Core Gui Widgets Svg REQUIRED)

        function( scaler_add_sources   return source_dir)
                file(GLOB_RECURSE _return Qt-scaler/${source_dir}/*.*
                        test/${source_dir}/*.cpp)
                set(${return} ${_return} PARENT_SCOPE)
        endfunction()

        function(scaler_add_executable src_dir exe_name)
                scaler_add_sources(sources ${src_dir})
                set(exe_combine_name "${exe_name}_${src_dir}")
                add_executable(${exe_combine_name} ${sources} )
                foreach (src ${sources})
                        message("add source ${src}")
                endforeach ()
                set(modules   ${ARGN})
                foreach (qt6_module IN LISTS modules)
                        message("Current linked directory is ${qt6_module}")
                        target_link_libraries(${exe_combine_name}  PUBLIC ${qt6_module})
                endforeach ()
                message("The number  of Arguments is  ${ARGC}")
        endfunction()

        scaler_add_executable(carousel carousel  Qt6::Widgets  Qt6::Core  Qt6::Svg)
        scaler_add_executable(graphics white_board  Qt6::Widgets)
        scaler_add_executable(widgets white_board  Qt6::Widgets)

        get_target_property(linked_libs carousel_carousel LINK_LIBRARIES)
        message(STATUS "Linked libraries: ${linked_libs}")

        add_executable(MyApp main.cpp)
        target_link_libraries(MyApp PRIVATE
                Qt6::Core
                Qt6::Gui
                Qt6::Widgets
        )

function(scaler_add_qrc)

endfunction()



