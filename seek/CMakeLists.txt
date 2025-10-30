cmake_minimum_required(VERSION 3.31)
project(seek)
set(QT_PATH "C:/Qt/6.9.1/mingw_64")  # 修改为你的路径
set(CMAKE_PREFIX_PATH ${QT_PATH})
set(CMAKE_CXX_STANDARD 20)
set(CMAKE_AUTOMOC ON)
set(CMAKE_AUTOUIC ON)
set(CMAKE_AUTORCC ON)

find_package(Qt6 REQUIRED COMPONENTS Core Widgets)

add_executable(SeekProject main.cpp tab_widget.qrc)
add_executable(test test1.cpp )
target_link_libraries(SeekProject Qt6::Core Qt6::Widgets)
target_link_libraries(test Qt6::Core Qt6::Widgets)



