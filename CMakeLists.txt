cmake_minimum_required(VERSION 3.0.2)
project(unenigmail)

set(CMAKE_AUTOMOC ON)
set(CMAKE_INCLUDE_CURRENT_DIR ON)

file(GLOB unenigmail_src
    "src/*.h"
    "src/*.cpp"
)

file(GLOB bluetiger9_src
    "src/bluetiger9/*.h"
    "src/bluetiger9/*.cpp"
)

find_package(Qt5Core REQUIRED)

add_executable(unenigmail ${unenigmail_src} ${bluetiger9_src})
qt5_use_modules(unenigmail Core)
