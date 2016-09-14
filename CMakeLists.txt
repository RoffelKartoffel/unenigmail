cmake_minimum_required(VERSION 3.5.0)
project(unenigmail)

##
## Qt stuff
##
set(CMAKE_AUTOMOC ON)
set(CMAKE_INCLUDE_CURRENT_DIR ON)
find_package(Qt5Core REQUIRED)
find_package(Qt5Test REQUIRED)

##
## source files
##
file(GLOB unenigmail_src
    "src/*.h"
    "src/*.cpp"
)
file(GLOB bluetiger9_src
    "src/bluetiger9/*.h"
    "src/bluetiger9/*.cpp"
)
set(unenigmail_src ${unenigmail_src} ${bluetiger9_src})
set(testing_sources ${unenigmail_src})
list(REMOVE_ITEM testing_sources ${CMAKE_CURRENT_SOURCE_DIR}/src/main.cpp)

##
## application
##
add_executable(unenigmail ${unenigmail_src})
qt5_use_modules(unenigmail Core)

##
## tests
##
enable_testing()

add_executable(test_gpg ${testing_sources} "tests/test_gpg/Test_Gpg.h")
qt5_use_modules(test_gpg Test Core)
add_test(Test_Gpg test_gpg)
