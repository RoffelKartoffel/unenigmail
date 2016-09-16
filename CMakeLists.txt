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

configure_file(${CMAKE_CURRENT_SOURCE_DIR}/tests/test_mbox/mbox1 ${CMAKE_BINARY_DIR} COPYONLY)
configure_file(${CMAKE_CURRENT_SOURCE_DIR}/tests/test_mbox/mbox1_output ${CMAKE_BINARY_DIR} COPYONLY)
add_executable(test_mbox ${testing_sources} "tests/test_mbox/Test_MBox.h")
qt5_use_modules(test_mbox Test Core)
add_test(Test_MBox test_mbox)

configure_file(${CMAKE_CURRENT_SOURCE_DIR}/tests/test_mailconverter/mbox2 ${CMAKE_BINARY_DIR} COPYONLY)
configure_file(${CMAKE_CURRENT_SOURCE_DIR}/tests/test_mailconverter/mbox2_output ${CMAKE_BINARY_DIR} COPYONLY)
add_executable(test_mailconverter ${testing_sources} "tests/test_mailconverter/Test_MailConverter.h")
qt5_use_modules(test_mailconverter Test Core)
add_test(Test_MailConverter test_mailconverter)
