cmake_minimum_required(VERSION 3.9)

project(bitcoin-tidy VERSION 1.0.0 DESCRIPTION "clang plugin for Bitcoin Core")

include(GNUInstallDirs)

set(CMAKE_CXX_STANDARD 17)
set(CMAKE_CXX_STANDARD_REQUIRED True)
set(CMAKE_CXX_EXTENSIONS False)

add_compile_options(-fno-rtti)
add_compile_options(-fno-exceptions)

add_library(bitcoin-core-clang SHARED CheckStringNewline.cpp )

install(TARGETS bitcoin-core-clang LIBRARY DESTINATION ${CMAKE_INSTALL_LIBDIR})
