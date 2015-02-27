cmake_minimum_required(VERSION 2.8.8)

project(Rhodium)

set (Rhodium_VERSION_MAJOR 1)
set (Rhodium_VERSION_MINOR 0)

# LLVM Discovery
find_package(LLVM REQUIRED CONFIG)

message(STATUS "Found LLVM ${LLVM_PACKAGE_VERSION}")
message(STATUS "Using LLVMConfig.cmake in: ${LLVM_DIR}")

# add path to Haskell modules
set (CMAKE_MODULE_PATH "${CMAKE_MODULE_PATH};${CMAKE_SOURCE_DIR}/cmake-haskell")

add_subdirectory(tools)
add_subdirectory(test)
