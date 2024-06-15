cmake_minimum_required(VERSION 3.17)
project(clang-plugin-test)

set(CMAKE_CXX_STANDARD 17)

add_compile_options(
        "$<$<CXX_COMPILER_ID:GNU,Clang>:-fno-rtti>"
)

set(MY_LLVM "/home/hzy/hzydata/softwares/llvm/clang+llvm-16.0.0-x86_64-linux-gnu-ubuntu-18.04/lib/cmake/" CACHE STRING "LLVM lib/cmake path")

message("MY_LLVM: ${MY_LLVM}")

set(CMAKE_PREFIX_PATH ${CMAKE_PREFIX_PATH} ${MY_LLVM})

find_package(LLVM REQUIRED CONFIG)
find_package(Clang REQUIRED CONFIG)

message("LLVM STATUS:
  Definitions ${LLVM_DEFINITIONS}
  Includes    ${LLVM_INCLUDE_DIRS}
${CLANG_INCLUDE_DIRS}
  Libraries   ${LLVM_LIBRARY_DIRS}"
)

add_definitions(${LLVM_DEFINITIONS})

include_directories(SYSTEM ${LLVM_INCLUDE_DIRS} ${CLANG_INCLUDE_DIRS})

link_directories(${LLVM_LIBRARY_DIRS})

add_library(myplugin MODULE myplugin.cpp)