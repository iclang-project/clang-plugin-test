#!/bin/bash

LLVM_PATH=${LLVM_PATH:-"/root/hzy/softwares/llvm/clang+llvm-16.0.0-x86_64-linux-gnu-ubuntu-18.04"}
LLVM_BIN_PATH=${LLVM_PATH}/bin
LLVM_LIB_PATH=${LLVM_PATH}/lib
CLANG=${LLVM_BIN_PATH}/clang++

echo "LLVM_PATH:${LLVM_PATH}"
echo "LLVM_BIN_PATH:${LLVM_BIN_PATH}"
echo "LLVM_LIB_PATH:${LLVM_LIB_PATH}"
echo "CLANG:${CLANG}"

export LD_LIBRARY_PATH=${LLVM_LIB_PATH}

PLUGIN_NAME="funcxable"

# Obtain the absolute path of plugin
PLUGIN_PATH=$(realpath "../cmake-build-release/lib${PLUGIN_NAME}.so" 2>/dev/null)

# Check if the path is valid and successfully converted to an absolute path
if [ $? -ne 0 ]; then
  echo "Invalid or non-existent plugin path"
  exit 1
else
  echo "PLUGIN_PATH: $PLUGIN_PATH"
fi

# Check if a path argument is provided
if [ -z "$1" ]; then
  echo "Please provide a path."
  exit 1
fi

cd "$1"

# Check if the path is valid
if [ $? -ne 0 ]; then
  echo "Invalid or non-existent path provided: $1"
  exit 1
fi

${CLANG} -fplugin=${PLUGIN_PATH} -fplugin-arg-${PLUGIN_NAME}-hello -fplugin-arg-${PLUGIN_NAME}-world -c -o test.o test.cpp > test.log 2>&1

# Check
if [ $? -ne 0 ]; then
  echo "Failed, see $1/test.log"
  exit 1
else
  echo "Successful, see $1/test.log"
fi