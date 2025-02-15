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

${CLANG} -Xclang -ast-dump -fsyntax-only test.cpp

# Check if the test.cpp is valid
if [ $? -ne 0 ]; then
  echo "Invalid or non-existent path provided: $1/test.cpp"
  exit 1
else
  echo "Successful"
fi