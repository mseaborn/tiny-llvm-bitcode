#!/bin/bash

set -eu

# If ccache is available, use it to speed up rebuilds.
ccache=""
if which ccache >/dev/null 2>&1; then
  ccache=ccache
fi

llvm_config=~/devel/nacl-git3/native_client/toolchain/pnacl_linux_x86/host_x86_64/bin/llvm-config

mkdir -p out

$ccache g++ $($llvm_config --cxxflags) -c Freeze.cpp -o out/Freeze.o
$ccache g++ $($llvm_config --cxxflags) -c ReadWrite.cpp -o out/ReadWrite.o
$ccache g++ out/ReadWrite.o out/Freeze.o \
    $($llvm_config --ldflags --libs) \
    -ldl -o out/Freeze

./out/Freeze example.ll
