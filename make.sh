#!/bin/bash

set -eu

# If ccache is available, use it to speed up rebuilds.
ccache=""
if which ccache >/dev/null 2>&1; then
  ccache=ccache
fi

llvm_config=~/devel/nacl-git3/native_client/toolchain/pnacl_linux_x86/host_x86_64/bin/llvm-config

mkdir -p out

cflags="$($llvm_config --cxxflags) -g"
$ccache g++ $cflags -c ReadWrite.cpp -o out/ReadWrite.o
$ccache g++ $cflags -c Freeze.cpp -o out/Freeze.o
$ccache g++ $cflags -c Thaw.cpp -o out/Thaw.o

$ccache g++ out/ReadWrite.o out/Freeze.o \
    $($llvm_config --ldflags --libs) \
    -ldl -o out/Freeze
$ccache g++ out/ReadWrite.o out/Thaw.o \
    $($llvm_config --ldflags --libs) \
    -ldl -o out/Thaw

./out/Freeze example.ll
./out/Freeze example.ll | ./out/Thaw
