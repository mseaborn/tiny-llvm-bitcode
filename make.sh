#!/bin/bash

set -eu

# If ccache is available, use it to speed up rebuilds.
ccache=""
if which ccache >/dev/null 2>&1; then
  ccache=ccache
fi

llvm_bin=~/devel/nacl-git3/native_client/toolchain/pnacl_linux_x86/host_x86_64/bin
llvm_config=$llvm_bin/llvm-config

mkdir -p out

cflags="$($llvm_config --cxxflags) -g -Wall"
$ccache g++ $cflags -c ReadWrite.cpp -o out/ReadWrite.o
$ccache g++ $cflags -c Freeze.cpp -o out/Freeze.o
$ccache g++ $cflags -c Thaw.cpp -o out/Thaw.o

$ccache g++ out/ReadWrite.o out/Freeze.o \
    $($llvm_config --ldflags --libs) \
    -ldl -o out/Freeze
$ccache g++ out/ReadWrite.o out/Thaw.o \
    $($llvm_config --ldflags --libs) \
    -ldl -o out/Thaw

$llvm_bin/opt example.ll -o out/example.o
./out/Freeze example.ll > out/example.bc
wc -c out/example.o
wc -l out/example.bc
./out/Thaw < out/example.bc > out/example.bc.ll

$llvm_bin/opt example.ll -strip -S \
    | grep -v '; ModuleID =' > out/example.ll.orig
diff -u out/example.ll.orig out/example.bc.ll
echo PASS: .ll files are the same

# Sanity check: re-check syntax and run verifier pass.
# This is redundant after the diff test.
$llvm_bin/opt out/example.bc.ll -S > /dev/null
