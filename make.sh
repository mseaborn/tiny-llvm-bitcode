#!/bin/bash

set -eu

# If ccache is available, use it to speed up rebuilds.
ccache=""
if which ccache >/dev/null 2>&1; then
  ccache=ccache
fi

llvm_bin=toolchain/pnacl_linux_x86/host_x86_64/bin
llvm_config=$llvm_bin/llvm-config

if [ ! -e toolchain ]; then
  echo "Error: Expected to find the native_client/toolchain directory symlinked here"
  echo "Try: ln -s .../native_client/toolchain"
  exit 1
fi
if [ ! -e "$llvm_config" ]; then
  echo "Error: \"$llvm_config\" is not present"
  exit 1
fi

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

# Could do the following command here, but bash doesn't make failures
# in Freeze fatal:
#   ./out/Freeze non_canonical.ll | ./out/Thaw
./out/Freeze non_canonical.ll > out/non_canonical.bc
./out/Thaw < out/non_canonical.bc > out/non_canonical.bc.ll
$llvm_bin/opt out/non_canonical.bc.ll -S > /dev/null
echo PASS: non_canonical.ll works
