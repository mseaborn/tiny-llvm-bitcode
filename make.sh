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
$ccache g++ $cflags -c Writer.cpp -o out/Writer.o
$ccache g++ $cflags -c Reader.cpp -o out/Reader.o
$ccache g++ $cflags -c Freeze.cpp -o out/Freeze.o
$ccache g++ $cflags -c Thaw.cpp -o out/Thaw.o

$ccache g++ out/Writer.o out/Freeze.o \
    $($llvm_config --ldflags --libs) \
    -ldl -o out/Freeze
$ccache g++ out/Reader.o out/Thaw.o \
    $($llvm_config --ldflags --libs) \
    -ldl -o out/Thaw

./check_pexe.sh example.ll
echo PASS: example.ll reads back the same

# Could do the following command here, but bash doesn't make failures
# in Freeze fatal:
#   ./out/Freeze non_canonical.ll | ./out/Thaw
./out/Freeze non_canonical.ll > out/non_canonical.bc
./out/Thaw < out/non_canonical.bc > out/non_canonical.bc.ll
$llvm_bin/opt out/non_canonical.bc.ll -S > /dev/null
echo PASS: non_canonical.ll works
