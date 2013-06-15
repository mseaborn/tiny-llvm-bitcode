#!/bin/bash

# Check the reader and writer by serialising a pexe (or .ll file) and
# reading it back, and checking that it is unchanged based on LLVM's
# assembly output.

set -eu

llvm_bin=toolchain/pnacl_linux_x86/host_x86_64/bin

pexe="$1"

# As a side effect, this checks that $pexe is valid LLVM code.
$llvm_bin/opt "$pexe" -strip -S | grep -v '; ModuleID =' > out/pexe.orig.ll

./out/Freeze "$pexe" > out/pexe.new_bc
./out/Thaw out/pexe.new_bc > out/pexe.new_bc.ll

$llvm_bin/opt out/pexe.new_bc.ll -o out/pexe.new.pexe

# Check that there are no differences.
diff -u out/pexe.orig.ll out/pexe.new_bc.ll

# Sanity check: re-check syntax and run verifier pass.
# This is redundant after the diff test.
$llvm_bin/opt out/pexe.new_bc.ll -S > /dev/null
