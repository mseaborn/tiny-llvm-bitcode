
#include <assert.h>
#include <stdio.h>

#include "llvm/IR/LLVMContext.h"
#include "llvm/IR/Module.h"
#include "llvm/Support/IRReader.h"
#include "llvm/Support/raw_ostream.h"

#include "ReadWrite.h"

using namespace llvm;

int main(int Argc, char **Argv) {
  llvm::SMDiagnostic Err;
  llvm::LLVMContext &Context = llvm::getGlobalContext();
  Module *M = new Module(StringRef(), Context);
  if (Argc == 1) {
    ReadModuleFromFile(stdin, M);
  } else if (Argc == 2) {
    FILE *FP = fopen(Argv[1], "r");
    assert(FP);
    ReadModuleFromFile(FP, M);
    fclose(FP);
  } else {
    report_fatal_error("Unrecognized arguments");
  }
  outs() << *M;
  return 0;
}
