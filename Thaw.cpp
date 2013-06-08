
#include <assert.h>
#include <stdio.h>

#include "llvm/IR/LLVMContext.h"
#include "llvm/IR/Module.h"
#include "llvm/Support/IRReader.h"

#include "ReadWrite.h"

using namespace llvm;

int main(int Argc, char **Argv) {
  llvm::SMDiagnostic Err;
  llvm::LLVMContext &Context = llvm::getGlobalContext();
  Module *M = new Module(StringRef(), Context);
  ReadModuleFromFile(stdin, M);
  M->dump();
  return 0;
}
