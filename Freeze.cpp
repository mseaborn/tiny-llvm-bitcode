
#include <assert.h>
#include <stdio.h>

#include "llvm/IR/LLVMContext.h"
#include "llvm/IR/Module.h"
#include "llvm/Support/IRReader.h"

#include "ReadWrite.h"

using namespace llvm;

int main(int Argc, char **Argv) {
  assert(Argc >= 2);
  const char *InputFilename = Argv[1];

  llvm::SMDiagnostic Err;
  llvm::LLVMContext &Context = llvm::getGlobalContext();
  Module *M = ParseIRFile(InputFilename, Err, Context);
  if (!M) {
    fprintf(stderr, "failed to read file: %s\n", InputFilename);
    return 1;
  }
  WriteModuleToFile(stdout, M);
  return 0;
}
