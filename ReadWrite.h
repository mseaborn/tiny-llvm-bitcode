
#include <stdio.h>

namespace llvm {
  class Module;

  void WriteModuleToFile(FILE *FH, Module *M);
}
