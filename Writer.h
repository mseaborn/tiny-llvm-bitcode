#ifndef WRITER_H_
#define WRITER_H_

#include <stdio.h>

namespace llvm {
  class Module;

  void WriteModuleToFile(FILE *FH, Module *M);
}

#endif
