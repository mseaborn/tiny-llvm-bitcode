#ifndef READER_H_
#define READER_H_

#include <stdio.h>

namespace llvm {
  class Module;

  void ReadModuleFromFile(FILE *FH, Module *M);
}

#endif
