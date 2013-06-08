
#include <stdio.h>
#include <stdint.h> // XXX

#include "llvm/ADT/DenseMap.h"
#include "llvm/IR/Function.h"
#include "llvm/IR/Instructions.h"
#include "llvm/IR/Module.h"


namespace llvm {

class OutputStream {
  FILE *FH;
public:
  OutputStream(FILE *FH): FH(FH) {}

  void writeInt(int64_t Val, const char *Desc) {
    fprintf(FH, "%lli # %s\n", (long long) Val, Desc);
  }
};

class InputStream {
  FILE *FH;
public:
  InputStream(FILE *FH): FH(FH) {}

  int64_t readInt(const char *Desc) {
    long long Val;
    char Buf[101];
    fscanf(FH, "%lli # %100s", &Val, Buf);
    return Val;
  }
};

namespace Opcodes {
  enum Opcodes {
    INST_RET,
  };
};

void WriteFunction(OutputStream *Stream, Function *Func) {
  DenseMap<Value *, uint32_t> ValueMap;
  uint32_t NextIndex = 0;

  for (Function::arg_iterator Arg = Func->arg_begin(), E = Func->arg_end();
       Arg != E; ++Arg) {
    ValueMap[Arg] = NextIndex++;
  }

  for (Function::iterator BB = Func->begin(), E = Func->end();
       BB != E; ++BB) {
    for (BasicBlock::iterator Inst = BB->begin(), E = BB->end();
         Inst != E; ++Inst) {
      if (isa<IntToPtrInst>(Inst) ||
          isa<PtrToIntInst>(Inst) ||
          (isa<BitCastInst>(Inst) && Inst->getType()->isPointerTy())) {
        // These instructions are implicit.
      } else {
        ValueMap[Inst] = NextIndex++;
      }
    }
  }

  for (Function::iterator BB = Func->begin(), E = Func->end();
       BB != E; ++BB) {
    for (BasicBlock::iterator Inst = BB->begin(), E = BB->end();
         Inst != E; ++Inst) {
      switch (Inst->getOpcode()) {
        case Instruction::Ret: {
          ReturnInst *Ret = cast<ReturnInst>(Inst);
          Stream->writeInt(Opcodes::INST_RET, "opcode");
          Stream->writeInt(ValueMap[Ret->getReturnValue()], "val");
          break;
        }
      }
    }
  }
}

void WriteModule(OutputStream *Stream, Module *M) {
  for (Module::iterator Func = M->begin(), E = M->end(); Func != E; ++Func) {
    WriteFunction(Stream, Func);
  }
}

void WriteModuleToFile(FILE *FH, Module *M) {
  OutputStream Stream(FH);
  WriteModule(&Stream, M);
}

}
