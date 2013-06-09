
#include <stdio.h>
#include <stdint.h> // XXX

#include "llvm/ADT/DenseMap.h"
#include "llvm/IR/Function.h"
#include "llvm/IR/Instructions.h"
#include "llvm/IR/Module.h"
#include "llvm/Support/raw_ostream.h"


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
    char Buf[101] = "";
    int Got = fscanf(FH, "%lli # %100s", &Val, Buf);
    if (Got != 2 || strcmp(Buf, Desc) != 0) {
      errs() << "Expected \"" << Desc << "\" but got \"" << Buf << "\"\n";
      report_fatal_error("Read mismatch");
    }
    return Val;
  }
};

namespace Opcodes {
  enum {
    // Put i32 as 0 since it is the most commonly used type.
    TYPE_I32,
    TYPE_I1,
    TYPE_I8,
    TYPE_I16,
    TYPE_I64,
    TYPE_VOID,
    TYPE_FLOAT,
    TYPE_DOUBLE,
  };

  enum Opcodes {
    INST_RET_VOID,
    INST_RET_VALUE,
  };
};


void WriteType(OutputStream *Stream, Type *Ty) {
  uint32_t TypeVal;
  switch (Ty->getTypeID()) {
    case Type::VoidTyID:
      TypeVal = Opcodes::TYPE_VOID;
      break;
    case Type::FloatTyID:
      TypeVal = Opcodes::TYPE_FLOAT;
      break;
    case Type::DoubleTyID:
      TypeVal = Opcodes::TYPE_DOUBLE;
      break;
    case Type::IntegerTyID:
      switch (Ty->getIntegerBitWidth()) {
        case 1: TypeVal = Opcodes::TYPE_I1; break;
        case 8: TypeVal = Opcodes::TYPE_I8; break;
        case 16: TypeVal = Opcodes::TYPE_I16; break;
        case 32: TypeVal = Opcodes::TYPE_I32; break;
        case 64: TypeVal = Opcodes::TYPE_I64; break;
        default:
          report_fatal_error("Disallowed integer size");
      }
      break;
    default:
      errs() << "Type: " << *Ty << "\n";
      report_fatal_error("Disallowed type");
  }
  Stream->writeInt(TypeVal, "type");
}

Type *ReadType(LLVMContext &Context, InputStream *Stream) {
  uint32_t TypeVal = Stream->readInt("type");
  switch (TypeVal) {
    case Opcodes::TYPE_VOID: return Type::getVoidTy(Context);
    case Opcodes::TYPE_I1: return IntegerType::get(Context, 1);
    case Opcodes::TYPE_I8: return IntegerType::get(Context, 8);
    case Opcodes::TYPE_I16: return IntegerType::get(Context, 16);
    case Opcodes::TYPE_I32: return IntegerType::get(Context, 32);
    case Opcodes::TYPE_I64: return IntegerType::get(Context, 64);
    case Opcodes::TYPE_FLOAT: return Type::getFloatTy(Context);
    case Opcodes::TYPE_DOUBLE: return Type::getDoubleTy(Context);
    default:
      report_fatal_error("Bad type ID");
  }
}


void WriteFunctionDecl(OutputStream *Stream, Function *Func) {
  FunctionType *FTy = Func->getFunctionType();
  WriteType(Stream, FTy->getReturnType());
  Stream->writeInt(FTy->getNumParams(), "arg_count");
  for (unsigned I = 0; I < FTy->getNumParams(); ++I) {
    WriteType(Stream, FTy->getParamType(I));
  }
}

Function *ReadFunctionDecl(InputStream *Stream, Module *M) {
  Type *ReturnType = ReadType(M->getContext(), Stream);
  uint32_t ArgCount = Stream->readInt("arg_count");
  SmallVector<Type *, 10> ArgTypes;
  ArgTypes.reserve(ArgCount);
  for (unsigned I = 0; I < ArgCount; ++I) {
    ArgTypes.push_back(ReadType(M->getContext(), Stream));
  }
  FunctionType *FTy = FunctionType::get(ReturnType, ArgTypes, false);
  return Function::Create(FTy, GlobalValue::ExternalLinkage, "", M);
}


void WriteFunction(OutputStream *Stream, Function *Func) {
  Stream->writeInt(Func->getBasicBlockList().size(), "basic_block_count");

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
          if (Ret->getReturnValue()) {
            Stream->writeInt(Opcodes::INST_RET_VALUE, "opcode");
            Stream->writeInt(ValueMap[Ret->getReturnValue()], "val");
          } else {
            Stream->writeInt(Opcodes::INST_RET_VOID, "opcode");
          }
          break;
        }
      }
    }
  }
}

void ReadFunction(InputStream *Stream, Function *Func) {
  uint32_t BBCount = Stream->readInt("basic_block_count");
  SmallVector<BasicBlock *, 10> BasicBlocks;
  BasicBlocks.reserve(BBCount);
  assert(BBCount > 0);
  for (unsigned I = 0; I < BBCount; ++I) {
    BasicBlocks.push_back(BasicBlock::Create(Func->getContext(), "", Func));
  }

  SmallVector<Value *, 64> ValueList;

  for (Function::arg_iterator Arg = Func->arg_begin(), E = Func->arg_end();
       Arg != E; ++Arg) {
    ValueList.push_back(Arg);
  }

  unsigned CurrentBBIndex = 0;
  BasicBlock *CurrentBB = BasicBlocks[0];
  for (;;) {
    uint32_t Opcode = Stream->readInt("opcode");
    switch (Opcode) {
      case Opcodes::INST_RET_VALUE: {
        Value *RetVal = ValueList[Stream->readInt("val")];
        ReturnInst *Ret = ReturnInst::Create(Func->getContext(), RetVal,
                                             CurrentBB);
        break;
      }
      case Opcodes::INST_RET_VOID: {
        ReturnInst *Ret = ReturnInst::Create(Func->getContext(),
                                             CurrentBB);
        break;
      }
      default:
        report_fatal_error("Unrecognized instruction opcode");
    }

    CurrentBBIndex++;
    if (CurrentBBIndex == BBCount)
      break;
    CurrentBB = BasicBlocks[CurrentBBIndex];
  }
}


void WriteModule(OutputStream *Stream, Module *M) {
  Stream->writeInt(M->getFunctionList().size(), "function_count");
  for (Module::iterator Func = M->begin(), E = M->end(); Func != E; ++Func) {
    WriteFunctionDecl(Stream, Func);
  }
  for (Module::iterator Func = M->begin(), E = M->end(); Func != E; ++Func) {
    WriteFunction(Stream, Func);
  }
}

void ReadModule(InputStream *Stream, Module *M) {
  uint32_t FuncCount = Stream->readInt("function_count");
  SmallVector<Function *, 10> FuncList;
  FuncList.reserve(FuncCount);
  for (unsigned I = 0; I < FuncCount; ++I) {
    FuncList.push_back(ReadFunctionDecl(Stream, M));
  }
  for (unsigned I = 0; I < FuncCount; ++I) {
    ReadFunction(Stream, FuncList[I]);
  }
}


void WriteModuleToFile(FILE *FH, Module *M) {
  OutputStream Stream(FH);
  WriteModule(&Stream, M);
}

void ReadModuleFromFile(FILE *FH, Module *M) {
  InputStream Stream(FH);
  ReadModule(&Stream, M);
}

}
