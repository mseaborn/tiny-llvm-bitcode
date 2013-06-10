
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
  void writeMarker(const char *Desc) {
    writeInt(0, Desc);
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
  void readMarker(const char *Desc) {
    readInt(Desc);
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
    INST_LOAD,
    INST_STORE,
    INST_BR_UNCOND,
    INST_BR_COND,
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


class FunctionWriter {
  OutputStream *Stream;
  Function *Func;
  DenseMap<Value *, uint32_t> ValueMap;
  DenseMap<BasicBlock *, uint32_t> BasicBlockMap;
  uint32_t CurrentValueID;

  void computeValueIndexes();
  void writeOperand(Value *Val);
  void writeBasicBlockOperand(BasicBlock *BB);
  void writeInstruction(Instruction *Inst);

public:
  FunctionWriter(OutputStream *Stream, Function *Func):
    Stream(Stream), Func(Func) {}

  void write();
};

static bool instructionHasValueId(Instruction *Inst) {
  // These instructions are implicit.
  if (isa<IntToPtrInst>(Inst) ||
      isa<PtrToIntInst>(Inst) ||
      (isa<BitCastInst>(Inst) && Inst->getType()->isPointerTy()))
    return false;
  return !Inst->getType()->isVoidTy();
}

void FunctionWriter::computeValueIndexes() {
  uint32_t NextBasicBlockID = 0;
  uint32_t NextValueID = 0;

  // Add globals to ValueMap.  TODO: For efficiency, do this once per
  // module rather than once per function.
  Module *M = Func->getParent();
  for (Module::iterator F = M->begin(), E = M->end(); F != E; ++F) {
    ValueMap[F] = NextValueID++;
  }
  for (Function::arg_iterator Arg = Func->arg_begin(), E = Func->arg_end();
       Arg != E; ++Arg) {
    ValueMap[Arg] = NextValueID++;
  }
  // This is the ID at which instructions start.  Save this for later.
  CurrentValueID = NextValueID;

  for (Function::iterator BB = Func->begin(), E = Func->end();
       BB != E; ++BB) {
    BasicBlockMap[BB] = NextBasicBlockID++;
    for (BasicBlock::iterator Inst = BB->begin(), E = BB->end();
         Inst != E; ++Inst) {
      if (instructionHasValueId(Inst)) {
        ValueMap[Inst] = NextValueID++;
      }
    }
  }
}

void FunctionWriter::writeOperand(Value *Val) {
  if (IntToPtrInst *Cast = dyn_cast<IntToPtrInst>(Val)) {
    Val = Cast->getOperand(0);
  } else if (PtrToIntInst *Cast = dyn_cast<PtrToIntInst>(Val)) {
    Val = Cast->getOperand(0);
  }
  if (ValueMap.count(Val) != 1) {
    errs() << "Value: " << *Val << "\n";
    report_fatal_error("Can't get value ID");
  }
  uint32_t ID = ValueMap[Val];
  Stream->writeInt(ID, "val");
  if (ID >= CurrentValueID) {
    // Forward reference:  write type as well.
    Stream->writeMarker("fwd_ref");
    WriteType(Stream, Val->getType());
  }
}

void FunctionWriter::writeBasicBlockOperand(BasicBlock *BB) {
  assert(BasicBlockMap.count(BB) == 1);
  Stream->writeInt(BasicBlockMap[BB], "basic_block_ref");
}

void FunctionWriter::writeInstruction(Instruction *Inst) {
  switch (Inst->getOpcode()) {
    case Instruction::Ret: {
      ReturnInst *Ret = cast<ReturnInst>(Inst);
      if (Ret->getReturnValue()) {
        Stream->writeInt(Opcodes::INST_RET_VALUE, "opcode");
        writeOperand(Ret->getReturnValue());
      } else {
        Stream->writeInt(Opcodes::INST_RET_VOID, "opcode");
      }
      break;
    }
    case Instruction::Load: {
      LoadInst *Load = cast<LoadInst>(Inst);
      Stream->writeInt(Opcodes::INST_LOAD, "opcode");
      WriteType(Stream, Load->getType());
      writeOperand(Load->getPointerOperand());
      break;
    }
    case Instruction::Store: {
      StoreInst *Store = cast<StoreInst>(Inst);
      Stream->writeInt(Opcodes::INST_STORE, "opcode");
      writeOperand(Store->getOperand(0));
      writeOperand(Store->getOperand(1));
      break;
    }
    case Instruction::Br: {
      BranchInst *Br = cast<BranchInst>(Inst);
      if (Br->isUnconditional()) {
        Stream->writeInt(Opcodes::INST_BR_UNCOND, "opcode");
        writeBasicBlockOperand(Br->getSuccessor(0));
      } else {
        Stream->writeInt(Opcodes::INST_BR_COND, "opcode");
        writeBasicBlockOperand(Br->getSuccessor(0));
        writeBasicBlockOperand(Br->getSuccessor(1));
        writeOperand(Br->getCondition());
      }
      break;
    }
    case Instruction::IntToPtr:
    case Instruction::PtrToInt:
      // These casts are implicit.
      break;
    default:
      errs() << "Instruction: " << *Inst << "\n";
      report_fatal_error("Unhandled instruction type");
  }
}

void FunctionWriter::write() {
  Stream->writeMarker("function_def_start");
  Stream->writeInt(Func->getBasicBlockList().size(), "basic_block_count");
  computeValueIndexes();

  for (Function::iterator BB = Func->begin(), E = Func->end();
       BB != E; ++BB) {
    for (BasicBlock::iterator Inst = BB->begin(), E = BB->end();
         Inst != E; ++Inst) {
      writeInstruction(Inst);
      if (instructionHasValueId(Inst)) {
        assert(CurrentValueID == ValueMap[Inst]);
        CurrentValueID++;
      }
    }
  }
}

class FunctionReader {
  InputStream *Stream;
  Function *Func;
  uint32_t BBCount;
  SmallVector<BasicBlock *, 10> BasicBlocks;
  SmallVector<Value *, 64> ValueList;
  uint32_t CurrentValueID;

  Value *readOperand();
  BasicBlock *readBasicBlockOperand();

public:
  FunctionReader(InputStream *Stream, Function *Func):
    Stream(Stream), Func(Func) {}

  void read();
};

Value *FunctionReader::readOperand() {
  uint32_t ID = Stream->readInt("val");
  if (ID < CurrentValueID) {
    Value *V = ValueList[ID];
    assert(V);
    return V;
  }

  if (ID >= ValueList.size())
    ValueList.resize(ID + 1);

  // Forward reference:  create new placeholder if necessary.
  Stream->readMarker("fwd_ref");
  Type *Ty = ReadType(Func->getContext(), Stream);
  if (Value *V = ValueList[ID])
    return V;
  Value *Placeholder = new Argument(Ty);
  assert(!ValueList[ID]);
  ValueList[ID] = Placeholder;
  return Placeholder;
}

BasicBlock *FunctionReader::readBasicBlockOperand() {
  uint32_t ID = Stream->readInt("basic_block_ref");
  assert(ID < BasicBlocks.size());
  return BasicBlocks[ID];
}

void FunctionReader::read() {
  Stream->readMarker("function_def_start");
  BBCount = Stream->readInt("basic_block_count");
  assert(BBCount > 0);
  BasicBlocks.reserve(BBCount);
  for (unsigned I = 0; I < BBCount; ++I) {
    BasicBlocks.push_back(BasicBlock::Create(Func->getContext(), "", Func));
  }

  // Add globals to ValueList.  TODO: For efficiency, do this once per
  // module rather than once per function.
  Module *M = Func->getParent();
  for (Module::iterator F = M->begin(), E = M->end(); F != E; ++F) {
    ValueList.push_back(F);
  }
  for (Function::arg_iterator Arg = Func->arg_begin(), E = Func->arg_end();
       Arg != E; ++Arg) {
    ValueList.push_back(Arg);
  }

  unsigned CurrentBBIndex = 0;
  BasicBlock *CurrentBB = BasicBlocks[0];
  CurrentValueID = ValueList.size();
  for (;;) {
    uint32_t Opcode = Stream->readInt("opcode");
    Instruction *NewInst = NULL;
    switch (Opcode) {
      case Opcodes::INST_RET_VALUE: {
        Value *RetVal = readOperand();
        NewInst = ReturnInst::Create(Func->getContext(), RetVal, CurrentBB);
        break;
      }
      case Opcodes::INST_RET_VOID: {
        NewInst = ReturnInst::Create(Func->getContext(), CurrentBB);
        break;
      }
      case Opcodes::INST_LOAD: {
        Type *Ty = ReadType(Func->getContext(), Stream);
        Value *Ptr = readOperand();
        Value *Ptr2 = new IntToPtrInst(Ptr, Ty->getPointerTo(), "", CurrentBB);
        NewInst = new LoadInst(Ptr2, "", CurrentBB);
        break;
      }
      case Opcodes::INST_STORE: {
        Value *Val = readOperand();
        Value *Ptr = readOperand();
        Value *Ptr2 = new IntToPtrInst(Ptr, Val->getType()->getPointerTo(),
                                       "", CurrentBB);
        NewInst = new StoreInst(Val, Ptr2, CurrentBB);
        break;
      }
      case Opcodes::INST_BR_UNCOND: {
        BasicBlock *BB = readBasicBlockOperand();
        NewInst = BranchInst::Create(BB, CurrentBB);
        break;
      }
      case Opcodes::INST_BR_COND: {
        BasicBlock *BBIfTrue = readBasicBlockOperand();
        BasicBlock *BBIfFalse = readBasicBlockOperand();
        Value *Cond = readOperand();
        NewInst = BranchInst::Create(BBIfTrue, BBIfFalse, Cond, CurrentBB);
        break;
      }
      default:
        report_fatal_error("Unrecognized instruction opcode");
    }

    if (isa<TerminatorInst>(NewInst)) {
      CurrentBBIndex++;
      if (CurrentBBIndex == BBCount)
        break;
      CurrentBB = BasicBlocks[CurrentBBIndex];
    } else if (instructionHasValueId(NewInst)) {
      if (CurrentValueID >= ValueList.size())
        ValueList.resize(CurrentValueID + 1);
      if (Value *Placeholder = ValueList[CurrentValueID]) {
        // Resolve placeholder value.
        Placeholder->replaceAllUsesWith(NewInst);
        delete Placeholder;
      }
      ValueList[CurrentValueID] = NewInst;
      CurrentValueID++;
    }
  }
}


void WriteModule(OutputStream *Stream, Module *M) {
  Stream->writeInt(M->getFunctionList().size(), "function_count");
  for (Module::iterator Func = M->begin(), E = M->end(); Func != E; ++Func) {
    WriteFunctionDecl(Stream, Func);
  }
  for (Module::iterator Func = M->begin(), E = M->end(); Func != E; ++Func) {
    FunctionWriter(Stream, Func).write();
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
    FunctionReader(Stream, FuncList[I]).read();
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
