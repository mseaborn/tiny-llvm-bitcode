
#include <stdio.h>
#include <stdint.h> // XXX

#include "llvm/ADT/DenseMap.h"
#include "llvm/IR/DataLayout.h"
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

  enum InstOpcode {
    INST_RET_VOID,
    INST_RET_VALUE,
    INST_LOAD,
    INST_STORE,
    INST_ALLOCA_FIXED,
    INST_ALLOCA_VARIABLE,
    INST_CALL,
    INST_BR_UNCOND,
    INST_BR_COND,
    // Binary operators
#define HANDLE_BINARY_INST(LLVM_OP, WIRE_OP) WIRE_OP,
#include "Instructions.def"
#undef HANDLE_BINARY_INST
    // Pseudo-instructions.
    // FWD_REF(TYPE) creates a placeholder for a forward reference.
    INST_FWD_REF,
    // FWD_DEF(N) replaces the Nth FWD_REF placeholder with the
    // previous instruction.
    INST_FWD_DEF,
    INST_CONSTANT_INT,
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


void WriteString(OutputStream *Stream, const std::string &String) {
  Stream->writeInt(String.size(), "string_length");
  for (unsigned I = 0, E = String.size(); I < E; ++I) {
    Stream->writeInt(String[I], "char");
  }
  Stream->writeMarker(("string=" + String).c_str());
}

std::string ReadString(InputStream *Stream) {
  std::string Str;
  unsigned Size = Stream->readInt("string_length");
  for (unsigned I = 0; I < Size; ++I) {
    char C = Stream->readInt("char");
    // TODO: Avoid taking O(n^2) time.
    Str += C;
  }
  Stream->readMarker(("string=" + Str).c_str());
  return Str;
}


IntegerType *GetIntPtrType(LLVMContext &Context) {
  return IntegerType::get(Context, 32);
}

// Convert pointer types in order to handle intrinsics.
// TODO: Be stricter and allow this only for intrinsics.
static Type *StripPtrType(Type *Ty) {
  if (Ty->isPointerTy())
    Ty = GetIntPtrType(Ty->getContext());
  return Ty;
}

void WriteFunctionDecl(OutputStream *Stream, Function *Func) {
  // TODO: Make this stricter.
  Stream->writeInt((Func->getLinkage() == GlobalValue::ExternalLinkage ? 1 : 0),
                   "is_external");
  if (Func->getLinkage() == GlobalValue::ExternalLinkage) {
    WriteString(Stream, Func->getName());
  }

  FunctionType *FTy = Func->getFunctionType();
  WriteType(Stream, StripPtrType(FTy->getReturnType()));
  Stream->writeInt(FTy->getNumParams(), "arg_count");
  for (unsigned I = 0; I < FTy->getNumParams(); ++I) {
    WriteType(Stream, StripPtrType(FTy->getParamType(I)));
  }
}

Function *ReadFunctionDecl(InputStream *Stream, Module *M) {
  GlobalValue::LinkageTypes Linkage =
    (Stream->readInt("is_external") ?
     GlobalValue::ExternalLinkage : GlobalValue::InternalLinkage);
  std::string FuncName;
  if (Linkage == GlobalValue::ExternalLinkage) {
    FuncName = ReadString(Stream);
  }

  Type *ReturnType = ReadType(M->getContext(), Stream);
  uint32_t ArgCount = Stream->readInt("arg_count");
  SmallVector<Type *, 10> ArgTypes;
  ArgTypes.reserve(ArgCount);
  for (unsigned I = 0; I < ArgCount; ++I) {
    ArgTypes.push_back(ReadType(M->getContext(), Stream));
  }
  // Restore pointer arguments in intrinsics.
  Type *PtrTy = IntegerType::get(M->getContext(), 8)->getPointerTo();
  if (StringRef(FuncName).startswith("llvm.memcpy.")) {
    ArgTypes[0] = PtrTy;
    ArgTypes[1] = PtrTy;
  } else if (FuncName == "llvm.nacl.read.tp") {
    ReturnType = PtrTy;
  }
  FunctionType *FTy = FunctionType::get(ReturnType, ArgTypes, false);
  return Function::Create(FTy, Linkage, FuncName, M);
}


class FunctionWriter {
  OutputStream *Stream;
  Function *Func;
  DenseMap<BasicBlock *, uint32_t> BasicBlockMap;

  uint32_t NextValueID;
  DenseMap<Value *, uint32_t> ValueMap;
  // Forward-referenced values that have not been defined yet.
  uint32_t NextFwdRefID;
  DenseMap<Value *, uint32_t> FwdRefs;

  void allocateEarlyValueIDs();
  void materializeOperand(Value *Val);
  void writeOperand(Value *Val);
  void writeBasicBlockOperand(BasicBlock *BB);
  void writeInstruction(Instruction *Inst);

public:
  FunctionWriter(OutputStream *Stream, Function *Func):
    Stream(Stream), Func(Func) {}

  void write();
};

static bool instructionHasValueId(Value *Inst) {
  // These instructions are implicit.
  if (isa<IntToPtrInst>(Inst) ||
      isa<PtrToIntInst>(Inst) ||
      (isa<BitCastInst>(Inst) && Inst->getType()->isPointerTy()))
    return false;
  return !Inst->getType()->isVoidTy();
}

void FunctionWriter::allocateEarlyValueIDs() {
  uint32_t NextBasicBlockID = 0;
  NextValueID = 0;
  NextFwdRefID = 0;

  // Add globals to ValueMap.  TODO: For efficiency, do this once per
  // module rather than once per function.
  Module *M = Func->getParent();
  for (Module::iterator F = M->begin(), E = M->end(); F != E; ++F) {
    ValueMap[F] = NextValueID++;
  }
  for (Module::global_iterator GV = M->global_begin(), E = M->global_end();
       GV != E; ++GV) {
    ValueMap[GV] = NextValueID++;
  }
  // Allocate value IDs for the function's arguments.
  for (Function::arg_iterator Arg = Func->arg_begin(), E = Func->arg_end();
       Arg != E; ++Arg) {
    ValueMap[Arg] = NextValueID++;
  }
  // Allocate IDs for basic blocks.
  for (Function::iterator BB = Func->begin(), E = Func->end();
       BB != E; ++BB) {
    BasicBlockMap[BB] = NextBasicBlockID++;
  }
}

static Value *stripPtrCasts(Value *Val) {
  if (BitCastInst *Cast = dyn_cast<BitCastInst>(Val)) {
    if (Cast->getType()->isPointerTy()) {
      Val = Cast->getOperand(0);
    }
  } else {
    if (IntToPtrInst *Cast = dyn_cast<IntToPtrInst>(Val))
      Val = Cast->getOperand(0);
    if (PtrToIntInst *Cast = dyn_cast<PtrToIntInst>(Val))
      Val = Cast->getOperand(0);
  }
  return Val;
}

// Ensure that Val has a value ID allocated for it.  Write an opcode
// to the output stream if necessary.
void FunctionWriter::materializeOperand(Value *Val) {
  Val = stripPtrCasts(Val);
  if (isa<BasicBlock>(Val) || ValueMap.count(Val) == 1)
    return;
  // GlobalValues should already be in ValueMap.
  assert(!isa<GlobalValue>(Val));
  if (ConstantInt *C = dyn_cast<ConstantInt>(Val)) {
    Stream->writeInt(Opcodes::INST_CONSTANT_INT, "opcode");
    // TODO: Could we omit the type here to save space?
    WriteType(Stream, C->getType());
    Stream->writeInt(C->getZExtValue(), "constant_int");
    ValueMap[Val] = NextValueID++;
    return;
  }
  Stream->writeInt(Opcodes::INST_FWD_REF, "opcode");
  WriteType(Stream, Val->getType());
  ValueMap[Val] = NextValueID++;
  FwdRefs[Val] = NextFwdRefID++;
}

void FunctionWriter::writeOperand(Value *Val) {
  Val = stripPtrCasts(Val);
  if (ValueMap.count(Val) != 1) {
    errs() << "Value: " << *Val << "\n";
    report_fatal_error("Can't get value ID");
  }
  Stream->writeInt(ValueMap[Val], "val");
}

void FunctionWriter::writeBasicBlockOperand(BasicBlock *BB) {
  assert(BasicBlockMap.count(BB) == 1);
  Stream->writeInt(BasicBlockMap[BB], "basic_block_ref");
}

static Opcodes::InstOpcode getOpcodeToWrite(Instruction *Inst) {
  switch (Inst->getOpcode()) {
#define HANDLE_BINARY_INST(LLVM_OP, WIRE_OP) \
    case Instruction::LLVM_OP: return Opcodes::WIRE_OP;
#include "Instructions.def"
#undef HANDLE_BINARY_INST
    default:
      errs() << "Instruction: " << *Inst << "\n";
      report_fatal_error("No opcode defined for instruction");
  }
}

void FunctionWriter::writeInstruction(Instruction *Inst) {
  // First, ensure the instruction's operands have been allocated value IDs.
  for (unsigned I = 0, E = Inst->getNumOperands(); I < E; ++I) {
    materializeOperand(Inst->getOperand(I));
  }

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
      // TODO: Handle "align", "volatile" and atomic attributes.
      LoadInst *Load = cast<LoadInst>(Inst);
      Stream->writeInt(Opcodes::INST_LOAD, "opcode");
      WriteType(Stream, Load->getType());
      writeOperand(Load->getPointerOperand());
      break;
    }
    case Instruction::Store: {
      // TODO: Handle "align", "volatile" and atomic attributes.
      StoreInst *Store = cast<StoreInst>(Inst);
      Stream->writeInt(Opcodes::INST_STORE, "opcode");
      writeOperand(Store->getOperand(0));
      writeOperand(Store->getOperand(1));
      break;
    }
    case Instruction::Alloca: {
      // TODO: Handle "align".
      AllocaInst *Alloca = cast<AllocaInst>(Inst);
      ArrayType *Ty = dyn_cast<ArrayType>(Alloca->getType()->getElementType());
      if (!Ty || !Ty->getElementType()->isIntegerTy(8))
        report_fatal_error("Non-i8-array alloca");
      Stream->writeInt((Alloca->isArrayAllocation() ?
                        Opcodes::INST_ALLOCA_VARIABLE :
                        Opcodes::INST_ALLOCA_FIXED), "opcode");
      Stream->writeInt(Ty->getNumElements(), "alloca_size");
      if (Alloca->isArrayAllocation())
        writeOperand(Alloca->getArraySize());
      break;
    }
    case Instruction::Call: {
      CallInst *Call = cast<CallInst>(Inst);
      // TODO: Handle "tail" attribute.
      // TODO: Add a separate opcode for direct calls.  In that case,
      // we won't need to output the return type and the argument
      // count.
      Stream->writeInt(Opcodes::INST_CALL, "opcode");
      // Note: internally, CallInsts put the callee as the last
      // operand, but here we write out the callee as the first
      // operand.
      writeOperand(Call->getCalledValue());
      // Write the return type.
      WriteType(Stream, StripPtrType(Call->getType()));
      Stream->writeInt(Call->getNumArgOperands(), "argument_count");
      for (unsigned I = 0, E = Call->getNumArgOperands(); I < E; ++I) {
        writeOperand(Call->getArgOperand(I));
      }
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
    case Instruction::BitCast:
      // These casts are implicit.
      break;
    default:
      if (BinaryOperator *Op = dyn_cast<BinaryOperator>(Inst)) {
        Stream->writeInt(getOpcodeToWrite(Inst), "opcode");
        writeOperand(Op->getOperand(0));
        writeOperand(Op->getOperand(1));
        break;
      }
      errs() << "Instruction: " << *Inst << "\n";
      report_fatal_error("Unhandled instruction type");
  }
}

void FunctionWriter::write() {
  Stream->writeMarker("function_def_start");
  Stream->writeInt(Func->getBasicBlockList().size(), "basic_block_count");
  allocateEarlyValueIDs();

  for (Function::iterator BB = Func->begin(), E = Func->end();
       BB != E; ++BB) {
    for (BasicBlock::iterator Inst = BB->begin(), E = BB->end();
         Inst != E; ++Inst) {
      writeInstruction(Inst);
      if (instructionHasValueId(Inst)) {
        ValueMap[Inst] = NextValueID++;
        // If this resolves a forward reference, declare that.
        if (FwdRefs.count(Inst)) {
          Stream->writeInt(Opcodes::INST_FWD_DEF, "opcode");
          Stream->writeInt(FwdRefs[Inst], "fwd_ref_id");
          FwdRefs.erase(Inst);
        }
      }
    }
  }
  // Sanity check to ensure that every FWD_REF had a FWD_DEF.
  if (FwdRefs.size() != 0) {
    for (DenseMap<Value *, uint32_t>::iterator Iter = FwdRefs.begin(),
           E = FwdRefs.end(); Iter != E; ++Iter) {
      errs() << "Forward ref: " << *Iter->first << "\n";
    }
    report_fatal_error("Unresolved forward references when writing");
  }
}

class FunctionReader {
  InputStream *Stream;
  Function *Func;
  Type *IntPtrType;
  uint32_t BBCount;
  SmallVector<BasicBlock *, 10> BasicBlocks;
  SmallVector<Value *, 64> ValueList;
  BasicBlock *CurrentBB;

  SmallVector<Value *, 16> FwdRefs;
  uint32_t FwdRefsResolved;

  Value *readRawOperand();
  Value *castOperand(Value *Val, Type *Ty);
  Value *readScalarOperand();
  Value *readPtrOperand(Type *Ty);
  BasicBlock *readBasicBlockOperand();
  Value *readBinOp(Instruction::BinaryOps Opcode);
  Value *readInstruction();

public:
  FunctionReader(InputStream *Stream, Function *Func):
      Stream(Stream), Func(Func), FwdRefsResolved(0) {
    IntPtrType = IntegerType::get(Func->getContext(), 32);
  }

  void read();
};

Value *FunctionReader::readRawOperand() {
  uint32_t ID = Stream->readInt("val");
  assert(ID < ValueList.size());
  return ValueList[ID];
}

// If Ty is NULL, this coerces Val to be a scalar.  Otherwise, it
// coerces Val to be a pointer-to-Ty.
Value *FunctionReader::castOperand(Value *Val, Type *Ty) {
  if (Ty) {
    if (Val->getType()->isPointerTy()) {
      Val = new BitCastInst(Val, Ty->getPointerTo(), "", CurrentBB);
    } else {
      Val = new IntToPtrInst(Val, Ty->getPointerTo(), "", CurrentBB);
    }
  } else {
    if (Val->getType()->isPointerTy())
      Val = new PtrToIntInst(Val, IntPtrType, "", CurrentBB);
  }
  return Val;
}

Value *FunctionReader::readScalarOperand() {
  return castOperand(readRawOperand(), NULL);
}

Value *FunctionReader::readPtrOperand(Type *Ty) {
  assert(Ty);
  return castOperand(readRawOperand(), Ty);
}

BasicBlock *FunctionReader::readBasicBlockOperand() {
  uint32_t ID = Stream->readInt("basic_block_ref");
  assert(ID < BasicBlocks.size());
  return BasicBlocks[ID];
}

Value *FunctionReader::readBinOp(Instruction::BinaryOps Opcode) {
  Value *Op1 = readScalarOperand();
  Value *Op2 = readScalarOperand();
  return BinaryOperator::Create(Opcode, Op1, Op2, "", CurrentBB);
}

Value *FunctionReader::readInstruction() {
  uint32_t Opcode = Stream->readInt("opcode");
  switch (Opcode) {
    case Opcodes::INST_FWD_REF: {
      Type *Ty = ReadType(Func->getContext(), Stream);
      Value *Placeholder = new Argument(Ty);
      FwdRefs.push_back(Placeholder);
      return Placeholder;
    }
    case Opcodes::INST_FWD_DEF: {
      assert(ValueList.size() > 0);
      Value *NewVal = ValueList[ValueList.size() - 1];
      uint32_t ID = Stream->readInt("fwd_ref_id");
      Value *Placeholder = FwdRefs[ID];
      assert(Placeholder);
      Placeholder->replaceAllUsesWith(NewVal);
      delete Placeholder;
      FwdRefs[ID] = NULL;
      ++FwdRefsResolved;
      return NULL;
    }
    case Opcodes::INST_RET_VALUE: {
      Value *RetVal = readScalarOperand();
      return ReturnInst::Create(Func->getContext(), RetVal, CurrentBB);
    }
    case Opcodes::INST_RET_VOID: {
      return ReturnInst::Create(Func->getContext(), CurrentBB);
    }
    case Opcodes::INST_LOAD: {
      Type *Ty = ReadType(Func->getContext(), Stream);
      Value *Ptr = readPtrOperand(Ty);
      return new LoadInst(Ptr, "", CurrentBB);
    }
    case Opcodes::INST_STORE: {
      Value *Val = readScalarOperand();
      Value *Ptr = readPtrOperand(Val->getType());
      return new StoreInst(Val, Ptr, CurrentBB);
    }
    case Opcodes::INST_ALLOCA_FIXED:
    case Opcodes::INST_ALLOCA_VARIABLE: {
      uint32_t TypeSize = Stream->readInt("alloca_size");
      Type *I8Ty = IntegerType::get(Func->getContext(), 8);
      Type *Ty = ArrayType::get(I8Ty, TypeSize);
      Value *ArraySize = NULL;
      if (Opcode == Opcodes::INST_ALLOCA_VARIABLE)
        ArraySize = readScalarOperand();
      return new AllocaInst(Ty, ArraySize, "", CurrentBB);
    }
    case Opcodes::INST_CALL: {
      Value *Callee = readRawOperand();
      Type *ReturnType = ReadType(Func->getContext(), Stream);
      uint32_t ArgCount = Stream->readInt("argument_count");
      SmallVector<Value *, 10> Args;
      if (Function *DirectFunc = dyn_cast<Function>(Callee)) {
        // Handle direct calls specially in order to handle intrinsics
        // that take pointer arguments.
        // TODO: Produce a nicer error message if a function is
        // direct-called with the wrong argument/return types.
        for (unsigned I = 0; I < ArgCount; ++I) {
          Type *ArgType = DirectFunc->getFunctionType()->getParamType(I);
          if (ArgType->isPointerTy()) {
            Args.push_back(readPtrOperand(ArgType->getPointerElementType()));
          } else {
            Args.push_back(readScalarOperand());
          }
        }
      } else {
        SmallVector<Type *, 10> ArgTypes;
        for (unsigned I = 0; I < ArgCount; ++I) {
          Value *Arg = readScalarOperand();
          Args.push_back(Arg);
          ArgTypes.push_back(Arg->getType());
        }
        Type *FuncTy = FunctionType::get(ReturnType, ArgTypes, false);
        Callee = castOperand(Callee, FuncTy);
      }
      return CallInst::Create(Callee, Args, "", CurrentBB);
    }
    case Opcodes::INST_BR_UNCOND: {
      BasicBlock *BB = readBasicBlockOperand();
      return BranchInst::Create(BB, CurrentBB);
    }
    case Opcodes::INST_BR_COND: {
      BasicBlock *BBIfTrue = readBasicBlockOperand();
      BasicBlock *BBIfFalse = readBasicBlockOperand();
      Value *Cond = readScalarOperand();
      return BranchInst::Create(BBIfTrue, BBIfFalse, Cond, CurrentBB);
    }
    case Opcodes::INST_CONSTANT_INT: {
      Type *Ty = ReadType(Func->getContext(), Stream);
      uint64_t IntVal = Stream->readInt("constant_int");
      return ConstantInt::get(Ty, IntVal);
    }

#define HANDLE_BINARY_INST(LLVM_OP, WIRE_OP) \
    case Opcodes::WIRE_OP: return readBinOp(Instruction::LLVM_OP);
#include "Instructions.def"
#undef HANDLE_BINARY_INST

    default:
      report_fatal_error("Unrecognized instruction opcode");
  }
}

void FunctionReader::read() {
  Stream->readMarker("function_def_start");
  BBCount = Stream->readInt("basic_block_count");
  // TODO: Make this stricter and handle declarations separately.
  // assert(BBCount > 0);
  if (BBCount == 0)
    return;
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
  for (Module::global_iterator GV = M->global_begin(), E = M->global_end();
       GV != E; ++GV) {
    ValueList.push_back(GV);
  }
  for (Function::arg_iterator Arg = Func->arg_begin(), E = Func->arg_end();
       Arg != E; ++Arg) {
    ValueList.push_back(Arg);
  }

  unsigned CurrentBBIndex = 0;
  CurrentBB = BasicBlocks[0];
  for (;;) {
    Value *NewInst = readInstruction();
    if (!NewInst) {
      // Nothing to do.
    } else if (isa<TerminatorInst>(NewInst)) {
      CurrentBBIndex++;
      if (CurrentBBIndex == BBCount)
        break;
      CurrentBB = BasicBlocks[CurrentBBIndex];
    } else if (instructionHasValueId(NewInst)) {
      ValueList.push_back(NewInst);
    }
  }
  if (FwdRefsResolved != FwdRefs.size()) {
    errs() << FwdRefsResolved << " forward references resolved, but "
           << FwdRefs.size() << " requested\n";
    report_fatal_error("Mismatch in forward references");
  }
}


void WriteGlobal(OutputStream *Stream, GlobalVariable *GV) {
  // Use DataLayout as a convenience for getTypeAllocSize().
  // TODO: Handle "align" attribute.
  DataLayout DL("");
  Type *Ty = GV->getType()->getPointerElementType();
  Stream->writeInt(DL.getTypeAllocSize(Ty), "global_size");
  Stream->writeInt(GV->isConstant(), "is_constant");
}

Value *ReadGlobal(InputStream *Stream, Module *M) {
  uint32_t Size = Stream->readInt("global_size");
  bool IsConstant = Stream->readInt("is_constant");
  Type *Ty = ArrayType::get(Type::getInt8Ty(M->getContext()), Size);
  Constant *Init = ConstantAggregateZero::get(Ty);
  return new GlobalVariable(*M, Ty, IsConstant, GlobalValue::InternalLinkage,
                            Init, "");
}


void WriteModule(OutputStream *Stream, Module *M) {
  Stream->writeInt(M->getGlobalList().size(), "global_count");
  for (Module::global_iterator GV = M->global_begin(), E = M->global_end();
       GV != E; ++GV) {
    WriteGlobal(Stream, GV);
  }

  Stream->writeInt(M->getFunctionList().size(), "function_count");
  for (Module::iterator Func = M->begin(), E = M->end(); Func != E; ++Func) {
    WriteFunctionDecl(Stream, Func);
  }
  for (Module::iterator Func = M->begin(), E = M->end(); Func != E; ++Func) {
    FunctionWriter(Stream, Func).write();
  }
}

void ReadModule(InputStream *Stream, Module *M) {
  uint32_t GlobalCount = Stream->readInt("global_count");
  for (unsigned I = 0; I < GlobalCount; ++I) {
    ReadGlobal(Stream, M);
  }

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
