
#include <stdio.h>
#include <stdint.h> // XXX

#include "llvm/IR/Function.h"
#include "llvm/IR/Instructions.h"
#include "llvm/IR/Module.h"
#include "llvm/Support/raw_ostream.h"

#include "OpcodeEnums.h"
#include "Shared.h"


namespace llvm {

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

uint32_t ReadAlignmentVal(InputStream *Stream) {
  return (1 << Stream->readInt("align")) >> 1;
}

void ReadBytes(InputStream *Stream, uint8_t *Buf, uint32_t Size) {
  for (unsigned I = 0; I < Size; ++I)
    Buf[I] = Stream->readInt("byte");
}

std::string ReadString(InputStream *Stream) {
  unsigned Size = Stream->readInt("string_length");
  std::string Str;
  Str.reserve(Size);
  for (unsigned I = 0; I < Size; ++I) {
    char C = Stream->readInt("byte");
    Str += C;
  }
  Stream->readMarker(("string=" + Str).c_str());
  return Str;
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
  } else if (StringRef(FuncName).startswith("llvm.memset.")) {
    ArgTypes[0] = PtrTy;
  } else if (FuncName == "llvm.nacl.read.tp") {
    ReturnType = PtrTy;
  }
  FunctionType *FTy = FunctionType::get(ReturnType, ArgTypes, false);
  return Function::Create(FTy, Linkage, FuncName, M);
}

typedef SmallVector<Value *, 64> ReaderValueList;

class FunctionReader {
  InputStream *Stream;
  Function *Func;
  Type *IntPtrType;
  uint32_t BBCount;
  SmallVector<BasicBlock *, 10> BasicBlocks;
  ReaderValueList ValueList;
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
    Type *PtrTy = Ty->getPointerTo();
    if (Val->getType()->isPointerTy()) {
      if (Val->getType() != PtrTy)
        Val = new BitCastInst(Val, PtrTy, "", CurrentBB);
    } else {
      Val = new IntToPtrInst(Val, PtrTy, "", CurrentBB);
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
      Placeholder->replaceAllUsesWith(castOperand(NewVal, NULL));
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
      uint32_t Align = ReadAlignmentVal(Stream);
      Value *Ptr = readPtrOperand(Ty);
      return new LoadInst(Ptr, "", false, Align, CurrentBB);
    }
    case Opcodes::INST_STORE: {
      uint32_t Align = ReadAlignmentVal(Stream);
      Value *Val = readScalarOperand();
      Value *Ptr = readPtrOperand(Val->getType());
      return new StoreInst(Val, Ptr, false, Align, CurrentBB);
    }
    case Opcodes::INST_ATOMICRMW: {
      AtomicRMWInst::BinOp Op =
          (AtomicRMWInst::BinOp) Stream->readInt("atomicrmw_op");
      Value *Val = readScalarOperand();
      Value *Ptr = readPtrOperand(Val->getType());
      return new AtomicRMWInst(Op, Ptr, Val, SequentiallyConsistent,
                               CrossThread, CurrentBB);
    }
    case Opcodes::INST_ATOMICCMPXCHG: {
      Value *OldVal = readScalarOperand();
      Value *NewVal = readScalarOperand();
      Value *Ptr = readPtrOperand(OldVal->getType());
      return new AtomicCmpXchgInst(Ptr, OldVal, NewVal, SequentiallyConsistent,
                                   CrossThread, CurrentBB);
    }
    case Opcodes::INST_ALLOCA_FIXED:
    case Opcodes::INST_ALLOCA_VARIABLE: {
      uint32_t TypeSize = Stream->readInt("alloca_size");
      Type *I8Ty = IntegerType::get(Func->getContext(), 8);
      Type *Ty = ArrayType::get(I8Ty, TypeSize);
      Value *ArraySize = NULL;
      if (Opcode == Opcodes::INST_ALLOCA_VARIABLE)
        ArraySize = readScalarOperand();
      Value *Ptr = new AllocaInst(Ty, ArraySize, "", CurrentBB);
      return new PtrToIntInst(Ptr, IntPtrType, "", CurrentBB);
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
    case Opcodes::INST_SWITCH: {
      Value *Condition = readScalarOperand();
      IntegerType *CondTy = dyn_cast<IntegerType>(Condition->getType());
      if (!CondTy)
        report_fatal_error("Switch condition is not of integer type");
      BasicBlock *DefaultBB = readBasicBlockOperand();
      uint32_t CaseCount = Stream->readInt("case_count");
      SwitchInst *Switch = SwitchInst::Create(Condition, DefaultBB,
                                              CaseCount, CurrentBB);
      for (unsigned I = 0; I < CaseCount; ++I) {
        uint64_t CaseVal = Stream->readInt("case_val");
        BasicBlock *Dest = readBasicBlockOperand();
        Switch->addCase(ConstantInt::get(CondTy, CaseVal), Dest);
      }
      return Switch;
    }
    case Opcodes::INST_UNREACHABLE: {
      return new UnreachableInst(Func->getContext(), CurrentBB);
    }
    case Opcodes::INST_SELECT: {
      Value *Op1 = readScalarOperand();
      Value *Op2 = readScalarOperand();
      Value *Op3 = readScalarOperand();
      return SelectInst::Create(Op1, Op2, Op3, "", CurrentBB);
    }
    case Opcodes::INST_PHI: {
      uint32_t Size = Stream->readInt("phi_size") + 1;
      // Read the first operand so that we know the type.
      BasicBlock *BB1 = readBasicBlockOperand();
      Value *Val1 = readScalarOperand();
      PHINode *Phi = PHINode::Create(Val1->getType(), Size, "", CurrentBB);
      Phi->addIncoming(Val1, BB1);
      for (unsigned I = 1; I < Size; ++I) {
        BasicBlock *BB = readBasicBlockOperand();
        Value *Val = readScalarOperand();
        Phi->addIncoming(Val, BB);
      }
      return Phi;
    }
    case Opcodes::INST_CONSTANT_INT: {
      Type *Ty = ReadType(Func->getContext(), Stream);
      uint64_t IntVal = Stream->readInt("constant_int");
      return ConstantInt::get(Ty, IntVal);
    }
    case Opcodes::INST_CONSTANT_FLOAT: {
      union {
        uint8_t Bytes[4];
        float Val;
      } U;
      ReadBytes(Stream, U.Bytes, sizeof(U.Bytes));
      return ConstantFP::get(Func->getContext(), APFloat(U.Val));
    }
    case Opcodes::INST_CONSTANT_DOUBLE: {
      union {
        uint8_t Bytes[8];
        double Val;
      } U;
      ReadBytes(Stream, U.Bytes, sizeof(U.Bytes));
      return ConstantFP::get(Func->getContext(), APFloat(U.Val));
    }
    case Opcodes::INST_CONSTANT_UNDEFVALUE: {
      return UndefValue::get(ReadType(Func->getContext(), Stream));
    }

#define HANDLE_BINARY_INST(LLVM_OP, WIRE_OP) \
    case Opcodes::WIRE_OP: return readBinOp(Instruction::LLVM_OP);
#include "Instructions.def"
#undef HANDLE_BINARY_INST

    case Opcodes::INST_CMP: {
      CmpInst::Predicate Predicate =
          (CmpInst::Predicate) Stream->readInt("predicate");
      Value *Op1 = readScalarOperand();
      Value *Op2 = readScalarOperand();
      if (Predicate >= CmpInst::FIRST_ICMP_PREDICATE &&
          Predicate <= CmpInst::LAST_ICMP_PREDICATE) {
        return new ICmpInst(*CurrentBB, Predicate, Op1, Op2);
      } else if (Predicate >= CmpInst::FIRST_FCMP_PREDICATE &&
                 Predicate <= CmpInst::LAST_FCMP_PREDICATE) {
        return new FCmpInst(*CurrentBB, Predicate, Op1, Op2);
      } else {
        report_fatal_error("Bad comparison predicate");
      }
    }
    case Opcodes::INST_CAST: {
      Instruction::CastOps CastOpcode =
          (Instruction::CastOps) Stream->readInt("cast_opcode");
      Type *Ty = ReadType(Func->getContext(), Stream);
      Value *Val = readScalarOperand();
      return CastInst::Create(CastOpcode, Val, Ty, "", CurrentBB);
    }
    default:
      report_fatal_error("Unrecognized instruction opcode");
  }
}

static void addGlobalsToValueList(ReaderValueList *ValueList, Module *M) {
  for (Module::iterator F = M->begin(), E = M->end(); F != E; ++F) {
    ValueList->push_back(F);
  }
  for (Module::global_iterator GV = M->global_begin(), E = M->global_end();
       GV != E; ++GV) {
    ValueList->push_back(GV);
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

  // TODO: For efficiency, add globals once per module rather than
  // once per function.
  addGlobalsToValueList(&ValueList, Func->getParent());
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
    } else if (!NewInst->getType()->isVoidTy()) {
      ValueList.push_back(NewInst);
    }
  }
  if (FwdRefsResolved != FwdRefs.size()) {
    errs() << FwdRefsResolved << " forward references resolved, but "
           << FwdRefs.size() << " requested\n";
    report_fatal_error("Mismatch in forward references");
  }
}

Constant *ReadGlobalInitializer(InputStream *Stream, LLVMContext &Context,
                                ReaderValueList *ValueList) {
  bool IsZero = Stream->readInt("is_zero");
  uint32_t Size = Stream->readInt("global_size");
  if (IsZero) {
    Type *Ty = ArrayType::get(Type::getInt8Ty(Context), Size);
    return ConstantAggregateZero::get(Ty);
  }
  uint8_t *Buf = new uint8_t[Size];
  assert(Buf);
  for (unsigned I = 0; I < Size; ++I)
    Buf[I] = Stream->readInt("byte");

  Constant *Init;
  uint32_t RelocCount = Stream->readInt("reloc_count");
  if (RelocCount == 0) {
    Init = ConstantDataArray::get(Context, ArrayRef<uint8_t>(Buf, Buf + Size));
  } else {
    SmallVector<Constant *, 10> Elements;
    Type *IntPtrType = GetIntPtrType(Context);
    uint32_t PrevPos = 0;
    for (unsigned I = 0; I < RelocCount; ++I) {
      uint32_t RelocOffset = Stream->readInt("reloc_offset");
      assert(PrevPos + RelocOffset <= Size);
      if (RelocOffset > 0) {
        ArrayRef<uint8_t> Slice(Buf + PrevPos, Buf + PrevPos + RelocOffset);
        Elements.push_back(ConstantDataArray::get(Context, Slice));
      }
      uint32_t RelocValueID = Stream->readInt("reloc_ref");
      Constant *BaseVal = cast<Constant>((*ValueList)[RelocValueID]);
      Constant *Val = ConstantExpr::getPtrToInt(BaseVal, IntPtrType);
      // This assumes little endian.
      uint32_t Addend = *(uint32_t *) (Buf + PrevPos + RelocOffset);
      if (Addend)
        Val = ConstantExpr::getAdd(Val, ConstantInt::get(IntPtrType, Addend));
      Elements.push_back(Val);
      PrevPos += RelocOffset + sizeof(uint32_t);
    }
    if (PrevPos < Size) {
      ArrayRef<uint8_t> Slice(Buf + PrevPos, Buf + Size);
      Elements.push_back(ConstantDataArray::get(Context, Slice));
    }
    if (Elements.size() == 1) {
      Init = Elements[0];
    } else {
      Init = ConstantStruct::getAnon(Context, Elements, true);
    }
  }
  delete[] Buf;
  return Init;
}

GlobalVariable *ReadGlobal(InputStream *Stream, Module *M,
                           ReaderValueList *ValueList) {
  uint32_t Align = ReadAlignmentVal(Stream);
  bool IsConstant = Stream->readInt("is_constant");
  Constant *Init = ReadGlobalInitializer(Stream, M->getContext(), ValueList);
  GlobalVariable *GV = new GlobalVariable(
      *M, Init->getType(), IsConstant, GlobalValue::InternalLinkage, Init, "");
  GV->setAlignment(Align);
  return GV;
}

void ReadModule(InputStream *Stream, Module *M) {
  uint32_t FuncCount = Stream->readInt("function_count");
  SmallVector<Function *, 10> FuncList;
  FuncList.reserve(FuncCount);
  // Create a declaration for each function as a placeholder.
  for (unsigned I = 0; I < FuncCount; ++I) {
    FuncList.push_back(ReadFunctionDecl(Stream, M));
  }

  uint32_t GlobalCount = Stream->readInt("global_count");
  // Create a placeholder for each GlobalVariable so that relocations
  // can refer to these placeholders.  Any type will do here.
  Type *PlaceholderType = Type::getInt8Ty(M->getContext());
  for (unsigned I = 0; I < GlobalCount; ++I) {
    new GlobalVariable(*M, PlaceholderType, false,
                       GlobalValue::ExternalLinkage, NULL);
  }

  // Fill in the definition of each GlobalVariable.
  ReaderValueList ValueList;
  addGlobalsToValueList(&ValueList, M);
  for (unsigned I = 0; I < GlobalCount; ++I) {
    uint32_t Idx = FuncCount + I;
    GlobalVariable *Placeholder = cast<GlobalVariable>(ValueList[Idx]);
    GlobalVariable *RealGlobal = ReadGlobal(Stream, M, &ValueList);
    Placeholder->replaceAllUsesWith(
        ConstantExpr::getBitCast(RealGlobal, Placeholder->getType()));
    Placeholder->eraseFromParent();
    ValueList[Idx] = RealGlobal;
  }

  // Fill in the body of each function.
  for (unsigned I = 0; I < FuncCount; ++I) {
    FunctionReader(Stream, FuncList[I]).read();
  }
}

void ReadModuleFromFile(FILE *FH, Module *M) {
  InputStream Stream(FH);
  ReadModule(&Stream, M);
}

}  // namespace llvm
