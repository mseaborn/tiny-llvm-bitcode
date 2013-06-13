
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
    INST_ATOMICRMW, // TODO: Use a higher opcode, since this is rare
    INST_ALLOCA_FIXED,
    INST_ALLOCA_VARIABLE,
    INST_CALL,
    INST_BR_UNCOND,
    INST_BR_COND,
    INST_SWITCH,
    INST_UNREACHABLE,
    INST_SELECT,
    INST_PHI,
    // Binary operators
#define HANDLE_BINARY_INST(LLVM_OP, WIRE_OP) WIRE_OP,
#include "Instructions.def"
#undef HANDLE_BINARY_INST
    INST_CMP,
    INST_CAST,
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
  } else if (StringRef(FuncName).startswith("llvm.memset.")) {
    ArgTypes[0] = PtrTy;
  } else if (FuncName == "llvm.nacl.read.tp") {
    ReturnType = PtrTy;
  }
  FunctionType *FTy = FunctionType::get(ReturnType, ArgTypes, false);
  return Function::Create(FTy, Linkage, FuncName, M);
}


class ValueWriterMap {
  uint32_t NextValueID;
  DenseMap<Value *, uint32_t> ValueToID;

public:
  ValueWriterMap(): NextValueID(0) {}

  void addIDForValue(Value *Val) {
    ValueToID[Val] = NextValueID++;
  }
  bool hasIDForValue(Value *Val) {
    return ValueToID.count(Val) == 1;
  }
  uint32_t getIDForValue(Value *Val) {
    assert(hasIDForValue(Val));
    return ValueToID[Val];
  }
  void addGlobals(Module *M) {
    for (Module::iterator F = M->begin(), E = M->end(); F != E; ++F) {
      addIDForValue(F);
    }
    for (Module::global_iterator GV = M->global_begin(), E = M->global_end();
         GV != E; ++GV) {
      addIDForValue(GV);
    }
  }
};

class FunctionWriter {
  OutputStream *Stream;
  Function *Func;
  DenseMap<BasicBlock *, uint32_t> BasicBlockMap;

  ValueWriterMap ValueMap;
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
  NextFwdRefID = 0;

  // TODO: For efficiency, add globals once per module rather than
  // once per function.
  ValueMap.addGlobals(Func->getParent());
  // Allocate value IDs for the function's arguments.
  for (Function::arg_iterator Arg = Func->arg_begin(), E = Func->arg_end();
       Arg != E; ++Arg) {
    ValueMap.addIDForValue(Arg);
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
  if (isa<BasicBlock>(Val) || ValueMap.hasIDForValue(Val))
    return;
  // GlobalValues should already be in ValueMap.
  assert(!isa<GlobalValue>(Val));
  if (ConstantInt *C = dyn_cast<ConstantInt>(Val)) {
    Stream->writeInt(Opcodes::INST_CONSTANT_INT, "opcode");
    // TODO: Could we omit the type here to save space?
    WriteType(Stream, C->getType());
    Stream->writeInt(C->getZExtValue(), "constant_int");
    ValueMap.addIDForValue(Val);
    return;
  }
  Stream->writeInt(Opcodes::INST_FWD_REF, "opcode");
  WriteType(Stream, Val->getType());
  ValueMap.addIDForValue(Val);
  FwdRefs[Val] = NextFwdRefID++;
}

void FunctionWriter::writeOperand(Value *Val) {
  Val = stripPtrCasts(Val);
  Stream->writeInt(ValueMap.getIDForValue(Val), "val");
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
  // First, ensure the instruction's operands have been allocated
  // value IDs.  SwitchInsts use array/vector constants internally, so
  // skip them here.
  if (!isa<SwitchInst>(Inst)) {
    for (unsigned I = 0, E = Inst->getNumOperands(); I < E; ++I) {
      materializeOperand(Inst->getOperand(I));
    }
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
    case Instruction::AtomicRMW: {
      AtomicRMWInst *Op = cast<AtomicRMWInst>(Inst);
      Stream->writeInt(Opcodes::INST_ATOMICRMW, "opcode");
      // TODO: Ensure that these opcode values stay stable.
      // TODO: Check the Ordering and SynchScope fields.
      // TODO: Handle "volatile" attribute.
      Stream->writeInt(Op->getOperation(), "atomicrmw_op");
      // Swap operands and put pointer second for consistency with INST_STORE.
      writeOperand(Op->getOperand(1));
      writeOperand(Op->getOperand(0));
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
    case Instruction::Switch: {
      SwitchInst *Switch = cast<SwitchInst>(Inst);
      // For SwitchInsts, materializeOperand() only works for the
      // condition operand, not other operands, so we call it
      // explicitly here.
      materializeOperand(Switch->getCondition());
      Stream->writeInt(Opcodes::INST_SWITCH, "opcode");
      writeOperand(Switch->getCondition());
      writeBasicBlockOperand(Switch->getDefaultDest());

      uint32_t CaseCount = 0;
      for (SwitchInst::CaseIt Case = Switch->case_begin(),
             E = Switch->case_end(); Case != E; ++Case) {
        IntegersSubset CaseRanges = Case.getCaseValueEx();
        for (unsigned I = 0, E = CaseRanges.getNumItems(); I < E ; ++I) {
          ++CaseCount;
        }
      }
      Stream->writeInt(CaseCount, "case_count");

      for (SwitchInst::CaseIt Case = Switch->case_begin(),
             E = Switch->case_end(); Case != E; ++Case) {
        IntegersSubset CaseRanges = Case.getCaseValueEx();
        for (unsigned I = 0, E = CaseRanges.getNumItems(); I < E ; ++I) {
          uint64_t Low =
              CaseRanges.getItem(I).getLow().toConstantInt()->getZExtValue();
          uint64_t High =
              CaseRanges.getItem(I).getHigh().toConstantInt()->getZExtValue();
          // TODO: Handle ranges once I work out how to test this.
          // Ranges do not seem to be expressible in the assembly
          // syntax, and they are not automatically introduced by the
          // assembler.
          assert(High == Low);
          Stream->writeInt(Low, "case_val");
          writeBasicBlockOperand(Case.getCaseSuccessor());
        }
      }
      break;
    }
    case Instruction::Unreachable:
      Stream->writeInt(Opcodes::INST_UNREACHABLE, "opcode");
      break;
    case Instruction::Select:
      Stream->writeInt(Opcodes::INST_SELECT, "opcode");
      writeOperand(Inst->getOperand(0));
      writeOperand(Inst->getOperand(1));
      writeOperand(Inst->getOperand(2));
      break;
    case Instruction::PHI: {
      PHINode *Phi = cast<PHINode>(Inst);
      Stream->writeInt(Opcodes::INST_PHI, "opcode");
      uint32_t Size = Phi->getNumIncomingValues();
      // Empty phi nodes are not allowed.
      assert(Size > 0);
      // Reduce the size by 1 to make the numbers smaller, making this
      // a tiny bit more compressible, and to remove the need for a
      // check in the reader.
      Stream->writeInt(Size - 1, "phi_size");
      // TODO: Merge consecutive phi nodes together so that the basic
      // block operands are not repeated.
      for (unsigned I = 0; I < Size; ++I) {
        writeBasicBlockOperand(Phi->getIncomingBlock(I));
        writeOperand(Phi->getIncomingValue(I));
      }
      break;
    }
    case Instruction::IntToPtr:
    case Instruction::PtrToInt:
      // These casts are implicit.
      break;
    default:
      if (BinaryOperator *Op = dyn_cast<BinaryOperator>(Inst)) {
        Stream->writeInt(getOpcodeToWrite(Inst), "opcode");
        writeOperand(Op->getOperand(0));
        writeOperand(Op->getOperand(1));
        break;
      }
      if (CmpInst *Op = dyn_cast<CmpInst>(Inst)) {
        Stream->writeInt(Opcodes::INST_CMP, "opcode");
        // TODO: Ensure that getPredicate() values stay stable.
        Stream->writeInt(Op->getPredicate(), "predicate");
        writeOperand(Op->getOperand(0));
        writeOperand(Op->getOperand(1));
        break;
      }
      if (isa<BitCastInst>(Inst) && Inst->getType()->isPointerTy()) {
        // Bitcasts of pointers are implicit, but other bitcasts are not.
        break;
      }
      if (CastInst *Op = dyn_cast<CastInst>(Inst)) {
        Stream->writeInt(Opcodes::INST_CAST, "opcode");
        // TODO: Ensure that these cast opcode values stay stable.
        Stream->writeInt(Op->getOpcode(), "cast_opcode");
        WriteType(Stream, Op->getType());
        writeOperand(Op->getOperand(0));
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
        ValueMap.addIDForValue(Inst);
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
    case Opcodes::INST_ATOMICRMW: {
      AtomicRMWInst::BinOp Op =
          (AtomicRMWInst::BinOp) Stream->readInt("atomicrmw_op");
      Value *Val = readScalarOperand();
      Value *Ptr = readPtrOperand(Val->getType());
      return new AtomicRMWInst(Op, Ptr, Val, SequentiallyConsistent,
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


class FlattenedConstant {
  uint8_t *Buf;
  uint32_t Offset;
  uint32_t Size;

  struct Reloc {
    unsigned RelOffset;
    GlobalValue *GlobalRef;
  };
  typedef SmallVector<Reloc, 10> RelocArray;
  RelocArray Relocs;

  void writeSimpleElement(Constant *C) {
    if (ArrayType *Ty = dyn_cast<ArrayType>(C->getType())) {
      assert(Ty->getElementType()->isIntegerTy(8));
      uint32_t Size = Ty->getNumElements();
      if (isa<ConstantAggregateZero>(C)) {
        // Nothing to do: Buf is already zero-initialized.
      } else {
        ConstantDataSequential *CD = cast<ConstantDataSequential>(C);
        StringRef Data = CD->getRawDataValues();
        memcpy(Buf + Offset, Data.data(), Size);
      }
      Offset += Size;
      return;
    }
    if (C->getType()->isIntegerTy(32)) {
      ConstantExpr *CE = dyn_cast<ConstantExpr>(C);
      assert(CE);
      if (CE->getOpcode() == Instruction::Add) {
        ConstantInt *Addend = dyn_cast<ConstantInt>(CE->getOperand(1));
        assert(Addend);
        // This assumes little endian.
        *(uint32_t *) (Buf + Offset) = Addend->getZExtValue();
        CE = dyn_cast<ConstantExpr>(CE->getOperand(0));
        assert(CE);
      }
      assert(CE->getOpcode() == Instruction::PtrToInt);
      GlobalValue *GV = dyn_cast<GlobalValue>(CE->getOperand(0));
      assert(GV);
      Reloc NewRel = { Offset, GV };
      Relocs.push_back(NewRel);
      Offset += sizeof(uint32_t);
      return;
    }
    errs() << "Value: " << *C << "\n";
    report_fatal_error("Global initializer is not a SimpleElement");
  }

public:
  FlattenedConstant(uint32_t Size, Constant *C): Offset(0), Size(Size) {
    Buf = new uint8_t[Size];
    memset(Buf, 0, Size);
    if (ConstantStruct *CS = dyn_cast<ConstantStruct>(C)) {
      assert(CS->getType()->isPacked());
      assert(!CS->getType()->hasName());
      for (unsigned I = 0; I < CS->getNumOperands(); ++I)
        writeSimpleElement(CS->getOperand(I));
    } else {
      writeSimpleElement(C);
    }
    assert(Offset == Size);
  }
  ~FlattenedConstant() {
    delete[] Buf;
  }

  void write(OutputStream *Stream, ValueWriterMap *ValueMap) {
    // TODO: Write bulk data more efficiently than this.
    for (unsigned I = 0; I < Size; ++I)
      Stream->writeInt(Buf[I], "byte");
    // Write relocations.
    Stream->writeInt(Relocs.size(), "reloc_count");
    uint32_t PrevPos = 0;
    for (RelocArray::iterator Rel = Relocs.begin(), E = Relocs.end();
         Rel != E; ++Rel) {
      Stream->writeInt(Rel->RelOffset - PrevPos, "reloc_offset");
      Stream->writeInt(ValueMap->getIDForValue(Rel->GlobalRef), "reloc_ref");
      PrevPos = Rel->RelOffset + sizeof(uint32_t);
    }
  }
};

void WriteGlobal(OutputStream *Stream, GlobalVariable *GV) {
  // Use DataLayout as a convenience for getTypeAllocSize().
  // TODO: Handle "align" attribute.
  DataLayout DL("");
  Type *Ty = GV->getType()->getPointerElementType();
  Stream->writeInt(GV->isConstant(), "is_constant");
  assert(GV->hasInitializer());
  bool IsZero = GV->getInitializer()->isNullValue();
  Stream->writeInt(IsZero, "is_zero");
  uint32_t Size = DL.getTypeAllocSize(Ty);
  Stream->writeInt(Size, "global_size");
  if (!IsZero) {
    FlattenedConstant Buffer(Size, GV->getInitializer());
    ValueWriterMap ValueMap;
    // Add globals to ValueMap.  TODO: For efficiency, do this once
    // per module rather than once per function.
    ValueMap.addGlobals(GV->getParent());
    Buffer.write(Stream, &ValueMap);
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
  bool IsConstant = Stream->readInt("is_constant");
  Constant *Init = ReadGlobalInitializer(Stream, M->getContext(), ValueList);
  return new GlobalVariable(*M, Init->getType(), IsConstant,
                            GlobalValue::InternalLinkage,
                            Init, "");
}


void WriteModule(OutputStream *Stream, Module *M) {
  Stream->writeInt(M->getFunctionList().size(), "function_count");
  for (Module::iterator Func = M->begin(), E = M->end(); Func != E; ++Func) {
    WriteFunctionDecl(Stream, Func);
  }

  Stream->writeInt(M->getGlobalList().size(), "global_count");
  for (Module::global_iterator GV = M->global_begin(), E = M->global_end();
       GV != E; ++GV) {
    WriteGlobal(Stream, GV);
  }

  for (Module::iterator Func = M->begin(), E = M->end(); Func != E; ++Func) {
    FunctionWriter(Stream, Func).write();
  }
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


void WriteModuleToFile(FILE *FH, Module *M) {
  OutputStream Stream(FH);
  WriteModule(&Stream, M);
}

void ReadModuleFromFile(FILE *FH, Module *M) {
  InputStream Stream(FH);
  ReadModule(&Stream, M);
}

}
