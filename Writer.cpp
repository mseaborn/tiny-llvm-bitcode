
#include <stdio.h>
#include <stdint.h> // XXX

#include "llvm/ADT/DenseMap.h"
#include "llvm/IR/DataLayout.h"
#include "llvm/IR/Function.h"
#include "llvm/IR/Instructions.h"
#include "llvm/IR/Module.h"
#include "llvm/Support/raw_ostream.h"

#include "OpcodeEnums.h"
#include "Shared.h"


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

void WriteAlignmentVal(OutputStream *Stream, uint32_t Alignment) {
  Stream->writeInt(Log2_32(Alignment) + 1, "align");
}

void WriteBytes(OutputStream *Stream, const uint8_t *Data, uint32_t Size) {
  // TODO: Write bulk data more efficiently than this.
  for (unsigned I = 0; I < Size; ++I)
    Stream->writeInt(Data[I], "byte");
}

void WriteString(OutputStream *Stream, const std::string &String) {
  Stream->writeInt(String.size(), "string_length");
  WriteBytes(Stream, (uint8_t *) String.data(), String.size());
  Stream->writeMarker(("string=" + String).c_str());
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
  if (isa<Constant>(Val)) {
    // GlobalValues should already be in ValueMap.
    assert(!isa<GlobalValue>(Val));
    if (ConstantInt *C = dyn_cast<ConstantInt>(Val)) {
      Stream->writeInt(Opcodes::INST_CONSTANT_INT, "opcode");
      // TODO: Could we omit the type here to save space?
      WriteType(Stream, C->getType());
      Stream->writeInt(C->getZExtValue(), "constant_int");
    } else if (ConstantFP *CF = dyn_cast<ConstantFP>(Val)) {
      if (CF->getType()->isFloatTy()) {
        Stream->writeInt(Opcodes::INST_CONSTANT_FLOAT, "opcode");
      } else if (CF->getType()->isDoubleTy()) {
        Stream->writeInt(Opcodes::INST_CONSTANT_DOUBLE, "opcode");
      } else {
        report_fatal_error("Unrecognized ConstantFP type");
      }
      APInt Data = CF->getValueAPF().bitcastToAPInt();
      assert(Data.getBitWidth() % 8 == 0);
      WriteBytes(Stream, (uint8_t *) Data.getRawData(), Data.getBitWidth() / 8);
    } else if (isa<UndefValue>(Val)) {
      Stream->writeInt(Opcodes::INST_CONSTANT_UNDEFVALUE, "opcode");
      WriteType(Stream, Val->getType());
    } else {
      errs() << "Value: " << *Val << "\n";
      report_fatal_error("Unhandled constant");
    }
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
      // TODO: Handle "volatile" and atomic attributes.
      LoadInst *Load = cast<LoadInst>(Inst);
      Stream->writeInt(Opcodes::INST_LOAD, "opcode");
      WriteType(Stream, Load->getType());
      WriteAlignmentVal(Stream, Load->getAlignment());
      writeOperand(Load->getPointerOperand());
      break;
    }
    case Instruction::Store: {
      // TODO: Handle "volatile" and atomic attributes.
      StoreInst *Store = cast<StoreInst>(Inst);
      Stream->writeInt(Opcodes::INST_STORE, "opcode");
      WriteAlignmentVal(Stream, Store->getAlignment());
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
    case Instruction::AtomicCmpXchg: {
      AtomicCmpXchgInst *Op = cast<AtomicCmpXchgInst>(Inst);
      // TODO: Check the Ordering and SynchScope fields.
      // TODO: Handle "volatile" attribute.
      Stream->writeInt(Opcodes::INST_ATOMICCMPXCHG, "opcode");
      writeOperand(Op->getOperand(1));
      writeOperand(Op->getOperand(2));
      // Put pointer last to simplify type handling in the reader.
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
    WriteBytes(Stream, Buf, Size);
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

void WriteModuleToFile(FILE *FH, Module *M) {
  OutputStream Stream(FH);
  WriteModule(&Stream, M);
}

}  // namespace llvm
