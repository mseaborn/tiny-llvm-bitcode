#ifndef SHARED_H_
#define SHARED_H_

namespace llvm {

IntegerType *GetIntPtrType(LLVMContext &Context) {
  return IntegerType::get(Context, 32);
}

bool instructionHasValueId(Value *Inst) {
  // These instructions are implicit.
  if (isa<IntToPtrInst>(Inst) ||
      isa<PtrToIntInst>(Inst) ||
      (isa<BitCastInst>(Inst) && Inst->getType()->isPointerTy()))
    return false;
  return !Inst->getType()->isVoidTy();
}

}

#endif
