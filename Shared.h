#ifndef SHARED_H_
#define SHARED_H_

namespace llvm {

IntegerType *GetIntPtrType(LLVMContext &Context) {
  return IntegerType::get(Context, 32);
}

}

#endif
