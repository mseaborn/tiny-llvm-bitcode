
define internal void @empty() {
  ret void
}

define internal void @other_types(double %d, float %f) {
  ret void
}

define internal i32 @func(i32 %arg1, i32 %arg2) {
  ret i32 %arg1
  ret i32 %arg2
}

define internal i16 @load(i32 %ptr) {
  %ptr.p = inttoptr i32 %ptr to i16*
  %val = load i16* %ptr.p
  ret i16 %val
}

define internal void @store(i32 %ptr, i32 %val) {
  %ptr.p = inttoptr i32 %ptr to i32*
  store i32 %val, i32* %ptr.p
  ret void
}

define internal void @unconditional_branch() {
  br label %bb
bb:
  ret void
}

define internal void @conditional_branch(i1 %cond) {
  br i1 %cond, label %iftrue, label %iffalse
iftrue:
  ret void
iffalse:
  ret void
}

define internal i16 @forward_ref(i32 %ptr) {
  br label %bb1
bb2:
  ret i16 %val
  ; Test multiple references.
  ret i16 %val
bb1:
  %ptr.p = inttoptr i32 %ptr to i16*
  %val = load i16* %ptr.p
  br label %bb2
}

define internal i16 @forward_ref2(i32 %ptr) {
  br label %bb1
bb2:
  ; Test multiple references.
  ret i16 %val2
  ret i16 %val1
bb1:
  %ptr.p1 = inttoptr i32 %ptr to i16*
  %val1 = load i16* %ptr.p1
  %ptr.p2 = inttoptr i32 %ptr to i16*
  %val2 = load i16* %ptr.p2
  br label %bb2
}

define internal i32 @global_ref() {
  %ptr = ptrtoint i32 ()* @global_ref to i32
  ret i32 %ptr
}

define internal i16 @direct_call(i32 %ptr) {
  %result = call i16 @load(i32 %ptr)
  ret i16 %result
}

define internal i16 @indirect_call(i32 %func, i64 %arg) {
  %func.p = inttoptr i32 %func to i16 (i64)*
  %result = call i16 %func.p(i64 %arg)
  ret i16 %result
}

define internal i32 @alloca_fixed() {
  %ptr.p = alloca [10 x i8]
  %ptr = ptrtoint [10 x i8]* %ptr.p to i32
  ret i32 %ptr
}

define internal i32 @alloca_variable(i32 %size) {
  %ptr.p = alloca [12 x i8], i32 %size
  %ptr = ptrtoint [12 x i8]* %ptr.p to i32
  ret i32 %ptr
}

define internal i32 @const_int() {
  ret i32 123
}

declare void @llvm.memcpy.p0i8.p0i8.i32(i8*, i8*, i32, i32, i1)

define internal void @call_memcpy(i32 %dest, i32 %src, i32 %size) {
  %dest.p = inttoptr i32 %dest to i8*
  %src.p = inttoptr i32 %src to i8*
  call void @llvm.memcpy.p0i8.p0i8.i32(i8* %dest.p, i8* %src.p, i32 %size, i32 1, i1 0)
  ret void
}

declare i8* @llvm.nacl.read.tp()

define i32 @get_thread_pointer() {
  %ptr.p = call i8* @llvm.nacl.read.tp()
  %ptr = ptrtoint i8* %ptr.p to i32
  ret i32 %ptr
}
