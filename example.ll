
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

define internal i32 @atomicrmw(i32 %ptr, i32 %val) {
  %ptr.p = inttoptr i32 %ptr to i32*
  %result = atomicrmw xchg i32* %ptr.p, i32 %val seq_cst
  ret i32 %result
}

define internal i32 @cmpxchg(i32 %ptr, i32 %old, i32 %new) {
  %ptr.p = inttoptr i32 %ptr to i32*
  %result = cmpxchg i32* %ptr.p, i32 %old, i32 %new seq_cst
  ret i32 %result
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

define internal void @switch(i32 %x) {
  switch i32 %x, label %default [i32 123, label %case1
                                 i32 124, label %case1
                                 i32 456, label %case2]
case1:
  ret void
case2:
  ret void
default:
  ret void
}

; SwitchInst's operand must be handled specially, so test that here.
define internal void @switch_const() {
  switch i32 123, label %next [i32 0, label %next]
next:
  ret void
}

define internal void @unreachable() {
  unreachable
}

define internal i32 @select(i1 %cond, i32 %val1, i32 %val2) {
  %result = select i1 %cond, i32 %val1, i32 %val2
  ret i32 %result
}

define internal i32 @phi(i1 %cond) {
  br i1 %cond, label %iftrue, label %iffalse
iftrue:
  br label %exit
iffalse:
  br label %exit
exit:
  %phi = phi i32 [ 123, %iftrue ], [ 456, %iffalse ]
  ret i32 %phi
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

define internal float @const_float() {
  ret float 1.25
}

define internal double @const_double() {
  ret double 1.5
}

define internal i16 @const_undef() {
  ret i16 undef
}

; Test that we can declare intrinsics and that their pointer types are
; preserved.
declare void @llvm.memcpy.p0i8.p0i8.i32(i8*, i8*, i32, i32, i1)
declare void @llvm.memset.p0i8.i32(i8* nocapture, i8, i32, i32, i1)

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

define void @arith(i16 %x, i16 %y) {
  ; BinaryOperators
  add i16 %x, %y
  sub i16 %x, %y
  mul i16 %x, %y
  udiv i16 %x, %y
  sdiv i16 %x, %y
  urem i16 %x, %y
  srem i16 %x, %y
  shl i16 %x, %y
  lshr i16 %x, %y
  ashr i16 %x, %y
  and i16 %x, %y
  or i16 %x, %y
  xor i16 %x, %y

  ; Comparisons
  icmp eq i16 %x, %y
  icmp ne i16 %x, %y

  ret void
}

define void @fp_arith(double %x, double %y) {
  fadd double %x, %y
  fsub double %x, %y
  fmul double %x, %y
  fdiv double %x, %y
  frem double %x, %y
  ret void
}

define void @casts(i32 %val) {
  zext i32 %val to i64
  sext i32 %val to i64
  trunc i32 %val to i16
  uitofp i32 %val to double
  sitofp i32 %val to double
  bitcast i32 %val to float
  ret void
}

@var = internal global [123 x i8] zeroinitializer
@const_var = internal constant [123 x i8] zeroinitializer

@data = internal global [4 x i8] c"foo!"
@reloc_var = internal global i32 ptrtoint (i32* @reloc_var to i32)
@reloc_func = internal global i32 ptrtoint (void ()* @empty to i32)

@addend_ptr = internal global i32 add (i32 ptrtoint (i32* @reloc_var to i32), i32 1)

@compound = internal global <{ [3 x i8], i32, [5 x i8], i32, [5 x i8] }>
    <{ [3 x i8] c"foo",
       i32 ptrtoint (void ()* @empty to i32),
       [5 x i8] c"data1",
       i32 ptrtoint (void ()* @unconditional_branch to i32),
       [5 x i8] c"data2" }>

define i32 @get_var_addr() {
  %ptr = ptrtoint [123 x i8]* @var to i32
  ret i32 %ptr
}

define i32 @cast_of_var_addr() {
  ; bitcasts of globals are allowed.
  %bc = bitcast [123 x i8]* @var to i32*
  %val1 = load i32* %bc
  ret i32 %val1
}

define i32 @load_without_cast() {
  %val = load i32* @reloc_var
  ret i32 %val
}
