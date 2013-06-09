
define void @empty() {
  ret void
}

define void @other_types(double %d, float %f) {
  ret void
}

define i32 @func(i32 %arg1, i32 %arg2) {
  ret i32 %arg1
  ret i32 %arg2
}

define i16 @load(i32 %ptr) {
  %ptr.p = inttoptr i32 %ptr to i16*
  %val = load i16* %ptr.p
  ret i16 %val
}

define void @store(i32 %ptr, i32 %val) {
  %ptr.p = inttoptr i32 %ptr to i32*
  store i32 %val, i32* %ptr.p
  ret void
}

define void @branch() {
  br label %bb
bb:
  ret void
}
