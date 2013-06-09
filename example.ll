
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

define void @unconditional_branch() {
  br label %bb
bb:
  ret void
}

define void @conditional_branch(i1 %cond) {
  br i1 %cond, label %iftrue, label %iffalse
iftrue:
  ret void
iffalse:
  ret void
}

define i16 @forward_ref(i32 %ptr) {
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
