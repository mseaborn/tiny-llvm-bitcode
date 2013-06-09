
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
