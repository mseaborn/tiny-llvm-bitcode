
@var = internal global [123 x i8] zeroinitializer

define i32 @cast_of_var_addr() {
  ; ptrtoint+inttoptr of globals are allowed too, though this is
  ; redundant because a bitcast is shorter.  This will be read back as
  ; a bitcast.
  %ptr1 = ptrtoint [123 x i8]* @var to i32
  %ptr2 = inttoptr i32 %ptr1 to i32*
  %val2 = load i32* %ptr2
  ret i32 %val2
}
