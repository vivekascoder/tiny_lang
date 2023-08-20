define i32 @add_two_integers(i32 %x, i32 %y) {
  %z = add i32 %x, %y
  ret i32 %z
}

define void @print_result(i32 %result) {
  %ptr = bitcast i32* %result to i8*
  call void @llvm.print.i32(i8* %ptr)
  ret void
}

define void @main() {
  %x = alloca i32
  %y = alloca i32
  store i32 1, i32* %x
  store i32 2, i32* %y
  %z = call i32 @add_two_integers(i32* %x, i32* %y)
  call void @print_result(i32 %z)
  ret void
}