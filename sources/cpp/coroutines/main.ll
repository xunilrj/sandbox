define i32 @main() {
entry:
  call void @print(i32 4)
  call void @print(i32 5)
  call void @print(i32 6)
  ret i32 0
}