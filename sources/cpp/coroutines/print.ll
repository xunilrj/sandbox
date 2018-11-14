declare i32 @putchar(i32) #1

define i32 @main() #0 {
entry:
  %retval = alloca i32, align 4
  store i32 0, i32* %retval, align 4
  %call = call i32 @putchar(i32 65)
  ret i32 0
}
