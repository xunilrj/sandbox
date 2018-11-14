; Copied directly from the documentation
; Declare the string constant as a global constant.
@.str = private unnamed_addr constant [13 x i8] c"hello world\0A\00"

; External declaration of the puts function
declare i32 @puts(i8* nocapture) nounwind
declare i8* @malloc(i32)
declare void @llvm.coro.resume(i8*)


; Definition of main function
define i32 @main() { ; i32()*
    %hdl = call i8* @malloc(i32 4)
    call void @llvm.coro.resume(i8* %hdl)

    ; Convert [13 x i8]* to i8  *...
    %cast210 = getelementptr [13 x i8],[13 x i8]* @.str, i64 0, i64 0

    ; Call puts function to write out the string to stdout.
    call i32 @puts(i8* %cast210)
    ret i32 0
}

; Named metadata
!0 = !{i32 42, null, !"string"}
!foo = !{!0}