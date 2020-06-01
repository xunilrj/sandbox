	.text
	.file	"main.001.c"
	.section	.text.main,"",@
	.globl	main                    # -- Begin function main
	.type	main,@function
main:                                   # @main
	.functype	main (i32, i32) -> (i32)
	.local  	i32
# %bb.0:
	global.get	__stack_pointer
	i32.const	16
	i32.sub 
	local.tee	2
	i32.const	0
	i32.store	12
	local.get	2
	local.get	0
	i32.store	8
	local.get	2
	local.get	1
	i32.store	4
	i32.const	0
                                        # fallthrough-return
	end_function
.Lfunc_end0:
	.size	main, .Lfunc_end0-main
                                        # -- End function
	.ident	"clang version 10.0.0-4ubuntu1 "
	.globaltype	__stack_pointer, i32
	.section	.custom_section.producers,"",@
	.int8	1
	.int8	12
	.ascii	"processed-by"
	.int8	1
	.int8	5
	.ascii	"clang"
	.int8	15
	.ascii	"10.0.0-4ubuntu1"
	.section	.text.main,"",@
