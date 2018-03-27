	.file	"test.ll"
	.text
	.globl	main
	.align	16, 0x90
	.type	main,@function
main:                                   # @main
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%rax
.Ltmp1:
	.cfi_def_cfa_offset 16
	movl	$.Lfmt, %edi
	movl	$.Ltmp, %esi
	xorl	%eax, %eax
	callq	printf
	xorl	%eax, %eax
	popq	%rdx
	ret
.Ltmp2:
	.size	main, .Ltmp2-main
	.cfi_endproc

	.type	.Lfmt,@object           # @fmt
	.section	.rodata.str1.1,"aMS",@progbits,1
.Lfmt:
	.asciz	"%s\n"
	.size	.Lfmt, 4

	.type	.Ltmp,@object           # @tmp
.Ltmp:
	.asciz	"hello world"
	.size	.Ltmp, 12


	.section	".note.GNU-stack","",@progbits
