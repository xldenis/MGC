	.section	__TEXT,__text,regular,pure_instructions
	.align	4, 0x90
_t:
	pushq	%rbp
	pushq	%r15
	pushq	%r14
	pushq	%r12
	pushq	%rbx
	subq	$64, %rsp
	movq	%rdi, %rbx
	movq	$4, 32(%rsp)
	leaq	8(%rsp), %rdi
	leaq	32(%rsp), %r9
	callq	_append
	movl	8(%rsp), %r14d
	movl	12(%rsp), %r15d
	movl	16(%rsp), %r12d
	movq	24(%rsp), %rbp
	movq	%rbp, 56(%rsp)
	movl	%r12d, 48(%rsp)
	movl	%r15d, 44(%rsp)
	movl	%r14d, 40(%rsp)
	movq	56(%rsp), %rax
	movq	(%rax), %rdi
	callq	_print.tinteger
	movl	$10, %edi
	callq	_print.trune
	movq	%rbp, 16(%rbx)
	movl	%r12d, 8(%rbx)
	movl	%r15d, 4(%rbx)
	movl	%r14d, (%rbx)
	addq	$64, %rsp
	popq	%rbx
	popq	%r12
	popq	%r14
	popq	%r15
	popq	%rbp
	ret

	.globl	_main
	.align	4, 0x90
_main:
	pushq	%rbx
	subq	$32, %rsp
	xorl	%edi, %edi
	movl	$10, %esi
	movl	$1, %edx
	callq	_new_slice
	movq	%rax, %rbx
	movq	16(%rbx), %r8
	movl	8(%rbx), %ecx
	movl	(%rbx), %esi
	movl	4(%rbx), %edx
	leaq	8(%rsp), %rdi
	callq	_t
	movl	8(%rsp), %eax
	movl	12(%rsp), %ecx
	movl	16(%rsp), %edx
	movq	24(%rsp), %rsi
	movq	%rsi, 16(%rbx)
	movl	%edx, 8(%rbx)
	movl	%ecx, 4(%rbx)
	movl	%eax, (%rbx)
	movq	16(%rbx), %rax
	movq	(%rax), %rax
	addq	$32, %rsp
	popq	%rbx
	ret

	.globl	_new_slice
	.align	4, 0x90
_new_slice:
	pushq	%rbp
	pushq	%r15
	pushq	%r14
	pushq	%rbx
	pushq	%rax
	movl	%edx, %r15d
	movl	%esi, %ebp
	movl	%edi, %r14d
	movl	$24, %edi
	callq	_malloc
	movq	%rax, %rbx
	movl	%r15d, %edi
	imull	%ebp, %edi
	callq	_malloc
	movl	%r14d, (%rbx)
	movl	%ebp, 4(%rbx)
	movl	%r15d, 8(%rbx)
	movq	%rax, 16(%rbx)
	movq	%rbx, %rax
	addq	$8, %rsp
	popq	%rbx
	popq	%r14
	popq	%r15
	popq	%rbp
	ret

	.globl	_del_slice
	.align	4, 0x90
_del_slice:
	ret

	.globl	_resize
	.align	4, 0x90
_resize:
	pushq	%rbp
	pushq	%r15
	pushq	%r14
	pushq	%r12
	pushq	%rbx
	movl	%esi, %r15d
	movq	%rdi, %rbx
	cmpl	%r15d, 4(%rbx)
	jg	LBB4_2
	movq	16(%rbx), %r14
	movl	8(%rbx), %ebp
	movl	%ebp, %edi
	imull	%r15d, %edi
	imull	(%rbx), %ebp
	callq	_malloc
	movq	%rax, %r12
	movq	%r12, %rdi
	movq	%r14, %rsi
	movl	%ebp, %edx
	callq	_memcpy
	movq	%r14, %rdi
	callq	_free
	movq	%r12, 16(%rbx)
	movl	%r15d, 4(%rbx)
LBB4_2:
	popq	%rbx
	popq	%r12
	popq	%r14
	popq	%r15
	popq	%rbp
	ret

	.globl	_copy
	.align	4, 0x90
_copy:
	pushq	%rbx
	movl	%edi, %ebx
	movq	24(%rsp), %rsi
	cmpl	%r8d, %ebx
	cmovgl	%r8d, %ebx
	imull	%ebx, %edx
	movq	%rcx, %rdi
	callq	_memcpy
	movl	%ebx, %eax
	popq	%rbx
	ret

	.globl	_append
	.align	4, 0x90
_append:
	pushq	%rbp
	pushq	%r15
	pushq	%r14
	pushq	%r13
	pushq	%r12
	pushq	%rbx
	subq	$56, %rsp
	movq	%r9, 48(%rsp)
	movq	%r8, 40(%rsp)
	movl	%ecx, %ebp
	movq	%rdx, 32(%rsp)
	movl	%esi, %ebx
	movq	%rdi, %r13
	leal	(%rdx,%rdx), %esi
	leal	1(%rbx), %edi
	movl	%ebp, %edx
	callq	_new_slice
	movq	%rax, %r14
	movq	16(%r14), %rcx
	movq	%rcx, 16(%rsp)
	movl	8(%r14), %r12d
	movl	(%r14), %edi
	movl	%edi, 28(%rsp)
	movl	4(%r14), %r15d
	movq	40(%rsp), %rax
	movq	%rax, 8(%rsp)
	movl	%ebp, (%rsp)
	movl	%r15d, %esi
	movl	%r12d, %edx
	movl	%ebx, %r8d
	movq	32(%rsp), %r9
	callq	_copy
	imull	%ebp, %ebx
	movslq	%ebx, %rdi
	addq	16(%r14), %rdi
	movq	48(%rsp), %rsi
	movl	%ebp, %edx
	callq	_memcpy
	movq	16(%rsp), %rax
	movq	%rax, 16(%r13)
	movl	%r12d, 8(%r13)
	movl	%r15d, 4(%r13)
	movl	28(%rsp), %eax
	movl	%eax, (%r13)
	addq	$56, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	popq	%rbp
	ret

	.globl	_add_string
	.align	4, 0x90
_add_string:
	pushq	%rbp
	pushq	%r15
	pushq	%r14
	pushq	%r13
	pushq	%r12
	pushq	%rbx
	pushq	%rax
	movl	%r9d, %r15d
	movq	%r8, %r14
	movl	%esi, %r12d
	movq	%rdi, %rbx
	movq	80(%rsp), %r13
	leal	(%r15,%r12), %edi
	movl	$1, %edx
	movl	%edi, %esi
	callq	_new_slice
	movq	%rax, %rbp
	movq	16(%rbp), %rdi
	movslq	%r12d, %rdx
	leaq	(%rdi,%rdx), %r12
	movq	%r14, %rsi
	callq	_memcpy
	movq	%r12, %rdi
	movq	%r13, %rsi
	movl	%r15d, %edx
	callq	_memcpy
	movl	(%rbp), %eax
	movl	4(%rbp), %ecx
	movl	8(%rbp), %edx
	movq	16(%rbp), %rsi
	movq	%rsi, 16(%rbx)
	movl	%edx, 8(%rbx)
	movl	%ecx, 4(%rbx)
	movl	%eax, (%rbx)
	addq	$8, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	popq	%rbp
	ret

	.globl	_string_constant
	.align	4, 0x90
_string_constant:
	pushq	%rbx
	movq	%rdi, %rbx
	movl	$1, %edx
	movl	%esi, %edi
	callq	_new_slice
	movq	%rbx, 16(%rax)
	popq	%rbx
	ret

	.globl	_print.tstring
	.align	4, 0x90
_print.tstring:
	pushq	%rbp
	pushq	%r14
	pushq	%rbx
	movq	%rcx, %rbx
	movl	%edi, %ebp
	movslq	%edx, %r14
	.align	4, 0x90
LBB9_1:
	movzbl	(%rbx), %edi
	callq	_putchar
	addq	%r14, %rbx
	decl	%ebp
	jne	LBB9_1
	popq	%rbx
	popq	%r14
	popq	%rbp
	ret

	.globl	_print.trune
	.align	4, 0x90
_print.trune:
	jmp	_putchar

	.globl	_print.float
	.align	4, 0x90
_print.float:
	leaq	L_.fmtf(%rip), %rdi
	movb	$1, %al
	jmp	_printf

	.globl	_print.slice
	.align	4, 0x90
_print.slice:
	ret

	.globl	_print.tinteger
	.align	4, 0x90
_print.tinteger:
	movq	%rdi, %rcx
	leaq	L_.fmti(%rip), %rdi
	xorl	%eax, %eax
	movq	%rcx, %rsi
	jmp	_printf

	.section	__TEXT,__cstring,cstring_literals
L_.fmtf:
	.asciz	"%f"

L_.fmti:
	.asciz	"%d"


.subsections_via_symbols
