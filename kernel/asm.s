/*
 *  linux/kernel/asm.s
 *
 *  (C) 1991  Linus Torvalds
 */

/*
 * asm.s contains the low-level code for most hardware faults.
 * page_exception is handled by the mm, so that isn't here. This
 * file also handles (hopefully) fpu-exceptions due to TS-bit, as
 * the fpu must be properly saved/resored. This hasn't been tested.
 */

.globl _divide_error,_debug,_nmi,_int3,_overflow,_bounds,_invalid_op
.globl _double_fault,_coprocessor_segment_overrun
.globl _invalid_TSS,_segment_not_present,_stack_segment
.globl _general_protection,_coprocessor_error,_irq13,_reserved
.globl _alignment_check

/*
	export some symbol to c by sky
 */
.globl divide_error,debug,nmi,int3,overflow,bounds,invalid_op
.globl double_fault,coprocessor_segment_overrun
.globl invalid_TSS,segment_not_present,stack_segment
.globl general_protection,coprocessor_error,irq13,reserved
.globl alignment_check

divide_error:
_divide_error:
	pushl $do_divide_error
no_error_code:
	xchgl %eax,(%esp)
	pushl %ebx
	pushl %ecx
	pushl %edx
	pushl %edi
	pushl %esi
	pushl %ebp
	push %ds
	push %es
	push %fs
	pushl $0		# "error code"
	lea 44(%esp),%edx
	pushl %edx
	movl $0x10,%edx
	mov %dx,%ds
	mov %dx,%es
	mov %dx,%fs
	call *%eax
	addl $8,%esp
	pop %fs
	pop %es
	pop %ds
	popl %ebp
	popl %esi
	popl %edi
	popl %edx
	popl %ecx
	popl %ebx
	popl %eax
	iret

debug:
_debug:
	pushl $do_int3		# _do_debug
	jmp no_error_code

nmi:
_nmi:
	pushl $do_nmi
	jmp no_error_code

int3:
_int3:
	pushl $do_int3
	jmp no_error_code

overflow:
_overflow:
	pushl $do_overflow
	jmp no_error_code

bounds:
_bounds:
	pushl $do_bounds
	jmp no_error_code

invalid_op:
_invalid_op:
	pushl $do_invalid_op
	jmp no_error_code

coprocessor_segment_overrun:
_coprocessor_segment_overrun:
	pushl $do_coprocessor_segment_overrun
	jmp no_error_code

reserved:
_reserved:
	pushl $do_reserved
	jmp no_error_code

irq13:
_irq13:
	pushl %eax
	xorb %al,%al
	outb %al,$0xF0
	movb $0x20,%al
	outb %al,$0x20
	jmp 1f
1:	jmp 1f
1:	outb %al,$0xA0
	popl %eax
	jmp _coprocessor_error

double_fault:
_double_fault:
	pushl $do_double_fault

error_code:
	xchgl %eax,4(%esp)		# error code <-> %eax
	xchgl %ebx,(%esp)		# &function <-> %ebx
	pushl %ecx
	pushl %edx
	pushl %edi
	pushl %esi
	pushl %ebp
	push %ds
	push %es
	push %fs
	pushl %eax			# error code
	lea 44(%esp),%eax		# offset
	pushl %eax
	movl $0x10,%eax
	mov %ax,%ds
	mov %ax,%es
	mov %ax,%fs
	call *%ebx
	addl $8,%esp
	pop %fs
	pop %es
	pop %ds
	popl %ebp
	popl %esi
	popl %edi
	popl %edx
	popl %ecx
	popl %ebx
	popl %eax
	iret

invalid_TSS:
_invalid_TSS:
	pushl $do_invalid_TSS
	jmp error_code

segment_not_present:
_segment_not_present:
	pushl $do_segment_not_present
	jmp error_code

stack_segment:
_stack_segment:
	pushl $do_stack_segment
	jmp error_code

general_protection:
_general_protection:
	pushl $do_general_protection
	jmp error_code

alignment_check:
_alignment_check:
	pushl $do_alignment_check
	jmp error_code

