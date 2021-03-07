/*
 *  linux/boot/head.s
 *
 *  (C) 1991  Linus Torvalds
 */

/*
 *  head.s contains the 32-bit startup code.
 *
 * NOTE!!! Startup happens at absolute address 0x00000000, which is also where
 * the page directory will exist. The startup code will be overwritten by
 * the page directory.
 */
.text
.globl _idt,_gdt,_pg_dir,_tmp_floppy_area,startup_32
/*
	export some symbol to c by sky
 */
.globl idt,gdt,pg_dir,tmp_floppy_area,startup_32

pg_dir:
_pg_dir:
startup_32:
	movl $0x10,%eax
	mov %ax,%ds
	mov %ax,%es
	mov %ax,%fs
	mov %ax,%gs

	# sky print
	push %ebp
	lea msg0, %ebp
	call safe_mode_print_str_no_page
	pop %ebp
	#

	lss stack_start,%esp

	# sky print
	push %ebp
	lea msg2, %ebp
	call safe_mode_print_str_no_page
	pop %ebp	
	#
	call setup_idt

	# sky print
	push %ebp
	lea msg1, %ebp
	call safe_mode_print_str_no_page
	pop %ebp	
	#
	call setup_gdt

	movl $0x10,%eax		# reload all the segment registers
	mov %ax,%ds		# after changing gdt. CS was already
	mov %ax,%es		# reloaded in 'setup_gdt'
	mov %ax,%fs
	mov %ax,%gs
	lss stack_start,%esp

	# sky print
	push %ebp
	lea msg3, %ebp
	call safe_mode_print_str_no_page
	pop %ebp	
	#
	xorl %eax,%eax
1:	incl %eax		# check that A20 really IS enabled
	movl %eax,0x000000	# loop forever if it isn't
	cmpl %eax,0x100000
	je 1b
/*
 * NOTE! 486 should set bit 16, to check for write-protect in supervisor
 * mode. Then it would be unnecessary with the "verify_area()"-calls.
 * 486 users probably want to set the NE (#5) bit also, so as to use
 * int 16 for math errors.
 */
 	# sky print
	push %ebp
	lea msg4, %ebp
	call safe_mode_print_str_no_page
	pop %ebp	
	#
	movl %cr0,%eax		# check math chip
	andl $0x80000011,%eax	# Save PG,PE,ET
/* "orl $0x10020,%eax" here for 486 might be good */
	orl $2,%eax		# set MP
	movl %eax,%cr0
	call check_x87
	jmp after_page_tables

/*
 * We depend on ET to be correct. This checks for 287/387.
 */
check_x87:
	fninit
	fstsw %ax
	cmpb $0,%al
	je 1f			/* no coprocessor: have to set bits */
	movl %cr0,%eax
	xorl $6,%eax		/* reset MP, set EM */
	movl %eax,%cr0
	ret
.align 4
1:	.byte 0xDB,0xE4		/* fsetpm for 287, ignored by 387 */
	ret

/*
 *  setup_idt
 *
 *  sets up a idt with 256 entries pointing to
 *  ignore_int, interrupt gates. It then loads
 *  idt. Everything that wants to install itself
 *  in the idt-table may do so themselves. Interrupts
 *  are enabled elsewhere, when we can be relatively
 *  sure everything is ok. This routine will be over-
 *  written by the page tables.
 */
setup_idt:
	lea ignore_int,%edx
	movl $0x00080000,%eax
	movw %dx,%ax		/* selector = 0x0008 = cs */
	movw $0x8E00,%dx	/* interrupt gate - dpl=0, present */

	lea _idt,%edi
	mov $256,%ecx
rp_sidt:
	movl %eax,(%edi)
	movl %edx,4(%edi)
	addl $8,%edi
	dec %ecx
	jne rp_sidt
	lidt idt_descr
	ret

/*
 *  setup_gdt
 *
 *  This routines sets up a new gdt and loads it.
 *  Only two entries are currently built, the same
 *  ones that were built in init.s. The routine
 *  is VERY complicated at two whole lines, so this
 *  rather long comment is certainly needed :-).
 *  This routine will beoverwritten by the page tables.
 */
setup_gdt:
	lgdt gdt_descr
	ret




/*  Sky's safe_mode_print_str(const char *src_str) designed on 2021/02/07, do not's use bios interrupts.
 *  we can print string on screen, when the tty-module is not ready.
 *  We must use this after setup-program build some param at 0x90000
 *  
 *
 *  @param es:bp is src_str.
 *
 *  gs: video memory (0xb8000)
 *  es: kernel param memory (0x9000), we will read/write the cursor's pos
 *  ds: the src_str data segment
 *  
 *  Intel Asm
*/
safe_mode_print_str_no_page:
    push %ebx #首先保存要用到的寄存器，EAX由调用者负责保存。the screen memeory
	push %eax # 
	push %ecx # the cursor pos

	#intel asm : section:[base+index*scale+displacement]
	#at&t asm  : section:displacement(base,index,scale)
	#get current cursor pos
	mov (0x90000), %cl # cols, ds=0x9000
	mov (0x90001), %ch # rows


clean_current_line:
	push %es
	push %edi
	push %ecx

	#clean the current line , set ax to be es:di
	#the es is 0x10, set edi = 0xb8000

	#set di
	xor %edi, %edi
	xor %ax, %ax
    mov %ch, %al #再从变量scr_loc中取目前字符显示位置值。 get current line
	mov $80, %bx
	mul %bx
	mov %ax, %di 
	shl $1, %di 
	add $0xb8000, %edi

	#clean line 
	xor %ecx, %ecx
	mov $80, %cx
	mov $0x0700, %ax # erase char

	cld
	rep
	stosw # clean current line

	pop %ecx
	pop %edi
	pop %es

print_str_loop:
	xor %ax, %ax
	xor %ebx, %ebx
    mov %ch, %al #再从变量scr_loc中取目前字符显示位置值。
	mov $80, %bx
	mul %bx
	mov %ax, %bx
	xor %ax, %ax
	mov %cl, %al
	add %ax, %bx
    shl $1, %bx #因为在屏幕上每个字符还有一个属性字节，因此字符实际显示位置对应的显示内存偏移地址要乘2。
	add $0xb8000, %ebx
	
	xor %eax, %eax
	mov %ds:0(%ebp, %eax, 1), %al # get char from (ds:bp)
	inc %bp # idx += 1

    mov %al, (%ebx) #print 	

	# process cl
	inc %cl
	cmp $80, %cl
	jl skip_reset_cl
	mov $0, %cl
	inc %ch # new line
skip_reset_cl:
	# process ch
	cmp $25, %ch
	jl skip_reset_ch
	mov $0, %ch
skip_reset_ch:
	cmp $0, %al
	#cmp $0xA, %al  # '\n'
	jne print_str_loop
	
	#auto newline
	inc %ch
	cmp $25, %ch
	jl auto_newline_end
	mov $0, %ch

auto_newline_end:
	#save current cursor pos
	mov $0, %cl    #set the cursor to line-start
	mov %cl, (0x90000) # cols
	mov %ch, (0x90001)# rows

print_str_exit:
	pop %ecx
	pop %eax
    pop %ebx  #并弹出保存的寄存器内容，返回。
	ret






msg0:	.ascii	"startup_32 begin ... ..."
		.byte 0x00
msg1:	.ascii	"re-setup gdt ... ..."
		.byte 0x00
msg2:	.ascii	"re-setup idt ... ..."
		.byte 0x00
msg3:	.ascii	"Check the A20 is ready ... ..."
		.byte 0x00
msg4:	.ascii	"Check the math-chip is ready ... ..."
		.byte 0x00
msg5:	.ascii	"Setup page -> enable page ... ..."
		.byte 0x00

/*
 * I put the kernel page tables right after the page directory,
 * using 4 of them to span 16 Mb of physical memory. People with
 * more than 16MB will have to expand this.
 */
.org 0x1000
pg0:

.org 0x2000
pg1:

.org 0x3000
pg2:

.org 0x4000
pg3:

.org 0x5000
/*
 * tmp_floppy_area is used by the floppy-driver when DMA cannot
 * reach to a buffer-block. It needs to be aligned, so that it isn't
 * on a 64kB border.
 */
tmp_floppy_area:
_tmp_floppy_area:
	.fill 1024,1,0

after_page_tables:
	# sky print
	push %ebp
	lea msg5, %ebp
	call safe_mode_print_str_no_page
	pop %ebp	
	#
	pushl $0		# These are the parameters to main :-)
	pushl $0
	pushl $0
	pushl $L6		# return address for main, if it decides to.
	pushl $main
	jmp setup_paging
L6:
	jmp L6			# main should never return here, but
				# just in case, we know what happens.

/* This is the default interrupt "handler" :-) */
int_msg:
	.asciz "Unknown interrupt\n\r"
.align 4
ignore_int:
	pushl %eax
	pushl %ecx
	pushl %edx
	push %ds
	push %es
	push %fs
	movl $0x10,%eax
	mov %ax,%ds
	mov %ax,%es
	mov %ax,%fs
	pushl $int_msg
	call printk
	popl %eax
	pop %fs
	pop %es
	pop %ds
	popl %edx
	popl %ecx
	popl %eax
	iret


/*
 * Setup_paging
 *
 * This routine sets up paging by setting the page bit
 * in cr0. The page tables are set up, identity-mapping
 * the first 16MB. The pager assumes that no illegal
 * addresses are produced (ie >4Mb on a 4Mb machine).
 *
 * NOTE! Although all physical memory should be identity
 * mapped by this routine, only the kernel page functions
 * use the >1Mb addresses directly. All "normal" functions
 * use just the lower 1Mb, or the local data space, which
 * will be mapped to some other place - mm keeps track of
 * that.
 *
 * For those with more memory than 16 Mb - tough luck. I've
 * not got it, why should you :-) The source is here. Change
 * it. (Seriously - it shouldn't be too difficult. Mostly
 * change some constants etc. I left it at 16Mb, as my machine
 * even cannot be extended past that (ok, but it was cheap :-)
 * I've tried to show which constants to change by having
 * some kind of marker at them (search for "16Mb"), but I
 * won't guarantee that's all :-( )
 */
.align 4
setup_paging:
	movl $1024*5,%ecx		/* 5 pages - pg_dir+4 page tables */
	xorl %eax,%eax
	xorl %edi,%edi			/* pg_dir is at 0x000 */
	cld;rep;stosl
	movl $pg0+7,_pg_dir		/* set present bit/user r/w */
	movl $pg1+7,_pg_dir+4		/*  --------- " " --------- */
	movl $pg2+7,_pg_dir+8		/*  --------- " " --------- */
	movl $pg3+7,_pg_dir+12		/*  --------- " " --------- */
	movl $pg3+4092,%edi
	movl $0xfff007,%eax		/*  16Mb - 4096 + 7 (r/w user,p) */
	std
1:	stosl			/* fill pages backwards - more efficient :-) */
	subl $0x1000,%eax
	jge 1b
	xorl %eax,%eax		/* pg_dir is at 0x0000 */
	movl %eax,%cr3		/* cr3 - page directory start */
	movl %cr0,%eax
	orl $0x80000000,%eax
	movl %eax,%cr0		/* set paging (PG) bit */
	ret			/* this also flushes prefetch-queue */

.align 4
.word 0
idt_descr:
	.word 256*8-1		# idt contains 256 entries
	.long _idt
.align 4
.word 0
gdt_descr:
	.word 256*8-1		# so does gdt (not that that's any
	.long _gdt		# magic number, but it works for me :^)

	.align 8

idt:
_idt:	.fill 256,8,0		# idt is uninitialized

gdt:
_gdt:	.quad 0x0000000000000000	/* NULL descriptor */
	.quad 0x00c09a0000000fff	/* 16Mb */
	.quad 0x00c0920000000fff	/* 16Mb */
	.quad 0x0000000000000000	/* TEMPORARY - don't use */
	.fill 252,8,0			/* space for LDT's and TSS's etc */
