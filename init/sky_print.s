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
.globl clean_current_page_on_screen
clean_current_page_on_screen:
	push %edx
	push %ecx
	push %edi
	push %eax
	push %ebx
	push %es
	

	#clean the current line , set ax to be es:di
	#the es is 0x10, set edi = 0xb8000
	#get current cursor pos
	mov (0x90000), %dl # cols, ds=0x9000
	mov (0x90001), %dh # rows

clean_loop:
	push %edx # backup cursor
	
	#set di
	xor %edi, %edi
	xor %ax, %ax
    mov %dh, %al #再从变量scr_loc中取目前字符显示位置值。 get current line
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

	pop %edx # backup cursor

	inc %dh
	cmp $25, %dh
	jl clean_loop
	
	pop %es
	pop %ebx
	pop %eax
	pop %edi
	pop %ecx
	pop %edx

.globl safe_mode_print_str_after_page
safe_mode_print_str_after_page:
    push %ebx #首先保存要用到的寄存器，EAX由调用者负责保存。the screen memeory
	push %eax # 
	push %ecx # the cursor pos

	#intel asm : section:[base+index*scale+displacement]
	#at&t asm  : section:displacement(base,index,scale)
	#get current cursor pos
	mov (0x90000), %cl # cols, ds=0x9000
	mov (0x90001), %ch # rows


clean_current_line_after_page:
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

print_str_loop_after_page:
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
	mov %ss:0(%ebp, %eax, 1), %al # get char from (ds:bp)
	inc %bp # idx += 1

    mov %al, (%ebx) #print 	

	# process cl
	inc %cl
	cmp $80, %cl
	jl skip_reset_cl_after_page
	mov $0, %cl
	inc %ch # new line
skip_reset_cl_after_page:
	# process ch
	cmp $25, %ch
	jl skip_reset_ch_after_page
	mov $0, %ch
skip_reset_ch_after_page:
	cmp $0, %al
	#cmp $0xA, %al  # '\n'
	jne print_str_loop_after_page
	
	#auto newline
	inc %ch
	cmp $25, %ch
	jl auto_newline_end_after_page
	mov $0, %ch

auto_newline_end_after_page:
	#save current cursor pos
	mov $0, %cl    #set the cursor to line-start
	mov %cl, (0x90000) # cols
	mov %ch, (0x90001)# rows

print_str_exit_after_page:
	pop %ecx
	pop %eax
    pop %ebx  #并弹出保存的寄存器内容，返回。
	ret