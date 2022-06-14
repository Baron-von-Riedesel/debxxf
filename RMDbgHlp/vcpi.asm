
	.286
	.MODEL SMALL
	.386

?WINDOWS	equ 0
?32BIT		equ 0

	include const.inc
	include dpmi.inc
	include rmdbghlp.inc

DGROUP group _TEXT, _DATA

	.code

oldint67 dd 0

externdef csalias:WORD
externdef cssegm:WORD


myint67 proc
	cmp ax, 0DE00h
	jz @F
	jmp dword ptr cs:oldint67
@@:
	mov ah,84h
	iret
myint67 endp

VCPI proc far pascal

	pop ecx
	pop edx
	pop ax		;get wEnable argument
	push edx
	push ecx
	push ds
	push bx
	mov ds, cs:csalias
	mov cx, word ptr oldint67+2
	.if (ax && cx)
		mov dx, word ptr oldint67+0
		mov bl, 67h
		mov ax, 0201h
		int 31h
		mov ds:oldint67,0
	.elseif ((!ax) && (!cx))
		mov bl, 67h
		mov ax, 0200h
		int 31h
		mov ax,cx
		or	ax,dx
		.if (ax)
			mov word ptr ds:oldint67+0,dx
			mov word ptr ds:oldint67+2,cx
			mov cx,cssegm
			mov dx,offset myint67
			mov bl,67h
			mov ax, 0201h
			int 31h
		.endif
	.endif
	pop bx
	pop ds
	db 66h
	ret
VCPI endp

	end
