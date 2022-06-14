
;--- read a file of any size in extended memory.
;--- works with both 16- and 32-bit DOS extenders.

	.286
	.model small
	.386

?BLOCKSIZE = 8000h

	.code

_hwrite proc pascal public hFile:word, buffer:ptr far32, dwSize:dword
	mov ax, 4000h
	jmp readwrite
_hwrite endp

_hread proc pascal public hFile:word, buffer:ptr far32, dwSize:dword

local result:dword
local base:dword
local cursiz:word
local mode:word

	mov ax,3F00h
readwrite::
	mov mode, ax
	sub eax, eax
	mov result, eax
	mov cx, 1
	int 31h
	jc err0
	push ds
	mov bx, ax
	mov ds, bx
	xor ecx, ecx
	mov dx, ?BLOCKSIZE-1
	mov ax, 8
	int 31h
	jc err1
	mov bx, word ptr [buffer+4]
	mov ax, 6
	int 31h
	jc err1
	push cx
	push dx
	pop eax
	add eax, dword ptr [buffer+0]
	mov base, eax
nextblock:
	mov eax, dwSize
	and eax, eax
	jz done
	mov ecx, ?BLOCKSIZE
	sub eax, ecx
	jnc @F
	add ecx, eax
	sub eax, eax
@@:
	mov cursiz, cx
	mov dwSize, eax
	mov dx, word ptr [base+0]
	mov cx, word ptr [base+2]
	mov bx, ds
	mov ax, 7
	int 31h
	jc err1
	xor edx, edx
	movzx ecx, cursiz
	mov bx, hFile
	mov ax, mode
	int 21h
	jc err1
	movzx eax, ax
	add result, eax
	cmp ax, ?BLOCKSIZE
	jnz done
	add base, eax
	jmp nextblock
err0:
	or eax, -1
	jmp exit
err1:
	or eax, -1
	mov result, eax
done:
	mov bx, ds
	pop ds
	mov ax, 1
	int 31h
@@:
	mov eax, result
exit:
	ret
_hread endp

	END
