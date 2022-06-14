
;--- run as exe

	.286
	.model small
	.stack 4096
	option casemap:none

	include showfile.inc

	.data

nmode dw 0
_psp  dw 0

err0  db "No valid filename",CR,LF,'$'
err1  DB "File not found",   CR,LF,'$'
err2  DB "Not enough memory",CR,LF,'$'
err3  DB "Error during read",CR,LF,'$'

	.code

externdef mainproc:near

if 0
deztest proc
	cmp al, '0'
	jc deztst1
	cmp al, '9' + 1
	jnc deztst1
	sub al, '0'
	and al, al
	ret
deztst1:
	stc
	ret
deztest endp

_getdez proc
	push dx
	mov ch, 0
	xor dx, dx
getdez2:
	mov al,es:[bx]
	call deztest
	jc getdez1
	inc ch
	movzx ax, al
	imul dx, 10
	add dx, ax
	inc bx
	jmp getdez2
getdez1:
	mov ax, dx
	pop dx
	ret
_getdez endp

;--- get cmdline option 
;--- understands /v:xxx

setoption proc
	inc	  bx
	mov	  al, es:[bx]
	or	  al, 20h
	cmp	  al, 'v'
	jnz	  setoption_1
	inc	  bx
	mov	  al, es:[bx]
	cmp	  al, ':'
	jnz	  @F
	inc	  bx
	mov	  al, es:[bx]
@@:
	call _getdez
	cmp ch, 0
	jz setoption_1
	mov [nmode], ax
setoption_1:
	ret
setoption endp
endif

parsecmdline:
	mov es, [_psp]
	mov bx, 80h
	mov cl, es:[bx]			  ; Get length
	sub ch, ch
	jcxz parsecmdline_er
@@:
	inc bx
main_1:
	mov al, es:[bx]
	cmp al,' '
	jz @B
	cmp al, 00
	jz parsecmdline_er
	cmp al, 0Dh
	jz parsecmdline_er
	mov dx, bx
@@:
	mov ah, 00
if 0
	cmp al, '/'
	jnz main_10
	call setoption
	jmp main_1
endif
main_10:
	inc bx
	mov al, es:[bx]
	cmp al,' '
	jz main_11
	cmp al, 00
	jz main_11
	cmp al, 0Dh
	jz main_11
	cmp al,'*'
	jnz @F
	or ah, 1
@@:
	cmp al, '?'
	jnz @F
	or ah, 1
@@:
	jmp main_10
main_11:
	mov byte ptr es:[bx], 0
	clc
	ret
parsecmdline_er:
	stc
	ret

main proc

	mov [_psp], es
	mov byte ptr fHex, 0
	call parsecmdline
	jnc @F
	push offset err0
	jmp main_err			; 'no valid filename'
@@:
	call mainproc
	and ax,ax
	jz exit
	cmp ax, 1
	jnz @F
	push offset err1		; 'File not found'
	jmp main_err
@@:
	cmp ax, 2
	jnz @F
	push offset err2		; 'Not enough mem'
	jmp main_err
@@:
	cmp ax, 3
	jnz @F
	push offset err3		; 'Error during read'
	jmp main_err
@@:
exit:
	ret
main_err:
	pop dx
	mov ah, 9
	int 21h
	ret
main endp

start:
	call main
	mov ah, 4Ch
	int 21h

	end start
