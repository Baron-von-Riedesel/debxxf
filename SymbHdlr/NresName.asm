
	.286
	.model small
	.386

	include windows.inc

	.code
    
GETNRESNAMES PROC FAR pascal uses si di hModule:WORD

local wSize:word
local lpByte:LPSTR
local hFile:word
local lpmodstruc:LPSTR
local szPath[260]:byte

	mov si, hModule
	verr si
	jnz $L2482
	sub bx, bx
	mov es, si
	mov WORD PTR lpmodstruc+0,bx
	mov WORD PTR lpmodstruc+2,es
	cmp WORD PTR es:[bx],"EN"
	jne $L2482

	invoke GetModuleFileName, si, addr szPath, sizeof szPath

	lea dx, szPath
	push ds
	push ss
	pop ds
	mov ax,3D00h
	int 21h
	pop ds
    
	mov hFile, ax
	cmp ax, -1
	je $L2482

	mov bx, word ptr lpmodstruc+0
	mov es, si
	mov ax, WORD PTR es:[bx+32]    ; length nresnames
	mov wSize, ax
	inc ax
	movzx eax, ax
	invoke GlobalAlloc, 2, eax
	mov si, ax
	or si, ax
	je $I2478

	sub cx, cx
	mov WORD PTR lpByte+0, cx
	mov WORD PTR lpByte+2, ax

	les bx, lpmodstruc
	mov cx, es:[bx+46]    ; address nresnames
	mov dx, es:[bx+44]
	mov bx, hFile
	mov ax, 4200h
	int 21h

	push ds
	lds dx, lpByte
	mov cx, wSize
	mov ax, 3F00h
	int 21h
	pop ds
	jc $I2479
	cmp ax, wSize
	jne $I2479
	mov bx, 1
	jmp $I2480
$I2479:
	xor bx,bx
$I2480:
	les di, lpByte
	add di, wSize
	or bx, bx
	mov BYTE PTR es:[di], 0
	jne $I2478
	invoke GlobalFree, si
	xor si, si
$I2478:
	mov bx, hFile
	mov ah, 3Eh
	int 21h
	mov ax, si
	ret
$L2482:
	xor ax,ax
	ret

GETNRESNAMES ENDP

END
