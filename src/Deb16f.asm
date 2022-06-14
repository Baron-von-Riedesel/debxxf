
;--- 16 bit NE executable to load the debugger overlay

	.286
	.MODEL SMALL
	.386

	.nolist
	.nocref
	include winnt.inc
	include debxxf.inc
	.cref
	.list

_TRACE_ = 0

@traceout macro xx
if _TRACE_
	pusha
	@strout <xx>
	popa
endif
endm


	.DATA

	dw 0,0,5,0,0,0,0,0

szDebugLib	db "DEB16F.OVL",0
LSIZDBGLIB equ $ - szDebugLib
szError1	db "DEB16F.OVL not found",13,10,'$'
szError2	db "DEB16F.OVL is invalid",13,10,'$'
szError3	db "DPMI: out of memory",13,10,'$'
szError4	db "DPMI: out of selectors",13,10,'$'
wSP		dw 0
wBP		dw 0

	.CODE

main proc c

local	dbgparm:DEBUGPARM
local	wRC:word
local	psp:word
local	dwFileSize:DWORD
local	dwHdrSize:DWORD
local	dwMemSize:DWORD
local	memhdl:DWORD
local	base:DWORD
local	dfInitProc:PF32
local	filehdl:WORD
local	scratchsel:WORD
local	szPath[260]:byte

	@traceout <"entry DEB16F",cr,lf>
	pushad
	mov wRC,-1
	mov filehdl, 1
	mov scratchsel,0
	mov memhdl,-1
	mov psp,es

;--- get path of executable 	   

	mov es, es:[2ch]
	xor di, di
	mov cx,-1
	.while (1)
		mov al,0
		repnz scasb
		.break .if ( byte ptr es:[di] == 0 )
	.endw
	inc di
	cmp word ptr es:[di],1
	jnz exit
	inc di
	inc di
	mov si,di
	push es
	pop ds
	push ss
	pop es
	lea di, szPath
	mov dx, di
	.while (1)
		lodsb
		stosb
		.break .if (!al)
		.if (al == '\' || al == ':')
			mov dx, di
		.endif
	.endw

;--- replace name of executable by name of debugger overlay

	push ss
	pop ds
	mov si, offset szDebugLib
	mov cx, LSIZDBGLIB
	mov di, dx
	rep movsb

;--- build a flat selector

	mov ax,0
	mov cx,1
	int 31h
	jc error4
	mov scratchsel,ax
	mov bx,ax
	mov cx,-1
	mov dx,-1
	mov ax,8
	int 31h

;--- read debugger overlay

	lea dx, szPath
	mov ax, 3D00h
	int 21h
	jc error1
	mov bx,ax
	mov filehdl, ax

	mov cx,sizeof szPath
	mov ax,3f00h
	int 21h
	jc error2
	cmp ax,cx
	jc error2
	cmp szPath.IMAGE_DOS_HEADER.e_magic,"ZM"
	jnz error2

	movzx edx, szPath.IMAGE_DOS_HEADER.e_cparhdr	;size exehdr
	shl edx,4
	mov dwHdrSize, edx
	push edx
	pop dx
	pop cx
	mov ax, 4200h				;pos from file start
	int 21h
	jc error2

	movzx eax, szPath.IMAGE_DOS_HEADER.e_cp	;number of 200h pages
	movzx ecx, szPath.IMAGE_DOS_HEADER.e_cblp
	jecxz @F
	dec eax									;last one is not full	
@@:
	shl eax,9
	add eax,ecx
	sub eax, dwHdrSize
	mov dwFileSize, eax						;bytes to read 
	add eax,0Fh
	and al,0F0h								;align to paras

	movzx ecx, szPath.IMAGE_DOS_HEADER.e_minalloc	;get _BSS size
	shl ecx, 4
	add eax, ecx
	mov dwMemSize, eax
	push eax
	pop cx
	pop bx

	mov ax,0501h
	int 31h
	jc error3
	mov word ptr memhdl+0,di
	mov word ptr memhdl+2,si
	mov word ptr base+0,cx
	mov word ptr base+2,bx

	mov edi, dwFileSize
	mov esi, base

;--- read the overlay. in 16bit dpmi mode the file has to be read
;--- in chunks < 64 kB!

	.while (edi)
		mov bx, scratchsel
		push esi
		pop dx
		pop cx
		mov ax,7
		int 31h
		.if (edi > 8000h)
			mov cx,8000h
		.else
			mov cx, di
		.endif
		xor dx,dx
		push ds
		mov ds,bx
		mov bx, filehdl
		mov ax,3f00h
		int 21h
		pop ds
		jc error2
		cmp ax,cx
		jnz error2
		movzx eax,ax
		add esi, eax
		sub edi, eax
	.endw
	mov bx, filehdl
	mov ah,3Eh
	int 21h
	mov filehdl,-1

;--- handle relocations

	mov bx, scratchsel			;make a flat selector
	xor dx, dx
	xor cx, cx
	mov ax,7
	int 31h

	mov es,bx
	mov cx, szPath.IMAGE_DOS_HEADER.e_crlc		;no of relocs
	lea si, szPath
	add si, szPath.IMAGE_DOS_HEADER.e_lfarlc	;offset relocs
	mov edi,base
	.while (cx)
		movzx eax,word ptr [si+2]
		shl eax, 4
		movzx edx,word ptr [si+0]
		add eax, edx
		mov word ptr es:[edi+eax],bx
		add si,4
		dec cx
	.endw

;--- relocations done, now make scratchsel be a 32bit flat code selector

	mov dx, word ptr base+0
	mov cx, word ptr base+2
	mov ax,7
	int 31h

	mov eax, dwMemSize
	dec eax
	push eax
	pop dx
	pop cx
	mov ax,8
	int 31h

	lar ecx, ebx
	shr ecx, 8
	xor cl,8			;make a code selector
	or ch,040h			;32bit
	mov ax,0009
	int 31h

;--- codeselector is set, now call entry point

	mov eax,81h
	mov dword ptr dbgparm.DEBUGPARM.pCmdLine+0,eax
	mov ax,psp
	mov word ptr dbgparm.DEBUGPARM.pCmdLine+4,ax
	movzx eax,sp
	mov dword ptr dbgparm.DEBUGPARM.pHostStack,eax
	mov word ptr dbgparm.DEBUGPARM.pHostStack+4,ss

	xor eax, eax
	mov dword ptr dfInitProc+0,eax
	mov word ptr dfInitProc+4,bx
	push ss
	pop es
	lea ebx, dbgparm
	call dfInitProc

	@traceout <"enable debugger",cr,lf>
	push 0
	push ss
	lea eax,dbgparm
	push eax
	call dbgparm.Enable
	and eax, eax
	jz exit

	popad
	mov wSP, sp
	mov wBP, bp
	mov es,psp
	@traceout <"calling debug entry",cr,lf>
	call dbgparm.DebugEntry
	mov sp,ss:wSP
	mov bp,ss:wBP
	push ss
	pop ds
	mov wRC,ax
	call dbgparm.Disable
	jmp exit
error1:
	mov dx,offset szError1
	jmp errout
error2:
	mov dx,offset szError2
	jmp errout
error3:
	mov dx,offset szError3
	jmp errout
error4:
	mov dx,offset szError4
	jmp errout
errout:
	mov ah,9
	int 21h
exit:
	.if (filehdl != -1)
		mov bx, filehdl
		mov ah,3eh
		int 21h
	.endif
	.if (scratchsel)
		mov bx,scratchsel
		mov ax,1
		int 31h
	.endif
	.if (memhdl != -1)
		mov di,word ptr memhdl+0
		mov si,word ptr memhdl+2
		mov ax,0502h
		int 31h
	.endif
	mov ax, wRC
	ret
main endp

InitTask proto far16 pascal

astart proc
;	call InitTask
	call main
	mov ah,4Ch
	int 21h
astart endp

end astart

