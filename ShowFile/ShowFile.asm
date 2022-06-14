
	.286
	.model small
	.386
	option casemap:none

	include showfile.inc

	.data

ife ?USEPREDEFSEGS
mono	dw 0
color	dw 0
endif

	.code

;--- WEP is (should be?) called with DS=DGROUP

WEP proc far pascal wCode:word

ife ?USEPREDEFSEGS
	mov ax, 1
	mov bx, color
	int 31h
	mov bx, mono
	int 31h
endif
	mov ax, 1
	ret
WEP endp

SetVideoParms proc far pascal uses ds wtCrt:word, wPageStart:word, cols:word, trows:word

	mov ax, DGROUP
	mov ds, ax
	mov ax, wtCrt
	mov wCrt, ax
	mov ax, wPageStart
	mov vidOfs, ax
	mov ax, cols
	mov columns,al
	mov ax, trows
	dec ax
	mov rows, ax
	ret
SetVideoParms endp

ShowTextFile proc far pascal uses ds es ebx esi edi path:fword, parms:dword

	mov  ax, DGROUP
	mov  ds, ax
	les  edx, path
	mov  eax, parms
	call DllEntry
	ret

ShowTextFile endp

ShowMemory proc far pascal uses ds es ebx esi edi selector:word, laenge:dword, position:dword, sfexit:fword

	mov  ax, DGROUP
	mov  ds, ax

	mov  eax, dword ptr sfexit+0
	mov  dword ptr savefileexit+0, eax
	mov  ax, word ptr sfexit+4
	mov  word ptr savefileexit+4, ax
	mov  dx, selector		;selector
	mov  ecx, position		;minimaler offset
	mov  ebx, ecx			;aktueller offset
	mov  eax, laenge
	add  eax, ecx			;maximaler offset
	jnc  @F
	mov  eax, -1
@@:
	call DllEntry2
	ret
ShowMemory endp

LibMain proc far pascal

ife ?USEPREDEFSEGS
	mov  cx, 2			;alloc 2 selectors for B0000 and B8000
	xor  ax, ax
	int  31h
	jc exit
	mov  bx, ax
	mov  color, bx
	mov  cx, 000Bh
	mov  dx, 8000h
	mov  ax, 7
	int  31h
	mov  dx, 7FFFh
	mov  cx, 0
	mov  ax, 8
	int  31h
	add  bx, 8
	mov  mono, bx
	mov  cx, 000Bh
	mov  dx, 0000h
	mov  ax, 7
	int  31h
	mov  dx, 7FFFh
	mov  cx, 0
	mov  ax, 8
	int  31h
endif
	mov  ax, 1
exit:
	ret
LibMain endp

end LibMain
