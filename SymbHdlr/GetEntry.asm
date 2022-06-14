
;*** translate SSSS:OOOO to Entry#

	.286
	.model small
	.386

ifdef _DEBUG
CONST2 segment word public 'CODE'
CONST2 ends
CGROUP group _TEXT, CONST2
endif

@trace macro text:vararg
local sym
ifdef _DEBUG
CONST2 segment
sym db text, 0
CONST2 ends
	pusha
	mov si, offset sym
	mov ax, 12h
	int 41h
	popa
endif
endm

	include windows.inc

GetSegmentID     proto far pascal :word, :word
GetSegmentHandle proto far pascal :word, :word

ENTRIES equ 4   ;offset in MD

	.code

;--- get entry# from hModule:dwAddress

GetEntryID proc far pascal uses si di wModule:word, dwAddr:dword

	@trace <"GetEntryID enter",13,10>
	xor 	dx, dx
	mov 	ax,wModule		 ; check if hModule ok
	verr	ax
	jnz 	error
	mov 	es, ax
	mov 	ax, es:[0]
	cmp 	ax, "EN"
	jnz 	error
	push	es
	invoke	GetSegmentID, es, word ptr dwAddr+2 ; get Segment#
	mov 	word ptr dwAddr+2,ax
	pop 	es
	and 	ax,ax
	jz		error

	@trace <"GetEntryID: call GetSegmentID ok",13,10>

	mov 	si, es:[ENTRIES]	; start entry table
	mov 	dx, 1				; init DX with start entry#
	mov 	di, word ptr dwAddr+0
	mov 	ch,00
L115E:
	mov 	cl, es:[si]			; cl = no of entries
	jcxz	error				; end of table
	inc 	si
	mov 	bl, es:[si]
	inc 	si
	and 	bl, bl
	jnz 	@F
	add 	dx, cx
	jmp 	L115E			; --->
@@:
L1176:						; <---
	mov 	bh, bl
	cmp 	bl, 0FFh
	jnz 	@F
	add 	si, 3
	mov 	bh, es:[si+0]
@@:
	cmp 	di, es:[si+1]
	jnz 	@F
	cmp 	al,bh
	jz		L1181			; found!
@@:
	add 	si, 3
	inc 	dx
	loop	L1176			; next entry --->
	jmp 	L115E			; --->
L1181:
	mov 	ax, dx
	@trace <"GetEntryID succeeded",13,10>
	jmp 	@F
error:
	xor 	ax, ax
	@trace <"GetEntryID failed",13,10>
@@:
	ret
GetEntryID endp

GetEntryAddress proc far pascal uses si di wModule:word, wID:word

	@trace <"GetEntryAddress enter",13,10>
	mov 	ax,wModule		; check hModule
	verr	ax
	jnz 	error
	mov 	es, ax
	mov 	ax,es:[0]
	cmp 	ax,"EN"
	jnz 	error

	mov 	si,es:[ENTRIES]  ;Start der Entry-Tabelle
	mov 	dx,1			 ;in DX wird die aktuelle Entry# verwaltet
	mov 	ax,wID
	xor 	cx,cx
	mov 	bh,00
LX115E:
	mov 	cl,es:[si]		 ;<----
	jcxz	error
	inc 	si
	mov 	di,si			 ;di sichert ptr auf segmentnr
	mov 	bl,es:[si]
	inc 	si
	and 	bl,bl
	jnz 	@F
	add 	dx,cx
	jmp 	LX115E			 ;---->
@@:
	cmp 	bl,0FFh 		 ;"bewegliche segmente"?
	mov 	bl,3
	jnz 	LX1176
	mov 	bl,6			 ;dann 6 bytes/eintrag
LX1176: 						 ;<----
	cmp 	ax,dx
	jz		@F
	add 	si,bx
	inc 	dx
	loop	LX1176			 ;---->
	jmp 	LX115E			 ;---->
@@:
	mov 	bx,es:[si+1]	 ;offset
	mov 	al,es:[di]		 ;segmentnr
	cmp 	al,0FFh
	jnz 	@F
	add 	si,3
	mov 	al,es:[si]		 ;logische Segmentnummer
	mov 	bx,es:[si+01]	 ;offset des Entrys (->BX)
@@:
	cmp 	al,0FEh
	jnz 	@F
	mov 	dx,bx
	jmp 	L1197
@@:
	push	bx
	mov 	ah,0
	invoke	GetSegmentHandle, wModule, ax
	mov 	dx,ax
	pop 	bx
L1197:
	@trace <"GetEntryAddress succeeded",13,10>
	mov 	ax, bx
	jmp 	exit
error:
	@trace <"GetEntryAddress failed",13,10>
	xor 	ax, ax
	xor 	dx, dx
exit:
	ret
GetEntryAddress endp

end

