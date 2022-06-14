
	.286
    .model small
	.386

	include windows.inc

GETNRESNAMES	proto FAR pascal :word
GETENTRYADDRESS	proto FAR pascal :word, :word

	.code

_SearchAddr PROC NEAR

; Line 26
	push bp
	mov bp, sp
	sub sp, 4
	push	di
	push	si
;	lpp1 = 4
;	wLen = 8
;	lpName = 10
;	wSize = 14
;	register cx = i
;	lpp2 = -4

	les	di,DWORD PTR [bp+4]	;lpp1
	cmp	BYTE PTR es:[di],0
	je	SHORT $FB2449
$FC2448:
; Line 32
	mov	es,WORD PTR [bp+6]
	mov	cl,BYTE PTR es:[di]
	sub	ch,ch
	inc	di
	mov	WORD PTR [bp+4],di	;lpp1
; Line 33
	cmp	WORD PTR [bp+14],cx	;wSize
	jne	SHORT $I2450
; Line 35
	mov	ax,WORD PTR [bp+10]	;lpName
	mov	dx,WORD PTR [bp+12]
	mov	WORD PTR [bp-2],dx
	or	cx,cx
	je	SHORT $L2508
	mov	si,ax
$F2451:
	mov	es,WORD PTR [bp+6]
	mov	al,BYTE PTR es:[di]
	mov	es,WORD PTR [bp-2]
	cmp	BYTE PTR es:[si],al
	jne	SHORT $L2508
	inc	di
	inc	si
	dec	cx
	jne	SHORT $F2451
$L2508:
	mov	WORD PTR [bp+4],di	;lpp1
; Line 36
	or	cx,cx
	je	SHORT $L2497
; Line 39
$I2450:
; Line 40
	mov	ax,cx
	add	ax,2
	add	WORD PTR [bp+4],ax	;lpp1
	les	bx,DWORD PTR [bp+4]	;lpp1
	cmp	BYTE PTR es:[bx],0
	je	SHORT $FB2449
	mov	di,bx
	jmp	SHORT $FC2448
	nop	
$L2497:
; Line 37
	les	bx,DWORD PTR [bp+4]	;lpp1
	mov	ax,WORD PTR es:[bx]
	pop	si
	pop	di
	mov sp, bp
	pop bp
	ret
; Line 40
$FB2449:
; Line 41
	xor	ax,ax
; Line 42
	pop	si
	pop	di
	mov sp, bp
	pop bp
	ret

_SearchAddr ENDP

GETPROCEDUREADDREX PROC FAR pascal uses si hModule:word, lpName:dword, wSize:word, hMem:word

local lpp1:dword
local lpmodstruc:dword
local y:word
local flen:word

;	lpmodstruc = -8
;	flen = -12
;	y = -10
;	lpp1 = -4

; Line 63
	mov	ax, hModule
	mov	dx,ax
	sub	cx,cx
	mov	si,cx
	mov	WORD PTR lpmodstruc+2,ax
	mov	es,ax
	mov	ax,WORD PTR es:[si+38]
	mov	WORD PTR lpp1+0,ax
	mov	WORD PTR lpp1+2,dx
; Line 65
	push	wSize
	push	lpName
	sub		ax,WORD PTR es:[si+40]
	neg		ax
	push	ax
	push	dx
	push	WORD PTR lpp1+0
	call	_SearchAddr
	add	sp,12
	mov	y,ax
	or	ax,ax
	jne	SHORT $EX2464

	mov	es,WORD PTR lpmodstruc+2
	mov	ax,WORD PTR es:32
	mov	flen,ax
; Line 68
	mov	ax, hMem
	mov	dx,ax
	sub	cx,cx
	mov	WORD PTR lpp1+0,cx
	mov	WORD PTR lpp1+2,ax
	or	dx,cx
	jne	$L2507
	mov	si, y
	jmp	$I2470

$L2507:
; Line 70
	push	wSize
	push	lpName
	push	flen
	push	ax
	push	WORD PTR lpp1
	call	_SearchAddr
	add	sp,12	;000cH
	mov	si,ax
; Line 73
$I2470:
	mov	ax,si
; Line 74
$EX2464:
	ret

GETPROCEDUREADDREX ENDP

GETPROCEDUREENTRY PROC FAR pascal uses si di hModule:word, lpName:dword, wSize:word, hMem:word

local	hMyMem:word

	mov	di, hMem
	mov	hMyMem,di
	or	di,di
	jne	$I2483
    
	invoke	GETNRESNAMES, hModule
	mov	hMyMem,ax
    
$I2483:
	invoke	GETPROCEDUREADDREX, hModule, lpName, wSize, hMyMem
	mov	si,ax

	or	di,di
	jne	$I2484
	invoke	GlobalFree, hMyMem
$I2484:
	mov	ax,si
	ret

GETPROCEDUREENTRY ENDP

GETSYMBOLADDR PROC FAR pascal uses si di lpName:dword, hModule:WORD, hMem:word

local	x:word
local	wSize:word

	les di, lpName
	mov cx, -1
	xor ax,ax
	repnz scasb
	not cx
	dec cx
    
	invoke GETPROCEDUREENTRY, hModule, lpName, cx, hMem
	or ax,ax
	je $I2494
	invoke GETENTRYADDRESS, hModule, ax
    ret
$I2494:
	cwd
	ret

GETSYMBOLADDR ENDP

END
