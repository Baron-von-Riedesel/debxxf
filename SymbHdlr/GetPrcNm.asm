
	.286
	.model small
	.386

GETNRESNAMES proto FAR PASCAL :WORD
GLOBALFREE proto FAR PASCAL :WORD

	.code

	PUBLIC	_SearchName
	PUBLIC	GETPROCEDURENAMEEX

_SearchName PROC NEAR
; Line 22
	push bp
	mov bp, sp
	sub sp, 2
	push	di
	push	si
;	i = -2
;	lpp2 = -6
;	lpp1 = 4
;	wLen = 8
;	wIndex = 10
;	lpstr = 12
;	wSize = 16
	mov	si,WORD PTR [bp+4]	;lpp1
; Line 26
	mov	es,WORD PTR [bp+6]
	cmp	BYTE PTR es:[si],0
	je	$FB2436
	mov	cx,WORD PTR [bp+10]	;wIndex
$FC2435:
; Line 29
	mov	al,BYTE PTR es:[si]
	cbw	
	mov	di,ax
	mov	bx,si
	cmp	WORD PTR es:[bx+1][di],cx
	je	$L2476
; Line 38
	cbw	
	add	ax,3
	add	si,ax
	cmp	BYTE PTR es:[si],0
	jne	$FC2435
	jmp	SHORT $FB2436
$L2476:
	mov	WORD PTR [bp+4],si	;lpp1
; Line 32
	inc	WORD PTR [bp+4]	;lpp1
	mov	al,BYTE PTR es:[bx]
	cbw	
	mov	cx,WORD PTR [bp+16]	;wSize
	dec	cx
	cmp	ax,cx
	jbe	$L2479
	mov	ax,cx
$L2479:
	mov	WORD PTR [bp+16],ax	;wSize
	mov	WORD PTR [bp-2],ax	;i
	or	ax,ax
	je	$FB2440
	mov	ax,WORD PTR [bp+12]	;lpstr
	mov	dx,WORD PTR [bp+14]
	mov	bx,WORD PTR [bp+4]	;lpp1
	mov	cx,WORD PTR [bp-2]	;i
	shr	cx,1
	push	ds
	push	es
	mov	di,ax
	mov	si,bx
	mov	es,dx
	pop	ds
	ASSUME DS: DGROUP
	rep	movsw
	jae	$L2480
	movsb
$L2480:
	pop	ds
	ASSUME DS: DGROUP
	mov	ax,WORD PTR [bp-2]	;i
	add	WORD PTR [bp+12],ax	;lpstr
; Line 33
$FB2440:
; Line 34
	les	bx,DWORD PTR [bp+12]	;lpstr
	mov	BYTE PTR es:[bx],0
; Line 35
	mov	ax,WORD PTR [bp+16]	;wSize
	jmp	SHORT $EX2431
; Line 38
$FB2436:
; Line 39
	xor	ax,ax
; Line 40
$EX2431:
	pop	si
	pop	di
	mov sp, bp
	pop bp
	ret

_SearchName ENDP

;	hModule = 16
;	wIndex = 14
;	lpstr = 10
;	wSize = 8
;	hMem = 6
;	lpmodstruc = -4
;	flen = -12
;	y = -10
;	lpp1 = -8

GETPROCEDURENAMEEX PROC FAR
; Line 53
	push bp
	mov bp, sp
	sub sp, 12
	push si
; Line 62
	push	WORD PTR [bp+8]	;wSize
	push	WORD PTR [bp+12]
	push	WORD PTR [bp+10]	;lpstr
	push	WORD PTR [bp+14]	;wIndex
	mov	ax,WORD PTR [bp+16]	;hModule
	and	al,252	;00fcH
	sub	bx,bx
	mov	es,ax
	mov	si,bx
	mov	WORD PTR [bp-2],es
	mov	ax,WORD PTR es:40
	sub	ax,WORD PTR es:38
	push	ax
	mov	cx,WORD PTR es:[si+38]
	push	es
	push	cx
	call	_SearchName
	add	sp,14	;000eH
	mov	WORD PTR [bp-10],ax	;y
	or	ax,ax
	je	$I2456
; Line 63
	or	ah,128	;0080H
	jmp	SHORT $EX2451
; Line 64
$I2456:
	mov	es,WORD PTR [bp-2]
	mov	ax,WORD PTR es:32
	mov	WORD PTR [bp-12],ax	;flen
; Line 65
	mov	ax,WORD PTR [bp+6]	;hMem
	and	al,252	;00fcH
	mov	dx,ax
	sub	cx,cx
	mov	WORD PTR [bp-8],cx	;lpp1
	or	dx,cx
	jne	$L2482
	mov	si,WORD PTR [bp-10]	;y
	jmp	SHORT $I2457
$L2482:
; Line 67
	push	WORD PTR [bp+8]	;wSize
	push	WORD PTR [bp+12]
	push	WORD PTR [bp+10]	;lpstr
	push	WORD PTR [bp+14]	;wIndex
	push	WORD PTR [bp-12]	;flen
	push	ax
	push	WORD PTR [bp-8]	;lpp1
	call	_SearchName
	add	sp,14	;000eH
	mov	si,ax
; Line 70
$I2457:
	mov	ax,si
; Line 71
$EX2451:
	pop si
	mov sp, bp
	pop bp
	ret 12	;0000000cH

GETPROCEDURENAMEEX ENDP

GETPROCEDURENAME PROC FAR pascal uses si di hModule:word, wIndex:word, lpstr:dword, wSize:word

	invoke GETNRESNAMES, hModule
	mov	di,ax
	or	di,ax
	je	$I2469
	push	hModule
	push	wIndex
	push	lpstr
	push	wSize
	push	di
	push	cs
	call	NEAR PTR GETPROCEDURENAMEEX
	mov	si,ax
	invoke	GLOBALFREE, di
	jmp	SHORT $I2470
$I2469:
	xor	si,si
$I2470:
	mov	ax,si
	ret
	align 2
    
GETPROCEDURENAME ENDP

END
