
;--- defines procedures
;--- PowerOf10
;--- FloatToBCD
;--- FloatToStr

	.386
if ?FLAT
	.model flat
else
	.model tiny
endif
	option casemap:none

	include const.inc
	include function.inc
	include debxxfd.inc

; These are bits in the FP status word.

FP_LESSTHAN	equ	01h
FP_EQUALTO	equ	40h

@byte	equ <byte ptr>
@word	equ <word ptr>
@dword	equ <dword ptr>

	.code

ten		dq 10.0
ten16	dq 1.0e16
rounder	dq 5.0e10

ten_1	dt 1.0e1	;10.0
	dt 1.0e2		;100.0
	dt 1.0e3		;1,000.0
	dt 1.0e4		;10,000.0
	dt 1.0e5		;100,000.0
	dt 1.0e6		;1,000,000.0
	dt 1.0e7		;10,000,000.0
	dt 1.0e8		;100,000,000.0
	dt 1.0e9		;1,000,000,000.0
	dt 1.0e10		;10,000,000,000.0
	dt 1.0e11		;100,000,000,000.0
	dt 1.0e12		;1,000,000,000,000.0
	dt 1.0e13		;10,000,000,000,000.0
	dt 1.0e14		;100,000,000,000,000.0
	dt 1.0e15		;1,000,000,000,000,000.0

ten_16	dt 1.0e16
	dt 1.0e32
	dt 1.0e48
	dt 1.0e64
	dt 1.0e80
	dt 1.0e96
	dt 1.0e112
	dt 1.0e128
	dt 1.0e144
	dt 1.0e160
	dt 1.0e176
	dt 1.0e192
	dt 1.0e208
	dt 1.0e224
	dt 1.0e240

ten_256	dt 1.0e256

; The remaining exponents are only necessary if we decide to support
; 10-byte doubles.  FloatToStr and StrToFloat only support 8-byte,
; but PowerOf10 doesn't care, so we'll include them.

	dt 1.0e512
	dt 1.0e768
	dt 1.0e1024
	dt 1.0e1280
	dt 1.0e1536
	dt 1.0e1792
	dt 1.0e2048
	dt 1.0e2304
	dt 1.0e2560
	dt 1.0e2816
	dt 1.0e3072
	dt 1.0e3328
	dt 1.0e3584
	dt 1.0e4096
	dt 1.0e4352
	dt 1.0e4608
	dt 1.0e4864

;--- in: EAX

PowerOf10 PROC public

	mov ecx, eax
	cmp eax, 0		;.IF	(SDWORD PTR eax < 0)
	jge @F
	neg eax
@@:
	fld1
	mov dl, al
	and edx, 0fh
	je @F			;.IF (!ZERO?)
	lea edx, [edx+edx*4]
	fld ten_1[edx*2-10]
	fmulp st(1), st
@@:
	mov dl, al
	shr dl, 4
	and edx, 0fh
	je @F			;.IF (!ZERO?)
	lea edx, [edx+edx*4]
	fld ten_16[edx*2-10]
	fmulp st(1), st
@@:
	mov dl, ah
	and edx, 1fh
	je @F			;.IF (!ZERO?)
	lea edx, [edx+edx*4]
	fld ten_256[edx*2-10]
	fmulp st(1), st
@@:
	cmp ecx, 0		;.IF (SDWORD PTR ecx < 0)
	jge @F
	fdivp st(1), st
	ret
@@:
	fmulp st(1), st
	ret

PowerOf10 ENDP

; Convert a floating point register to ASCII.  For internal use.
; The result always has exactly 18 digits, with zero padding on the
; left if required.
;
; Entry:    ST(0) = a number to convert, 0 <= ST(0) < 1E19.
;             edi = an 18-character buffer.
;

FloatToBCD PROC public uses esi

	sub esp, 12

	; The fbstp instruction converts the top of the stack to a
	; packed BCD form in ten bytes, with two digits per byte.  The top 
	; byte has the sign, which we ignore.

	fbstp [esp]

	; Now we need to unpack the BCD to ASCII.

	lea esi, [esp+8]
	mov ecx, 9

nextdigits:
	mov al, [esi]	; xxxx xxxx AAAA BBBB
	dec esi
	rol ax, 12		; BBBB xxxx xxxx AAAA
	rol ah, 4		; xxxx BBBB xxxx AAAA
	and ax, 0f0fh	; 0000 BBBB 0000 AAAA
	add ax, 3030h	; 0011 BBBB 0011 AAAA
	mov [edi], ax
	add edi, 2
	dec ecx
	jnz nextdigits
	add esp,12
	ret

FloatToBCD ENDP

;
; Convert a double precision number to a string.
;
; Entry:    fpin = 8-byte double to convert
;          szDbl = character buffer
;
; Exit:    szDbl = converted value
;
; szDbl should be at least 19 bytes long.
;

FloatToStr PROC stdcall public USES esi edi pfpin: ptr TBYTE, szDbl: PTR BYTE

	LOCAL iExp: DWORD
	LOCAL mystat: WORD
	local fpin: TBYTE
	LOCAL stat: WORD
	local szTemp[22]:BYTE


; Special case zero.  fxtract fails for zero.

	mov edi, [szDbl]

	mov edx, pfpin
	mov eax, @dword [edx+0]
	mov ecx, @dword [edx+4]
	movzx edx, @word [edx+8]
	mov @dword [fpin+0],eax
	mov @dword [fpin+4],ecx
	mov @word [fpin+8],dx
	or eax, edx
	or eax, ecx
	jnz @F
	mov @word [edi+0], '0'
	jmp ftsExit2
@@:

; Check for a negative number.

	test @byte [fpin+9],80h
	jz ispositive
	and @byte [fpin+9], 07fh	; change to positive
	mov @byte [edi], '-'		; store a minus sign
	inc edi
ispositive:

; Initialize the floating point unit and load our value onto the stack.

	fclex
	fstcw [stat]
	mov [mystat], 027fh
	fldcw [mystat]

	fld [fpin]
	fld st(0)

; Compute the closest power of 10 below the number.  We can't get an
; exact value because of rounding.  We could get close by adding in
; log10(mantissa), but it still wouldn't be exact.  Since we'll have to
; check the result anyway, it's silly to waste cycles worrying about
; the mantissa.
;
; The exponent is basically log2(fpin).  Those of you who remember
; algebra realize that log2(fpin) x log10(2) = log10(fpin), which is
; what we want.

	fxtract				; ST=> mantissa, exponent, fpin
	fstp st(0)			; drop the mantissa
	fldlg2				; push log10(2)
	fmulp st(1), st		; ST = log10(fpin), fpin
	fistp [iExp]		; ST = fpin

; An 8-byte double can carry almost 16 digits of precision.  Actually, it's
; 15.9 digits, so some numbers close to 1E17 will be wrong in the bottom
; digit.  If this is a concern, change the '16' to a '15'.
;
; A 10-byte double can carry almost 19 digits, but fbstp only stores the
; guaranteed 18.  If you're doing 10-byte doubles, change the '16' to '18'.

	cmp [iExp],18
	jnc notbelow18
	fld st(0)		; ST = fpin, fpin
	frndint			; ST = int(fpin), fpin
	fcomp st(1)		; ST = fpin, status set
	fstsw ax
	test ah, FP_EQUALTO
	jz notequal

; We have an integer!  Lucky day.  Go convert it into a temp buffer.

	push edi
	lea edi, [szTemp]
	call FloatToBCD
	pop edi

	mov eax, 17
	mov ecx, [iExp]
	sub eax, ecx
	inc ecx
	lea esi, [szTemp+eax]

; The off-by-one order of magnitude problem below can hit us here.  
; We just trim off the possible leading zero.

	cmp @byte [esi],'0'
	jnz @F
	inc esi
	dec ecx
@@:

; Copy the rest of the converted BCD value to our buffer.

	rep movsb
	jmp ftsExit

notequal:
notbelow18:

; Have fbstp round to 17 places.

	mov eax, 16		; experiment
	sub eax, [iExp]	; adjust exponent to 17
	call PowerOf10

; Either we have exactly 17 digits, or we have exactly 16 digits.  We can
; detect that condition and adjust now.

	fcom [ten16]
	; x0xxxx00 means top of stack > ten16
	; x0xxxx01 means top of stack < ten16
	; x1xxxx00 means top of stack = ten16
	fstsw ax
	test ah,1
	jz @F
	fmul [ten]
	dec iExp
@@:

; Go convert to BCD.

	push edi
	lea  edi, [szTemp]
	call FloatToBCD
	pop edi

	lea esi, [szTemp+1]		; point to converted buffer

; If the exponent is between -15 and 16, we should express this as a number
; without scientific notation.

	mov ecx, [iExp]
	push ecx
	add ecx,15
	cmp ecx,15+16
	pop ecx
	ja sm6

; If the exponent is less than zero, we insert '0.', then -ecx
; leading zeros, then 16 digits of mantissa.  If the exponent is
; positive, we copy ecx+1 digits, then a decimal point (maybe), then 
; the remaining 16-ecx digits.

	inc ecx
	cmp ecx, 0
	jg ispos1
	mov @word [edi], '.0'
	add edi, 2
	neg ecx
	mov al, '0'
	rep stosb
	mov ecx, 16
	jmp sm3
ispos1:
	rep movsb
	mov @byte [edi], '.'
	inc edi
	mov ecx, 16
	sub ecx, [iExp]
sm3:
	rep movsb

; Trim off trailing zeros.

nextitem2:
	cmp @byte [edi-1],'0'
	jnz @F
	dec edi
	jmp nextitem2
@@:

; If we cleared out all the decimal digits, kill the decimal point, too.

	cmp @byte [edi-1], '.'
	jnz @F
	dec edi
@@:

; That's it.

	jmp ftsExit
sm6:


; Now convert this to a standard, usable format.  If needed, a minus
; sign is already present in the outgoing buffer, and edi already points
; past it.

	movsb				; copy the first digit
	mov @byte [edi], '.'; plop in a decimal point
	inc edi
	movsd				; copy four more digits
	movsw				; copy two more digits

if 0

; The printf %g specified trims off trailing zeros here.  I dislike
; this, so I've disabled it.  Comment out the if 0 and endif if you
; want this.

@@:
	cmp @byte [edi-1],'0'
	jz @F
	dec edi
	jmp @B
@@:
endif

; Shove in the exponent.  If you support 10-byte reals, remember to
; allow 4 digits for the exponent.

	mov @byte [edi], 'e'	; start the exponent
	mov eax, [iExp]
	and eax, eax
	jns @F
	mov @byte [edi+1], '-'
	neg eax
	jmp sm8
@@:
	mov @byte [edi+1], '+'
sm8:

	mov ecx, 10

	xor edx, edx
	div ecx
	add dl, '0'
	mov [edi+4], dl	; shove in the ones exponent digit

	xor edx, edx
	div ecx
	add dl, '0'
	mov [edi+3], dl	; shove in the tens exponent digit

	xor edx, edx
	div ecx
	add dl, '0'
	mov [edi+2], dl	; shove in the hundreds exponent digit

	add edi, 5		; point to terminator

; Clean up and go home.

ftsExit:
	mov @byte [edi], 0
	fldcw [stat]		; restore control word
	fwait
ftsExit2:
	ret

FloatToStr ENDP

	end
