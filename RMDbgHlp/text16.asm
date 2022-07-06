
	.286
	.MODEL SMALL
	.386
	option casemap:none

?WINDOWS	equ 0
?32BIT		equ 0

	include const.inc
	include dpmi.inc
	include rmdbghlp.inc

;--- flags in highbyte EFL

FL_TRACE    equ 1
FL_INT      equ 2

DGROUP  group _TEXT, _DATA

VCPI proto far pascal :word

	public csalias
	public cssegm

	.code

vars DEBRMVAR <sizeof DEBRMVAR, offset Wep, offset cancelrm, offset intr00r, offset pm2rmentry, offset intr21r, offset VCPI>

;--- this code should be callable by 32bit code with ESP >= 10000h.
;--- so dont access stack variables with [bp+xx]!

;Init proc far stdcall uses ds wAlias:WORD, wSegment:WORD
Init proc far stdcall

	pop ecx		;far ret offset
	pop edx		;far ret segm
	pop eax		;wAlias+wSegm
	push edx
	push ecx
;	mov	 ax, wAlias
	push ds
	mov ds, ax
	mov ds:csalias, ax
	shr eax,16
;	mov ax, wSegment
	mov ds:cssegm, ax

if 0
	mov vars.wExit, offset Wep
	mov vars.wCancelRM, offset cancelrm
	mov vars.wIntr00r, offset intr00r
	mov vars.wPM2RMEntry, offset pm2rmentry
	mov vars.wIntr21r, offset intr21r
	mov vars.wVCPI, offset VCPI
endif
	push bx
	mov bl,1
	mov ax,0200h
	int 31h
	mov word ptr vars.oldint01r+0,dx
	mov word ptr vars.oldint01r+2,cx
	mov bl,3
	int 31h
	mov word ptr vars.oldint03r+0,dx
	mov word ptr vars.oldint03r+2,cx
	pop bx
if ?TRAPRM15
	mov vars.wIntr15r, offset myint15r
endif
if ?TRAPRM2A        
	mov vars.wIntr2Ar, offset myint2Ar
endif
if ?TRAPRM2F
	mov vars.wIntr2Fr, offset myint2Fr
endif
	mov ax, offset vars
	pop ds
	db 66h	;do a far ret
	ret
Init endp

csalias dw 0
cssegm	dw 0

Wep proc far
	push 1
	push 0
	push cs
	call near32 ptr VCPI
	db 66h
	ret
Wep endp

;--- the following code is executed in real-mode

startofrealmodesegm:

intr00r:
	push 0
	jmp  intrxxr
	push 1
	jmp  intrxxr
	push 2
	jmp  intrxxr
	push 3
	jmp  intrxxr
	push 4
	jmp  intrxxr
	push 5
	jmp  intrxxr
	push 6
	jmp  intrxxr
	push 7
;;	jmp  intrxxr
intrxxr:
	cli
	pop  cs:[vars.intno]
	pop  cs:[vars.rm.rIP]
	pop  cs:[vars.rm.rCS]
	pop  cs:[vars.rm.rFlags]
	mov  cs:[vars.rm.rSP],sp
	mov  cs:[vars.rm.rSS],ss
if 1
	pushfd
	add  sp,2
	pop  cs:[vars.flhigh]
endif
	push cs
	pop  ss
	mov  sp,offset vars.rm.rIP
	push gs
	push fs
	push ds
	push es
	sub  sp,2
	pushad
	lss  sp,cs:[vars.rm.rSSSP]
	smsw ax
	mov cs:[vars.bMode],al
if ?RMCALLBACK
	jmp  dword ptr cs:[intxxcb]
else
	mov  al, 0						; save task state 
	les  di, cs:[vars.savestatebuffRM]; es:di -> save state buffer 
	call dword ptr cs:[vars.rmsavestate]
	mov  ax, cs:[vars.pmds]			; DS
	mov  cx, ax						; ES
	mov  dx, ax						; SS
	mov  ebx, cs:[vars.pmesp]		; (E)SP
	mov  si, cs:[vars.pmcs]			; CS
	mov  edi, cs:[vars.pmeip]		; (E)IP
	jmp  dword ptr cs:[vars.rm2pmjump]
pm2rmentry: 						; real mode entry bei raw mode switch
	les  di, cs:[vars.savestatebuffRM]; zurueck in debuggee
	mov  al, 1						; restore task state
	call dword ptr cs:[vars.rmsavestate]
	cli
	mov  ax, cs
	mov  ss, ax
	mov  sp, offset vars.rm
	popad
	inc  sp
	inc  sp
	pop  es
	pop  ds
	pop  fs
	pop  gs
	lss  sp, dword ptr cs:[vars.rm.rSP]
	or byte ptr cs:[vars.flhigh], 1		;set restart flag
	push cs:[vars.flhigh]
	push cs:[vars.rm.rFlags]
	push 0
	push cs:[vars.rm.rCS]
	push 0
	push cs:[vars.rm.rIP]
	iretd

endif	;?RMCALLBACK

cancelrm:
	mov  ax,4CFFh
	int  21h			;int 21h im real mode
if ?TRAPRM214B
intr21r:
	pushf
	test cs:[vars.bFlags], FLRM_NOTACTIVE
	jnz  default
	cmp  ax,4B00h
	jz	 execrm_2
	cmp  ax,2501h
	jz	 setint01
	cmp  ax,2503h
	jz	 setint03
	cmp  ax,3501h
	jz	 getint01
	cmp  ax,3503h
	jz	 getint03
if ?TRAPRM214C
	cmp  ah,4ch
	jnz  @F
	int  3
@@:
	cmp  ah,31h
	jnz  @F
	int  3
@@:
	cmp  ah,00h
	jnz  @F
	int  3
@@:
endif
default:
	popf
	jmp  dword ptr cs:[vars.oldint21r]
setint01:
	mov  word ptr cs:[vars.oldint01r+0], dx
	mov  word ptr cs:[vars.oldint01r+2], ds
	popf
	iret
setint03:
	mov  word ptr cs:[vars.oldint03r+0], dx
	mov  word ptr cs:[vars.oldint03r+2], ds
	popf
	iret
getint01:
	les  bx, cs:[vars.oldint01r+0]
	popf
	iret
getint03:
	les  bx, cs:[vars.oldint03r+0]
	popf
	iret
execrm_2:						; int 21h,ax=4B00
	popf
	mov  cs:[vars.essave],es	; goal: we want to stop at
	pop  cs:[vars.ipsave]		; program entry.
	pop  cs:[vars.cssave]
	pop  cs:[vars.flsave]
	push ds
	push di
	mov  di,offset vars.execbl
	push es
	pop  ds
	push cs
	pop  es
	push si
	mov  si,bx
	mov  bx,di
	push cx
	mov  cx,14/2				; copy env, cmdline, fcb1, fcb2
	cld
	rep  movsw
	pop  cx
	pop  si
	pop  di
	pop  ds
	mov  al,01h					; load but dont execute
	pushf
	call dword ptr cs:[vars.oldint21r]
	mov  es,cs:[vars.essave]
	jc	 execrm_1
	lss  sp,dword ptr cs:[vars.execbl.sssp]
	mov  ah,62h 					  ; get psp
	int  21h						  ; int 21h real mode
	mov  es,bx
	mov  ds,bx
	mov  ax,cs:[vars.ipsave] 		  ; int 22 im psp setzen
	mov  bx,cs:[vars.cssave]
	mov  ds:[000Ah],ax
	mov  ds:[000Ch],bx
	xor  bx,bx
	pop  ax 						  ; ax auf child stack
	pushf
	push bp
	mov bp,sp
	or byte ptr [bp+2+1],FL_TRACE or FL_INT	;trace exception
	pop bp
	popf
	jmp dword ptr cs:[vars.execbl.csip]
execrm_1:
	jmp dword ptr cs:[vars.csipsave]


endif	;?TRAPRM214B

if ?TRAPRM2F

myint2Fr proc far				; doesn't work, since interrupt
	cmp 	ax,1687h			; may be handled in ring 0 (win9x)
	jz		@F
	jmp 	dword ptr cs:[vars.oldint2Fr]
@@:
	pushf
	call	dword ptr cs:[vars.oldint2Fr]
	and		ax,ax
	jnz		@F
	mov 	word ptr cs:[vars.truepmentry+0],di
	mov 	word ptr cs:[vars.truepmentry+2],es
	push	cs
	pop 	es
	mov 	di,offset mypmentry
@@:
	iret
mypmentry:
	call	dword ptr cs:[vars.truepmentry]
	jnc 	@F
	retf
@@:
	pushf
	push	bp
	mov 	bp,sp
	or		byte ptr [bp+2+1],1
	pop 	bp
	popf
	retf
myint2Fr endp
endif

if ?TRAPRM2A

myint2Ar proc far
	cmp ah, 80h						; begin critical section
	jz	int2ar80
	cmp ah, 81h						; begin critical section
	jz	int2ar81
	cmp ah, 82h						; begin critical section
	jz	int2ar82
int2ar:
	jmp dword ptr cs:[vars.oldint2Ar]
int2ar80:
	@rmout <"enter critical section",cr,lf>
	inc cs:[vars.int2ACnt]
	jmp int2ar
int2ar81:
	@rmout <"exit critical section",cr,lf>
	dec cs:[vars.int2ACnt]
	jmp int2ar
int2ar82:
	@rmout <"exit all critical sections",cr,lf>
	mov cs:[vars.int2ACnt],0
	jmp int2ar
myint2Ar endp
endif

if ?TRAPRM15

myint15r proc far
	cmp ax, 9000h
	jz	int15r90
	cmp ax, 9100h
	jz	int15r91
int15r:
	jmp dword ptr cs:[vars.oldint15r]
int15r90:
;	@rmout  <"int 15, ax=9000",cr,lf>
	inc cs:[vars.int15Cnt]
	jmp int15r
int15r91:
;	@rmout  <"int 15, ax=9100",cr,lf>
	dec cs:[vars.int15Cnt]
	jmp int15r
myint15r endp
endif

if 0 ; ?RMSTRUSED
rmstrout:
	movzx esp, sp
	xchg si, [esp]
	pushf
	push ax
	in al,21h
	push ax
	mov al,-1
	out 21h,al
@@:
	lodsb byte ptr cs:[vars.essave]
	and al,al
	jz @F
	mov ah,0Eh
	int 10h
	jmp @B
@@:
	pop ax
	out 21h,al
	pop ax
	popf
	movzx esp, sp
	xchg si, [esp]
	ret
endif

endofrealmodesegm equ $

	end
