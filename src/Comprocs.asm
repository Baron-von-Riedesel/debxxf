
;--- serial port handling

	.386
if ?FLAT
	.MODEL FLAT
else
	.MODEL TINY
endif
	option proc:private
	option casemap:none

	include const.inc
	include ascii.inc
	include function.inc
	include keyboard.inc
	include dpmi.inc
	include putf.inc
	include debxxfd.inc
	include errors.inc
	include toolhelp.inc
	include fcntl.inc
	include extern32.inc
	include extern16.inc

XON 	equ 11h			; Ctrl-Q
XOFF	equ 13h			; Ctrl-S

?CHECKINPSTAT@CR equ 1	; check input status only if CR is displayed

CF_CTS     equ 1	; check CTS
CF_XONXOFF equ 2	; check XON/XOFF

	.DATA

if ?32BIT
oldint0B  PF32 0
else
oldint0B  PF16 0
endif
comport   dd 0
_comno    dd 1		; current COM #
comirq	  db 0
oldiereg  db 0		; original inhalt interrupt enable register

bEscSeqf  db 0
bEscChar  db 0

bComFlags db CF_CTS or CF_XONXOFF
bInit     db 0

	.CODE

installirqcom proc stdcall public

	test [__inpmode],_SERINP
	jz installirqcom_ex
	cmp word ptr [oldint0B+?SEGOFFS],0
	jnz installirqcom_ex
	@loadflat
;	call _GetComNum
	mov eax, [_comno]
	mov ebx,eax
	mov bx,@flat:[ebx*2-2+400h]
	and ebx,ebx
	jz installirqcom_ex

	mov [comport],ebx
	and al,1
	xor al,1
	sub al,0Ch		  ;0B oder 0C
	neg al
	mov bl,al
	mov [comirq],al
	sub al,8		  ;3=COM2/COM4	4=COM1/COM3
	mov cl,al
	mov ax,1
	shl ax,cl
	or [wPicOn],ax
	xor ax,0FFFFh
	and [wPicValue],ax

	mov ax, 204h
	int 31h
	mov dword ptr [oldint0B+0],edx
	mov word ptr [oldint0B+?SEGOFFS],cx
if ?CS16ALIAS
	mov ecx,[__cs16alias]
else
	mov ecx,cs
endif        
	mov edx,offset intr0B0C
	mov ax, 205h
	int 31h

	mov edx,[comport]
	mov ecx,edx
	in al,dx			;sicherheitshalber buffer leeren
	inc edx
	in al,dx
	mov [oldiereg],al

	add dx,3			;DTR setzen
	mov al,0Bh
	out dx,al

	call resetctrlctrap
installirqcom_ex:
	ret
installirqcom endp

setctrlctrap proc stdcall public
	push eax
	mov al,1
	call setiereg
	pop eax
	ret
setctrlctrap endp

;--- dont change any register

resetctrlctrap proc stdcall public
	push eax
	mov al,00
	call setiereg
	pop eax
	ret
resetctrlctrap endp

setiereg:
	test [__inpmode], _SERINP
	jz @F
	push edx
	mov edx,[comport]
	inc edx
	out dx,al			;interrupt enable register setzen
	pop edx
@@:
	ret

;-------------------------------------------------
;--- IRQ 0B/0C

intr0B0C proc
	@switch32bit
	push eax
	push edx
	mov edx,cs:[comport]
	inc edx
	inc edx
	in al,dx
	test al,1		   ;liegt interrupt an?
	jz @F
	pop edx
	pop eax
	jmp cs:[oldint0B]
@@:
	dec edx
	dec edx
	in al,dx
	cmp al,3		   ;CTRL-C?
	jnz exit
	push ds
	mov ds,cs:[__csalias]
	and [fMode], not FMODE_EXEACTIVE
	test [fMode], FMODE_INDEBUG
	jnz done
	test byte ptr fKeybrd,FKB_EXECINT01 or FKB_EXECINT03
	jz settraceflg
	test byte ptr fKeybrd,FKB_EXECINT03
	pop ds
	mov al,20h
	out 20h,al
	pop edx
	pop eax
	sti
	jnz @F
	int 01
	@iret
@@:
	int 03
	@iret
settraceflg:
	push ebp
	@loadesp ebp
	or byte ptr [ebp+4*4].IRETS.rFL+1,FL_TRACE
	pop ebp
done:
	pop ds
exit:
	mov al,20h
	out 20h,al
	pop edx
	pop eax
	sti
	@iret
intr0B0C endp

;*** XonXoffCheck: called by comwrite
;*** inp: AL=char to output

XonXoffCheck proc stdcall uses ds

	pushad
	mov ds, cs:[__csalias]	; DS evtl. undefiniert!
	test fEscapemode, 1		; nicht wenn wir escapes an terminal
	jnz exit				; senden
	inc [ttycurpos]
	cmp al, cr
	jnz @F
	call checkifshouldwait	; schauen ob waitl erreicht
@@:
if ?CHECKINPSTAT@CR
	cmp al, cr				; nur pruefen bei CR
	jnz exit
endif
	call GetInpStatus
	jz exit					; kein zeichen da
	call getcharex			; get key (ohne translate)
	call ctrlccheck
	cmp al,XOFF
	jnz exit
@@:
	call GetInpStatus
	jz @B
	call getcharex			; get key (ohne translate)
	call ctrlccheck
	cmp al,XON
	jnz @B
exit:
	popad
	ret
XonXoffCheck endp

;*** ctsCheck: Check CTS before writing byte to UART

ctsCheck proc stdcall
	ret
ctsCheck endp

deinstallirqcom proc stdcall public
	pushad
	mov cx,word ptr [oldint0B+?SEGOFFS]
	jcxz @F
	mov edx,dword ptr [oldint0B+0]
	mov bl,[comirq]
	mov ax, 205h
	int 31h
	mov edx,[comport]
	mov al,[oldiereg]
	inc edx
	out dx,al
@@:
	popad
	ret
deinstallirqcom endp

; *** status von COMx holen (funktioniert auch bei gesperrten interrupts) ***

GetComStatus proc stdcall public
	push eax
	push edx
	mov edx,cs:[comport]
	add dx,5
	in al,dx
	test al,1				;liegt zeichen im Input buffer?
	pop edx
	pop eax
	ret
GetComStatus endp

termemu proc stdcall
	cmp ax,"[A"
	jz te_cursorup
	cmp ax,"[B"
	jz te_cursordown
	cmp ax,"[C"
	jz te_cursorright
	cmp ax,"[D"
	jz te_cursorleft
	mov ax,00
	ret
te_cursorup:
	mov ax,__CURSOR_UP * 256 + 0E0h
	ret
te_cursordown:
	mov ax,__CURSOR_DOWN * 256 + 0E0h
	ret
te_cursorright:
	mov ax,__CURSOR_RIGHT * 256 + 0E0h
	ret
te_cursorleft:
	mov ax,__CURSOR_LEFT * 256 + 0E0h
	ret

termemu endp

normemu proc stdcall
	cmp al,7Fh				  ;DELETE
	jz ne_1
	ret
ne_1:
	mov ax,__DEL_MAKE * 256 + 0E0h
	ret
normemu endp

ComWrite proc stdcall uses ebx port:dword,zeichen:dword

	mov ebx,port
	movzx ebx,bl
	dec ebx
	cmp ebx,4
	jnb error
	add ebx,ebx
	mov bx,@flat:[ebx+400h]
	and ebx,ebx
	jz error

;--- check if LSR.TEMT is ok ( last byte sent )

	mov edx, ebx
	add edx, 5			;LSR - Leitungs Status Register
	xor ecx, ecx
@@:
	in al, dx
	test al, 40h		;TEMT - transmitter empty?
	loopz @B

	test cs:bComFlags, CF_XONXOFF
	jz @F
	mov al,byte ptr zeichen
	call XonXoffCheck
@@:
	test cs:bComFlags, CF_CTS
	jz sm2
	call ctsCheck
	jnc sm2
error:
	xor eax,eax
	jmp sm1
sm2:
	mov edx,ebx
	mov al,byte ptr zeichen
	out dx,al
	mov al,1
sm1:
	ret
ComWrite endp

InitCOM proc stdcall uses ds ebx

	mov ds, [__csalias]
	or bInit,1
	xor eax,eax
	mov ebx,[_comno]
	movzx ebx,bl
	dec ebx
	cmp ebx,4
	jnb InitCOMErr
	add ebx,ebx
	mov bx,word ptr @flat:[ebx+400h]
	and ebx,ebx
	jz InitCOMErr
	mov edx,ebx
	add edx,4
	in al, dx
	or al, 1		; set DTR
	out dx, al
InitCOMErr:
	ret
InitCOM endp


_AUXPutChar proc stdcall public char:dword

	pushad
	test   cs:bInit,1
	jz @F
	invoke InitCOM
@@:
	invoke ComWrite, cs:[_comno], char
	popad
	ret

_AUXPutChar endp

translatechar proc
	cmp al,1Bh
	jnz @F
	xor bEscSeqf,1
	jz @F
	mov bEscChar, 0
	mov al,00
	ret
@@:
	test bEscSeqf,1			; liegt escapeseq an?
	jnz @F
	call normemu
	ret
@@:
	cmp bEscChar,0
	jnz @F
	xchg al, bEscChar
	ret 					; AX=0 -> kein zeichen
@@:
	xchg ah, bEscChar
	mov bEscSeqf, 0
	call termemu
	ret
translatechar endp

;*** get char from COMx ( works with interrupts disabled)

GetComChar proc stdcall public
	call GetComStatus
	jnz @F
	xor eax,eax
	ret
@@:
	mov ah, 00
	push edx
	mov edx,[comport]
	in al, dx
	pop edx
	call translatechar
	ret

GetComChar endp

;*** get char from I14

GetI14Char proc stdcall public
	call _I14GetChar
	test ah,80h	; timeout?
	mov ah,00
	jz @F
	xor eax, eax			; then return 00
	ret
@@:
	call translatechar
	ret
GetI14Char endp

_I14GetChar proc stdcall

	push edx
	mov  edx,cs:[_comno]
	dec  edx
	mov  ah,2
	int  14h
	pop  edx
	ret
_I14GetChar endp

_I14PutChar proc stdcall public zeichen:dword

	push edx
	mov edx, [_comno]
	dec edx
	mov al, byte ptr zeichen
	mov ah, 01h
	int 14h
	call XonXoffCheck
	pop edx
	ret
_I14PutChar endp

SetComSpeed proc stdcall public uses ebx comnr:dword,speed:dword

local	port:dword

		xor 	eax,eax 	;rc=0 ist fehler
		xor 	edx,edx
		mov 	ecx,speed
		jecxz	exit		;0 nicht moeglich
		mov 	eax,115200
		div 	ecx
		and 	edx,edx 	;rest uebrig?
		jnz 	exit
		mov 	ecx,eax 	;teiler -> ecx
		mov 	ebx,comnr	;1-4
		dec 	ebx 		;0-3
		cmp 	ebx,4
		jnb 	exit
		add 	ebx,ebx
		mov 	ax,@flat:[ebx+400h]
		and 	eax,eax
		jz		exit
		mov 	port,eax
		mov 	edx,port
		add 	dl,3
		in		al,dx
		push	eax
		or		al,80h	   ;baudratenregister selektieren
		out 	dx,al
		mov 	edx,port
		mov 	al,cl
		out 	dx,al
		inc 	dl
		mov 	al,ch
		out 	dx,al
		add 	dl,2
		pop 	eax
		out 	dx,al
		mov 	al,1
exit:
		ret
SetComSpeed endp

GetComSpeed proc stdcall public uses ebx comnr:dword

local	port:dword

		xor 	eax,eax
		mov 	ebx,comnr
		dec 	ebx
		cmp 	ebx,4
		jnb 	getcomspeed_err
		add 	ebx,ebx
		mov 	ax,@flat:[ebx+400h]
		and 	eax,eax
		jz		getcomspeed_err
		mov 	port,eax

		mov 	edx,eax
		add 	dl,3
		in		al,dx
		push	eax
		or		al,80h		 ;baudratenregister selektieren
		out 	dx,al
		mov 	edx,port
		in		al,dx
		inc 	dl
		mov 	ah,al
		in		al,dx
		add 	dl,2
		xchg	ah,al
		movzx	ecx,ax
		pop 	eax
		out 	dx,al
		mov 	eax,115200
		xor 	edx,edx
		cmp 	ecx,1
		jb		getcomspeed_1
		div 	ecx
getcomspeed_1:
getcomspeed_err:
		ret
GetComSpeed endp

if ?SUPPVCD

xfstr	db "COM%u autoassign=%d",cr,lf,0
xfstr2	db "COM%u owner=%0X",cr,lf,0

_vcd proc c public pb:PARMBLK

local	vcdproc:PF16
local	array:dword
local	szStr[80]:byte

	call checkifenhmode
if ?WINDOWS 						 ;folgendes geht nur unter windows
	mov ax,1684h
	mov bx,VCD_DEVICE_ID
	push es
	@callint2F
	mov eax,es
	pop es
	mov cx,ax
	or cx,di
	jz novcd
	mov word ptr vcdproc+0,di
	mov word ptr vcdproc+2,ax
if 0
	mov dx,0000				 ;get version
	call vcdproc
	@stroutc "version="
	@wordout eax
	@cr_out
endif
	mov dx,0001				 ;get port array
	call vcdproc
	mov array,eax
if 0
	@stroutc "port array="
	@wordout eax
	@cr_out
endif
	mov eax,1
	mov cx,0
vcd_01:
	pushad
	test eax,array
	jz nextcom
	mov dx,0002
	call vcdproc
	jc nextcom
	push bx
	push ax
	pop ebx
	inc ecx
	invoke printf, offset xfstr, ecx, ebx
nextcom:
	popad
	inc ecx
	shl eax,1
	and ax,ax
	jnz vcd_01
novcd:
endif
	mov ecx,4
	mov eax,1
vcd_1:
	pushad
	@ring0call getcomx
	jc	 @F
	mov  ecx,eax
;	invoke sprintf,ebx,ecx,addr xfstr2,addr szStr
	invoke sprintf,addr szStr, addr xfstr2, ecx, ebx

	lea eax,szStr
	@strout eax
@@:
	popad
	inc eax
	loop vcd_1
	ret
_vcd endp

getcomx:
	push ds
	push es

	push ss
	pop ds
	push ss
	pop es
	VxDCall VCD_DEVICE_ID,VCD_Get_Focus

	pop es
	pop ds
	ret

endif

	END

