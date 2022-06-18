
	.386
if ?FLAT
	.MODEL FLAT
else
	.MODEL SMALL
endif
	option proc:private
	option casemap:none

	.nolist
	include const.inc
	include ascii.inc
	include function.inc
	include debxxfd.inc
	include keyboard.inc
	include dpmi.inc
	include putf.inc
	include errors.inc
	include toolhelp.inc
	include fcntl.inc
	include extern32.inc
	include extern16.inc
	include isvbop.inc
	.list

;--- there was a problem in hdpmi v3.19, causing a crash in debxxf after
;--- a client has terminated and ?CATCHINT09 was active. Fixed in hdpmi v3.20

?CATCHINT09		equ 1 - ?WINDOWS
?LLSYSREQ		equ 1	; 1=low-level ctrl-sysreq detection
?USERMINT09		equ 0	; 1=call real mode IRQ1 directly

?SAVEINT09		equ 0	;1 - ?WINDOWS; not sure what the purpose of this is

?MAXSAVELINE	equ 12
?PROMPTSIZE		equ 3
?DOSLEEP		equ 1	; std=1
?USEINT16		equ 0	; 1=use int 16h instead of BIOS variable 41Ah/41Ch
?SAVEINT15RM	equ 1 - ?WINDOWS; 1=save/restore int 15h (because int 15h, ah=4Fh)
?DISABLEPD		equ 1 - ?WINDOWS; 1=disable "pointing device" while in debugger

;*** keyboard BIOS variables

KBDSTAT  equ   417h
BUFSTA	 equ   41Ah
BUFEND	 equ   41Ch
EBUFSTA  equ   480h
EBUFEND  equ   482h

ESCAPE	 equ 1Bh

@callint16 macro func
	mov ah, func
	@CallInt [oldint16]
endm

	.DATA

lastkey    dd 0			;taste mit der die eingabe beendet wurde
ttycurpos  dd 0
if ?CATCHINT09
 if ?USERMINT09
rmint09    dd 0			;real mode int 09
 endif
 if ?32BIT
oldint09 PF32 0
 else
oldint09 PF16 0
 endif
endif

if ?USEINT16
 if ?32BIT
oldint16 PF32 0
 else
oldint16 PF16 0
 endif
endif
if ?DISABLEPD
 if ?32BIT
oldint74 PF32 0
 else
oldint74 PF16 0
 endif
endif

if ?SAVEINT15RM
oldint15r   dd 0
savedint15r dd 0
endif

kbdvartab label byte
if ?CATCHINT09
	@symbol 'KEYBFlags'	   ,__BYTE__, ,fKeybrd
 if ?USERMINT09
	@symbol 'INT_09RM'	   ,_RDONLY_+__RMLPTR__, ,rmint09
 endif
 if ?32BIT
	@symbol 'INT_09'	   ,_RDONLY_+__FPTR__, ,oldint09
 else
	@symbol 'INT_09'	   ,_RDONLY_+__LPTR__, ,oldint09
 endif
endif
kbdvartabend label byte

fDontSave	db 0
bAlt		db 0
lastk		db 0

;--- key translation to use if keyboard polling is active

normalk db 0,1Bh			;00-01 NULL, escape
	db '1234567890-='		;02-0D
	db 8,9					;0E-0F backspace + tab
	db 'qwertyuiop[]'		;10-1B
	db 0Dh,0				;1C-1D return, ctrl-l 
	db "asdfghjkl;'`"		;1E-29
	db 0					;2A    shift-l
	db '\'					;2B
	db "zxcvbnm,./"			;2C-35
k56n db "\"        			;56

shiftk db 0,1Bh				;00-01
	db '!@#$%^&*()_+'		;02-0D
	db 8,0Fh				;0E-0F backspace + tab
	db 'QWERTYUIOP{}'		;10-1B
	db 0Dh,0				;1C-1D return, ctrl-l 
	db 'ASDFGHJKL:"~'		;1E-29
	db 0					;2A    shift-l
	db '|'					;2B
	db 'ZXCVBNM<>?'			;2C-35
k56s db "|"        			;56

altgrk	db 36h+1 dup (0)		;00-35, 56
        
ctrlk db 0,1Bh											;00-01
	db 0Ch dup (0)										;02-0D
	db 7Fh,0											;0E-0F
	db 11h,17h,05h,12h,14h,19h,15h,09h,0Fh,10h,1Bh,1Dh	;10-1B
	db 0Ah,00											;1C-1D
	db 01h,13h,04h,06h,07h,08h,0Ah,0Bh,0Ch				;1E-26
	db 0,0,0,0,0										;27-2B
	db 1Ah,18h,03h,16h,02h,0Eh,0Dh						;2C-32
	db 0,0,0											;33-35
	db 0												;56

	.CODE

if ?DISABLEPD
myint74 proc
	push es
	pushd 0	;ss:sp
	pushd 0	;cs:ip
	pushd 0	;gs,fs
	pushd 0	;ds,es
	pushf
	pushad
	@loadesp edi
	push ss
	pop es
	mov bl, 74h
	xor ecx, ecx
	mov ax, 300h
	int 31h
	popad
	add esp, 4*4+2
	pop es
	@iret
myint74 endp
endif

;--- cl=0 : standard keys
;--- cl=1 : shift keys
;--- cl=2 : altgr keys

setkeys proc stdcall public uses esi
	and eax,eax		;nothing read
	jz exit
	mov edi, offset normalk+2
	cmp cl,0
	jz @F
	mov edi, offset shiftk+2
	cmp cl,1
	jz @F
	mov edi, offset altgrk+2
@@:
	mov esi, pNearHeap
	mov ecx, eax
	cmp ecx, 37h
	jbe @F
	mov ecx, 37h
@@:
nextkey:
	lodsb
	cmp al,' '
	jbe @F
	mov [edi],al
@@:
	inc edi
	loop nextkey
exit:
	ret
setkeys endp

if ?CATCHINT09

if ?LLSYSREQ

;--- ensure that the bits in the BIOS data are set to allow
;--- Ctrl-SysReq detection.
;--- it might be better to trap rm int 15h, ah=85h instead.

setsysreqbits proc
	push ds
	mov ds, cs:[__flatsel]
	mov ah, al
	and al, 7Fh
	cmp al, 1Dh			; CTRL?
	jnz @F
	test ah,80h
	setz al
	shl al, 2
	and byte ptr ds:[417h], 0FBh
	or ds:[417h], al
	jmp done
@@:
	test cs:[fKeybrd], FKB_ALTSCROLL
	jz usesysreq
@@:
	cmp al,46h			; SCROLL?
	jnz done
	test ah,80h
	setz al
	shl al, 4
	and byte ptr ds:[418h], 0EFh
	or ds:[418h], al
	jmp done
usesysreq:
	cmp al,54h			; SYSREQ?
	jnz done
	test ah,80h
	setz al
	shl al, 2
	and byte ptr ds:[418h], 0FBh	;reset sysreq
	or ds:[418h], al
done:
	pop ds
	ret
setsysreqbits endp
endif

;*** int 09 interrupt handler

myint09 proc far public

 if ?HIDEINT09
	test cs:[fMode], FMODE_INDEBUG
	jnz indebug
  if ?LLSYSREQ
	push eax
	in al, 64h
	in al, 60h
	push eax
  endif
	@CallInt [oldi09p]	; if debuggee won't set BIOS variables, we can't detect sysreq!
  if ?LLSYSREQ
	pop eax
	call setsysreqbits
	pop eax
  endif
	jmp sm1
indebug:
 endif

ife ?USERMINT09
	@CallInt [oldint09]
else
	pushad
	push es
	sub esp, sizeof RMCS
	@loadesp ebp
	mov eax, cs:[rmint09]
	xor ecx, ecx
	mov [ebp].RMCS.rCSIP, eax
	mov [ebp].RMCS.rSSSP, ecx
	mov [ebp].RMCS.rFlags, 002h	; IF clear
	mov edi,ebp
	push ss
	pop es
	xor ebx, ebx
	@DpmiCall 302h		; call real mode far proc with IRET frame
	add esp, sizeof RMCS
	pop es
	popad
endif
	test cs:[fMode], FMODE_INDEBUG	; do nothing while in debugger
	jnz @F
sm1:
	test cs:[fKeybrd], FKB_SYSREQ
	jnz checksysreq
@@:
exit:
	@iret

checksysreq:
	push ds
	push eax
	mov ds, cs:[__flatsel]
	mov ax, ds:[KBDSTAT]
	test cs:fKeybrd, FKB_ALTSCROLL
	jnz check_ctrlaltscroll
	and al, ah
	test al, 4	;sysreq+any ctrl?
	pop eax
	jnz issysreq
	pop ds
	jmp exit
check_ctrlaltscroll:
	and ax, 1004h	; any ctrl + scroll?
	cmp ax, 1004h
	pop eax
	jz issysreq
	pop ds
	jmp exit
issysreq:
	mov ds, cs:[__csalias]
	and [fMode], not FMODE_EXEACTIVE
	@tprintf <"SysReq pressed",lf>
	or fEntry, FENTRY_SYSREQ
	test fKeybrd, FKB_EXECINT01 or FKB_EXECINT03
	jz settraceflg
	test fKeybrd, FKB_EXECINT03
	pop ds
	jnz @F
	int 1
	@iret
@@:
	int 3
	@iret
settraceflg:
	push ebp
	@loadesp ebp
	or byte ptr [ebp+2*4].IRETS.rFL+1, 1
	pop ebp
	pop ds
	@iret

myint09 endp

endif ;?CATCHINT09

;*** translate key ***

keytranstab label byte
;	db __CTRL_G,__F5_MAKE
	db __CTRL_B,__F9_MAKE		 ;breakpoint at eip
	db __CTRL_R,__F6_MAKE		 ;register
	db __CTRL_E,__F7_MAKE		 ;unassemble
	db __CTRL_W,__F8_MAKE		 ;trace
	db __CTRL_Y,__F10_MAKE		 ;perform
	db __CTRL_A,__F12_SCAN		 ;repeat last input
	db 00h

keytrans proc stdcall
	push esi
	mov esi,offset keytranstab
keytrans_1:
	cmp byte ptr [esi],0
	jz keytrans_ex
	cmp al,[esi]
	jz @F
	inc esi
	inc esi
	jmp keytrans_1
@@:
	mov al,0
	mov ah,[esi+1]
keytrans_ex:
	pop  esi
	ret
keytrans endp

;*** get char from I14

geti14char:
	call _I14GetChar
	test ah,80h
	mov ah,00
	jz @F
	mov al,00
@@:
	ret

if ?CATCHINT09
 if ?SAVEINT09
CheckInt09 proc
	cmp word ptr saveCurInt09+?SEGOFFS,0
	jnz done
	dec word ptr saveCurInt09+?SEGOFFS
	pushad
	mov bl, 9
	mov ax, 204h
	@DpmiCall
	mov eax, cs
	cmp ax, cx
	jnz @F
	cmp edx, offset myint09
	jz no09
@@:
	mov dword ptr saveCurInt09, edx
	mov word ptr saveCurInt09+?SEGOFFS, cx
	mov ecx, cs
	mov edx, offset myint09
	mov ax, 205h
	@DpmiCall
no09:
	popad
done:
	ret
CheckInt09 endp
 endif
endif

;*** get kbd state
;*** ZERO? if no key available

GetKbdStatus proc
	call GetVIF
	jz @F
	in al, 21h
	test al, 2			; IRQ 1 enabled?
	jnz @F
	mov al, 0Bh
	out 20h, al
	in al, 20h
	test al, 03h		; irq 0 or irq 1 in service?
	jz nopoll
@@:
	in al, 64h
	test al, 01h		; input buffer full?
	jz nokey
	mov ah, al
	in al, 60h
	test ah, 20h		; is it input from PS/2?
	jnz nokey
	call setkbdbiosvars	; set kbd BIOS variables
	mov lastk, al
	jc nokey
	cmp al,80h
	jnc nokey
	ret
nokey:
	xor al,al
	ret
nopoll:
if ?SAVEINT09
	call CheckInt09
endif
if ?USEINT16
	@callint16 11h
else
 if ?WINDOWS
;	test [fVideo], FVIDEO_ISDBGER
;	jz usedbgconsole
	cmp pPeekDbgConsole, 0
	jnz usedbgconsole
 endif
	test [fStat], FSTAT_ISNT
	jnz @F
	push eax
	mov ax,@flat:[BUFSTA]	; char in buffer?
	cmp ax,@flat:[BUFEND]
	pop eax
endif
	ret
@@:
	push eax
	mov ah, 1
	int 16h		; using int 16h in NTVDM although ?USEINT16 is zero ( reduces cpu time )
	pop eax
	ret
if ?WINDOWS
usedbgconsole:
	pushad
	invoke callproc32, pPeekDbgConsole, 0
	movzx ebp, sp
	mov [ebp+1ch], eax
	popad
	and eax, eax
	ret
endif

GetKbdStatus endp

cntlkeystab label byte
	db 36h		;R-SH		;codes der umschalttasten
	db 2Ah		;L-SH		;reihenfolge entspricht bits
	db 1Dh		;CTRL		;in 40:17h
	db 38h		;ALT
	db 46h		;SCROLL lock;10
	db 45h		;NUM lock	;20
	db 3Ah		;CAPS		;40
	db 52h		;INS		;80
LCTLKEYS equ $ - cntlkeystab

;--- set kbd status bios variable
;--- used only if interrupts disabled

setkbdbiosvars proc
	pushad
	MOV BH,AL 				;die "kombinationstasten" ueberpruefen
	AND AL,7Fh
	MOV EDI,offset cntlkeystab
	MOV BL,00
	MOV ECX,LCTLKEYS
	REPNZ SCASB
	JNZ nostd
	MOV BL,80h
	SHR BL,CL
	MOV CH,BH
;	and byte ptr @flat:[KBDSTAT+1],not 4 ;reset Sys-Req
	mov ax,@flat:[KBDSTAT]
	and ah,not 4			; reset sys-req
	test ch,80h				; key released or pressed?
	jz @F
	xor bl,0FFh
	and al,bl				; reset flag
	and ah,bl
	jmp setflags
@@:
	or al,bl				; set flag
	or ah,bl				; set flag
setflags:        
	cmp cl,4				; RSHIFT,LSHIFT,CTRL,ALT?
	jnb @F
	mov @flat:[KBDSTAT+1],ah
	xor @flat:[KBDSTAT],ah
	jmp donestat
@@:
	mov @flat:[KBDSTAT],al
donestat:
	and ch,7Fh
	cmp ch,38h				;Alt key?
	jnz @F
	@tprintf <"altgr pressed/released 1",lf>
	cmp lastk,0E0h			;last key 0E0? 
	jnz @F
	and byte ptr @flat:[496h],not 8	;AltGr
	and ah,08
	or @flat:[496h],ah
	@tprintf <"altgr pressed/released 2",lf>
@@:  
	cmp ch,52h				;INSERT IS a key!
	jz nostd
	popad
	stc
	RET
nostd:
	popad
	clc
	ret
setkbdbiosvars endp

; *** get char from KBD without wait

GetKbdChar proc
	call GetKbdStatus
	jz nokey
	call GetVIF			; interrupts disabled?
	jnz getkbdchar_1
	mov al,lastk
	test al,80h
	jnz nokey			;key released, no further processing
	cmp al,39h			;space?
	jz isspace
	cmp al,56h
	ja getkbdxchar
	jb @F
	mov al,36h
@@:
	cmp al,36h
	ja getkbdxchar
	mov ebx, offset normalk
	mov ah,@flat:[KBDSTAT]
	test ah,3			;shift pressed
	jz @F
	mov ebx, offset shiftk
@@:
	test ah,4			;ctrl pressed?
	jz @F
	mov ebx, offset ctrlk
@@:
	test ah,8					;alt pressed?
	jz @F
	test byte ptr @flat:[496h],8	;AltGr pressed?
	jz getkbdxchar
	mov ebx, offset altgrk
@@:
	movzx eax,al
	mov bl,byte ptr [ebx+eax]
	and bl,bl
	jz getkbdxchar
	mov al,bl
	ret
isspace:
	mov ah,al
	mov al,' '
	ret
getkbdxchar:
	mov ah,al
	mov al,00
	ret

getkbdchar_1:
if ?USEINT16
	@callint16 10h
@@:
else
 if ?WINDOWS
;	test [fVideo], FVIDEO_ISDBGER
;	jz usedbgconsole
	cmp pReadDbgConsole, 0
	jnz usedbgconsole
 endif
	mov BX, @flat:[BUFSTA]	 ; get key from buffer (without wait)
	cmp BX, @flat:[BUFEND]
	jz nokey
	movzx ebx, bx
	mov AX, @flat:[EBX+400h]
	INC eBX
	INC eBX
	CMP BX, @flat:[EBUFEND]
	JNZ @F
	MOV BX, @flat:[EBUFSTA]
@@:
	MOV @flat:[BUFSTA], BX
endif
	ret
nokey:
	xor eax,eax
	ret
if ?WINDOWS
usedbgconsole:
	invoke callproc32, pReadDbgConsole, 0
	ret
endif
GetKbdChar endp


GetAltChar proc
	call GetKbdChar
	ret
GetAltChar endp

GetDosChar proc
	mov ebx,[__inpstream]
	mov ax,4406h
	@DosCallx					;test if device is character
	and al,al
	jz getmsgdos
	call _DOSGetChar
	mov ah,00				;keine control codes
	and ax,ax
	jnz exit
getmsgdos:
	mov ebx,[__inpstream]	;dont close console
	and ebx,ebx
	jz getmsgdos1
	mov ah,3eh
	@DosCall
	mov [__inpstream],0
	and [__inpmode], not _DOSINP
	jnz getmsgdos1
	mov [__inpmode], _KBDINP
getmsgdos1:
	xor eax,eax
exit:
	ret
GetDosChar endp

;*** check if there's a char on any input device

GetInpStatus proc stdcall public

	test [__inpmode], _SERINP
	jz @F
	call GetComStatus
	jnz exit
@@:
	test [__inpmode], _KBDINP or _ALTINP
	jz @F
	call GetKbdStatus
;;	jnz exit
@@:
exit:
	ret
GetInpStatus endp

;*** get char from input device

_getchar proc stdcall public

	call getcharex
	call keytrans
	ret

_getchar endp

Sleep proc stdcall public uses ebx edi dwInterval:dword

if ?DOSLEEP
local rmcs:RMCS

	test cs:[fStat], FSTAT_ISNT
	jnz @F
	mov word ptr rmcs.rEAX, 1680h
	mov rmcs.rSSSP, 0
	mov rmcs.rFlags, 202h
	lea edi, rmcs
	mov bx, 02Fh
	xor ecx, ecx
	mov ax, 300h
	int 31h
	ret
@@:
 if ?WINDOWS
	invoke callproc32, pPeekDbgConsole, 1
 else
  if ?USEINT16
	@callint16 01h
  else
	mov ah, 1
	int 16h
  endif
 endif
endif
	ret

Sleep endp

;--- this is a loop to wait for a char from any source

getcharex proc stdcall public
	test [fIrq],1
	jz @F
	sti 					; enable interrupts
@@:
getchar_1:
	test [__inpmode], _DBGINP
	jz @F
	mov ax,0001
	int 41h
	cmp al,1
	ja exit
@@:
	test [__inpmode], _SERINP
	jz @F
	call GetComChar
	and ax,ax
	jnz exit
@@:
	test [__inpmode], _KBDINP
	jz @F
	call GetKbdChar
	and ax,ax
	jnz exit
@@:
	test [__inpmode], _DOSINP
	jz @F
	call GetDosChar
	and ax,ax
	jnz exit
@@:
	test [__inpmode], _ALTINP
	jz @F
	call GetAltChar
	and ax,ax
	jnz exit
@@:
	test [__inpmode], _I14INP
	jz @F
	call geti14char
	and ax,ax
	jnz exit
@@:
if ?DOSLEEP
	test [fIrq],1
	jz label1
	invoke Sleep, 0
endif
label1:
	jmp getchar_1
exit:
	ret
getcharex endp

promptout:
	@putchr cr
	mov al, [cLoad]
	invoke _hexout
	test [fEntry], FENTRY_REAL
	jz promptout_2
	test [fCPUMode], 1
	jnz promptout_1
	@putchr bPromptRM
	ret
promptout_1:
	@putchr bPromptV86
	ret
promptout_2:
	@putchr bPromptPM
	ret

;*** check last key 
;*** inp: key in ax
;*** inp: esi=last key buffer (for buffer scroll)
;*** rc: C-> display prompt and continue

checklastkey proc stdcall
	mov ebx, [pKbdBuff]
	cmp ax, 0000		; buffer full?
	jnz @F
	clc
	ret
@@:
	cmp al,cr
	jnz @F
	clc
	ret
@@:
	cmp al,ESCAPE
	jnz @F
	mov byte ptr [ebx], 0
	invoke _crout
	stc
	ret
@@:
	cmp ah,__CURSOR_UP    ;retrieve?
	jnz @F
	call clearline
	call restoreline_up
	stc
	ret
@@:
	cmp ah,__CURSOR_DOWN  ;retrieve?
	jnz @F
	call clearline
	call restoreline_down
	stc
	ret
@@:
	cmp ah,__F12_SCAN
	jnz @F
	call restoreline_up
	clc
	ret
@@:
	cmp ah,__F11_SCAN
	jnz @F
	mov dword ptr [ebx],"1 J"
	inc [fSkipLF]
	jmp dontsave
@@:
	cmp ah,__F8_MAKE
	jnz @F
	mov word ptr [ebx],"T"
	inc [fSkipLF]
	jmp dontsave
@@:
	cmp ah,__F9_MAKE
	jnz @F
	mov dword ptr [ebx+0],"E PB"
	mov dword ptr [ebx+4],"PI"
	jmp dontsave
@@:
	cmp ah,__F10_MAKE
	jnz @F
	mov word ptr [ebx],"P"
	inc [fSkipLF]
	jmp dontsave
@@:
	cmp ah,__F7_MAKE
	jnz @F
	mov word ptr [ebx],"U"
	jmp dontsave
@@:
	cmp ah,__F6_MAKE
	jnz @F
	mov word ptr [ebx],"R"
	jmp dontsave
@@:
	cmp ah,__F5_MAKE
	jnz @F
	mov word ptr [ebx],"G"
	jmp dontsave
@@:
	cmp ah,__F4_MAKE
	jnz @F
	mov word ptr [ebx],"V"
;	inc [fSkipLF]
	inc [fSkipLF]
	jmp dontsave
@@:
	cmp ah,__F1_MAKE
	jnz @F
	mov word ptr [ebx],"?"
	jmp dontsave
@@:
	@putchr cr
	stc
	ret
dontsave:
	mov fDontSave, 1
	clc
	ret
checklastkey endp

;*** count chars in a line 
;*** and then display spaces to clear them
;*** ebx = key buffer

clearline proc
	mov cl,?PROMPTSIZE
	dec cl
@@:
	mov al,[ebx]
	inc cl
	inc ebx
	cmp al,0
	jnz @B

	@putchr cr
@@:
	@putchr ' '
	dec cl
	jnz @B
	@putchr cr
	ret
clearline endp

;*** read a line into buffer

getline proc stdcall public uses esi

	test [fStat], FSTAT_BUFNOTEMPTY
	jz getline_1
@@:
	and [fStat], not FSTAT_BUFNOTEMPTY
	test fTrace,1
	jz exit
	call promptout
	@strout [pKbdBuff]
	jmp exit2
getline_1:
	call saveline			; get pointer to Kbd buffer
	mov ebx, [pKbdBuff]
	mov esi, ebx 			; for buffer scroll
	mov byte ptr [ebx], 0
;	call croutx
	jc getline_3

getline_2:						;<----
	call promptout
getline_3:
	mov ttycurpos,0 			; virtual cursor for TTY devices
	invoke strlen, [pKbdBuff]	; position cursor at EOS
	invoke GetString, [pKbdBuff], ?KBDBUFSIZ-1, eax
;	mov cl,al
;	shr eax,8
;	mov al,cl
	mov [lastkey],eax
	call checklastkey
	jc getline_2
exit2:
	call croutx
exit:
	mov eax, [pKbdBuff]
	ret
getline endp

;*** return pointer to new input buffer

saveline proc stdcall
	pushad
	mov al,0
	xchg al,fDontSave
	cmp al,0
	jnz exit
	mov eax, [pKbdBuff]
	cmp byte ptr [eax], 0	; line empty?
	jz exit
	mov ecx, ?MAXSAVELINE	; max # buffers
	xor esi, esi
	xor edi, edi
nextitem:
	mov ebx,[eax-4]		; follow pointer chain
	cmp ebx,[pKbdBuff]
	jz notfound
	mov edx,[pKbdBuff]
	mov esi,edi			; save predecessor
	mov edi,ebx			; save start
@@:
	mov al,[edx]		; compare lines
	mov ah,[ebx]
	inc edx
	inc ebx
	cmp al,ah
	jnz @F
	and al,al
	jnz @B
found:
;--- line is already saved, so it's free
;--- but line must be brought to top
	and esi,esi				; is line already on top?
	jz exit
	mov eax, [edi-4]		; remove line from chain
	mov [esi-4], eax
	mov eax, [pKbdBuff]
	mov ecx, [eax-4]
	mov [eax-4], edi
	mov [edi-4], ecx
	jmp exit
@@:
	mov eax,edi				; next buffer
	loop nextitem
							; notfound, but dont create another element
	mov ebx,[pKbdBuff]
	invoke strcpy, edi, ebx
	mov [esi-4],ebx
	mov eax,edi
	jmp saveline_2
notfound:					; line not found
							; create a ring of buffers (max 10 entries)
							; eax=pointer to last entry
	lea eax, [ebx-4]		; ebx=[kbdbuff]
	invoke PutBytes2Heap, eax, ?KBDBUFSIZ+4
	add eax,4
saveline_2:
	mov esi,eax
	xchg eax,[ebx-4]		; position to start of chain
	mov [esi-4],eax
exit:
	popad
	ret
saveline endp

if 0
_testproc proc c pb:PARMBLK
	mov esi, [pKbdBuff]
@@:
	@dwordout esi
	@putchr ':'
	@strout esi
	@putchr ':'
	invoke _crout
	mov esi, [esi-4]
	cmp esi, [pKbdBuff]
	jnz @B
	ret
_testproc endp
endif

restoreline_up proc
	mov ecx, 2
@@:
	mov esi, [esi-4]
	cmp esi, [pKbdBuff]
	loopz @B
	invoke strcpy, [pKbdBuff], esi
	ret
restoreline_up endp

restoreline_down proc
	mov ecx, 2
rl_1:
	mov eax, esi
@@:
	mov edx, eax
	mov eax, [eax-4]
	cmp eax, esi 		  ;kette durchlaufen?
	jnz @B
	mov esi, edx
	cmp esi, [pKbdBuff]
	loopz rl_1
	invoke strcpy, [pKbdBuff], esi
	ret
restoreline_down endp

CopyStringInKbdBuff proc stdcall public pString:dword

	pushad
	mov edi, [pKbdBuff]
	mov esi, pString
@@:
	lodsb
	stosb
	and al, al
	jz @F
	cmp al, lf
	jnz @B
	xor al, al
@@:
	stosb
	popad
	or [fStat], FSTAT_BUFNOTEMPTY
	ret
CopyStringInKbdBuff endp

_interpret proc c public pb:PARMBLK

	cmp pb.p1.bType, __STRING__
	jz @F
	@errorout ERR_STRING_EXPECTED
	jmp interpret_ex
@@:
	push pb.dwOffs1
	call CopyStringInKbdBuff
interpret_ex:
	ret
_interpret endp

;--- function STRCAT()

mystrcat proc c public uses esi edi parm2:dword, parm1:dword

	invoke CopyString2TmpHeap, parm1
	mov esi, eax
	mov ebx, eax
@@:
	lodsb
	and al,al
	jnz @B
	dec esi
	mov edi, esi
	mov esi, parm2
@@:
	lodsb
	stosb
	and al,al
	jnz @B
	mov [pNearHeap], edi
	mov eax, ebx
	ret
mystrcat endp

CURSOR_RIGHT equ 00435B1Bh
CURSOR_LEFT  equ 00445B1Bh

dottycursor proc stdcall

if 0
	push eax
	or fEscapemode,1
	@strout <"req. pos=">
	pop eax
	push eax
	@hexout al
	@stroutc ",act. pos="
	pop eax
	push eax
	@hexout ah
	invoke _crout
	and fEscapemode,0FEh
	pop eax
endif
	sub al,ah	;al=gewuenschte spalte
	jz exit
	jc dty_1 	;ah=aktuelle spalte
	mov cl,al
	mov eax,CURSOR_RIGHT
	jmp dty_2
dty_1:				;cursor nach links
	neg al
	mov cl,al
	mov eax,CURSOR_LEFT
dty_2:
	or fEscapemode,1
dty_21:
	push eax
	mov ch,3
dty_3:
	push ecx
	push eax
	invoke _AUXPutChar,eax
	pop eax
	pop ecx
	shr eax,8
	dec ch
	jnz dty_3
	pop eax
	dec cl
	jnz dty_21
	and fEscapemode,0FEh
exit:
	ret
dottycursor endp

CSRPOS struct
wCol dw ?
wRow dw ?
CSRPOS ends

;*** Cursorposition setzen in GETSTRING() ***

setCursorPos proc stdcall xy:CSRPOS

	test [__outmode], _SEROUT ;TTY devices
	jz @F
	mov al, byte ptr xy.wCol
	mov ah, byte ptr ttycurpos
	mov byte ptr ttycurpos,al
	push ecx
	call dottycursor
	pop ecx
@@:
	mov al, byte ptr xy.wCol
	mov ah, byte ptr xy.wRow
	test [__outmode], _VIOOUT
	jz @F
	invoke VioSetCurPosDir, addr stdcrt, eax
@@:
	test [__outmode], _ALTOUT
	jz @F
	invoke VioSetCurPosDir, addr altcrt, eax
@@:
	ret

setCursorPos endp

;*** Cursorposition ermitteln in GETSTRING() ***

getCursorPos proc

	test [__outmode], _VIOOUT
	jz @F
	invoke VioGetCurPosDir, addr stdcrt
	mov cl,al
	shl eax,8
	movzx ax,cl
	jmp exit
@@:
	test [__outmode], _ALTOUT
	jz @F
	invoke VioGetCurPosDir, addr altcrt
	mov cl,al
	shl eax,8
	movzx ax,cl
	jmp exit
@@:
	test [__outmode], _SEROUT
	jz @F
	mov eax,ttycurpos
	jmp exit
@@:
	mov eax,ttycurpos
exit:
	ret

getCursorPos endp


GetInsertStatus proc
	mov ah, @flat:[KBDSTAT]
	ret
GetInsertStatus endp

;--- flags for getstring
FL_ESCAPE    = 01h
FL_TERMINATE = 02h
FL_BSOUT     = 04h

; cursor: position of cursor on entry

GetString proc stdcall uses esi edi ebx pBuffer:dword, maxlen:dword, cursor:dword

local	flags:dword 	; bool: escape (terminate without update)
local	curspos:dword	; cursor position
local	strsta:dword	; string start
local	_lastkey:dword

	mov ecx, maxlen		; copy string onto stack
	sub esp, ecx
	@loadesp edi		; edi = cursor
	mov strsta, edi

	xor eax, eax
	mov flags, eax
	call getCursorPos
	mov curspos, eax	; HIWORD=row,LOWORD=col

	mov esi, pBuffer
@@:
	lodsb
	stosb
	and al,al
	jnz @B

	mov eax, cursor		; cursor is just an offset
	mov ecx, maxlen
	mov edi, strsta
	mov esi, edi
	add edi, eax 		; EDI -> current char
	sub ecx, eax 		; maxlen - cursor
	jbe strex
	call str_out 		; display string at esi
nextchar:				; <--- get next key
	push ecx 			; ecx=max remaining length
	mov ecx, curspos	; position of string
	mov eax, edi 		; current offset -> eax
	sub eax, strsta
	add cx, ax
	invoke setCursorPos, ecx
	call _getchar
	movzx eax, ax
	mov _lastkey, eax
	pop ecx

	call chkmsg
	test byte ptr flags,FL_ESCAPE
	jnz str_1
	test byte ptr flags,FL_TERMINATE
	jz nextchar
	mov esi, strsta
	mov ecx, maxlen
	mov edi, pBuffer
	rep movsb
str_1:
strex:
	add esp, maxlen
	mov eax,_lastkey
	ret

smexit:
	or byte ptr flags,FL_TERMINATE
	retn
chkmsg:
	and al, al					; ctrl key?
	jz chkm2
	cmp al, 0E0h
	jz chkm2
	cmp al, 8					; handle BS?
	jnz @F
	call bsv
	retn
@@:
	cmp al, 09					; TAB?
	jz smexit
	cmp al, cr
	jz smexit
	cmp al,lf
	jz smexit
	cmp al,1bh					; ESC?
	jnz @F
	or byte ptr flags,FL_ESCAPE
	retn
@@:
	cmp al,20h					; exit if any key below space
	jb smexit

	call GetInsertStatus
	test ah, 80h
	jz @F
	call insert
	and al,al
	jnz chkm3
	retn
@@:
	mov ah, [edi]
	mov [edi], al
	and ah,ah					; cursor at EOL?
	jnz @F
	cmp ecx,2
	jb @F
	mov byte ptr [edi+1],0		; mark new EOL
@@:
	@putchr al					; display echo
chkm3:
	inc edi 					; "cursor" right
	dec ecx
	cmp ecx,1					; room for key?
	ja @F
	or byte ptr flags,FL_TERMINATE
@@:
	retn

str_out:
	pushad
@@:
	lodsb
	and al,al
	jz @F
	@putchr al
	jmp @B
@@:
	test byte ptr flags,FL_BSOUT ; clear last char?
	jz @F
	@putchr ' '
	mov byte ptr flags, 0
@@:
	popad
	retn

	.const
tab1 label byte
	db __CURSOR_LEFT
	db __CURSOR_RIGHT
	db __HOME_MAKE
	db __END_MAKE
	db __DEL_MAKE
	db __INS_MAKE
sizetab1 equ $ - offset tab1
tab1cmd label dword
	dd offset cursleft
	dd offset cursright
	dd offset curshome
	dd offset cursend
	dd offset del_key
	dd offset _ret
	.code

chkm2:							; handle ctrl keys
	xor ebx, ebx
	mov al, ah
@@:
	cmp ebx, sizetab1
	jz @F
	mov al, [ebx][tab1]
	inc ebx
	cmp al, ah
	jnz @B
	dec ebx
	jmp dword ptr [ebx*4][tab1cmd]
@@:
	jmp smexit

;*** ctrl key routines

curshome:
	call cursleft
	jnz curshome
_ret:
	retn
cursend:
	call cursright
	jnc cursend
	retn

cursleft:
	cmp ecx,maxlen
	jz _ret
	inc ecx
	dec edi
	retn
del_key:
	cmp byte ptr [edi],00
	jz _ret
	inc edi
	jmp bsv3
cursright:
	cmp ecx, 2
	jna @F
	cmp byte ptr [edi],00
	jz @F
	dec ecx
	inc edi
	retn
@@:
	stc
	retn
bsv:					; handle backspace
	cmp ecx,maxlen
	jz _ret				; if cursor at start of line
	inc ecx
	push ecx
	mov eax,edi
	dec eax
	sub eax,strsta
	mov edx,curspos
	add dx, ax
	invoke setCursorPos, edx
	pop ecx
bsv3:					; <--- DEL key
	or byte ptr flags, FL_BSOUT
	mov esi,edi
	dec edi
	push edi
@@:
	lodsb
	stosb
	and al,al
	jnz @B
	pop edi
	mov esi, edi			; redisplay line from cursor pos
	call str_out
	retn

insert: 					; insert mode
	push eax
	push edi
	mov edx, ecx

	mov ecx,-1
	xor al,al
	repnz scasb
	neg ecx
	dec ecx
	mov eax,edx
	sub eax,maxlen
	neg eax
	add eax,ecx
	cmp eax,maxlen
	jnb ins1
	mov esi,edi
	dec esi
	std
	rep movsb 			; get room for key
	cld
	mov ecx,edx
	pop edi
	pop eax
	mov [edi],al
	inc esi

	call str_out		; display line from cursor pos
	mov al,-1
	retn
ins1:					; error at insert
	mov cx,dx 			; restore CX
	push ecx
;	invoke Beep,800,20
	pop ecx
	pop edi
	pop eax
	xor al,al
	retn
GetString endp

installirqkbd proc stdcall public
	test [__inpmode], _KBDINP or _ALTINP
	jz @F
	or byte ptr [wPicOn], 02h   ;enable IRQ 01 (kbd)
	or byte ptr [wPicOn+1], 10h	;enable IRQ 12 (PS/2 mouse)
@@:
	ret
installirqkbd endp


;*** handling of interrupt vector 09 is different for dos / windows
;*** for DOS simply save the vector now,
;*** set it to our value and reset it on termination

kbd_init proc public

if ?USEINT16
	mov bl, 16h						; if int 16 is used, don't
	mov ax, 204h
	int 31h 						; call it directly
	mov dword ptr oldint16, edx
	mov word ptr oldint16+?SEGOFFS, cx
endif
if ?SAVEINT15RM
	mov bl, 15h
	mov ax, 200h
	int 31h
	mov word ptr oldint15r+0, dx
	mov word ptr oldint15r+2, cx
endif

if ?CATCHINT09
 if ?USERMINT09
;--- our protected-mode int 09 will call rm int 9 by "sim rm int"
	mov bl, 9
	mov ax, 200h	; get rm int
	int 31h
	mov word ptr rmint09+0, dx
	mov word ptr rmint09+2, cx
 endif
	mov bl, 9
	mov ax, 204h	; get pm int
	@DpmiCall
	mov dword ptr oldint09+0, edx
	mov word ptr  oldint09+?SEGOFFS, cx
  if ?HIDEINT09
	mov dword ptr oldi09p, edx
	mov word ptr oldi09p+?SEGOFFS, cx
  endif
	mov ecx, cs
	mov edx, offset myint09
	mov ax, 205h
	@DpmiCall
endif
	ret

kbd_init endp

;--- exit debugger
;--- since ?HIDEINT09 may be 1,
;--- it's important NOT to use INT 31h directly here
;--- or at least check that we're called AFTER int 31h is restored
;--- ( usually we are NOT! )

kbd_exit proc public
if ?CATCHINT09
	@tprintf <"restore int 09",lf>
	mov edx, dword ptr [oldint09+0]
	mov cx, word ptr   [oldint09+?SEGOFFS]
	mov bl, 9
	@DpmiCall 205h
endif
	ret
kbd_exit endp

kbd_setdebuggervecs proc public
if ?SAVEINT15RM
	mov bl, 15h
	mov ax, 200h
	@DpmiCall
	mov word ptr [savedint15r+0], dx
	mov word ptr [savedint15r+2], cx

	mov dx, word ptr [oldint15r+0]
	mov cx, word ptr [oldint15r+2]
	mov bl, 15h
	mov ax, 201h
	@DpmiCall
endif
if ?DISABLEPD
	mov bl, 74h
	mov ax, 204h
	@DpmiCall
	mov dword ptr [oldint74+0], edx
	mov word ptr [oldint74+?SEGOFFS], cx
	mov ecx, cs
	mov edx, offset myint74
	mov ax, 205h
	@DpmiCall
endif
    ret
kbd_setdebuggervecs endp

kbd_setdebuggeevecs proc public
if ?SAVEINT15RM
	mov dx, word ptr [savedint15r+0]
	mov cx, word ptr [savedint15r+2]
	jcxz @F
	mov bl, 15h
	mov ax, 201h
	@DpmiCall
@@:
endif
if ?DISABLEPD
	mov edx, dword ptr [oldint74+0]
	mov cx, word ptr [oldint74+?SEGOFFS]
	mov bl, 74h
	mov ax, 205h
	@DpmiCall
endif
    ret
kbd_setdebuggeevecs endp

;--- after a debuggee has terminated,
;--- check protected-mode interrupt 09
;--- current value is in AX:(E)DX
;--- set cx to new cs if value isn't ok.

kbd_checkpmints proc public
if ?CATCHINT09
	push ebx
	mov ebx, cs
	cmp ax, bx
	pop ebx
	jz @F
	mov ecx, cs
	mov edx, offset myint09
@@:
endif
	ret
kbd_checkpmints endp

;--- command .KBD

_kbdstate proc c public pb:PARMBLK

	mov ebx,offset kbdvartab
	mov eax,offset kbdvartabend
	mov ch,_RDONLY_
	call symtout
	ret
_kbdstate endp

	END

