
;--- breakpoint procs

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
	include dpmi.inc
	include debxxfd.inc
	include putf.inc
	include errors.inc
	include toolhelp.inc
	include fcntl.inc
	include extern32.inc
	include extern16.inc
	include isvbop.inc

HWBP struct
hwbpAddr	 dd ?		;lineare adresse
hwbpHandle	 dw ?		;dpmi handle
hwbpLen 	 db ?		;Bit 7: Auto-reset Flag
hwbpTyp 	 db ?		;0=execute,1=read,2=read&write
hwbpState	 db ?		;status
HWBP ends

;--- hwbpState flags
FHWBP_TRIGGERED	equ 01h	;B0: 1 -> watchpoint ausgeloest (von DPMI 0B02)
FHWBP_SET		equ 80h	;B7: 1 -> watchpoint aktiv

BREAK struct
pNext		dd ?
bFlags1		db ?	;FBRP Flags (see debxxfd.inc)
bFlags2		db ?	;FBRP2 Flags
brpByte		db ?	;saved byte at breakpoint address
bCondTyp	db ?	;condition Typ (0=inaktiv)
brpAddr		df ?	;address breakpoint (ausser FBRP_NOADDR)
res			dw ?
brpDefCmd	dd ?	;default cmd falls FBRP_CMDSET
count		dd ?	;count bis break, 0=deaktiv
brpCAdr		dd ?	;condition Addresse
brpCVa1		dd ?	;condition Wert1
brpCVa2		dd ?	;condition Wert2
brpHW		dd ?	;hw-handle falls FBRP_USEHW=1
BREAK ends


	.DATA

breakpointtab dd 0		  ;^ breakpoints
if ?WINDOWS
brkbyte db 0CCh
endif

bErrors db 0

	.CODE

getlinearbp proc stdcall uses ebx

	mov ebx, dword ptr [esi].BREAK.brpAddr+0
	mov cx, word ptr [esi].BREAK.brpAddr+4
	mov dl, 1
	test [esi].BREAK.bFlags1, FBRP_REAL
	jnz @F
	mov dl, 0
	lar eax, ecx
	jnz error
	test ah, 4		;expand down?
	jnz @F
	lsl eax, ecx
	jnz error
	cmp eax, dword ptr [esi].BREAK.brpAddr+0
	jc error
@@:
	call getlinearaddr			; adresse von CX:EBX holen
	ret
error:
	stc
	ret
getlinearbp endp

getactfl:
	test [fEntry], FENTRY_REAL
	jnz @F
	mov eax, [r1.rEfl]
	ret
@@:
	mov ax, [r1r.rFlags]
	ret
setactfl:
	test [fEntry], FENTRY_REAL
	jnz @F
	mov [r1.rEfl], eax
	ret
@@:
	mov [r1r.rFlags], ax
	ret

;--- read a byte into CH from flat:[EBX]

if ?LOADVDD

VerifyAddress proc stdcall public dqAddress:QWORD, dwMode:dword
	push eax
	push ebx
	push ecx
	push edx
	mov ecx, dwMode		;readable/writeable address?
	mov ebx, dword ptr dqAddress+0
	mov edx, dword ptr dqAddress+4
	mov eax, [hVDD]
 if ?FLAT
	invoke RunBop, 2
 else
	DispatchCall 
 endif
	pop edx
	pop ecx
	pop ebx
	pop eax
	ret
VerifyAddress endp        

endif

;--- read byte at @flat:ebx into ch

readbyte proc
	push [excexit]
	mov [excexit], offset error
if ?WINDOWS
	@switch16bit
	db 67h, 65h, 8ah, 2bh	; mov "ch, gs:[ebx]" in 16-bit
	@switch32bit
else
 if ?LOADVDD
	cmp [hVDD],-1
	jz @F
	push 1
	push @flat
	push ebx
	call VerifyAddress
	jc error
@@:
 endif
	mov ch,@flat:[ebx]
endif
	clc
	jmp exit
error:
	@tprintf <"readbyte at %X failed",lf>, ebx
	stc
exit:
	pop [excexit]
	ret

readbyte endp

;--- write byte CL at @flat:[EBX]

writebyte proc
	push [excexit]
	mov [excexit], offset error
if ?WINDOWS
	@switch16bit
	db 67h, 65h, 88h, 0bh	; mov "gs:[ebx], cl" in 16-bit
	@switch32bit
else
 if ?LOADVDD
	cmp [hVDD],-1
	jz @F
	push 2
	push @flat
	push ebx
	call VerifyAddress
	jc error
@@:
 endif
	mov @flat:[ebx], cl
endif
	clc
	jmp exit
error:
	@tprintf <"writebyte at %X failed",lf>, ebx
	inc [bErrors]
	stc
exit:
	pop [excexit]
	ret
writebyte endp

;*** activate software breakpoints that are in bp table.
;*** checks
;--- 1) if bp is at current cs:eip; in this case no
;***    (soft) breakpoint is written, but single step is activated.
;--- 2) if bp could be written; if no, try to use a hardware bp.
;*** called by: _loadpgm, _reset , intr41

SetTheBreaks proc stdcall public uses esi

if ?WINDOWS
local dwOldAttr:dword
local dwMem:dword
endif
local aktcseip:dword

	mov bErrors, 0
if ?WINDOWS
	invoke getpsp					; debugger is current task?
endif
	call getlinearcseip				; linear cs:eip -> eax
	mov aktcseip, eax
	or byte ptr [r1.rEfl+2], 1		; always set resume flag
	mov esi, breakpointtab
	@loadflat
nextitem:
	and esi, esi
	jz exit
if ?WINDOWS
	mov dwOldAttr, 0
endif
	pushad
	mov al, [esi].BREAK.bFlags1
	test al, FBRP_ENABLED			; breakpoint enabled?
	jz doneitem
	@tprintf <"SetBreaks: bp %X",lf>, esi
	test [esi].BREAK.bFlags2, FBRP2_SET	; already set?
	jnz doneitem
	test al, FBRP_NOADDR			; watchpoint?
	jnz ssb_51
	call getlinearbp 				; get lineare address
	jc doneitem
	@tprintf <"SetBreaks: bp with addr %X",lf>,eax
	mov ebx, eax
if ?WINDOWS
	test [fMode], FMODE_STRICT
	jnz @F
	lar ecx, r1.rSS
	test ecx,400000h				; currently in 32-bit mode?
	jz @F
	invoke setptentry, ebx, 2, 2	; R/W bit is 2
	jc doneitem						; page is r/o
	test al, 2						; attribute changed?
	jnz @F
	mov dwOldAttr, eax
	mov dwMem, ebx
@@:
endif
	cmp ebx, aktcseip				; break an cs:eip?
	jz ssb_5
	test [esi].BREAK.bFlags1, FBRP_USEHW; hw-break?
	jnz ssb_4						; those are written later
	invoke readbyte					; read byte into CH
	jc doneitem1					; cannot read
	mov cl, 0CCh
	cmp cl, ch						; is it an int3 already
	jnz @F
	test [esi].BREAK.bFlags1, FBRP_AUTO
	jz doneitem
	call clearbreakpnt1				; clear breakpoint esi
	jmp doneitem
@@:
	invoke writebyte				; write byte in CL at @flat:[ebx]
	jc @F							; unable to write
	mov dl, ch						; save original byte
	invoke readbyte					; read again into CH (may be rom)
	cmp cl, ch						; was able to write?
	mov ch, dl
	jz setthebreaks_4
@@:        
;	cmp ch, 63h						; is it a (Windows) VM breakpoint (ARPL)?
;	jz doneitem						; then no HW break
	@tprintf <"a hw break will be used for %X",lf>, ebx
	mov dx, 0001h					; size 1, type execute, auto reset
	call savedebughandle 			; try to add a hw-break (temporarily activated)
	jnc @F
	test [esi].BREAK.bFlags1, FBRP_AUTO
	jz doneitem
	call clearbreakpnt1				; clear breakpoint esi
	jmp doneitem
@@:        
	@tprintf <"hw break allocated %X",lf>, eax
	mov [esi.BREAK.brpHW], eax
	or [esi].BREAK.bFlags1, FBRP_USEHW
setthebreaks_4:
	mov [esi.BREAK.brpByte], ch		; wird bei hw-breaks ignoriert
ssb_4:
	or [esi].BREAK.bFlags2, FBRP2_SET
	jmp doneitem
ssb_51:								; watchpoint
	or [esi].BREAK.bFlags2, FBRP2_SET
ssb_5:								; break at CS:EIP (for hw breaks too)
	test fExit, FEXIT_TRACE or FEXIT_LOADPGM
	jnz doneitem
	mov al,0
	call settracevars
	or [fMode], FMODE_EXEACTIVE
	mov [exitproc], offset restart_after_trace
	@tprintf <"exitproc activated (breakpoint at cs:eip)",lf>
doneitem1:
doneitem:
if ?WINDOWS
	cmp dwOldAttr, 0
	jz @F
	invoke setptentry, dwMem, dwOldAttr, 2	;R/W bit ist 2
@@:
endif
	popad
	mov esi, [esi.BREAK.pNext]
	jmp nextitem
exit:
	movzx eax, bErrors
	cmp al,1
	cmc
	ret

SetTheBreaks endp

restart_after_trace:
	and [fMode], not FMODE_NODISP
	jmp _reset

;--- check a conditional breakpoint
;--- out: C if condition is FALSE

checkbpcondition proc stdcall

	@tprintf <"checkpbcondition enter",lf>
        
	cmp [esi.BREAK.bCondTyp], __STRING__
	jz checkbpcondition_3
	mov ebx, [esi.BREAK.brpCAdr]
	mov ebx, [ebx.SYMBOL.dwProc]
	mov eax, [ebx]
	mov ebx, [esi.BREAK.brpCVa1]
	mov ecx, [esi.BREAK.brpCVa2]
	cmp [esi.BREAK.bCondTyp], __BYTE__
	jz checkbpcondition_1
	test [esi.BREAK.bCondTyp], __WORD__
	jz checkbpcondition_2
	cmp eax, ebx
	jb falsecond
	cmp eax, ecx
	ja falsecond
	clc
	ret
falsecond:
	stc
	ret
checkbpcondition_1:
	cmp al, bl
	jb falsecond
	cmp al, cl
	ja falsecond
	clc
	ret
checkbpcondition_2:
	cmp ax, bx
	jb falsecond
	cmp ax, cx
	ja falsecond
	clc
	ret
checkbpcondition_3:
	push esi
	mov esi, [esi.BREAK.brpCAdr]
	invoke GetExpression, __STRING__
	pop esi
	@tprintf <"checkbpcondition eax=%X",lf>, eax
	cmp eax, 1			;eax==0 -> C
	ret
checkbpcondition endp

;*** software breakpoints reset
;*** out: bpcount

ResetBreaks proc stdcall public

local	korreip:dword
local	bpflags:dword
if ?WINDOWS
local	dwOldAttr:dword
local	dwMem:dword
endif

	pushad
	xor eax, eax
	mov [bpCount], al
	mov korreip, eax
	mov bpflags, eax
	call getlinearcseip			;CS:EIP -> EAX
	mov edi, eax
;	@tprintf <"ResetBreaks: eip=%X (%X:%X)",lf>, edi, ecx, ebx
	test [fEntry], FENTRY_INT03
	jz @F
	dec edi
@@:
	mov esi,breakpointtab
nextitem:
	and esi, esi
	jz done
	@tprintf <"ResetBreaks: bp=%X",lf>, esi
if ?WINDOWS
	mov dwOldAttr, 0
endif
	pushad
	test [esi].BREAK.bFlags2, FBRP2_SET
	jz doneitem					; bp isn't set, check next one
	@tprintf <"ResetBreaks: bp is set, addr=%X, flags=%X",lf>, dword ptr [esi].BREAK.brpAddr, dword ptr [esi].BREAK.bFlags1
	and [esi].BREAK.bFlags2, not FBRP2_SET
	and [esi].BREAK.bFlags2, not FBRP2_TRAPPED
	test [esi].BREAK.bFlags1, FBRP_NOADDR
	jnz rtb_31
	@tprintf <"ResetBreaks: bp has addr",lf>
	call getlinearbp 			; lin addr of bp -> EAX
	jc doneitem					; can't get linear address, skip
	mov ebx, eax
	@tprintf <"ResetBreaks: bp linaddr=%X",lf>, ebx
	test [esi].BREAK.bFlags1, FBRP_USEHW ; if hw-break, no memory change
	jnz resetthebreaks_02
if ?WINDOWS
	test [fMode], FMODE_STRICT
	jnz @F
	lar ecx, r1.rSS
	test ecx,400000h			; 32-bit stack?
	jz @F
	invoke setptentry, ebx, 2, 2; R/W bit is 2
	jc doneitem					; page invalid
	test al, 2					; attribute changed?
	jnz @F
	mov dwOldAttr, eax
	mov dwMem, ebx
@@:
endif
	mov cl,[esi.BREAK.brpByte]
	call writebyte
resetthebreaks_02:
	cmp ebx, edi 				; bp address == curr CS:EIP?
if 0        
	jz resetthebreaks_3
	inc edi
	cmp ebx, edi
	jnz doneitem				; next bp
	jmp resetthebreaks_5		; reset auto breakpoint if
								; it has been reached.
else
	jnz doneitem
endif
resetthebreaks_3:
	test [fEntry], FENTRY_INT03
	jz rtb_31
	mov dword ptr [korreip], 1
rtb_31:
	cmp [esi.BREAK.bCondTyp], 0
	jz @F
	call checkbpcondition
	jc doneitem					; next bp
@@:
	cmp dword ptr [esi.BREAK.count],0
	jz @F
	dec [esi.BREAK.count]
	jnz doneitem				; next bp
@@:
	test [esi].BREAK.bFlags1, FBRP_CMDSET
	jz @F
	push [esi.BREAK.brpDefCmd]
	call CopyStringInKbdBuff
	or [fMode], FMODE_NODISP
@@:
resetthebreaks_5:
	@tprintf <"ResetBreaks: breakpoint %X hit at %X",lf>, esi, ebx
	or [esi].BREAK.bFlags2, FBRP2_TRAPPED
	inc byte ptr [bpCount]
	test [esi].BREAK.bFlags1, FBRP_AUTO
	jz noautoreset
	test [esi].BREAK.bFlags1, FBRP_SILENT
	jz @F
	or [fEntry], FENTRY_SILENTBRK
@@:        
	@tprintf <"ResetBreaks: breakpoint %X cleared automatically",lf>, esi
	call clearbreakpnt1
noautoreset:
doneitem:
if ?WINDOWS
	cmp dwOldAttr,0
	jz @F
	invoke setptentry, dwMem, dwOldAttr, 2	;R/W bit ist 2
@@:        
endif
	popad
	mov esi, [esi.BREAK.pNext]
	jmp nextitem
done:
	mov ecx, korreip
	jecxz resetthebreaks_4	   ; kein breakpoint aktuell

	test [fEntry], FENTRY_REAL
	jnz @F
	sub [r1.rEip], ecx
	jmp resetthebreaks_2
@@:
	sub [r1r.rIP], cx
resetthebreaks_2:
resetthebreaks_4:
	@tprintf <"ResetBreaks exit",lf>
	popad
	ret
ResetBreaks endp

ClearAllAutoBreaks proc stdcall public

	mov esi,breakpointtab
nextitem:
	and esi,esi
	jz done
	test [esi].BREAK.bFlags1, FBRP_AUTO
	jz skipitem
	call clearbreakpnt1
skipitem:        
	mov esi,[esi.BREAK.pNext]
	jmp nextitem
done:        
	ret
ClearAllAutoBreaks endp

;*** breakpoint in liste eintragen	 ***
;*** oder alle breakpoints auflisten ***

	@cmdproc	; no prologue ( ok as long as no locals are defined )

_sethwbreak3 proc c public pb:PARMBLK 		;io watchpoint (v2.11)
	mov al, FBRP_USEHW
	mov ah, 03
	jmp setbreakpnt
_sethwbreak3 endp

_sethwbreak2 proc c public pb:PARMBLK 		;read or write watchpoint
	mov al, FBRP_USEHW
	mov ah, 02
	jmp setbreakpnt
_sethwbreak2 endp

_sethwbreak1 proc c public pb:PARMBLK 		;write watchpoint
	mov al, FBRP_USEHW
	mov ah, 01
	jmp setbreakpnt
_sethwbreak1 endp

_sethwbreak0 proc c public pb:PARMBLK 		;execute watchpoint
	mov al, FBRP_USEHW
	mov ah, 00
	jmp setbreakpnt
_sethwbreak0 endp

_setbreakpnt proc c public pb:PARMBLK 		;standard breakpoint
	mov al, 0
;	jmp setbreakpnt
_setbreakpnt endp

	@cmdprocend

setbreakpnt proc c public pb:PARMBLK 

local	bpflags:dword

	mov bpflags, eax
	cmp byte ptr pb.wArgc,0
	jnz setbreakpnt_1
	call breaksout
	jmp exit
setbreakpnt_1:
	call GetCurrentCS				  ;CS -> AX, type in dl
	mov ebx,pb.dwOffs1
	cmp cl,__VOID__ 				  ;address given?
	jnz @F
	test byte ptr bpflags, FBRP_USEHW
	jnz error1
	cmp byte ptr pb.wArgc, 4 		  ;watchpoint requires condition
	jb error3
	or byte ptr bpflags, FBRP_NOADDR
@@:
	cmp cl, __FPTR__
	jnz @F
	mov eax, [pb.wSeg1]
	mov dl, 00
@@:
	cmp cl, __RMLPTR__
	jnz @F
	mov eax, [a1.dwSeg]				;wSeg1 is selector
	mov dl, 01
@@:
	mov ecx, bpflags 			;Flags

	cmp pb.p3.bType, __STRING__
	jnz @F
	push eax
	invoke PutStringInHeap, pb.p3.dwOffs
	mov edi, eax
	pop eax
	or cl, FBRP_CMDSET
@@:
	call insertbrkpnt				;set breakpnt AX:EBX
	jc exit

	mov ebx, eax
	mov eax, pb.dwOffs2
	mov [ebx.BREAK.count], eax

	cmp byte ptr pb.wArgc, 3
	jna setbreakpnt_3

	cmp pb.p4.bType, __STRING__		; bedingter breakpoint
	jnz @F
	invoke transform, pb.p4.dwOffs		; in grossbuchstaben
	invoke PutStringInHeap, pb.p4.dwOffs
	mov [ebx.BREAK.bCondTyp], __STRING__
	jmp setbreakpnt_2
@@:
	push esi
	mov esi, [a4.ARGDESC.dwPtr]
	mov al, [esi.SYMBOL.bType]
	mov [ebx.BREAK.bCondTyp], al
	mov eax, esi
	pop esi
setbreakpnt_2:
	mov [ebx.BREAK.brpCAdr], eax
	mov eax, [a5.ARGDESC.dwOfs]
	mov [ebx.BREAK.brpCVa1], eax
	mov eax, [a6.ARGDESC.dwOfs]
	mov [ebx.BREAK.brpCVa2], eax
setbreakpnt_3:
	@errorout MSG_BREAKPNT_SET
exit:
	ret
error3:
	@errorout ERR_CONDITION_MISSING
	jmp exit
error1:
	@errorout ERR_NOHW_WATCHPOINT
	ret
setbreakpnt endp

;--- display type of hw break

disphwbreak proc

	movzx ebx, byte ptr [esi].HWBP.hwbpLen
	movzx edx, byte ptr [esi].HWBP.hwbpState
	invoke printf, CStr("hw: %02X %02X "), ebx, edx
disphwtype::
	mov al, [esi].HWBP.hwbpTyp
	cmp al, 0
	jnz @F
	@stroutc "execute"
	ret
@@:
	cmp al, 1
	jnz @F
	@stroutc "write"
	ret
@@:
	cmp al, 2
	jnz @F
	@stroutc "read & write"
	ret
@@:
	@stroutc "io"
	ret

disphwbreak endp

;*** alle breakpoints auflisten ***

breaksout proc stdcall

	pushad
	mov esi, breakpointtab
	xor edx, edx
nextitem:
	and esi,esi
	jz exit
	mov cl, [esi].BREAK.bFlags1
	test cl,FBRP_ACTIVE
	jz skipitem
	inc dh
	pushad
	mov al,' '
	test cl,FBRP_ENABLED
	jnz @F
	mov al,'*'
@@:
	@putchr al
	mov al, dl
	invoke _hexout
	@putchr ' '
	mov al,' '
	test cl,FBRP_REAL
	jz @F
	mov al,'&'
@@:
	@putchr al
	test cl,FBRP_NOADDR
	jz @F
	@stroutc "*************"
	jmp bo_1
@@:
	lea ebx, [esi.BREAK.brpAddr]
	mov cl, __FPTR__
	call symout
bo_1:
;	@putchr ' '
;	mov al, [esi].BREAK.bFlags1
;	invoke  _hexout
	movzx eax, [esi].BREAK.bFlags1
	invoke printf, CStr(" %02X %8X "), eax, [esi.BREAK.count]
	test [esi].BREAK.bFlags1, FBRP_CMDSET
	jz @F
	invoke printf, CStr("'%s'"),[esi.BREAK.brpDefCmd]
@@:
	cmp [esi.BREAK.bCondTyp], 0
	jz breaksout_2
	@stroutc " ("
	mov cl, [esi.BREAK.bCondTyp]
	mov al, cl
	invoke	_hexout
	@putchr ' '
	mov ebx, [esi.BREAK.brpCAdr]
	cmp cl, __STRING__
	jnz @F
	invoke printf, CStr("'%s'"), ebx
	jmp breaksout_3
@@:
	@strout [ebx.SYMBOL.pText]
	@stroutc " min="
	lea ebx, [esi.BREAK.brpCVa1]
	mov cl, [esi.BREAK.bCondTyp]
	call symout
	@stroutc " max="
	lea ebx, [esi.BREAK.brpCVa2]
	mov cl, [esi.BREAK.bCondTyp]
	call symout
breaksout_3:
	@putchr ')'
breaksout_2:
	test [esi].BREAK.bFlags1, FBRP_USEHW
	jz @F
	push esi
	mov esi, [esi.BREAK.brpHW]
	call disphwbreak
	pop esi
@@:
	invoke _crout
	popad
skipitem:
	inc dl
	mov  esi, [esi.BREAK.pNext]
	jmp nextitem
exit:
	and dh, dh
	jnz @F
	@errorout MSG_NO_BREAKPNTS
@@:
	popad
	ret
breaksout endp

;--- dont add multiple breakpoints for same address
;--- inp: ESI=start of BREAK tab
;--- CL=type, AX:EBX=address

checkifbpexists proc stdcall uses esi ecx

	and cl, not (FBRP_CMDSET or FBRP_USEHW)
	jmp skipitem
nextitem:
	mov esi, [esi].BREAK.pNext
	mov dh, [esi].BREAK.bFlags1
	and dh, FBRP_AUTO or FBRP_REAL or FBRP_ACTIVE or FBRP_ENABLED
	cmp cl, dh
	jnz skipitem
	cmp ebx, dword ptr [esi].BREAK.brpAddr
	jnz skipitem
	cmp ax, word ptr [esi].BREAK.brpAddr+4
	jnz skipitem
;	@stroutc "breakpoint exists",cr,lf
	mov eax, esi
	clc
	ret
skipitem:
	cmp [esi].BREAK.pNext, 0
	jnz nextitem
	stc
	ret
checkifbpexists endp

;*** add breakpoint (AX:EBX) into bp list
;*** type in DL:0=AX is selector/1=AX is segment
;*** Flags in CL (FBRP_AUTO, FBRP_CMDSET, FBRP_USEHW)
;*** Type in CH (if hw break): exec, write, read/write, io
;*** if FBRP_CMDSET: ^string in edi
;*** out: eax = breakpoint object

insertbrkpnt proc stdcall public uses esi

local	flags:dword
local	bpsel:dword

	mov bpsel, eax
	or cl,FBRP_ACTIVE or FBRP_ENABLED
	test dl, 1					   ;real mode address?
	jz @F
	or cl, FBRP_REAL
	movzx ebx, bx
@@:
	mov esi, offset breakpointtab
	call checkifbpexists
	jnc exit
nextitem:
	cmp [esi].BREAK.pNext,0
	jz endofitems
	mov esi,[esi].BREAK.pNext
	test [esi].BREAK.bFlags1, FBRP_ACTIVE
	jz unuseditem
	jmp nextitem
endofitems:
	invoke malloc, sizeof BREAK
	and eax, eax
	jnz @F
	@errorout ERR_MEMORY_ERROR
	stc
	jmp exit
@@:
	mov [esi].BREAK.pNext, eax
	mov esi, eax
unuseditem:
	mov [esi].BREAK.count, 0
	mov [esi].BREAK.bCondTyp, 0
	mov [esi].BREAK.brpDefCmd, edi
	mov dword ptr [esi].BREAK.brpAddr+0, ebx
	mov eax, bpsel
	mov word ptr [esi].BREAK.brpAddr+4, ax
	test cl, FBRP_USEHW
	jz ib_1
	push ecx
	call getbaseof			; get linear address of eax
	pop edx 				; dh=type 0,1,2,3
	jc hwb_err
	add ebx, eax
	mov dl, 01h				; dl=size 1,2,4
	call savedebughandle
	jnc hwb_ok
	@errorout WRN_NOMORE_HWBREAKS
hwb_err:
	and cl, not FBRP_USEHW
	jmp ib_1
hwb_ok:
	mov [esi].BREAK.brpHW, eax
ib_1:
	mov [esi].BREAK.bFlags1, cl
	mov eax, esi
	clc
exit:
	ret

getbaseof:
	test dl, 1
	jnz @F
	invoke getbaser, eax	; C if failure
	retn
@@:
	movzx eax, ax
	shl eax, 4
	retn

insertbrkpnt endp

;*** delete breakpnt ^esi

clearbreakpnt1 proc stdcall
	xor eax,eax
	mov [esi].BREAK.bFlags2, al
	xchg [esi].BREAK.bFlags1, al
	xchg [esi.BREAK.bCondTyp], ah
	test al,FBRP_ACTIVE
	jz error
	test al,FBRP_USEHW
	jz @F
	mov eax,[esi.BREAK.brpHW]
	call deletedebughandle
@@:
	ret
error:
	stc
	ret
clearbreakpnt1 endp

;*** find breakpoint EAX

findbreakpnt proc stdcall uses esi

	mov esi,breakpointtab
	xor ecx, ecx
@@:
	and esi,esi
	jz error
	cmp eax,ecx
	jz found
	mov esi,[esi].BREAK.pNext
	inc ecx
	jmp @B
error:
	stc
found:
	mov eax, esi
	ret
findbreakpnt endp

_clearbreakpnt proc c public pb:PARMBLK

	mov eax,pb.dwOffs1
	call findbreakpnt
	jnc @F
	@errorout <ERR_NO_BREAKPOINT>
        ret
@@:
	mov esi, eax
	call clearbreakpnt1
	ret
_clearbreakpnt endp

_disablebreakpnt proc c public pb:PARMBLK

	mov eax,pb.dwOffs1
	call findbreakpnt
	jnc @F
	@errorout <ERR_NO_BREAKPOINT>
	ret
@@:
	and [eax].BREAK.bFlags1, not FBRP_ENABLED
	ret
_disablebreakpnt endp

_enablebreakpnt proc c public pb:PARMBLK

	mov eax,pb.dwOffs1
	call findbreakpnt
	jnc @F
	@errorout <ERR_NO_BREAKPOINT>
	ret
@@:
	or [eax].BREAK.bFlags1, FBRP_ENABLED
	ret
_enablebreakpnt endp

;*****************
;*** hw breaks ***
;*****************

	.DATA

ANZDEBUGHANDLES equ 4

debughandles label word
	HWBP ANZDEBUGHANDLES dup (<0,0,0,-1,0>)

	.CODE

deletedebughandle proc stdcall uses esi

	mov esi,eax
	cmp [esi.HWBP.hwbpTyp],-1
	stc
	jz exit
	mov [esi.HWBP.hwbpTyp],-1
	clc
exit:
	ret
deletedebughandle endp

;*** add hw breakpoint to list
;--- called by insertbrkpnt() & SetTheBreaks() [if a soft break can't be written]
;*** ebx=linear address
;*** dl=size (1,2 or 4)
;*** dh=type ( 0=execute, 1=read, 2=read/write, 3=io )
;*** out: handle in eax (= address)

savedebughandle proc stdcall uses ecx esi edi

	mov esi, offset debughandles
	mov ecx, ANZDEBUGHANDLES
@@:
	cmp [esi.HWBP.hwbpTyp], -1
	jz savedebughandle_1
	add esi, sizeof HWBP
	loop @B
error:
	stc
	ret
savedebughandle_1:
	test bHWBrk, 1
	jz error
	mov edi, ebx
	push ebx
	pop cx
	pop bx
	mov ax,0B00h				;alloc a watchpoint
	@DpmiCall
	jc error
	mov ax,0B01h				;delete watchpoint
	@DpmiCall
	jnc @F
	movzx ebx,bx
	push ebx
	@errorout ERR_CANNOT_DELETE_HWBREAK
	add esp,4
@@:        
	mov [esi.HWBP.hwbpTyp], dh
	mov [esi.HWBP.hwbpLen], dl
	mov [esi.HWBP.hwbpAddr], edi
	mov eax, esi
	clc
	ret
savedebughandle endp

;*** hardware breakpoint setzen ***

if 0
_setdebugreg proc c pb:PARMBLK

	call GetCurrentCS
	mov ebx,eax
	mov cl, pb.p1.bType
	cmp cl,__FPTR__
	jnz @F
	mov ebx,pb.wSeg1
@@:
	cmp cl,__RMLPTR__
	jnz @F
	mov ebx,pb.wSeg1
	mov dl,1
@@:
setdebugreg_2:
	add pb.dwOffs1,eax
	mov ebx,pb.dwOffs1
	mov dh,byte ptr pb.p2.dwOffs+0
	mov dl,byte ptr pb.p3.dwOffs+0   ;muss 1,2 oder 4 sein

	mov al,byte ptr pb.wArgC
	cmp al,3
	jnb setdebugreg_1
	cmp al,1
	ja @F
	mov dl,1
@@:
	mov dh,0
setdebugreg_1:
	cmp dh,2					;0,1 oder 2
	ja setdebugreg_err1
	and dl,dl
	jz setdebugreg_err1
	cmp dl,4
	ja setdebugreg_err1
	cmp dl,3
	jz setdebugreg_err1
	call savedebughandle
	jc setdebugreg_err2
	@stroutc "breakpoint set"
	invoke _crout
	jmp setdebugreg_ex
setdebugreg_err2:
	@stroutc "Error"
	jmp setdebugreg_ex
setdebugreg_err1:
	@strout offset tQuestions
setdebugreg_ex:
	ret
_setdebugreg endp
endif

if ?SUPPBRL

listhwbreaks proc stdcall

	mov esi,offset debughandles
	mov ecx,ANZDEBUGHANDLES
listhwbreaks_1:
	cmp [esi].HWBP.hwbpTyp,-1
	jz listhwbreaks_3
	mov eax,ANZDEBUGHANDLES
	sub eax,ecx
	movzx ebx,byte ptr [esi].HWBP.hwbpLen
	movzx edx,byte ptr [esi].HWBP.hwbpState
	invoke printf, CStr("%4X %08X %8X[%02X] %02X "), eax, esi, [esi].HWBP.hwbpAddr, ebx, edx
	call disphwtype
	@putchr lf
listhwbreaks_3:
	add esi, sizeof HWBP
	dec ecx
	jnz listhwbreaks_1
	ret
listhwbreaks endp

_listhwbreaks proc c public pb:PARMBLK
	call listhwbreaks
	ret
_listhwbreaks endp
endif

;*** set debug watchpoints

ActAllHWBreaks proc stdcall public

	mov ecx,ANZDEBUGHANDLES
	mov esi,offset debughandles
nextitem:
	mov dh,[esi].HWBP.hwbpTyp
	cmp dh,-1					;used?
	jz skipitem
	test [esi].HWBP.hwbpState,FHWBP_SET	;set already?
	jnz skipitem
	mov [esi].HWBP.hwbpState,FHWBP_SET
	test bHWBrk,1
	jz nohwbreak
	push ecx
	mov bx,word ptr [esi].HWBP.hwbpAddr+2
	mov cx,word ptr [esi].HWBP.hwbpAddr+0
	mov dl,[esi].HWBP.hwbpLen
	mov ax,0B00h				;set watchpoint
	@DpmiCall
	pop ecx
	jnc @F
nohwbreak:        
	and [esi].HWBP.hwbpState,not FHWBP_SET
	push [esi].HWBP.hwbpAddr
	@errorout ERR_CANNOT_SET_HWBREAKS
	add esp,4
	jmp skipitem
@@:
	mov [esi].HWBP.hwbpHandle,bx
skipitem:
	add esi, sizeof HWBP
	loop nextitem
	ret
ActAllHWBreaks endp

;*** DeactAllHWBreaks is the first thing to be called on debugger entry
;*** registers aren't saved yet, but stack has been changed.
;--- if debuggee has terminated, all hw bps may have become invalid (hdpmi)!

DeactAllHWBreaks proc stdcall public
	pushad
	mov ecx,ANZDEBUGHANDLES
	mov esi,offset debughandles
nextitem:
	cmp [esi].HWBP.hwbpTyp,-1	;free?
	jz skipitem
	test [esi].HWBP.hwbpState,FHWBP_SET	;bp set?
	jz skipitem
	movzx ebx,[esi].HWBP.hwbpHandle
	mov ax,0B02h				;get watchpoint state
	@DpmiCall
	jnc @F
	push ebx
	@errorout ERR_CANNOT_GET_HWBREAK_STATE
	add esp,4
	mov al,[esi].HWBP.hwbpState
@@:
	test al,1
	jz @F
	or [fEntry], FENTRY_HWBREAK
@@:
	and [esi].HWBP.hwbpState,not (FHWBP_TRIGGERED or FHWBP_SET)
	mov ax,0B01h				;delete debug watchpoint
	@DpmiCall
	jnc @F
	push ebx
	@errorout ERR_CANNOT_DELETE_HWBREAK
	add esp,4
@@:        
skipitem:
	add esi, sizeof HWBP
	loop nextitem
	popad
	ret
DeactAllHWBreaks endp

HWBreakHit proc stdcall public
	pushad
	mov ecx,ANZDEBUGHANDLES
	mov esi,offset debughandles
nextitem:
	cmp [esi.HWBP.hwbpTyp],-1	;free
	jz skipitem
	test [esi.HWBP.hwbpState],FHWBP_SET	;bp set?
	jz skipitem
	movzx ebx,[esi.HWBP.hwbpHandle]
	mov ax,0B02h				;get watchpoint state
	@DpmiCall
	jc skipitem
	test al,1
	stc
	jnz found
skipitem:
	add esi, sizeof HWBP
	loop nextitem
	clc
found:  
	popad
	ret
HWBreakHit endp

	end

