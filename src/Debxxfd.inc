
    assume fs:nothing
    assume gs:nothing

if ?32BIT eq 0

?SEGOFFS equ 2
?MYNAME textequ <"deb16f">
 if ?WINDOWS
?MYNAME textequ ?MYNAME,<,"w">
 endif

else

?SEGOFFS equ 4
?MYNAME  textequ <"deb32f">
 ife ?FLAT
?MYNAME  textequ ?MYNAME,<,"o">
 endif

endif

?SUPPSFT	equ 1		;support .SFT command
?SUPPCDS	equ 1		;support .CDS command
ife ?32BIT
?SUPPLHEAP	equ 1		;support .DL command
else
?SUPPLHEAP	equ 0		;support .DL command
endif
?SUPPVPICD	equ 1		;support .VPICD command
?SUPPVCD	equ 1		;support .VCD command

CCONST segment dword public 'CODE'
CCONST ends
CONST segment dword public 'CONST'
CONST ends
ife ?FLAT
DGROUP group CCONST, CONST
endif

;*** type definitions

PF16 typedef far16 ptr
PF32 typedef far32 ptr

;*** constants

;--- high byte of cpu flags

FL_TRACE	equ 01h
FL_INT		equ 02h

;--- fStat flags
FSTAT_DEBUGINIT   equ 02h   ;firstentry has been called
FSTAT_free        equ 04h   ;
FSTAT_BUFNOTEMPTY equ 08h   ;line buffer is filled
FSTAT_DOSOUT      equ 10h   ;
FSTAT_DTAREAD	  equ 20h
FSTAT_RESIDENT    equ 40h   ;debugger is resident
FSTAT_ISNT        equ 80h   ;running under NT/2K/XP?

;--- fMode flags
FMODE_QUIET		equ 01h	; dont display debugger logo 
FMODE_SKIP		equ 02h	; skip is active
FMODE_NODISP	equ 04h	; no display on entry until prompt
FMODE_STRICT	equ 08h	; dont access GDT, IDT, LDT or CRx directly
FMODE_INDEBUG	equ 10h	; currently in debugger
FMODE_LDTDIRECT	equ 20h	; access LDT directly
FMODE_REGSOUT	equ 40h	; display register set
FMODE_EXEACTIVE	equ 80h	; an exit proc has been set

FMODE_SKIP_BIT	equ 1	; for BTx instructions

;--- fEntry flags
FENTRY_INT01	  equ 01h	;debugger has been entered thru EXC/INT 01
FENTRY_EXCEPTION  equ 02h	;debugger has been entered thru EXC 0,6,c,d,e
FENTRY_INT03	  equ 04h	;debugger has been entered thru EXC/INT 03
FENTRY_REAL 	  equ 08h	;debugger has been entered thru real mode
FENTRY_HWBREAK	  equ 10h	;debugger has been entered by a HW break
FENTRY_IDT		  equ 20h	;entry throu IDT directly
FENTRY_SILENTBRK  equ 40h	;int 03 entry with an silent breakpoint
FENTRY_SYSREQ	  equ 80h	;entry ctrl-alt-sysreq	

FTRAP_ACTIVE	  equ 01h	;exception trap aktiv
FTRAP_STOP		  equ 02h	;stop bei exception

;--- fExit flags
FEXIT_TRACE 	equ 01h		;trace exit
FEXIT_NOSWITCH	equ 02h		;dont switch to debuggee screen
FEXIT_CANCEL	equ 04h		;exit with cancel, quit, not go, perform
FEXIT_LOADPGM 	equ 08h		;exit with loadpgm

F80X87_FPU_PRESENT equ 01h
F80X87_STATE_SAVED equ 02h
F80X87_IGNORE	   equ 80h

;--- fException flags
FEXC_USEXREGS	equ 1		;use saved values of CS:EIP and EFL
FEXC_SAVESTACK	equ 2		;use saved values of SS:ESP

;--- fTMode flags
FTMODE_NOTRACEIFCSINGDT equ 01h	;dont set trace if CS is in GDT
FTMODE_CHECKINT 		equ 02h	;check if next instr is a INT
FTMODE_NOTRACEFORINT	equ 04h	;reset trace flag if next instr is a INT
FTMODE_ISDOS4GW			equ 08h	;assume debuggee is DOS4/GW
FTMODE_SWAPALWAYS		equ 10h	;swap in any case
FTMODE_CHECKREP 		equ 20h	;check if next instr is a REP
FTMODE_SKIPUNKNOWNEXC01 equ 40h	;skip unknown exceptions 01
FTMODE_STOPIFCTRLLOST	equ 80h	;stop trace (prompt user) if control may be
								;lost (unknown protected-mode breakpoint)
                                 
;--- bTraceFlags flags
FTRAC_LOG 			equ 01h	;log trace step

;--- fUnass flags
FUNAS_NOSHORTDISP	equ 01h	;dont display "short" for short distances
FUNAS_DISPLAYAX		equ 02h	;display "ax=xxxx" behind int 21/31
FUNAS_TRANSLATECS	equ 80h	;translate CS to owner

;--- fVideo flags
FVIDEO_GRAPH		equ 1	;debuggee is in a graphics mode                        
FVIDEO_ISDBGER		equ 2	;screen owned by debugger?
FVIDEO_NOSWITCH		equ 4	;dont switch to debugger screen
FVIDEO_CONSOLE		equ 8	;console allocated?
FVIDEO_WASINGRFX	equ 10h ;on last exit debuggee was in graphics mode!
FVIDEO_ERROR		equ 40h	;error on video output
FVIDEO_INIT			equ 80h	;debugger screen initialized?

;--- fKeybrd flags
FKB_SYSREQ		equ 01h		;handle Sysreq
FKB_EXECINT01	equ 02h		;exec an INT 01 in IRQ 1 routine for SYSREQ
FKB_EXECINT03	equ 04h		;exec an INT 03 in IRQ 1 routine for SYSREQ
FKB_ALTSCROLL	equ 08h		;use Alt-Scroll instead of SysReq

if ?WINDOWS
;--- fToolhelp flags
FTH_REGISTERED		equ 1	;Toolhelp aktiv
endif

;--- fDump flags
FDT_RMADDR		equ 1
FDT_SWITCHSCREEN equ 2		;switch to debuggee screen when getting bytes

;--- fUnastype flags
FUT_RMADDR		equ 1

SEM_NOOPENFILEERRORBOX equ 8000h
SEM_FAILCRITICALERRORS equ 0001h

;--- fDosUsed flags
FDU_SAVED		  equ 1
FDU_PSPSWITCHED   equ 2

;--- feature flags in EDX bei cpuid, eax=1
ID_INTFPU		equ 00000001h ;FPU: FPU is integrated
ID_V86EXT		equ 00000002h ;VME: supports CR4 VME/PVI, EFL VIF/VIP
ID_IOBREAKS 	equ 00000004h ;DE:  supports i/o breakpoints + CR4 DE
ID_4MPAGES		equ 00000008h ;PSE: 4MB page size supported + CR4 PSE
ID_TSCOUNT		equ 00000010h ;TSC: support for RDTSC + CR4 TSD
ID_MSR			equ 00000020h ;MSR: support for RDMSR/WRMSR
ID_PAE			equ 00000040h ;PAE: physical address extension + CR4 PAE
ID_MCE			equ 00000080h ;MCE: machine check exceptions + CR4 MCE
ID_CX8			equ 00000100h ;CX8: CMPXCHG8B supported
ID_APIC			equ 00000200h ;APIC: on chip APIC exists and enabled
ID_reserved1	equ 00000400h
ID_reserved2	equ 00000800h
ID_MTRR			equ 00001000h ;memory type range registers supported
ID_PGE			equ 00002000h ;PGE: support for CR4 PGE
ID_MCA			equ 00004000h ;MCA: MCA_GAP MSR supported
ID_CMOV			equ 00008000h ;CMOV: CMOV + FCMOV/FCOMI supported
ID_reserved3	equ 00010000h
ID_reserved4	equ 00020000h
ID_reserved5	equ 00040000h
ID_reserved6	equ 00080000h
ID_reserved7	equ 00100000h
ID_reserved8	equ 00200000h
ID_reserved9	equ 00400000h
ID_MMX			equ 00800000h ;MMX supported


;--- data types of debxxf symbol table
__VOID__	   equ 0
__BYTE__	   equ 1
__WORD__	   equ 2
__BOOL__	   equ 3
__DWORD__	   equ 4
__CONST__	   equ 4
__FWORD__	   equ 5		  ;for GDTR,IDTR BBBBBBBB(LLLL)
__FPTR__	   equ 6		  ;pointer SSSS:OOOOOOOO
__LPTR__	   equ 7		  ;pointer SSSS:OOOO
__QWORD__	   equ 8		  ;floating point format + MMX
__STRING__	   equ 09h		  ;asciiz-string
__LSTRING__    equ 0Ah		  ;Byteliste laenge in CH (nicht verwendet)
__RMLPTR__	   equ 0Bh		  ;real mode pointer SSSS:OOOO
__CHAR__	   equ 0Ch
__LIST__	   equ 0Dh		  ;Byteliste
__TBYTE__	   equ 0Eh		  ;ten byte (internes floating point format)
__OWORD__	   equ 0Fh		  ;XMM (16 byte)

;--- Flags SYMTYPE
_FNCALL_ equ 10h
_RDONLY_ equ 20h

;*** structs

PUSHADS struct
rEdi	dd ?
rEsi	dd ?		;+4
rEbp	dd ?		;+8
rEsp	dd ?        ;+C
rEbx	dd ?		;+10
rEdx	dd ?
rEcx	dd ?		;+18
rEax	dd ?		;+1C
PUSHADS ends

;--- kind of extended RMCS

MYREGSTR struct
		PUSHADS <>
rES		dd ?
rDS		dd ?
rFS		dd ?
rGS		dd ?
rEip	dd ?
rCS		dd ?
rEfl	dd ?
rSS		dd ?
		dd ?		;???
MYREGSTR ends

RMSPREGS struct
rFlags  dw ?
rES		dw ?
rDS		dw ?
rFS		dw ?
rGS		dw ?
rIP		dw ?
rCS		dw ?
rSP		dw ?
rSS		dw ?
RMSPREGS ends

IRETS struct
if ?32BIT
rIP 	dd ?
rCS 	dd ?
rFL 	dd ?
rSP 	dd ?
rSS 	dd ?
else
rIP 	dw ?
rCS 	dw ?
rFL 	dw ?
rSP 	dw ?
rSS 	dw ?
endif
IRETS ends

;--- flags for insertbreakpnt()
FBRP_ACTIVE 	equ 01h	;bp is active (enabled or disabled)
FBRP_ENABLED	equ 02h	;bp is enabled
FBRP_AUTO		equ 04h	;bp auto-reset nach "enter debug"
FBRP_REAL		equ 08h	;bp address is in real mode format
FBRP_CMDSET 	equ 10h	;Inhalt DefCmd gÅltig
FBRP_USEHW		equ 20h	;Hardware Breakpoint verwenden (im ROM)
FBRP_NOADDR 	equ 40h	;"Breakpoint" ohne Adresse (=Watchpoint)
FBRP_SILENT		equ 80h	;internal bp to help trace command

FBRP2_TRAPPED	equ 01h	;Breakpoint hat debug trap verursacht
FBRP2_SET		equ 02h	;bp is set

ARGDESC struct
dwOfs	dd ?			; value   (offset if type is an address)
dwSeg	dd ?			; segment (addresses only)
dwPx	dd ?			; pointer auf string
dwPtr	dd ?			; pointer in symboltabelle bei symbolen
dwType	dd ?			; type of argument
ARGDESC ends

PARM struct				; 1 parameter in PARMBLK
dwOffs	dd ?
union
dwSeg	dd ?
wSeg	dw ?
ends
union
wType	dw ?
bType	db ?
ends
		dw ?
PARM ends

; parameter struct for cmds and functions

PARMBLK struct
wArgc	dd ?			; no of arguments
p1		PARM <>
p2		PARM <>
p3		PARM <>
p4		PARM <>
p5		PARM <>
p6		PARM <>
PARMBLK ends

dwOffs1 textequ <p1.dwOffs>
dwOffs2 textequ <p2.dwOffs>
wSeg1   textequ <p1.dwSeg>
wSeg2   textequ <p2.dwSeg>

SYMBOL struct
pNext	db ?			;adresse naechstes symbol
pText	dd ?			;adresse symbolbezeichner
bType	db ?			;symboltyp
bType2	db ?			;zusatzinfos (0-3:parmanzahl)
dwProc	dd ?			;adresse procedur/variable
dwAddr	dd ?			;adresse variable (falls procedur in SYMPROC)
SYMBOL	ends

;--- values for bType2
;--- 10h=free?
;--- 20h=free?
;--- 40h=free?
_PARMOK_ equ 80h		;strings erlauben?

NUMNRITEMS equ 8

NRNT struct
NRNTMOD dd ?
NRNTHDL dd ?
NRNT ends

DISKIO struct
startsec dd ?
sectors  dw ?
if ?32BIT
buffofs  dd ?
else
buffofs  dw ?
endif
buffseg  dw ?
DISKIO ends

CRTPARMS struct
pBuffer dd ?
crtadr	dw ?		;crt port
curpos	dw ?
page_	db ?
cols	db ?
rows	db ?
bFlags	db ?
CRTPARMS ends

;--- values for CRTPARMS.bFlags

FCRT_SHARED	equ 1	;screen is shared with debuggee

	include debxxf.inc

_A_RDONLY equ 00000001h
_A_HIDDEN equ 00000002h
_A_SYSTEM equ 00000004h
_A_SUBDIR equ 00000010h

;*** MACROS ***

@cpuid macro
	db 0Fh,0A2h
endm

@rdmsr macro
	db 0Fh,032h
endm

@wrmsr macro
	db 0Fh,030h
endm

@close macro a
	mov ebx,a
	mov ah,3Eh
	@DosCall
endm

@setint24 macro
local xxxx
if ?WINDOWS
  if ?WATCHINT24
		cmp word ptr [oldint24+2],0000
		jnz xxxx
		mov bl, 24h
		mov ax, 204h
		int 31h
		mov word ptr [oldint24+0],dx
		mov word ptr [oldint24+2],cx
		mov ecx, cs
		mov edx, offset intr24
		mov ax, 205h
		int 31h
xxxx:
  endif
endif
endm

@resetint24 macro
local xxxx
if ?WINDOWS
  if ?WATCHINT24
		xor ecx,ecx
		xchg ecx,[oldint24]
		jecxz xxxx
		push ecx
		pop dx
		pop cx
		mov bl,24h
		mov ax,205h
		int 31h
xxxx:
  endif
endif
endm

@tprintf macro xx:REQ,p1,p2,p3,p4,p5,p6,p7,p8,p9,p10
local yy
if _TRACE_
tprintf proto stdcall
	for arg, <p10,p9,p8,p7,p6,p5,p4,p3,p2,p1>
	ifnb <arg>
		push arg
	endif
	endm
	.const
yy	db xx,0
	.code
	push offset yy
	call tprintf
endif
endm

;@ring0call macro x
;	push offset x
;	call _ring0
;endm

@DosCallx macro x
	call MarkDOSused
	@DosCall <x>
endm

@errorout macro x
	ifnb <x>
	   push x
	endif
	call _errorout
endm

@callint2F macro
if ?WATCHI2F
	call myint2F
else
	int 2Fh
endif
endm

@DpmiCall macro x
	ifnb <x>
	  mov ax,x
	endif
	call _dpmicall
endm

@DosCall macro x
	ifnb <x>
	  mov ah,x
	endif
	call _doscall
endm

;--- simple macro for a 3 byte move, used i.e.: @mov ecx, 3

@mov macro x, y
	push y
	pop x
endm

@moveaxcr4 macro
	db 0Fh,20h,0E0h
endm

@movcr4eax macro
	db 0Fh,22h,0E0h
endm

@moveaxdr4 macro
	db 0Fh,21h,0E0h
endm

@moveaxdr5 macro
	db 0Fh,21h,0E8h
endm

@moveaxtr3 macro
	db 0Fh,24h,0D8h
endm

@moveaxtr4 macro
	db 0Fh,24h,0E0h
endm

@moveaxtr5 macro
	db 0Fh,24h,0E8h
endm

@cli macro
if 0
	cli
else
	push eax
	@DpmiCall 900h
	pop eax
endif
endm

@sti macro
if 0
	sti
else
	push eax
	@DpmiCall 901h
	pop eax
endif
endm

VxDCall macro x,y
	int 20h
	dw y,x
endm

VMMCall macro x
	int 20h
	dw x,0001h
endm

VPICD_DEVICE_ID 			  equ 0003h
VPICD_Get_IRQ_Complete_Status equ 000Bh
VCD_DEVICE_ID				  equ 000Eh
VCD_Get_Focus				  equ 0002h
Get_Sys_VM_Handle			  equ 0003h

@savewinsegregs macro
	push gs
	push es
endm

@restorewinsegregs macro
	pop es
	pop gs
endm

@adjint25sp macro
if ?32BIT
	pop ebx
else
	pop bx
endif
endm

;---- define a symbol(a=name, b=type, c=type2, d=proc, e=dwAddr, f=label)
;---- bits 0-3 of type2 are number of arguments

@symbol macro a,b,c,d,e,f
local nlab,x
ifnb <f>
f	label byte
endif
	db offset nlab - $	; SYMBOL.pNext
if ?FLAT
CConst segment dword public 'CODE'
else
CConst segment dword public 'CONST'
DGROUP group CConst
endif
x	label byte
ifnb <a>
	db a
endif
	db 0
CConst ends
	dd offset x 		; SYMBOL.pText
	db b				; SYMBOL.bType
ifnb <c>
 ifnb <e>
	db c or _PARMOK_	; SYMBOL.bType2
 else
	db c				; SYMBOL.bType2
 endif
else
 ifnb <e>
	db _PARMOK_ 		; SYMBOL.bType2
 else
	db 0				; SYMBOL.bType2
 endif
endif
ifnb <d>
	dd offset d			; SYMBOL.dwProc
endif
ifnb <e>
	dd e				; SYMBOL.dwAddr
endif
nlab label byte
endm

@symchain macro x
	db -1
	dd offset x
	db -1
endm

@outitem macro a,b,c,d
local x
ifnb <a>
CCONST segment
x	label byte
	db a
	db 0
CCONST ends
	dd offset x
else
	dd 0
endif
ifnb <b>
	dd offset b
else
	dd 0
endif
ifnb <c>
	dd c
endif
ifnb <d>
	dd d
endif
endm

@iret macro
  if ?32BIT
	iretd
  else
	iret
  endif
endm  

@pushf macro
  if ?32BIT
	pushfd
  else
	pushf
  endif
endm  

@CallInt macro oldint
	@pushf
	call cs:oldint
endm


@loadflat macro
ife ?FLAT
	mov @flat, [__flatsel]
endif
endm

;--- is this just to save prologue bytes for cmd procs?

@cmdproc macro
	option prologue:none
	option epilogue:none
endm

@cmdprocend macro
	option prologue:@prologue
	option epilogue:epiloguedef
endm

;--- CStr() define a string in .CONST

CStr macro text:VARARG
local sym
CONST segment
ifidni <text>,<"">
sym db 0
else
sym db text,0
endif
CONST ends
	exitm <offset sym>
endm

;--- CCStr() define a string in CCONST.
;--- it's located before _DATA, and within the first 64k (for 16-bit)

CCStr macro text:vararg
local sym
CCONST segment
ifidni <text>,<"">
sym db 0
else
sym db text,0
endif
CCONST ends
	exitm <offset sym>
endm

@switch32bit macro        
if ?CS16ALIAS
	db 0eah
	dw lowword $+4
	dw DGROUP
endif
endm
        
@switch16bit macro
if ?CS16ALIAS
	push cs:[__cs16alias]
	push $+6
	retf
endif        
endm

;--- define a "stream"

@defstream macro x,y

if @Model eq 7
.stream$A segment byte public 'DATA'
else
_STREAMS segment byte public 'CODE'
endif
	db x
ifnb <y>
	dd offset y
endif
if @Model eq 7
.stream$A ends
else
_STREAMS ends
endif
endm

