
;*** ///////////////////////////////////
;*** / deb16/32f                       /
;*** / DPMI debugger                   /
;*** ///////////////////////////////////

	.386
	.387
if ?FLAT
	.MODEL FLAT
else
	.MODEL TINY
	.DOSSEG
endif
	option proc:private
	option dotname
	option casemap:none

;------------------------------------------------------------------------

;?INDCALL = 0
?LDTSEL  = 1	;show LDT selector (problems with NT!)
?LOGINT2F16 = 0	;log int 2f, ah=16h calls (not ah=1680!)
?MARKPSP = 1	;mark PSP as "protected mode psp"
?DR4DR5 = 0		;1=read DR4/DR5 (mirrored DR6/DR7)
?TR3TR5 = 0		;1=read TR3/TR5
?LOGINTDBGEXC = 0;1=log debug exception inside debugger,0=ignore them

	.nolist
	.nocref
	include const.inc
	include ascii.inc
	include function.inc
	include dpmi.inc
	include debxxfd.inc
	include dos.inc
	include putf.inc
	include errors.inc
	include toolhelp.inc
	include fcntl.inc
	include rmdbghlp.inc
	include debugsys.inc
	include extern32.inc
	include extern16.inc

if @Model eq 7
.stream  segment dword public 'DATA'
;	dd 0	;make sure this segment is not discarded
else
_STREAMBEG segment dword public 'CODE'
endif

	public _streamtab
_streamtab label byte

if @Model eq 7
.stream ends
.stream$B  segment byte public 'DATA'
	public _streamend
_streamend label byte
.stream$B  ends
else
_STREAMBEG ends
_STREAMS segment byte public 'CODE'
_STREAMS ends
_STREAMEND segment byte public 'CODE'
	public _streamend
_streamend label byte
_STREAMEND ends
DGROUP group _STREAMBEG,_STREAMS,_STREAMEND
endif

_AUXPutChar proto near stdcall :dword
_DbgPutChar proto near stdcall :dword
_DosWrtTTY  proto near stdcall :dword
_I14PutChar proto near stdcall :dword
    
    @defstream _SEROUT,_AUXPutChar
    @defstream _DBGOUT,_DbgPutChar
    @defstream _DOSOUT,_DosWrtTTY
    @defstream _I14OUT,_I14PutChar

	.list
	.cref

GetPrivateProfileStringA proto near stdcall :ptr BYTE, :ptr BYTE, :ptr BYTE, :ptr BYTE, :DWORD, :ptr BYTE
WritePrivateProfileStringA proto near stdcall :ptr BYTE, :ptr BYTE, :ptr BYTE, :ptr BYTE
if ?WINDOWS
UnAssemble proto stdcall :ptr BYTE, :QWORD, :DWORD, :DWORD
else
UnAssemble proto stdcall :ptr BYTE, :QWORD, :DWORD
endif
alloctmpheap proto stdcall :DWORD

;------------------------------------------------------------------------

	.data

startofheap label byte

	db ?DBHEAP	dup (?)

start_of_stack label byte

	db ?DBSTACK dup (?)

	public top_of_stack	;stack must be below 64 kB offset

top_of_stack label byte

if ?WINDOWS
__outmode db _VIOOUT
__inpmode db _KBDINP
_DEFAULTOUT equ _VIOOUT
else
  if 0;_TRACE_
__outmode db _SEROUT
__inpmode db _SERINP
_DEFAULTOUT equ _SEROUT
  else
__outmode db _VIOOUT
__inpmode db _KBDINP
_DEFAULTOUT equ _VIOOUT
  endif
endif

__inpstream dd 0		;fuer DOS eingabe

fStat db 0	; global flags
			; 0=free
			; 1=FSTAT_DEBUGINIT firstentry has happened
			; 2=free
			; 3=FSTAT_BUFNOTEMPTY line buffer not empty
			; 4=FSTAT_DOSOUT dos output
			; 5=FSTAT_DTAREAD dta read
			; 6=FSTAT_RESIDENT debugger is resident
			; 7=FSTAT_ISNT
fMode db 0	; mode flags
			; 0=FMODE_QUIET: no logo display on startup
			; 1=FMODE_SKIP: dont handle next exception
			; 2=FMODE_NODISP: no display on entry
			; 3=FMODE_STRICT: no direct r/w access to GDT and LDT
			; 4=FMODE_INDEBUG: debugger is executing
			; 5=FMODE_LDTDIRECT: LDT direkt lesen
			; 6=free
			; 7=FMODE_EXEACTIVE: an exit proc is active
fExit db 0	; will be cleared in debugger loop
			; 0=FEXIT_TRACE: Exit mit Trace
			; 1=FEXIT_NOSWITCH: dont switch to debuggee screen
			; 2=FEXIT_CANCEL: cancel,quit exit
fException db 0
			; 0=FEXC_USEXREGS: restore entry ueber exception
			; 1=FEXC_SAVESTACK
fDOSused db 0
			; 0=FDU_SAVED: client in dos + debugger benutzte dos
			; 1=FDU_PSPSWITCHED: PSP und DTA auf debugger umgesetzt
;--- trace mode flags
fTMode db FTMODE_CHECKINT or FTMODE_CHECKREP or FTMODE_SKIPUNKNOWNEXC01
			; 0=FTMODE_NOTRACEIFCSINGDT: falls CS in GDT, nicht weiter tracen
			; 1=FTMODE_CHECKINT: check if CS:EIP->INT xx and set bp if yes
			; 2=FTMODE_NOTRACEFORINT: dont set trace if CS:EIP->INT xx
			; 3=FTMODE_ISDOS4GW: assume debuggee is DOS4/GW
			; 4=FTMODE_SWAPALWAYS
			; 5=FTMODE_CHECKREP
			; 6=FTMODE_SKIPUNKNOWNEXC01
			; 7=FTMODE_STOPIFCTRLLOST
fUnass db FUNAS_DISPLAYAX	
			; 0=FUNAS_NOSHORTDISP: dont display "short"
			; 1=FUNAS_DISPLAYAX: display "(ax=xxxx) at int21/31
			; 7=FUNAS_TRANSLATECS: display owner as segment
fKeybrd db FKB_SYSREQ or FKB_EXECINT03
			; 0=FKB_SYSREQ: handle SYSREQ
			; 1=FKB_EXECINT01: exec int 01 on SYSREQ
			; 2=FKB_EXECINT03: exec int 03 on SYSREQ
			; 3=FKB_ALTSCROLL: use scroll instead of sysreq

fSkipLF		db 0		; strange flag to skip lfs in key translations...
fCPUMode	db 0		; CPU mode bei real mode entry (V86/real)
fIrq		db 1		; 1=Interrupts enabled in debugger
excentry	db 0		; count exception entry
bRC 		db 0		; init status
cLines		db 1		; lines displayed
cInt21RMTraps db 0
xLines		db 0FFh		; (profile written, must be dword) 
			db 3 dup (0)	
if ?WATCHINT23
bCtrlC		db 1		; profile written
			db 3 dup (0)
endif       
if ?HWBREAKS
bHWBrk		db 1		; profile written
			db 3 dup (0)
endif
bpCount		db 0		; number of breakpoints hit
ifcount		db 0		; .IF count
elsecnt		db 0		; .ELSE count
fNotActive	db 0		; flag bei if/else/endif: interpreter deaktiv?
fEcho		db 1		; display active
fTrace		db 0		; echo command line bei LINENOTEMPTY
ife ?WINDOWS
;--- the following variables are used in dpmildr calls before a debuggee is loaded
bLdr		db 1		; 1=dpmildr is enabled
wLdrFlgs	dw 8		; flags to set - see environment variable DPMILDR=xxx
wLdrMask	dw 8		; mask for flags (8=load one app only)
endif
wRMStop   dw 0h			; real-mode "stop trace" segment
wRMBreak  dw 0h			; real-mode breakpoint
wPMStop   dw 0h			; protected-mode "stop trace" selector
wPMBreak  dw 0h			; protected-mode breakpoint 
wDosVersion dw 0		; DOS version returned by int 21h, ah=30h
wWinVersion dw 0		; version returned by int 2Fh, ax=1600h (!=0 for Win3 enh mode or Win9x)

tmpvar	  dq 0			; zwischenspeicher fuer rueckgabewert von "funktionen"
exitproc  dd 0			; adresse der "exit" routine
excexit   dd 0			; exit for internal exceptions
dwExcEbp  dd 0			; saved EBP at internal exceptions
esptype   db 0
espinh	  dd 0
if ?USEHOSTSTACK
hoststack df 0			; stack des hosts (debxxf.exe)
endif
tracesteps dd 0			; number of repeats for TRACE command

;hModule   dd 0			; modulehandle fuer debxxf.dll
if ?WINDOWS
hMyTask		dw 0
hInstDbgee	dw 0		; instance of current debuggee
wRCDbgee	dw 0		; returncode of debuggee
endif
_psp		dd 0		; psp debugger (selector)
_pspseg		dd 0		; psp debugger (segment)
_environ	dd 0		; env debugger (selector)
__csalias	dd 0		; code segment data alias
__flatsel	dd 0		; flat zero-based selector
worksel		dd 0		; scratch selector for various tasks
if ?CS16ALIAS
__cs16alias dd 0		; 16 bit cs alias for _TEXT
endif
__RmCS		dd 0		; rmdbghlp: selector for _TEXT16
__RmDS		dd 0		; rmdbghlp: data alias for _TEXT16
__RmSeg		dw 0		; rmdbghlp: segment _TEXT16
if ?RMCALLBACK
else
pm2rmjump	df 0
endif
if ?WINDOWS
pszDebuggerName	dd 0
;__cs32			dd 0
endif

;--- displayed notifications default:
;---  2=str32 out
;--- 12=str16 out
;--- 50=loadseg16
;--- 52=freeseg16
;--- 59=starttask16
;--- 62=exittask16?
;--- 64=loaddll16
;--- 65=delmodule16
notifylog label dword	; 8*32=256 bits
;			               1               0
;			---C---8---4---0---C---8---4---0
		dd	00000000000001000000000000000100b	;00-1F
;			               3               2
;			---C---8---4---0---C---8---4---0
		dd	00000000000000000000000000000000b	;20-3F
;			               5               4
;			---C---8---4---0---C---8---4---0
		dd	00000010000001010000000000000000b	;40-5F
;			               7               6
;			---C---8---4---0---C---8---4---0
		dd	00000000000000000000000000110100b	;60-7F
		dd 4 dup (0)

notifyactive label dword	; 4*32=128 bits
;			               1               0
;			---C---8---4---0---C---8---4---0
		dd	00000000000000000000000000000000b	;00-1F
;			               3               2
;			---C---8---4---0---C---8---4---0
		dd	00000000000000000000000000000000b	;20-3F
;			               5               4
;			---C---8---4---0---C---8---4---0
		dd	00000000000000000000000000000000b	;40-5F
;			               7               6
;			---C---8---4---0---C---8---4---0
		dd	00000000000000000000000000000000b	;60-7F

ring0switch label fword
			dd 0		; offset wird ignoriert
ring0sel	dd 0		; selector Call Gate in 32Bit Code Segment
ring0proc	dd 0
;fRing0		db 0		; bit 0:1=im ring 0

sdaadr		dd 0		; address of DOS SDA (segment:offset format)
sdalen1		dd 0		; sda laenge falls in dos
sdalen2		dd 0		; sda laenge sonst
dwCurPsp	dd 0		; current psp (selector)
dfCurDta	df 0		; current dta
pSdaSave	dd 0		; SDA save area in flat heap
flatheap	dd 0		; ptr to flat heap (sda save usw.)
flathdl		dd 0		; zugehoeriges dpmi-handle
pHeap		dd 0		; ^ in heap (not near!)
pHeapMax	dd 0		; max heap address 
pHlpFile	dd 0
if ?WINDOWS
hdeb16fwh			dd 0	;hModule of deb16fwh.dll
szListModule		dd CCStr("ListModule")
pListModule32		dd 0
					dd CCStr("ListProcess")
pListProcess32		dd 0
					dd CCStr("ProcessInfo")
pProcessInfo32		dd 0
;					dd CCStr("WriteMemory")
;dwWriteMemory		dd 0
dwWin9xProcsEnd label dword
					dd CCStr("WriteDbgConsole")
pWriteDbgConsole	dd 0
					dd CCStr("PeekDbgConsole")
pPeekDbgConsole		dd 0
					dd CCStr("ReadDbgConsole")
pReadDbgConsole		dd 0
					dd CCStr("CloseDbgConsole")
pCloseDbgConsole	dd 0
					dd 0
endif

if ?LOADVDD
hVDD		dd -1
dwBopSel	dd 0
endif
pMacros		dd 0		; linked list of loaded macros
workselbase dd -1
dwFormStr dd 0			; format string used for ???

if 0
dwLoadedLibs dd 0		; geladene DLLs freigeben
libhandles label dword	; handles der geladenen 16bit DLLs
			dd ?MAXLIBS dup (0)
endif

pKbdBuff  dd offset linebuffer

idtexc	dd 0
idterrc dd 0
dstemp	dd 0

pSaveState	dd 0		; start linked list of savestates
dwLastRC	dd 0
MyBase		dd 0		; lineare adresse speicherbereich
dwTmp		dd 0		; fuer diverses
fEscapemode db 0		; tty output; bit 0: "escape" mode on
cmdline		db 00,0Dh,00,00

params label dword		; fuer int 21,4Bh
		dd 6 dup (0)

fcb label byte
		db 0
		db 11 dup (' ')
		db 20 dup (0)

dwUserSymbols dd 0		; ???

hdlNRes label NRNT
		NRNT NUMNRITEMS dup (<0,0>)

bTraceFlags db FTRAC_LOG; last flags for 'Trace'

unaslines	dd 10h		; anzahl zeilen fuer 'Unassemble'

if ?USETOOLHELP
fUseTH		db 1
			db 3 dup (0)
fToolHelp	db 0		; bit 0: FTH_REGISTERED
endif

pNearHeap	dd startofheap  ;heap im unteren 64k bereich von DS

eTraps		dd ?ETRAPS	; what exceptions (0-1F) are trapped?
eStops		dd ?ESTOPS	; on what exceptions the debugger will stop
eFirst		dd ?EFIRST	; what exceptions are handled first chance?

fCPUID		db 0		; is CPUID supported?
idcpu		db -1
idstep		db -1
idflags 	dd 0
dpmiversion dw 0		; DPMI ax=400h, ax (version)
dpmicpu 	db 0		; DPMI ax=400h, cl (cpu)
dpmiflags	dw 0		; DPMI ax=400h, bx (flags)
dpmipics	dw 0		; DPMI ax=400h, dx (PICs)
dpmiPMentry dd 0		; first entry protected-mode
dpmitaskdat dw 0		; size of task data to allocate
dpmi32bitap dw 0		; flags
if ?32BIT
dpmipm2rm	df 0		; DPMI raw switch protected-mode to real-mode
dpmisrtsPM	df 0
else
dpmipm2rm	dd 0
dpmisrtsPM	dd 0
endif
dpmirm2pm	dd 0		; DPMI raw switch real-mode to protected-mode
dpmisrtsRM	dd 0		; DPMI save/restore task state (real-mode)
dpmipagesiz dd 0		; DPMI ax=604h, bx:cx (page size)
if ?LDTSEL
ldtsel		dw 0
endif

;------------3----2----2----1----1----0----0
;------------0----5----0----5----0----5----0
MSRPIV	equ 00001000100000010000000000000011b	;pentium IV
MSRPI	equ 00001000100011110111101111110111b	;pentium
;msrflags dd MSRPI


rmtraps   dd 00000000000000000000000001001011b	;0,1,3 und 6 in RM

ife ?WINDOWS
pmtraps   dd 00000000000000000000000000000010b	;trap int 1 in PM
endif

oldrmvecs label dword
oldi00r  dd 0
oldi01r  dd 0
oldi02r  dd 0
oldi03r  dd 0
oldi04r  dd 0
oldi05r  dd 0
oldi06r  dd 0
oldi07r  dd 0

ife ?WINDOWS
if ?32BIT
oldpmvecs label fword
oldi00p  PF32 0
oldi01p  PF32 0
oldi02p  PF32 0
oldi03p  PF32 0
oldi04p  PF32 0
oldi05p  PF32 0
oldi06p  PF32 0
oldi07p  PF32 0
oldi08p  PF32 0
oldi09p  PF32 0
oldi0Ap  PF32 0
oldi0Bp  PF32 0
oldi0Cp  PF32 0
oldi0Dp  PF32 0
oldi0Ep  PF32 0
oldi0Fp  PF32 0
SIZEOLDPMVECS equ ($ - offset oldpmvecs) / sizeof fword
else
oldpmvecs label dword
oldi00p  PF16 0
oldi01p  PF16 0
oldi02p  PF16 0
oldi03p  PF16 0
oldi04p  PF16 0
oldi05p  PF16 0
oldi06p  PF16 0
oldi07p  PF16 0
oldi08p  PF16 0
oldi09p  PF16 0
oldi0Ap  PF16 0
oldi0Bp  PF16 0
oldi0Cp  PF16 0
oldi0Dp  PF16 0
oldi0Ep  PF16 0
oldi0Fp  PF16 0
SIZEOLDPMVECS equ ($ - offset oldpmvecs) / sizeof dword
endif
endif

if ?HIDEINT21
 if ?32BIT
oldi21p  PF32 0
 else
oldi21p  PF16 0
 endif
endif

oldidtvecs label qword
oldidtvec0 dq 0
oldidtvec1 dq 0
oldidtvec2 dq 0
oldidtvec3 dq 0
oldidtvec4 dq 0
oldidtvec5 dq 0
oldidtvec6 dq 0
oldidtvec7 dq 0
if _IDTVECS_ gt 8
oldidtvec8 dq 0
oldidtvec9 dq 0
oldidtvecA dq 0
oldidtvecB dq 0
oldidtvecC dq 0
oldidtvecD dq 0
oldidtvecE dq 0
oldidtvecF dq 0
endif

myidtvecs label dword
	dd offset int00idtentry
	dd offset int01idtentry
	dd offset int02idtentry
	dd offset int03idtentry
	dd offset int04idtentry
	dd offset int05idtentry
	dd offset int06idtentry
	dd offset int07idtentry
if _IDTVECS_ gt 8
	dd offset int08idtentry
	dd offset int09idtentry
	dd offset int0Aidtentry
	dd offset int0Bidtentry
	dd offset int0Cidtentry
	dd offset int0Didtentry
	dd offset int0Eidtentry
	dd offset int0Fidtentry
endif

;--- size of mypmvecs must match size of oldpmvecs

ife ?WINDOWS
mypmvecs label dword
if ?CS16ALIAS
	dd offset divbyzero_16	;00
	dd offset singlstep_16	;01
	dd offset intr02_16		;02
	dd offset breakpnt_16	;03
	dd offset xxxexcx_16	;04
	dd offset xxxexcx_16	;05
	dd offset invalidopcode_16 ;06
	dd offset xxxexcx_16	;07
	dd offset xxxirqx_16	;08
	dd offset xxxirqx_16	;09
	dd offset xxxirqx_16	;0A
	dd offset xxxirqx_16	;0B
	dd offset xxxirqx_16	;0C
	dd offset xxxirqx_16	;0D
	dd offset xxxirqx_16	;0E
	dd offset xxxirqx_16	;0F
else
	dd offset divbyzero		;00
	dd offset singlstep		;01
	dd offset intr02		;02
	dd offset breakpnt		;03
	dd offset xxxexcx		;04
	dd offset xxxexcx		;05
	dd offset invalidopcode ;06
	dd offset xxxexcx		;07
	dd offset xxxirqx		;08
	dd offset xxxirqx		;09
	dd offset xxxirqx		;0A
	dd offset xxxirqx		;0B
	dd offset xxxirqx		;0C
	dd offset xxxirqx		;0D
	dd offset xxxirqx		;0E
	dd offset xxxirqx		;0F
endif
endif

if ?WINDOWS
if ?WATCHINT24
oldint24 dd 0			; old int vector 24h (windows only)
endif
if ?USETOOLHELP
;---------------3----5----2----5----1----5----0
toolhlpmask dd 00000000000000000111000001001011b
toolhlpvecs dd -1
endif
endif

if ?32BIT
oldint41 PF32 0
oldint31 PF32 0
oldint21 PF32 0
  if ?SAVEINT08
oldint08 PF32 0
oldint1C PF32 0
  endif
  if ?WATCHI2F
oldint2F PF32 0
  endif
_FatalExit PF32 0
tempjump   PF32 0
else
oldint41 PF16 0
oldint31 PF16 0
oldint21 PF16 0
  if ?SAVEINT08
oldint08 PF16 0
oldint1C PF16 0
  endif
  if ?WATCHI2F
oldint2F PF16 0
  endif
_FatalExit PF16 0
tempjump   PF16 0
endif

if ?SETEXCVECS
if ?32BIT
saveCurExc0D	PF32 0
saveCurExc0E	PF32 0
  if ?SAVEINT08
saveCurInt08	PF32 0
saveCurInt1C	PF32 0
  endif
else
saveCurExc0D	PF16 0
saveCurExc0E	PF16 0
  if ?SAVEINT08
saveCurInt08	PF16 0
saveCurInt1C	PF16 0
  endif
endif
endif


pmintmou df 0
cInstalled_i41 db 0
cVisible_i41 db 1
dwIndos		dd 0		;Adresse INDOS flag (was logical, now linear)
dwLoL		dd 0		;lineare Adresse List of Lists (DOS)
loadopt		dw 0		;option beim programmladen (immer 0)
cLoad		db 0		;currently loaded apps
wPicOff		dw 0		;bit mask for IRQ 00-0F
wPicOn		dw 0		;bit mask for IRQ 00-0F
wPicValue	dw 0		;PIC mask values for debuggee
dwFlagsStr dd flagsstr
oldexcvecs label byte	; debuggee exception handler if debugger handles them first chance
		dd 2*20h dup (0)

previhandler dq 0h

		dd offset linebuffer	; this is to implement a chain of line buffers - see keybrd.asm, saveline()
linebuffer db ?KBDBUFSIZ dup (?) ; line buffer

tregs MYREGSTR <>

;--- erweiterte register

rTR		dw 0
rLDT	dw 0
rIDTR	df 0
rGDTR	df 0
rCR0	dd 0
rCR2	dd 0
rCR3	dd 0
rCR4	dd 0
rDR0	dd 0
rDR1	dd 0
rDR2	dd 0
rDR3	dd 0
rDR4	dd 0
rDR5	dd 0
rDR6	dd 0
rDR7	dd 0
rTR3	dd 0
rTR4	dd 0
rTR5	dd 0
rTR6	dd 0
rTR7	dd 0

;--- if we are notified by DPMI of exceptions, we first have to switch
;--- to our stack before we can enter the debugger. For this save
;--- CS:EIP, EFL and SS:ESP in exception handler and set 
;--- FEXC_USEXREGS

rCSx	dd 0
rEipx	dd 0
rEflx	dd 0

if ?SWITCHSTACK
rSSx	dd 0
rEspx	dd 0
endif

anzarg db ?		; anzahl benoetigte parameter

;*** im folgenden ist der aktuelle clientzustand gespeichert
;*** die "normalen" register, die real-mode register und
;*** die floating point register sowie lokale zustandsflags

	align 4

clientarea label byte

fEntry db 0		; entry flags (init beim exit):
				; B0=debugger exception (single step, hw watches)
				; B1=exception
				; B2=breakpoint
				; B3=entry aus real mode
				; B4=register auf heap ausgeben
				; B5=entry IDT
				; B6=Entry int3 ueber autobreak
				; B7=ctrl-alt-sysreq
f80x87 db 0		; bit 0: ist coprozessor da?
				; bit 1: wurde zustand gesichert?
				; bit 7: copro ignorieren
fEMU db 1		; Flag: soll EM-Bit (in CR0) gesichert werden?
fLoadMode db 0	; Lademodus:
				; 0=nichts
				; 1=App entry
				; 2=Dll entry
				; 4=Dll exit
				; 8=segment load
;fIrq db 1		; Interrupts enablen im Debugger
pSymtab dd 0	; zeiger auf registersatz
embits db 0		; client coprocessor bits (saved with int 31h, ax=0E00)

dumparg   dq 0	; letzte mit 'Display' ausgegebene adresse
fDump     db 0	; dump flags
				; B0=1 -> real mode selektor
				; B1=display words
				; B2=display dwords
unasarg dq 0	; letzte mit 'Unassemble' ausgegebene adresse
fUnastype db 0	; flags. B0=1 -> real mode

	align 4

r1 MYREGSTR <>	; current register protected-mode
r1r RMSPREGS <> ; current segment + ctrl registers real-mode

	align 4

if ?FLOATREGS

;--- current float registers (FNSAVE/FRSTOR)

frsave label byte
fcw 	dd ?	;+0
fsw 	dd ?	;+4
ftw 	dd ?	;+8
fip 	dd ?	;+12
fcs 	dw ?	;+16
fopc	dw ?
foo 	dd ?	;+20
fos 	dd ?	;+24
fst0	dt ?	;+28
fst1	dt ?	;+38
fst2	dt ?	;+48
fst3	dt ?	;+58
fst4	dt ?	;+68
fst5	dt ?	;+78
fst6	dt ?	;+88
fst7	dt ?	;+98
endif

myesp	dd offset top_of_stack	 ;stackpointer init wert
lclientarea equ $ -clientarea

;--------------------------------------------------------
;--- uninitialisierte variable

	.data?

rmcsi0y RMCS <>	;db 32h dup (?)	  ;fuer real mode callback

a0	ARGDESC <?,?,?,?,?>
a1	ARGDESC <?,?,?,?,?>
a2	ARGDESC <?,?,?,?,?>
a3	ARGDESC <?,?,?,?,?>
a4	ARGDESC <?,?,?,?,?>
a5	ARGDESC <?,?,?,?,?>
a6	ARGDESC <?,?,?,?,?>
	ARGDESC <?,?,?,?,?>
	ARGDESC <?,?,?,?,?>
	ARGDESC <?,?,?,?,?>
	ARGDESC <?,?,?,?,?>
	ARGDESC <?,?,?,?,?>
	ARGDESC <?,?,?,?,?>
	ARGDESC <?,?,?,?,?>
	ARGDESC <?,?,?,?,?>
	ARGDESC <?,?,?,?,?>
argend label byte

	.const

deffrmstr db cr,"eax=%X,ebx=%X,ecx=%X,edx=%X,esi=%X,edi=%X,ebp=%X",lf
          db "efl=%X,cs:eip=%X:%X,ss:esp=%X:%X,ds=%X,es=%X,fs=%X,gs=%X",lf
szNull    db 00

;*** start symbol tables

;*** SYMBOL.pType:
;***   bit 0-3 -> type
;***   bit 4: wert des symbols durch funktionsaufruf ermitteln/setzen
;***   bit 5: readonly
;***   bit 7: function
;*** SYMXTYP:
;***   if function:
;***    bit 0-2: anzahl argumente benoetigt
;***    bit 7  : typ von argument 1 kann string sein

_FUNCTN_ equ 80h

functab label byte
if ?SYMTRACE
	@symbol '.STRAC',	 _FUNCTN_,0,_symtrace
endif
	@symbol '.B',		 _FUNCTN_+2,0,_baudrate
	@symbol '.CD',		 _FUNCTN_+1,0+_PARMOK_,_changedir
;	 @symbol '.CDR',	  _FUNCTN_+1,1+_PARMOK_,_changedrive
if ?SUPPCDS
	@symbol '.CDS', 	 _FUNCTN_,0,_getcds
endif
	@symbol '.CLRDOS',	 _FUNCTN_,0,_cleardos
if ?WINDOWS or ?32BIT
	@symbol '.DA32',	 _FUNCTN_,2,_getproc32
endif
	@symbol '.DIR', 	 _FUNCTN_+1,0+_PARMOK_,_getfiles
if ?USETOOLHELP
	@symbol '.DG',		 _FUNCTN_,0,_globalheap
	@symbol '.TH',		 _FUNCTN_,1,_settoolhelp
endif
if ?SUPPLHEAP
	@symbol '.DL',		 _FUNCTN_,0,_localheap
endif
	@symbol '.DM',		 _FUNCTN_,0,_moduleout
if ?WINDOWS or ?32BIT
	@symbol '.DM32',	 _FUNCTN_,0,_moduleout32
endif
	@symbol '.DN',		 _FUNCTN_,1,_namesout
	@symbol '.DO',		 _FUNCTN_,0,_getowner
	@symbol '.DOS', 	 _FUNCTN_,0,_getdosparms
if 0;e ?WINDOWS
	@symbol '.DPMI',	 _FUNCTN_,0,_dpmi
endif
if ?WINDOWS
	@symbol '.DQ',		 _FUNCTN_,0,_tasklist
	@symbol '.DP32',	 _FUNCTN_,0,_processinfo32
	@symbol '.DQ32',	 _FUNCTN_,0,_processout32
endif
	@symbol '.DS',		 _FUNCTN_,1,_symbolout
	@symbol '.ENV', 	 _FUNCTN_,0,_getenvironment
	@symbol '.FEXIT',	_FUNCTN_,0,_fexit
	@symbol '.FREE',	_FUNCTN_,1,_freelib
if ?USESYMBOLS
	@symbol '.FS',		_FUNCTN_,1,_freenres
endif
if ?WINDOWS
	@symbol '.GDI', 	_FUNCTN_,0,_gdi
endif
	@symbol '.I41',		_FUNCTN_,1,_i41
	@symbol '.KBD',		_FUNCTN_,0,_kbdstate
	@symbol '.Kill',	_FUNCTN_,1,_killpsp
ife ?WINDOWS
	@symbol '.LDR',		_FUNCTN_,0,_loader
endif
if ?WINDOWS or ?32BIT
	@symbol '.LOAD32',	_FUNCTN_+1,1+_PARMOK_,_loaddll32
endif
if ?USESYMBOLS
	@symbol '.LS',		_FUNCTN_,0,_loadnres
endif
	@symbol '.MCB', 	_FUNCTN_,0,_getmcb
	@symbol '.MCBFree',	_FUNCTN_,1,_freemcb
	@symbol '.PIC', 	_FUNCTN_,0,_getpic
	@symbol '.PSP', 	_FUNCTN_,0,_getpsps
if ?WINDOWS
	@symbol '.Quit',	_FUNCTN_,0,_quitdeb
endif
	@symbol '.REBOOT',	_FUNCTN_,0,_reboot
ife ?WINDOWS
	@symbol '.RES', 	 _FUNCTN_,0,_resident
endif
	@symbol '.SEG', 	 _FUNCTN_,1,_segsmod
if ?SUPPSFT
	@symbol '.SFT', 	 _FUNCTN_,0,_getsft
endif
ife ?WINDOWS
	@symbol '.SHell',	 _FUNCTN_,0+_PARMOK_,_dosshell
endif
if ?USESYMBOLS
	@symbol '.SYM', 	 _FUNCTN_,0,_setsymhandler
endif
;;	@symbol '.T', 	   _FUNCTN_,0,_testproc
if ?SUPPVCD
	@symbol '.VCD', 	 _FUNCTN_,0,_vcd
endif
ife ?WINDOWS
	@symbol '.VCPI',	 _FUNCTN_,0,_vcpi
endif
	@symbol '.VIDeo',	 _FUNCTN_,0,_viostate
if ?SUPPVPICD
	@symbol '.VPICD',	 _FUNCTN_,0,_vpicd
endif
	@symbol '.XMS',		 _FUNCTN_,0,_xms
	@symbol '?',		 _FUNCTN_+1,0+_PARMOK_,_help
	@symbol '??',		 _FUNCTN_,0+_PARMOK_,_display,,symDisp
	@symbol 'ALLOCDOS',  _FUNCTN_,1,_allocdos
	@symbol 'ALLOCMem',  _FUNCTN_,1,_allocmem
	@symbol 'ALLOCMX',	 _FUNCTN_,2,_allocmemx
	@symbol 'ALLOCSel',  _FUNCTN_,0,_allocsel
	@symbol 'ALLOCSS',   _FUNCTN_,1,_allocssel
	@symbol 'BClear',	 _FUNCTN_,0,_clearbreakpnt
	@symbol 'BDisable',	 _FUNCTN_,0,_disablebreakpnt
	@symbol 'BEnable',	 _FUNCTN_,0,_enablebreakpnt
	@symbol 'BPoint',	 _FUNCTN_,0,_setbreakpnt
	@symbol 'BPReadwrite',_FUNCTN_,0,_sethwbreak2
	@symbol 'BPWrite',	 _FUNCTN_,0,_sethwbreak1
	@symbol 'BPX',		 _FUNCTN_,0,_sethwbreak0
;	@symbol 'BReak',	 _FUNCTN_,1,_setdebugreg
if ?SUPPBRL
	@symbol 'BRList',	 _FUNCTN_,0,_listhwbreaks
endif        
	@symbol 'CANcel',	 _FUNCTN_,0,_cancel
	@symbol 'CLS',		 _FUNCTN_,0,_cls
	@symbol 'CPStat',	 _FUNCTN_,0,_getfpustatus
	@symbol 'CPU',		 _FUNCTN_,0,_xregsout
	@symbol 'DA',		 _FUNCTN_,0,_dumpdt
	@symbol 'DB',		 _FUNCTN_,0,_dumpdb
	@symbol 'DD',		 _FUNCTN_,0,_dumpdd
	@symbol 'DG',		 _FUNCTN_,0,_gdtout
	@symbol 'DISCARD',	 _FUNCTN_,2,_discardpages
	@symbol 'DI',		 _FUNCTN_,0,_idtout
	@symbol 'DL',		 _FUNCTN_,0,_ldtout
	@symbol 'DP',		 _FUNCTN_,0,_pagetab
	@symbol 'DPD',		 _FUNCTN_,0,_pagedir
	@symbol 'DPMI', 	 _FUNCTN_,0,_dpmiinfo
	@symbol 'DT',		 _FUNCTN_,0,_tssout
	@symbol 'DW',		 _FUNCTN_,0,_dumpdw
	@symbol 'DYN',		 _FUNCTN_,0,_ushow
	@symbol 'Dump', 	 _FUNCTN_,0,_dump
	@symbol 'EB',		 _FUNCTN_,2,_editb
	@symbol 'ED',		 _FUNCTN_,2,_editd
	@symbol 'EW',		 _FUNCTN_,2,_editw
	@symbol 'Edit', 	 _FUNCTN_,2,_edit
	@symbol 'ELSE', 	 _FUNCTN_,0,_else
	@symbol 'ENDIF',	 _FUNCTN_,0,_endif
	@symbol 'EPM',		 _FUNCTN_,0,_excpm
	@symbol 'FCall',	 _FUNCTN_,1,_farcall
	@symbol 'FCRM', 	 _FUNCTN_,1,_farcallrm
	@symbol 'FREEDOS',	 _FUNCTN_,1,_freedos
	@symbol 'FREESel',	 _FUNCTN_,1,_freesel
	@symbol 'FREEMem',	 _FUNCTN_,1,_freemem
	@symbol 'Fill', 	 _FUNCTN_,3,_fill
	@symbol 'GDT',		 _FUNCTN_,0,_gdtout
	@symbol 'Go',		 _FUNCTN_,0,_go
	@symbol 'GPA',		 _FUNCTN_,3,_getpageattr
	@symbol 'Help', 	 _FUNCTN_+1,0+_PARMOK_,_help
	@symbol 'IDT',		 _FUNCTN_,0,_idtout
	@symbol 'IF',		 _FUNCTN_,1,_if
	@symbol 'Input',	 _FUNCTN_,1,_input
	@symbol 'ID',		 _FUNCTN_,1,_inpd
	@symbol 'INTcall',	 _FUNCTN_,1,_intcall
	@symbol 'INTErpret', _FUNCTN_,1+_PARMOK_,_interpret
	@symbol 'INTRM',	 _FUNCTN_,1,_intcallrm
	@symbol 'IOP',		 _FUNCTN_,1,_iop
	@symbol 'IORestrict',_FUNCTN_,0,_iorestr
	@symbol 'IPM',		 _FUNCTN_,0,_intpm
	@symbol 'IPMS', 	 _FUNCTN_,2,_setintpm
	@symbol 'IRB',		 _FUNCTN_,0,_setrmbreak
	@symbol 'IRM',		 _FUNCTN_,0,_intrm
	@symbol 'IRMS', 	 _FUNCTN_,2,_setintrm
	@symbol 'IW',		 _FUNCTN_,1,_inpw
	@symbol 'Jump', 	 _FUNCTN_,0,_jump
	@symbol 'K',		 _FUNCTN_,0,_stacktrace
	@symbol 'LDT',		 _FUNCTN_,0,_ldtout
	@symbol 'Load', 	 _FUNCTN_+1,1+_PARMOK_,_loadpgm
	@symbol 'LOCKRM',	 _FUNCTN_,2,_lockmemrm
	@symbol 'LOCKD',	 _FUNCTN_,1,_lockdrive
	@symbol 'LOCk', 	 _FUNCTN_,2,_lockmem
	@symbol 'MACro',	 _FUNCTN_+1,0+_PARMOK_,_listmacros
	@symbol 'MARKPage',  _FUNCTN_,2,_markpage
	@symbol 'MEMinfo',	 _FUNCTN_,0,_meminfo
	@symbol 'Move', 	 _FUNCTN_,3,_move
	@symbol 'MSR',		 _FUNCTN_,0,_getmsr
	@symbol 'Output',	 _FUNCTN_,2,_outp
	@symbol 'OD',		 _FUNCTN_,2,_outpd
	@symbol 'OW',		 _FUNCTN_,2,_outpw
	@symbol 'PDir', 	 _FUNCTN_,0,_pagedir
	@symbol 'Perform',	 _FUNCTN_,0,_perform
	@symbol 'PFlags',	 _FUNCTN_,1,_getptentry
	@symbol 'PMAP', 	 _FUNCTN_,1,_physmap
;	@symbol 'PRintf',	 _FUNCTN_+1,1+_PARMOK_,_myprintf
	@symbol 'PRintf',	 _FUNCTN_,1+_PARMOK_,_myprintf
	@symbol 'PTab', 	 _FUNCTN_,0,_pagetab
	@symbol 'PUNMAP',	 _FUNCTN_,1,_physunmap
	@symbol 'Quit', 	 _FUNCTN_,0,_quit
	@symbol 'REALLOCDOS',_FUNCTN_,2,_reallocdos
	@symbol 'REALLOCMem',_FUNCTN_,2,_reallocmem
	@symbol 'REALLOCMX', _FUNCTN_,3,_reallocmemx
	@symbol 'REAd', 	 _FUNCTN_+1,1+_PARMOK_,_readfile
if ?FLOATREGS
	@symbol 'RN',		 _FUNCTN_,0,_fpregsout
endif
if ?MMXREGS
	@symbol 'RM',		 _FUNCTN_,0,_mmxregsout
endif
	@symbol 'RP',		 _FUNCTN_,0,_regsoutp
	@symbol 'RR',		 _FUNCTN_,0,_regsoutr
	@symbol 'Register',  _FUNCTN_,0,_regsout
	@symbol 'R1',		 _FUNCTN_,0,_myregsout
	@symbol 'RS',		 _FUNCTN_,3,_readsecs
	@symbol 'RT',		 _FUNCTN_,0,_tregsout
if ?XMMREGS
	@symbol 'RX',		 _FUNCTN_,0,_xmmregsout
endif
	@symbol 'Search',	 _FUNCTN_,3,_search
	@symbol 'S2Desc',	 _FUNCTN_,1,_rmsel
	@symbol 'SB',		 _FUNCTN_,3,_searchb
	@symbol 'SW',		 _FUNCTN_,3,_searchw
	@symbol 'SD',		 _FUNCTN_,3,_searchd
	@symbol 'SHow', 	 _FUNCTN_,0,_show
	@symbol 'SKip', 	 _FUNCTN_,0,_skip
	@symbol 'SPA',		 _FUNCTN_,4,_setpageattr
if 1
	@symbol 'SRX',		 _FUNCTN_,0,_testring0
endif
	@symbol 'TYpe', 	 _FUNCTN_,0+_PARMOK_,_type
	@symbol 'TRAPs',	 _FUNCTN_,0,_trapsout
	@symbol 'Trace',	 _FUNCTN_,0,_traceproc
	@symbol 'TSS',		 _FUNCTN_,0,_tssout
	@symbol 'UNCOMM',	 _FUNCTN_,1,_uncommited
	@symbol 'UNLOCKRM',  _FUNCTN_,2,_unlockmemrm
	@symbol 'UNLOCKD',	 _FUNCTN_,1,_unlockdrive
	@symbol 'UNLock',	 _FUNCTN_,2,_unlockmem
	@symbol 'Unassemble',_FUNCTN_,0,_unass
	@symbol 'View', 	 _FUNCTN_,0,_view
	@symbol 'Write',	 _FUNCTN_+1,3+_PARMOK_,_writefile
	@symbol 'WS',		 _FUNCTN_,4,_writesecs
	@symbol 'Z',		 _FUNCTN_,0,_breakremove
	db 0h


rregtab label byte
	@symbol 'EIP',	   __WORD__,2,r1r.rIP,,symEIPr
	@symbol 'ESP',	   __WORD__,1,r1r.rSP,,symESPr
	@symbol 'EFL',	   __WORD__, ,r1r.rFlags,,symEFLr
	@symbol 'IP',	   __WORD__,2,r1r.rIP,,symIPr
	@symbol 'SP',	   __WORD__,1,r1r.rSP
	@symbol 'FL',	   __WORD__, ,r1r.rFlags
        
	@symbol 'CS',	   __WORD__, ,r1r.rCS,,symCSr
	@symbol 'SS',	   __WORD__, ,r1r.rSS,,symSSr
	@symbol 'DS',	   __WORD__, ,r1r.rDS,,symDSr
	@symbol 'ES',	   __WORD__, ,r1r.rES,,symESr
	@symbol 'FS',	   __WORD__, ,r1r.rFS,,symFSr
	@symbol 'GS',	   __WORD__, ,r1r.rGS,,symGSr

	@symchain symEAX


;--- 1 as 3. parameter: SS related
;--- 2 as 3. parameter: CS related

symtab label byte
	@symbol 'EIP',	   __DWORD__,2,r1.rEip,,symEIPp
	@symbol 'ESP',	   __DWORD__,1,r1.rEsp,,symESPp
	@symbol 'EFL',	   __DWORD__, ,r1.rEfl
	@symbol 'IP',	   __WORD__,2,r1.rEip
	@symbol 'SP',	   __WORD__,1,r1.rEsp
	@symbol 'FL',	   __WORD__, ,r1.rEfl

	@symbol 'CS',	   __WORD__, ,r1.rCS,,symCSp
	@symbol 'SS',	   __WORD__, ,r1.rSS,,symSSp
	@symbol 'DS',	   __WORD__, ,r1.rDS,,symDSp
	@symbol 'ES',	   __WORD__, ,r1.rES,,symESp
	@symbol 'FS',	   __WORD__, ,r1.rFS,,symFSp
	@symbol 'GS',	   __WORD__, ,r1.rGS,,symGSp

	@symbol 'EAX',	   __DWORD__, ,r1.rEax,,symEAX
	@symbol 'EBX',	   __DWORD__, ,r1.rEbx,,symEBX
	@symbol 'ECX',	   __DWORD__, ,r1.rEcx,,symECX
	@symbol 'EDX',	   __DWORD__, ,r1.rEdx,,symEDX
	@symbol 'ESI',	   __DWORD__, ,r1.rEsi,,symESI
	@symbol 'EDI',	   __DWORD__, ,r1.rEdi,,symEDI
	@symbol 'EBP',	   __DWORD__,1,r1.rEbp,,symEBP
	@symbol 'AX',	   __WORD__, ,r1.rEax
	@symbol 'BX',	   __WORD__, ,r1.rEbx
	@symbol 'CX',	   __WORD__, ,r1.rEcx
	@symbol 'DX',	   __WORD__, ,r1.rEdx
	@symbol 'BP',	   __WORD__,1,r1.rEbp
	@symbol 'SI',	   __WORD__, ,r1.rEsi
	@symbol 'DI',	   __WORD__, ,r1.rEdi
	@symbol 'AL',	   __BYTE__, ,r1.rEax
	@symbol 'BL',	   __BYTE__, ,r1.rEbx
	@symbol 'CL',	   __BYTE__, ,r1.rEcx
	@symbol 'DL',	   __BYTE__, ,r1.rEdx
	@symbol 'AH',	   __BYTE__, ,r1.rEax+1
	@symbol 'BH',	   __BYTE__, ,r1.rEbx+1
	@symbol 'CH',	   __BYTE__, ,r1.rEcx+1
	@symbol 'DH',	   __BYTE__, ,r1.rEdx+1

	@symbol 'TR',	   _RDONLY_+__WORD__, ,rTR,,symTR
	@symbol 'LDTR',    _RDONLY_+__WORD__, ,rLDT,,symLDTR
	@symbol 'GDTR',    _RDONLY_+__DWORD__, ,rGDTR+2,,symGDTR
	@symbol 'GDTLIM',  _RDONLY_+__WORD__, ,rGDTR,,symGDTLIM
	@symbol 'IDTR',    _RDONLY_+__DWORD__, ,rIDTR+2,,symIDTR
	@symbol 'IDTLIM',  _RDONLY_+__WORD__, ,rIDTR,,symIDTLIM
	@symbol 'MSW',     _FNCALL_+_RDONLY_+__WORD__, ,getmsw,,symMSW
        
	@symbol 'CR0',	   _FNCALL_+__DWORD__, ,getcr0,,symCR0
	@symbol 'CR2',	   _FNCALL_+__DWORD__, ,getcr2,,symCR2
;	@symbol 'CR3',	   _FNCALL_+_RDONLY_+__DWORD__, ,getcr3,,symCR3
	@symbol 'CR3',	   _FNCALL_+__DWORD__, ,getcr3,,symCR3
	@symbol 'CR4',	   _FNCALL_+__DWORD__, ,getcr4,,symCR4
	@symbol 'DR0',	   _RDONLY_+__DWORD__, ,rDR0,,symDR0
	@symbol 'DR1',	   _RDONLY_+__DWORD__, ,rDR1,,symDR1
	@symbol 'DR2',	   _RDONLY_+__DWORD__, ,rDR2,,symDR2
	@symbol 'DR3',	   _RDONLY_+__DWORD__, ,rDR3,,symDR3
if ?DR4DR5        
	@symbol 'DR4',	   _RDONLY_+__DWORD__, ,rDR4,,symDR4
	@symbol 'DR5',	   _RDONLY_+__DWORD__, ,rDR5,,symDR5
endif        
	@symbol 'DR6',	   _RDONLY_+__DWORD__, ,rDR6,,symDR6
	@symbol 'DR7',	   _RDONLY_+__DWORD__, ,rDR7,,symDR7
if ?TR3TR5        
	@symbol 'TR3',	   _RDONLY_+__DWORD__, ,rTR3,,symTR3
	@symbol 'TR4',	   _RDONLY_+__DWORD__, ,rTR4,,symTR4
	@symbol 'TR5',	   _RDONLY_+__DWORD__, ,rTR5,,symTR5
endif   
	@symbol 'TR6',	   _RDONLY_+__DWORD__, ,rTR6,,symTR6
	@symbol 'TR7',	   _RDONLY_+__DWORD__, ,rTR7,,symTR7

if ?FLOATREGS
floatregs label byte
	@symbol 'FCW',	   __WORD__, ,fcw,,symFCW
	@symbol 'FSW',	   __WORD__, ,fsw,,symFSW
	@symbol 'FTW',	   __WORD__, ,ftw,,symFTW
	@symbol 'FIP',	   __DWORD__, ,fip,,symFIP
	@symbol 'FCS',	   __WORD__, ,fcs,,symFCS
	@symbol 'FOPC',    __WORD__, ,fopc,,symFOPC
	@symbol 'FOO',	   __DWORD__, ,foo,,symFOO
	@symbol 'FOS',	   __WORD__, ,fos,,symFOS
	@symbol 'ST0',	  __TBYTE__, ,fst0,,symST0
	@symbol 'ST1',	  __TBYTE__, ,fst1,,symST1
	@symbol 'ST2',	  __TBYTE__, ,fst2,,symST2
	@symbol 'ST3',	  __TBYTE__, ,fst3,,symST3
	@symbol 'ST4',	  __TBYTE__, ,fst4,,symST4
	@symbol 'ST5',	  __TBYTE__, ,fst5,,symST5
	@symbol 'ST6',	  __TBYTE__, ,fst6,,symST6
	@symbol 'ST7',	  __TBYTE__, ,fst7,,symST7
endif
if ?MMXREGS
	@symbol 'MM0',	  __QWORD__, ,fst0,,symMM0
	@symbol 'MM1',	  __QWORD__, ,fst1,,symMM1
	@symbol 'MM2',	  __QWORD__, ,fst2,,symMM2
	@symbol 'MM3',	  __QWORD__, ,fst3,,symMM3
	@symbol 'MM4',	  __QWORD__, ,fst4,,symMM4
	@symbol 'MM5',	  __QWORD__, ,fst5,,symMM5
	@symbol 'MM6',	  __QWORD__, ,fst6,,symMM6
	@symbol 'MM7',	  __QWORD__, ,fst7,,symMM7
endif   

dosvartab  label byte
	@symbol 'DOS_Version'	 ,_FNCALL_+_RDONLY_+__WORD__, ,getdosversion,0
	@symbol 'PSP'			 ,_FNCALL_+_RDONLY_+__WORD__, ,getpsp,0
	@symbol 'PSPNAME'		 ,_FNCALL_+_RDONLY_+__STRING__, ,getpspname,0
if ?32BIT
	@symbol 'DTA'			 ,_FNCALL_+_RDONLY_+__FPTR__, ,getdta,0
else
	@symbol 'DTA'			 ,_FNCALL_+_RDONLY_+__LPTR__, ,getdta,0
endif
	@symbol 'CUR_DRive' 	 ,_FNCALL_+_RDONLY_+__CHAR__, ,getcurdrive,0
	@symbol 'CUR_DIR'		 ,_FNCALL_+_RDONLY_+__STRING__, ,getcurdir,0
	@symbol 'ADDRINDOSFlag'  ,		   _RDONLY_+__DWORD__, ,dwIndos
	@symbol 'INDOSFlag' 	 ,_FNCALL_+_RDONLY_+__BYTE__, ,getindosflag,0
	@symbol 'SDA'			 ,		   _RDONLY_+__RMLPTR__, ,sdaadr
	@symbol 'SDAMAX'		 ,		   _RDONLY_+__WORD__, ,sdalen1
	@symbol 'SDAMIN'		 ,		   _RDONLY_+__WORD__, ,sdalen2
dosvartabend label byte

dpmivartab label byte
	@symbol 'DPMIVERsion'	 ,_RDONLY_+__WORD__, ,dpmiversion
	@symbol 'DPMICPU'		 ,_RDONLY_+__BYTE__, ,dpmicpu
	@symbol 'DPMIFLags' 	 ,_RDONLY_+__WORD__, ,dpmiflags
	@symbol 'MASTERPIC' 	 ,_RDONLY_+__BYTE__, ,dpmipics+1
	@symbol 'SLAVEPIC'		 ,_RDONLY_+__BYTE__, ,dpmipics+0
	@symbol 'PMENTRY'		 ,_RDONLY_+__RMLPTR__, ,dpmiPMentry
	@symbol 'TASKDATA'		 ,_RDONLY_+__WORD__, ,dpmitaskdat
	@symbol '32BITAPP'		 ,_RDONLY_+__WORD__, ,dpmi32bitap
if ?32BIT
	@symbol 'PM2RMRAW'		 ,_RDONLY_+__FPTR__, ,dpmipm2rm
else
	@symbol 'PM2RMRAW'		 ,_RDONLY_+__LPTR__, ,dpmipm2rm
endif
	@symbol 'RM2PMRAW'		 ,_RDONLY_+__RMLPTR__, ,dpmirm2pm
	@symbol 'PAGESize'		 ,_RDONLY_+__DWORD__, ,dpmipagesiz
if ?LDTSEL
	@symbol 'LDTSelector'	 ,_RDONLY_+__WORD__, ,ldtsel
endif
dpmivartabend label byte

	@symbol 'SEG'		   ,_FNCALL_+_RDONLY_+__WORD__,1,getseg
	@symbol 'OFF'		   ,_FNCALL_+_RDONLY_+__DWORD__,1,getoff
	@symbol 'NOT'		   ,_FNCALL_+_RDONLY_+__DWORD__,1,getcompl
	@symbol 'LPTR'		   ,_FNCALL_+_RDONLY_+__LPTR__,1,getlptr
	@symbol 'RMFPTR'	   ,_FNCALL_+_RDONLY_+__RMLPTR__,1,getlptr
	@symbol 'BASE'		   ,_FNCALL_+_RDONLY_+__DWORD__,1,getbase
	@symbol 'SETBASE'	   ,_FNCALL_+_RDONLY_+__DWORD__,2,setbase
	@symbol 'LIM'		   ,_FNCALL_+_RDONLY_+__DWORD__,1,getlimit
	@symbol 'SETLIM'	   ,_FNCALL_+_RDONLY_+__DWORD__,2,setlimit
	@symbol 'ATTR'		   ,_FNCALL_+_RDONLY_+__WORD__,1,getattr
	@symbol 'SETATTR'	   ,_FNCALL_+_RDONLY_+__WORD__,2,setattr
	@symbol 'INPB'		   ,_FNCALL_+_RDONLY_+__BYTE__,1,inportb
	@symbol 'INPW'		   ,_FNCALL_+_RDONLY_+__WORD__,1,inportw
	@symbol 'OUTB'		   ,_FNCALL_+_RDONLY_+__BYTE__,2,outportb
	@symbol 'OUTW'		   ,_FNCALL_+_RDONLY_+__WORD__,2,outportw
	@symbol 'HIWORD'	   ,_FNCALL_+_RDONLY_+__WORD__,1,_highword
	@symbol 'LOWORD'	   ,_FNCALL_+_RDONLY_+__WORD__,1,_lowword
	@symbol 'HIBYTE'	   ,_FNCALL_+_RDONLY_+__BYTE__,1,_highbyte
	@symbol 'LOBYTE'	   ,_FNCALL_+_RDONLY_+__BYTE__,1,_lowbyte
	@symbol 'TYPE'		   ,_FNCALL_+_RDONLY_+__BYTE__,1,gettype
ife ?WINDOWS
	@symbol 'TPMI'		   ,_FNCALL_+_RDONLY_+__BYTE__,2,setpmtrap
endif
	@symbol 'TRMI'		   ,_FNCALL_+_RDONLY_+__BYTE__,2,setrmtrap
	@symbol 'TEXC'		   ,_FNCALL_+_RDONLY_+__BYTE__,2,setextrap
	@symbol 'TIDT'		   ,_FNCALL_+_RDONLY_+__BYTE__,2,setidttrap
	@symbol 'SETNOTIFY'    ,_FNCALL_+_RDONLY_+__BYTE__,2,setnotify
	@symbol 'STRCAT'	   ,_FNCALL_+_RDONLY_+__STRING__,2,mystrcat

trapvars label byte
	@symbol 'IDTvec'	   ,_FNCALL_+_RDONLY_+__BYTE__,1,idtuse
	@symbol 'EXCEption (first)',_FNCALL_+_RDONLY_+__BYTE__,1,etrapsfirstout
	@symbol 'EXCeption (last)',_FNCALL_+_RDONLY_+__BYTE__,1,etrapslastout
ife ?WINDOWS
	@symbol 'PMInt' 	   ,_FNCALL_+_RDONLY_+__BYTE__,1,pmtrapsout
endif
	@symbol 'RMInt' 	   ,_FNCALL_+_RDONLY_+__BYTE__,1,rmtrapsout
	@symbol 'I41Visible'   ,_RDONLY_+__BYTE__, ,cVisible_i41
	@symbol 'NOTIFY'	   ,_FNCALL_+_RDONLY_+__BYTE__,1,notifyout
if ?USETOOLHELP
	@symbol 'TH'		   ,_RDONLY_+__BYTE__, ,fUseTH
	@symbol 'TOOLHlpvec'   ,__DWORD__, ,toolhlpvecs
endif
trapvarsend label byte

viovartab label byte
if ?WINDOWS
	@symbol 'SCREENSwap'   ,_RDONLY_+__BOOL__, ,fSwap
else
	@symbol 'SCREENSwap'   ,_FNCALL_+__BOOL__, ,getswap
endif        
;	@symbol 'ALT_VAddr'    ,		 __DWORD__, ,altcrt.vidaddr
	@symbol 'ALT_CRTPort'  ,		 __WORD__, ,altcrt.crtadr
	@symbol 'ALT_COLs'	   ,		 __BYTE__, ,altcrt.cols
	@symbol 'ALT_ROWs'	   ,		 __BYTE__, ,altcrt.rows
	@symbol 'DCC'		   ,_RDONLY_+__WORD__, ,wDCC
ife ?WINDOWS        
	@symbol 'DBGEE_VPage'  ,_RDONLY_+__BYTE__, ,clVPage
	@symbol 'DBGER_VPage'  ,		 __BYTE__, ,stdcrt.page_
endif        
	@symbol 'DBGER_SWAPMem',_RDONLY_+__DWORD__, ,pDbgerSaveMem
ife ?WINDOWS
	@symbol 'DBGEE_SWAPMem',_RDONLY_+__DWORD__, ,pDbgeeSaveMem
	@symbol 'DBGEE_GFXMem',_RDONLY_+__DWORD__, ,g_pDbgeeSaveGfxBuff
;	@symbol 'DBGER_VMode'  ,		 __WORD__, ,wVideoMode
	@symbol 'VESAMode',_FNCALL_+_RDONLY_+__WORD__, 1,_getvesamode
endif        
viovartabend label byte

intvartab  label byte
	@symbol 'MYCS'		   ,_FNCALL_+_RDONLY_+__WORD__, ,getcs,0
	@symbol 'MYDS'		   ,_FNCALL_+_RDONLY_+__WORD__, ,getds,0
if 0;ife ?FLAT	;this is alway equal to MYDS
	@symbol 'CS_Alias'	   ,_RDONLY_+__WORD__, ,__csalias
endif
if ?CS16ALIAS
	@symbol 'CS16Alias'    ,_RDONLY_+__WORD__, ,__cs16alias
endif
ife ?FLAT
	@symbol 'FLAT_Selector',_RDONLY_+__WORD__, ,__flatsel
endif
	@symbol '386_Call_Gate',_RDONLY_+__WORD__, ,ring0sel
	@symbol 'RMDBGHLPCS'   ,_RDONLY_+__WORD__, ,__RmCS
	@symbol 'RMDBGHLPDS'   ,_RDONLY_+__WORD__, ,__RmDS
	@symbol 'MYPSP' 	   ,_RDONLY_+__WORD__, ,_psp
	@symbol 'MYESP' 	   ,_RDONLY_+__DWORD__, ,myesp
	@symbol 'MYHEAP'	   ,_RDONLY_+__DWORD__, ,pHeap
	@symbol 'FLATHeap'	   ,_RDONLY_+__DWORD__, ,flatheap
	@symbol 'FLATHPHandle' ,_RDONLY_+__DWORD__, ,flathdl
if ?32BIT        
;	@symbol 'INT_2F'	   ,_RDONLY_+__FPTR__, ,oldint2F
	@symbol 'INT_41'	   ,_RDONLY_+__FPTR__, ,oldint41
else
;	@symbol 'INT_2F'	   ,_RDONLY_+__LPTR__, ,oldint2F
	@symbol 'INT_41'	   ,_RDONLY_+__LPTR__, ,oldint41
endif
if ?SAVEINT08 and ?SETEXCVECS
	@symbol 'INT_08'	   ,_RDONLY_+__FPTR__, ,saveCurInt08
	@symbol 'INT_1C'	   ,_RDONLY_+__FPTR__, ,saveCurInt1C
endif        

if ?WINDOWS
  if ?WATCHINT24        
	@symbol 'INT_24'	   ,_RDONLY_+__LPTR__, ,oldint24
  endif        
endif

	@symbol 'INITState'    ,_RDONLY_+__BYTE__, ,bRC
	@symbol 'DOS_Used'	   ,_RDONLY_+__BYTE__, ,fDOSused
	@symbol 'WINversion'   ,_RDONLY_+__WORD__, ,wWinVersion
	@symbol 'VM_ID' 	   ,_FNCALL_+_RDONLY_+__WORD__, ,getvmid,0
	@symbol 'VM_Handle'    ,_FNCALL_+_RDONLY_+__DWORD__, ,getvmhandle,0
	@symbol 'REALMode'	   ,_FNCALL_+_RDONLY_+__BOOL__, ,getrealmode
	@symbol 'EMSave'       ,_FNCALL_+__BOOL__, ,getfEMU
	@symbol 'EMBits' 	   ,__BYTE__, ,embits
	@symbol 'INPMode'	   ,_FNCALL_+__BYTE__, ,inpmode
	@symbol 'OUTMode'	   ,_FNCALL_+__BYTE__, ,outmode
	@symbol 'INPMFN'	   ,__STRING__, ,inpmodefn
	@symbol 'OUTMFN'	   ,__STRING__, ,outmodefn
	@symbol 'STRICTMode'   ,_FNCALL_+__BOOL__, ,getstrictmode
	@symbol 'DIRECTMode'   ,_FNCALL_+__BOOL__, ,getdirectmode
	@symbol 'UNASSFlags'   ,__BYTE__, ,fUnass
	@symbol 'DUMPFlags'    ,__BYTE__, ,fDump
	@symbol 'TRACEFlags'   ,__BYTE__, ,fTMode
	@symbol 'RMBreak'      ,__WORD__, ,wRMBreak
	@symbol 'RMStop'       ,__WORD__, ,wRMStop
	@symbol 'PMBreak'      ,__WORD__, ,wPMBreak
	@symbol 'PMStop'       ,__WORD__, ,wPMStop
	@symbol 'WAITLines'    ,__BYTE__, ,xLines
	@symbol 'ECHO'		   ,__BOOL__, ,fEcho
	@symbol 'LOGINP'	   ,__BOOL__, ,fTrace
	@symbol 'FORMStr'	   ,__STRING__, ,dwFormStr
	@symbol 'PICOFF'       ,__WORD__, ,wPicOff
;	@symbol 'SPICOFF'      ,__BYTE__, ,wPicOff+1
	@symbol 'PICON'        ,__WORD__, ,wPicOn
;	@symbol 'SPICON'       ,__BYTE__, ,wPicOn+1
	@symbol 'PICValue'     ,__WORD__, ,wPicValue
;	@symbol 'SPICValue'    ,__BYTE__, ,wPicValue+1
	@symbol 'IRQ'		   ,_FNCALL_+__BOOL__, ,getfIrq
	@symbol 'COMFlags'	   ,__BYTE__, ,bComFlags
	@symbol 'COMPort'	   ,__BYTE__, ,_comno

intvartabend label byte

cpuvars label byte
	@symbol 'CPUID_Family', 	_RDONLY_+__BYTE__, ,idcpu,,symcpufam
	@symbol 'CPUID_Model_Step', _RDONLY_+__BYTE__, ,idstep,,symcpumodel
	@symbol 'CPUID_FLags'  ,	_RDONLY_+__DWORD__, ,idflags,,symcpuflags
	@symbol 'CPUID_MAnuf'  ,	_FNCALL_+_RDONLY_+__STRING__, ,getmanu,,symcpumanu
cpuvarsend label byte

tregister label byte
	@symbol 'TEAX', 	__DWORD__, ,tregs.rEax
	@symbol 'TEBX', 	__DWORD__, ,tregs.rEbx
	@symbol 'TECX', 	__DWORD__, ,tregs.rEcx
	@symbol 'TEDX', 	__DWORD__, ,tregs.rEdx
	@symbol 'TESI', 	__DWORD__, ,tregs.rEsi
	@symbol 'TEDI', 	__DWORD__, ,tregs.rEdi
	@symbol 'TEBP', 	__DWORD__, ,tregs.rEbp
	@symbol 'TESP', 	__DWORD__, ,tregs.rEsp
	@symbol 'TEFL', 	__DWORD__, ,tregs.rEfl
	@symbol 'TEIP', 	__DWORD__, ,tregs.rEip
	@symbol 'TDS',		__WORD__, ,tregs.rDS
	@symbol 'TES',		__WORD__, ,tregs.rES
	@symbol 'TFS',		__WORD__, ,tregs.rFS
	@symbol 'TGS',		__WORD__, ,tregs.rGS
	@symbol 'TCS',		__WORD__, ,tregs.rCS
	@symbol 'TSS',		__WORD__, ,tregs.rSS
	db 0h

tssstrings label byte
	@symbol "LINK", 	   __WORD__,,00h,  ,symTSSLINK
	@symbol "SS0:ESP0",    __FPTR__,,04h,,symTSSSP0
	@symbol "SS1:ESP1",    __FPTR__,,0ch,,symTSSSP1
	@symbol "SS2:ESP2",    __FPTR__,,14h,,symTSSSP2
	@symbol "CR3",		   __DWORD__,,1ch, ,symTSSCR3
	@symbol "EIP",		   __DWORD__,,20h, ,symTSSEIP
	@symbol "EFLAGS",	   __DWORD__,,24h, ,symTSSEFL
	@symbol "EAX",		   __DWORD__,,28h, ,symTSSEAX
	@symbol "ECX",		   __DWORD__,,2ch, ,symTSSECX
	@symbol "EDX",		   __DWORD__,,30h, ,symTSSEDX
	@symbol "EBX",		   __DWORD__,,34h, ,symTSSEBX
	@symbol "ESP",		   __DWORD__,,38h, ,symTSSESP
	@symbol "EBP",		   __DWORD__,,3ch, ,symTSSEBP
	@symbol "ESI",		   __DWORD__,,40h, ,symTSSESI
	@symbol "EDI",		   __DWORD__,,44h, ,symTSSEDI
	@symbol "ES",		   __WORD__,,48h,  ,symTSSES
	@symbol "CS",		   __WORD__,,4ch,  ,symTSSCS
	@symbol "SS",		   __WORD__,,50h,  ,symTSSSS
	@symbol "DS",		   __WORD__,,54h,  ,symTSSDS
	@symbol "FS",		   __WORD__,,58h,  ,symTSSFS
	@symbol "GS",		   __WORD__,,5ch,  ,symTSSGS
	@symbol "LDT",		   __WORD__,,60h,  ,symTSSLDT
	@symbol "IO-Bitmap",   __WORD__,,66h,  ,symTSSIOB
	db 0

;*** ende symboltabellen ***

;*** views ***

tsstab label byte
	@outitem "link=",symTSSLINK
	@outitem <cr,lf,"ss:sp0=">,symTSSSP0
	@outitem <cr,lf,"ss:sp1=">,symTSSSP1
	@outitem <cr,lf,"ss:sp2=">,symTSSSP2
	@outitem <cr,lf,"cr3=">,symTSSCR3
	@outitem <cr,lf,"eip=">,symTSSEIP
	@outitem " efl=",symTSSEFL
	@outitem <cr,lf,"eax=">,symTSSEAX
	@outitem " ecx=",symTSSECX
	@outitem " edx=",symTSSEDX
	@outitem " ebx=",symTSSEBX
	@outitem <cr,lf,"esp=">,symTSSESP
	@outitem " ebp=",symTSSEBP
	@outitem " esi=",symTSSESI
	@outitem " edi=",symTSSEDI
	@outitem <cr,lf,"es=">,symTSSES
	@outitem " cs=",symTSSCS
	@outitem " ss=",symTSSSS
	@outitem <cr,lf,"ds=">,symTSSDS
	@outitem " fs=",symTSSFS
	@outitem " gs=",symTSSGS
	@outitem <cr,lf,"ldtr=">,symTSSLDT
	@outitem <cr,lf,"IO-Bitmap=">,symTSSIOB
	dd -1

gdtitem label byte
	@outitem  "gdtr=",symGDTR
	@outitem  ".",symGDTLIM
	dd -1

idtitem label byte
	@outitem  <"idtr=">,symIDTR
	@outitem  ".",symIDTLIM
	dd -1

tritem label byte
	@outitem  <"tr=">,symTR
	dd -1

ldtitem label byte
	@outitem  "ldtr=",symLDTR
	dd -1

ptstab label byte
	@outitem  <cr,lf,"cr0=">,symCR0
	@outitem  " cr2=",symCR2
	@outitem  " cr3=",symCR3
	@outitem  " cr4=",symCR4
	@outitem  <cr,lf,"dr0=">,symDR0
	@outitem  " dr1=",symDR1
	@outitem  " dr2=",symDR2
	@outitem  " dr3=",symDR3
if ?DR4DR5        
	@outitem  <cr,lf,"dr4=">,symDR4
	@outitem  " dr5=",symDR5
	@outitem  " dr6=",symDR6
else
	@outitem  <cr,lf,"dr6=">,symDR6
endif
	@outitem  " dr7=",symDR7
if ?TR3TR5        
	@outitem  <cr,lf,"tr3=">,symTR3
	@outitem  " tr4=",symTR4
	@outitem  " tr5=",symTR5
	@outitem  " tr6=",symTR6
else
	@outitem  <cr,lf,"tr6=">,symTR6
endif
	@outitem  " tr7=",symTR7
	@outitem  <cr,lf,"Family=">,symcpufam
	@outitem  " Model=",symcpumodel
	@outitem  " Flags=",symcpuflags
	@outitem  " Manufacturer=",symcpumanu
	dd -1

rtstab label byte
	@outitem  "eax=",symEAX
	@outitem " ebx=",symEBX
	@outitem " ecx=",symECX
	@outitem " edx=",symEDX
	@outitem " esi=",symESI
	@outitem " edi=",symEDI
	@outitem <cr,lf,"ebp=">,symEBP
	@outitem " fl=",symEFLr
	@outitem " cs:ip=",symCSr
	@outitem ":",symIPr
	@outitem " ss:sp=",symSSr
	@outitem ":",symESPr
	@outitem <cr,lf,"ds=">,symDSr
	@outitem " es=",symESr
	@outitem " fs=",symFSr
	@outitem " gs=",symGSr
	dd -1

ftstab label byte
	@outitem  "fcw=",symFCW
	@outitem  " fsw=",symFSW
	@outitem  " ftw=",symFTW
	@outitem  <cr,lf,"fcs=">,symFCS
	@outitem  " fip=",symFIP
	@outitem  " fopc=",symFOPC
	@outitem  " fos=",symFOS
	@outitem  " foo=",symFOO
	dd -1
ftstab2 label dword       
	dd offset symST0
	dd offset symST1
	dd offset symST2
	dd offset symST3
	dd offset symST4
	dd offset symST5
	dd offset symST6
	dd offset symST7

if ?MMXREGS
mmxtab label dword       
	dd offset symMM0
	dd offset symMM1
	dd offset symMM2
	dd offset symMM3
	dd offset symMM4
	dd offset symMM5
	dd offset symMM6
	dd offset symMM7
endif

tstab label byte
	@outitem  "eax=",symEAX
	@outitem " ebx=",symEBX
	@outitem " ecx=",symECX
	@outitem " edx=",symEDX
	@outitem " esi=",symESI
	@outitem " edi=",symEDI
	@outitem <cr,lf,"cpiaVR NioODITSZ A P C esp=">,symESPp
	@outitem " ebp=",symEBP
	@outitem " [esp]=",,GetEspInhalt
	@outitem <cr,lf>,,GetEflInhalt
	@outitem " eip=",symEIPp
	@outitem " [eip]=",,GetEipInhalt
	@outitem <cr,lf,"cs=">,symCSp
	@outitem " base=",,getbasex,r1.rCS
	@outitem ",lim=",,getlimx,r1.rCS
	@outitem ",attr=",,getattrx,r1.rCS
	@outitem <cr,lf,"ss=">,symSSp
	@outitem " base=",,getbasex, r1.rSS
	@outitem ",lim=",,getlimx, r1.rSS
	@outitem ",attr=",,getattrx, r1.rSS
	@outitem <cr,lf,"ds=">,symDSp
	@outitem " base=",,getbasex,r1.rDS
	@outitem ",lim=",,getlimx,r1.rDS
	@outitem ",attr=",,getattrx,r1.rDS
	@outitem <cr,lf,"es=">,symESp
	@outitem " base=",,getbasex,r1.rES
	@outitem ",lim=",,getlimx,r1.rES
	@outitem ",attr=",,getattrx,r1.rES
	@outitem <cr,lf,"fs=">,symFSp
	@outitem " base=",,getbasex,r1.rFS
	@outitem ",lim=",,getlimx,r1.rFS
	@outitem ",attr=",,getattrx,r1.rFS
	@outitem <cr,lf,"gs=">,symGSp
	@outitem " base=",,getbasex,r1.rGS
	@outitem ",lim=",,getlimx,r1.rGS
	@outitem ",attr=",,getattrx,r1.rGS
	dd -1


excstr label byte
	db 0			;0
	db 0
	db 0
	db 0
	db 0
	db 0
	db 0
	db 0
	db 0			;8
	db 0
	db 0
	db offset excstrB - $
	db offset excstrC - $
	db offset excstrD - $
	db offset excstrE - $
	db 0
	db 16 dup (0)	   ;10-1F

excstrB db "not present exc. ",00
excstrC db "stack exc. ",00
excstrD db "protection exc. ",00
excstrE db "page error exc. ",00


tDispstr	db "?? ",0
tkaphelp	db "CMDS",00
tExcFlags	db "excflags",00
tErrCodes	db "#ERRORS",00
flagsstr	db "cpiaVR NioODITSZ A P C",0
tModout		db ".do",0

	.code

	jmp @F

szDbgDll db ?MYNAME,' V2.11 - dpmi debugger (C) Andreas Grech 1993-2022',00

@@:
	mov dword ptr es:[ebx].DEBUGPARM.Enable, offset _Enable
	mov dword ptr es:[ebx].DEBUGPARM.Disable, offset Disable
	mov dword ptr es:[ebx].DEBUGPARM.DebugEntry, offset DebugEntry
ife ?FLAT
	mov word ptr es:[ebx].DEBUGPARM.Enable+4, cs
	mov word ptr es:[ebx].DEBUGPARM.Disable+4, cs
	mov word ptr es:[ebx].DEBUGPARM.DebugEntry+4, cs
 if ?WINDOWS
	mov dword ptr es:[ebx].DEBUGPARM.CheckUnregister, offset CheckUnregister
	mov word ptr es:[ebx].DEBUGPARM.CheckUnregister+4, cs
 endif
	call LibEntry
 if ?WINDOWS
	push ds
	mov ds,cs:__csalias
	mov eax, es:[ebx].DEBUGPARM._LoadLibrary
	mov ds:_LoadLibrary, eax
	mov eax, es:[ebx].DEBUGPARM._FreeLibrary
	mov ds:_FreeLibrary, eax
	mov eax, es:[ebx].DEBUGPARM._GetProcAddress
	mov ds:_GetProcAddress, eax
	mov eax, es:[ebx].DEBUGPARM._OutputDebugString
	mov ds:_OutputDebugString, eax
	mov eax, es:[ebx].DEBUGPARM._ToolhelpHook
	mov ds:_ToolhelpHook, eax
	mov eax, es:[ebx].DEBUGPARM.pszDebuggerName
	mov pszDebuggerName, eax
	mov eax, es:[ebx].DEBUGPARM._ReadAccProc
	mov ds:_ReadAccProc, eax
	mov eax, es:[ebx].DEBUGPARM._PeekMessageProc
	mov ds:_PeekMessageProc, eax
	pop ds
 endif
	retf
else
	ret
endif

	align 4

if ?SUPPDOS4GW        
isdos4gw:        
 if ?HIDEINT21
	@CallInt [oldi21p]
 else
	@CallInt [oldint21]
 endif
	push ebp
	mov ebp, ss
	lar ebp, ebp
	test ebp,400000h
	mov  ebp, esp      ; here ESP is loaded deliberately
	jnz @F
	movzx ebp, bp
@@:
	or byte ptr [ebp+3*4+1],1
	pop ebp
	@iret
endif

clearcarryandiret:
ife ?32BIT
	push bp
	mov bp,sp
	and byte ptr [bp+3*2],0FEh
	pop bp
else
	push ebp
	mov ebp, ss
	lar ebp, ebp
	test ebp,400000h
	mov ebp, esp      ; here ESP is loaded deliberately
	jnz @F
	movzx ebp,bp
@@:
	and byte ptr [ebp+3*4],0FEh
	pop ebp
endif
	@iret

if ?WATCHI31

myint31 proc

	@switch32bit
	cmp ax, 202h				;get exception vector?
	jz dpmi02023
	cmp ax, 203h				;set exception vector?
	jz dpmi02023
 if ?WINDOWS
	cmp ax, 283h				;set exception vector?
	jz dpmi02023
 endif
 if ?WATCHI21
	cmp ax, 204h				;get interrupt vector?
	jz dpmi0204
	cmp ax, 205h				;set interrupt vector?
	jz dpmi0205
 endif
default:
	jmp cs:[oldint31]
dpmi02023:						;get/set exception vector
	push ebx
	push ecx
	mov ecx, cs:eFirst
	and ecx, cs:eTraps
	movzx ebx, bl
	bt ecx,ebx
	pop ecx
	pop ebx
	jnc default
	push ebx
	movzx ebx, bl
	test al,1					; set or get?
	jnz dpmi0203
 ife ?32BIT
	mov dx, word ptr cs:[ebx*8+oldexcvecs+0]
 else
	mov edx, dword ptr cs:[ebx*8+oldexcvecs+0]
 endif
	mov cx, word ptr cs:[ebx*8+oldexcvecs+?SEGOFFS]
	jmp exitexc
dpmi0203:
	push ds
	mov   ds,cs:[__csalias]
 ife ?32BIT
	mov word ptr [ebx*8+oldexcvecs+0],dx
 else
	mov dword ptr [ebx*8+oldexcvecs+0],edx
 endif
	mov word ptr [ebx*8+oldexcvecs+?SEGOFFS],cx
	pop ds
exitexc:
	pop ebx
exit:
	jmp clearcarryandiret

if ?WATCHI21
dpmi0204:
dpmi0205:
 if ?HIDEINT01
	cmp bl, 1				;int 01?
	jz dpmi020x_01
 endif
if ?HIDEINT09
	cmp bl, 9				;int 09?
	jz dpmi020x_09
endif        
 if ?HIDEINT21
	cmp bl, 21h				;int 21?
	jz dpmi020x_21
 endif
	jmp default				;jump to previous handler

 if ?HIDEINT01
dpmi020x_01:        
	bt cs:pmtraps,1
	jnc default
	cmp al, 5
	jz dpmi205_01
  ife ?32BIT
	mov dx, word ptr cs:[oldi01p+0]
  else
	mov edx, dword ptr cs:[oldi01p+0]
  endif
	mov cx, word ptr cs:[oldi01p+?SEGOFFS]
	jmp exit
dpmi205_01:
	push ds
	mov ds,cs:[__csalias]
  ife ?32BIT
	mov word ptr [oldi01p+0],dx
  else
	mov dword ptr [oldi01p+0],edx
  endif
	mov word ptr [oldi01p+?SEGOFFS],cx
	pop ds
	jmp exit
 endif				;?HIDEINT01

 if ?HIDEINT09        
dpmi020x_09:        
;	bt cs:pmtraps,9
;	jnc default
	cmp al, 5
	jz dpmi205_09
  ife ?32BIT
	mov dx, word ptr cs:[oldi09p+0]
  else
	mov edx, dword ptr cs:[oldi09p+0]
  endif		 
	mov cx, word ptr cs:[oldi09p+?SEGOFFS]
	jmp exit
dpmi205_09:
	push ds
	mov ds,cs:[__csalias]
  ife ?32BIT
	mov word ptr [oldi09p+0],dx
  else
	mov dword ptr [oldi09p+0],edx
  endif
	mov word ptr [oldi09p+?SEGOFFS],cx
	pop ds
	jmp exit
 endif		 ;?HIDEINT09

 if ?HIDEINT21        
dpmi020x_21:        
	cmp al, 5
	jz dpmi205_21
  ife ?32BIT
	mov dx, word ptr cs:[oldi21p+0]
  else
	mov edx, dword ptr cs:[oldi21p+0]
  endif
	mov cx, word ptr cs:[oldi21p+?SEGOFFS]
	jmp exit
dpmi205_21:
	push ds
	mov ds,cs:[__csalias]
  ife ?32BIT
	mov word ptr [oldi21p+0],dx
  else
	mov dword ptr [oldi21p+0],edx
  endif
	mov word ptr [oldi21p+?SEGOFFS],cx
	pop ds
	jmp exit
 endif		;?HIDEINT21

endif		;?WATCHI21

myint31 endp

endif	;?WATCHI31

if ?WATCHI21
myint21 proc

	@switch32bit
if ?HIDEINT01        
	cmp ax, 2501h
	jz getsetint01
	cmp ax, 3501h
	jz getsetint01
endif        
if ?HIDEINT09        
	cmp ax, 2509h
	jz setint09
	cmp ax, 3509h
	jz getint09
endif        
if ?HIDEINT21        
	cmp ax, 2521h
	jz setint21
	cmp ax, 3521h
	jz getint21
endif
if ?SUPPDOS4GW
	test cs:[fTMode], FTMODE_ISDOS4GW
	jz @F
	cmp ax, 0FF00h
	jnz @F
	cmp dx, 0078h
	jz isdos4gw
@@:
endif
	cmp ah, 4Ch
	jnz default
	push ebx
	push eax
if 1;e ?FLAT   ; no guarantee that DS is flat just because the debugger is a PE
	push ds
	mov ds, cs:[__flatsel]
endif
	movzx ebx, word ptr cs:[sdaadr+2]
	movzx eax, word ptr cs:[sdaadr+0]
	shl ebx, 4
	add ebx, eax
	mov ax, [ebx+10h]	;get psp
	cmp ax, word ptr cs:[_pspseg]	; is current PSP the PSP of debxxf?
if 1;e ?FLAT
	pop ds
endif
	pop eax
	pop ebx
	jz terminate
default:
if ?HIDEINT21
	jmp cs:[oldi21p]
else
	jmp cs:[oldint21]
endif

exit:
	jmp clearcarryandiret

terminate:
	call _disable
	mov ax, 4Cffh
	int 21h

if ?HIDEINT01
getsetint01:
	bt cs:pmtraps,1
	jnc default
	cmp ah, 25h
	jz setint01
 ife ?32BIT
	les bx, cs:[oldi01p]
 else
	les ebx, cs:[oldi01p]
 endif
	jmp exit
setint01:
	push es
	mov es,cs:[__csalias]
 ife ?32BIT
	mov word ptr es:[oldi01p+0],dx
 else
	mov dword ptr es:[oldi01p+0],edx
 endif
	mov word ptr es:[oldi01p+?SEGOFFS],ds
	pop es
	jmp exit
endif

if ?HIDEINT09
getint09:
 ife ?32BIT
	les bx, cs:[oldi09p]
 else
	les ebx, cs:[oldi09p]
 endif
	jmp exit
setint09:
	push es
	mov es,cs:[__csalias]
 ife ?32BIT
	mov word ptr es:[oldi09p+0],dx
 else
	mov dword ptr es:[oldi09p+0],edx
 endif
	mov word ptr es:[oldi09p+?SEGOFFS],ds
	pop es
	jmp exit
endif

if ?HIDEINT21
getint21:
 ife ?32BIT
	les bx, cs:[oldi21p]
 else
	les ebx, cs:[oldi21p]
 endif
	jmp exit
setint21:
	push es
	mov es,cs:[__csalias]
 ife ?32BIT
	mov word ptr es:[oldi21p+0],dx
 else
	mov dword ptr es:[oldi21p+0],edx
 endif
	mov word ptr es:[oldi21p+?SEGOFFS],ds
	pop es
	jmp exit
endif

myint21 endp
endif


_dpmicall proc stdcall public
	@CallInt [oldint31]
	ret
_dpmicall endp

if ?WATCHI2F

if ?LOGINT2F16
dispregs2f proc
	pushad
	push ds
	push es
	push gs
if ?32BIT
	mov ebp,esp
else
	movzx ebp,sp
endif
	mov ds,cs:[__csalias]
	push ds
	pop es
	or [fVideo], FVIDEO_NOSWITCH
	@loadflat
	movzx eax,ax
	movzx ebx,bx
	movzx edi,di
	movzx ecx,word ptr [ebp+4]
	invoke printf, CStr("int 2Fh, ax=%X, bx=%X, es:di=%X:%X",10), eax, ebx, ecx, edi
	and [fVideo], not FVIDEO_NOSWITCH
	call SwitchToDebuggeeScreen
	pop gs
	pop es
	pop ds
	popad
	ret
dispregs2f endp
endif


clientint2f:
	@switch32bit
if ?LOGINT2F16
	cmp ah,16h
	jnz @F
	cmp al,80h
	jz @F
	cmp al,89h
	jz @F
	call dispregs2f
@@:
endif
	jmp cs:[oldint2F]

myint2F proc stdcall public
	@CallInt [oldint2F]
	ret
myint2F endp
else
myint2F proc stdcall public	;required because of sleep()
	int 2Fh
	ret
myint2F endp
endif


if ?FLAT
	.data
dwDTASeg	dd 0
	.code

;--- repair int 21h, ah=1Ah for NT platforms

repair211a proc uses ds
	pushad
	mov esi,edx
	mov ebx,ds					;get base
	mov ax,0006
	@DpmiCall
	mov ds,cs:[__csalias]
	push cx
	push dx
	pop edx
	add esi,edx					;now linear address in esi
	mov ebx,dwDTASeg
	and ebx,ebx
	jnz @F
	mov cx,1
	mov ax,0					;alloc selector
	@DpmiCall
	mov dwDTASeg,eax
	mov ebx,eax
@@:
	push esi
	pop dx
	pop cx
	mov ax,0007					;set base
	@DpmiCall
	mov dx,103h
	xor ecx,ecx
	mov ax,0008					;set limit
	@DpmiCall
	mov ds,ebx
	xor edx,edx
	mov ah,1Ah
	@CallInt [oldint21]
	popad
	ret
repair211a endp
endif

_doscall proc stdcall public
if ?TRAPRM15
	cmp cs:[__RmDS], 0
	jz @F
	push fs
	mov fs, cs:[__RmDS]
	assume fs:_TEXT16
	cmp fs:[DEBRMVAR.int15cnt], 0
	pop fs
	assume fs:nothing
	jz @F
	@errorout ERR_DEVICE_BUSY
	jmp mains
@@:
endif
if 0			;_doscall will never be called from other code
	test cs:[fMode], FMODE_INDEBUG
	jz doscall_1
endif        
	test cs:[fStat], FSTAT_ISNT
	jz doscall_1
	cmp ah,71h				;LFN?
	jnz @F
	mov al,00h
	stc
	ret
@@:
if ?FLAT
	cmp ah,1ah
	jnz doscall_1
	test edx,0FFFF0000h
	jz doscall_1
	call repair211a
	ret
endif
doscall_1:
  ife ?WINDOWS
	@CallInt [oldint21]
  else
	if 0
	call _NoHookDosCall		;standard int 21 macht "sti"!
	else
	@CallInt [oldint21]
	endif
  endif
	ret
_doscall endp

if ?WINDOWS
_Movsb proc public
if 1
	@switch16bit
	db 67h 
	rep movsb
	@switch32bit
else
	rep movsb
endif
	ret
_Movsb endp

_Stosb proc public
if 1
	@switch16bit
	db 67h 
	rep stosb
	@switch32bit
else
	rep stosb
endif
	ret
_Stosb endp
endif

ctrlccheck proc stdcall public

	test [fMode], FMODE_INDEBUG
	jz @F
	cmp al,03
	jnz @F
	and [fMode], not FMODE_EXEACTIVE
	@stroutc "^C"
	jmp mains
@@:
	ret
ctrlccheck endp

;*** allgemein: anhalten nach xLines zeilen
;*** is called by XonXoffCheck/VioPutCharDir whenever a CR is written

checkifshouldwait proc stdcall public uses eax

	mov al,[xLines]
	cmp al,0					;0=nicht anhalten
	jz exit
	cmp al,-1					;aktuelle Zeilenanzahl verwenden
	jnz @F
	mov al,altcrt.rows
	test [__outmode], _ALTOUT
	jnz @F
	mov al,@flat:[484h]
@@:
	inc [cLines]
	cmp al,[cLines]
	ja @F
	mov [cLines], 1
	pushad
	call getcharex
	call ctrlccheck
	popad
@@:
exit:
	ret
checkifshouldwait endp

if 0
__strout16:
	pushad
	@loadesp ebp
	pushfd
	mov esi, [ebp+32]
	movzx esi, word ptr cs:[esi]
	add dword ptr [ebp+32], 2
	jmp stroutx1
endif

;--- __strout32: proc called by @stroutc macro
;--- all registers preserved

__strout32 proc public
	pushad
	@loadesp ebp
	pushfd
	mov esi, [ebp+32]
	mov esi, cs:[esi]
	add dword ptr [ebp+32],4
@@:
	lodsb cs:[esi]
	and al,al
	jz @F
	@putchr al
	jmp @B
@@:
	popfd
	popad
	ret
__strout32 endp

;--- this routine copied from lib32n
;--- and adjusted so it translates lf to cr+lf

___strout@4 proc public
	pushad
	@loadesp ebp
	pushfd
	cld
	mov esi,[ebp+32+4]
nextchar:
	lodsb
	and al,al
	jz done
	cmp al, 10
	jnz @F
	@putchr 13
@@:
	@putchr al
	jmp nextchar
done:
	popfd
	popad
	ret 4
___strout@4 endp

;*** die errorout routine sollte immer aufgerufen werden koennen ***

_errorout proc stdcall public error:dword

	pushad
	push gs
	push fs
	push es
	push ds
	cld
	mov ds, cs:[__csalias]
	push ds
	pop es
	@loadflat

	invoke searchkap, [pHlpFile], addr tErrCodes;[#ERRORS] suchen

	and eax, eax
	jz errorout_1
	mov esi,eax
errorout_0: 				;<---- compare next error#
	xor edx,edx
	xor eax,eax
@@:
	mov al,@flat:[esi]
	inc esi
	call chkhex		;convert hex digit to value 0-F
	jc @F
	shl edx,4
	add edx, eax
	jmp @B
@@:
	cmp edx,error	; error# found?
	jz error_found
@@:
	and al,al		; end reached?
	jz errorout_1
	mov al,@flat:[esi]
	inc esi
	cmp al,lf
	jnz @B
	cmp byte ptr @flat:[esi],'['
	jnz errorout_0
errorout_1:
	@stroutc "undefined "
	mov edx,error
	call errmsgout
	@wordout edx
	jmp exit
error_found:
	call msgout
	test dh,08
	jnz plainexit
exit:
	invoke _crout
plainexit:        
	pop ds
	pop es
	pop fs
	pop gs
	popad
	ret
_errorout endp

;--- display %x, %u, %s

errorformout proc

	mov ah,al
	or al,20h
	cmp al,'x'
	jz isok
	cmp al,'u'
	jz isok
	cmp al,'s'
	jnz isnotok
isok:
	push edx
	mov al,'%'
	movzx eax,ax
	push eax
	@loadesp ecx
	invoke printf, ecx, dword ptr ss:[ebx]
	add ebx, 4
	add esp, 4
	pop edx
	ret
isnotok:
	@putchr al
	ret
errorformout endp

;*** fehler# in [#ERRORS] gefunden ***

msgout proc
	call errmsgout			;display error# + type
	lea ebx,[ebp+12]		;zeiger auf parameter
nextchar:
	mov al,@flat:[esi]
	inc esi
	and al,al
	jz exit
	cmp al,cr
	jz exit
	cmp al,'%'
	jnz @F
	mov al,@flat:[esi]
	inc esi
	and al,al
	jz exit
	call errorformout
	jmp nextchar
@@:
	cmp al,"\"
	jnz @F
	mov al,@flat:[esi]
	inc esi
	and al,al
	jz exit
	call msgoutesc
	jc nextchar
@@:
	@putchr al
	jmp nextchar
exit:
	ret
msgout endp

;--- print excape (\) characters

msgoutesc proc
	cmp al,"\"			;is it '\\'?
	jz exit
	cmp al,"n"
	jnz @F
	@putchr cr
	mov al,lf
	jmp exit
@@:
	cmp al,"b"
	jnz	@F
	or dh,08h			;dont write a lf at end
	stc					;skip this character
	jmp	exit2
@@:
	cmp al,cr
	jnz @F
	mov ax,@flat:[esi]
	cmp al,lf			;is it a '\' at line end?
	jnz @F
	inc esi				;skip CRLF
	inc esi
	mov al,ah
	cmp al,0
	jnz @F
	dec esi
@@:        
exit:
	clc
exit2:
	ret
msgoutesc endp

;--- display error # and type (error, warning, msg)

errmsgout proc
	cmp dh,90h				;fehler# >= 9000h? -> nur texte
	jnc errmsg_ex
	cmp dh,20h				;fehler# >= 2000h? -> message#
	jnc errmsg_2
	cmp dh,10h				;fehler# >= 1000h? -> warning#
	jnc errmsg_3
	@stroutc "error "
	jmp errmsg_1
errmsg_3:
	@stroutc "warning "
	jmp errmsg_1
errmsg_2:
	@stroutc "msg "
errmsg_1:
	@wordout edx
	@stroutc ": "
errmsg_ex:
	ret
errmsgout endp

chkziff:
	cmp al,'0'
	jb	@F
	cmp al,'9'+1
	cmc
	jnc @F
	or	al,20h
	cmp al,'a'
	jb	@F
	cmp al,'g'
	cmc
@@:
	ret
chkhex:
	call chkziff
	jc chkhex_er
	sub al,'0'
	cmp al,9
	jbe @F
	sub al,'a'-'0'-10
@@:
	clc
chkhex_er:
	ret

checkcpuid proc stdcall
	pushfd
	push 200000h		;push ID flag
	popfd
	pushfd
	pop  eax
	popfd
	test eax,200000h	;is it set now?
	mov  al,00
	jz checkcpuid_ex
	push 1
	pop eax
	@cpuid
	clc
	ret
checkcpuid_ex:
	stc
	ret
checkcpuid endp

;*** folgende procedur wird am start aufgerufen ***

getdpmiparms proc stdcall

local	myrmcs:RMCS

	call checkcpuid
	jc @F
	or byte ptr fCPUID,1
	mov [idcpu],ah			 ;cpu
	mov [idstep],al 		 ;maske/stepping
	mov [idflags],edx
@@:
	@DpmiCall 400h
	mov dpmiversion,ax
	mov dpmicpu,cl
	mov dpmiflags,bx
	mov dpmipics,dx

	lea edi,myrmcs
	xor eax,eax
	mov dword ptr myrmcs.rSP,eax
	mov myrmcs.rFlags,ax
	mov myrmcs.rEAX,1687h
	mov bx,002Fh
	xor ecx,ecx
	@DpmiCall 300h
	jc @F
	mov eax,[edi.RMCS.rEDI]
	mov word ptr dpmiPMentry+0,ax
	mov ax,[edi.RMCS.rES]
	mov word ptr dpmiPMentry+2,ax
	mov eax,[edi.RMCS.rESI]
	mov dpmitaskdat,ax
	mov eax,[edi.RMCS.rEBX]
	mov dpmi32bitap,ax
	@DpmiCall 0306h 			 ; get raw mode switch addresses
	mov word ptr dpmirm2pm+0,cx
	mov word ptr dpmirm2pm+2,bx
if ?32BIT
	mov dword ptr dpmipm2rm+0,edi
	mov word ptr dpmipm2rm+4,si
else
	mov word ptr dpmipm2rm+0,di
	mov word ptr dpmipm2rm+2,si
endif
@@:
	test [fStat], FSTAT_ISNT
	jz @F
	mov wPMStop, si
@@:
	@DpmiCall 0604h
	mov word ptr dpmipagesiz+0,cx
	mov word ptr dpmipagesiz+2,bx

	push es
if ?LDTSEL							;has problems on NT platforms
	mov esi, CCStr("MS-DOS")
	mov ax,168Ah
	@callint2F
	cmp al,0
	jnz @F
 if ?32BIT
	test [fStat], FSTAT_ISNT
	jnz @F
	push es
	push edi
	mov ax,100h
	call fword ptr [esp]
	add esp,4+4
 else
	mov eax,es
	shl eax,16
	mov ax,di
	push eax
	movzx esi,sp
	mov ax,100h
	db 66h
	call fword ptr ss:[esi]
	pop edx
 endif
	mov ldtsel,ax
@@:
endif
ife ?WINDOWS
	mov esi, CCStr("HDPMI")
	mov ax, 168Ah
	@callint2F
	cmp al,0
	jnz @F
	mov ax, 5		;modify HDPMI (bit 5 only)
	mov bl, 0		;0=reset HDPMI=32
	push es
 if ?32BIT
	push edi
	call fword ptr [esp]
	add esp,4+4
 else
	push di
	call far16 ptr [esp]
	add esp,4+2
 endif
@@:
endif
	pop es
	ret
getdpmiparms endp

;--- erweiterte register retten
;--- muss im ring0 aufgerufen werden

savexregs proc stdcall public
	mov eax,cr0
	mov [rCR0],eax
	mov eax,cr2
	mov [rCR2],eax
	mov eax,cr3
	mov [rCR3],eax
	test byte ptr idflags,ID_V86EXT 	  ;v86 extensions da?
	jz @F
;	mov eax,cr4
	@moveaxcr4
	mov [rCR4],eax
@@:
	mov eax,dr0
	mov [rDR0],eax
	mov eax,dr1
	mov [rDR1],eax
	mov eax,dr2
	mov [rDR2],eax
	mov eax,dr3
	mov [rDR3],eax
if ?DR4DR5
	cmp byte ptr dpmicpu,4
	jb	@F
;	mov eax,dr4
	@moveaxdr4
	mov [rDR4],eax
;	mov eax,dr5
	@moveaxdr5
	mov [rDR5],eax
@@:
endif
	mov eax,dr6
	mov [rDR6],eax
	mov eax,dr7
	mov [rDR7],eax

if ?TR3TR5
	cmp byte ptr dpmicpu,4
	jb	@F
	cmp byte ptr idcpu,4
	jnz @F				;nicht fuer pentium
;	mov eax,tr3
	@moveaxtr3
	mov [rTR3],eax
;	mov eax,tr4
	@moveaxtr4
	mov [rTR4],eax
;	mov eax,tr5
	@moveaxtr5
	mov [rTR5],eax
@@:
endif
	cmp byte ptr idcpu,-1
	jnz @F				;nicht fuer pentium
	mov eax,tr6
	mov [rTR6],eax
	mov eax,tr7
	mov [rTR7],eax
@@:
savexregsex:
	ret
savexregs endp

GetEflInhalt proc stdcall

	push edi
	push ecx
	mov edi,[pNearHeap]
	push edi
	mov ebx,[r1.rEfl]
	shl ebx,10
	mov ecx,22
@@:
	shl ebx,1
	mov al,'0'
	adc al,0
	stosb
	loop @B
	mov al,00
	stosb
	pop eax
	pop ecx
	pop edi
	mov cl,__STRING__
	ret
GetEflInhalt endp

;*** memory von ax:esi nach es:edi kopieren (max 4 bytes)
;*** out: bl = anzahl tats„chlich kopierte bytes
;*** BL=0 if error

getmemory proc stdcall

local	dwESP:dword

	push [excexit]
	mov [excexit], offset getmemexit
	push ds

	mov dwESP, esp
	mov bl, __VOID__
	verr ax
	jnz getmemexit
	mov ds, eax
	lar ecx, eax
	lsl edx, eax
	mov bh, ch
	and bh, 1Ch
nextbyte:
	cmp bh, 14h			 ;bit4=1->Memory bit3=0->DATA bit2=1->expdn?
	jnz @F
	cmp esi, edx
	jb getmemexit
	jmp isok
@@:        
	cmp esi, edx
	ja getmemexit
isok:
	@mov ecx, 1
if ?WINDOWS
	call _Movsb
else
	movsb
endif
	inc bl
	cmp bl,4			;maximal 4 bytes holen
	jnz nextbyte
getmemexit:
	mov esp, dwESP
	pop ds
	pop [excexit]
	ret
getmemory endp

;*** [EIP] (nur pm) holen ***

GetEipInhalt proc stdcall uses edi esi

	mov edi, offset espinh
	mov dword ptr [edi], 0

	call getactcseipx		;CS:EIP -> AX:EBX
	mov esi, ebx

	call getmemory			;get content of AX:ESI to ES:EDI

	mov eax, offset espinh
	mov dl, bl
	mov cl, __LIST__
	cmp dl, 0
	jnz @F
	mov cl, __VOID__
@@:
	ret
GetEipInhalt endp

GetEspInhalt proc stdcall uses edi esi

	mov edi, offset espinh
	mov dword ptr [edi], 0
	mov eax, [r1.rSS]
	mov esi, [r1.rEsp]
	lar ecx, eax
	test ecx, 400000h
	jnz @F
	movzx esi, si
@@:
	call getmemory		;get content of AX:ESI to ES:EDI

	mov eax, [espinh]
	mov cl, bl
	cmp cl, 3
	jnz @F
	mov cl, __DWORD__
@@:
	ret
GetEspInhalt endp

savedebuggeefpustate proc        
	test fEMU, 0FFh
	jz @F
	@DpmiCall 0E00h
	jc @F
	and al, 3
	mov embits, al
@@:        
	ret
savedebuggeefpustate endp

setdebuggerfpustate proc
	test fEMU, 0FFh
	jz @F
	xor ebx, ebx
	@DpmiCall 0E01h
@@:        
	ret
setdebuggerfpustate endp

restoredebuggeefpustate proc        
	test fEMU, 0FFh
	jz @F
	movzx ebx, byte ptr embits
	@DpmiCall 0E01h
@@:
	ret
restoredebuggeefpustate endp

;*** registerzustand retten
;--- IN: ds=csalias

savereg proc

	mov [r1.rES], es
	mov [r1.rFS], fs
	mov [r1.rGS], gs

	test [fEntry], FENTRY_REAL
	jz @F
	push ds
	pop es
	mov esi, offset rmcsi0y
	mov edi, offset r1
	movsd
	movsd
	movsd
	add esi, 4	;skip ESP
	add edi, 4	;skip ESP
	movsd
	movsd
	movsd
	movsd

	mov edi, offset r1r
	mov ecx, 9
	rep movsw
	jmp stdregs
@@:
	mov [r1.rEax], eax
	mov [r1.rEbx], ebx
	mov [r1.rEcx], ecx
	mov [r1.rEdx], edx
	mov [r1.rEsi], esi
	mov [r1.rEdi], edi
	mov [r1.rEbp], ebp
stdregs:
	test [fException], FEXC_USEXREGS	; restore some regs?
	jz @F
	and [fException], not FEXC_USEXREGS	; clear instantly
	mov eax, [rCSx]
	mov [r1.rCS], eax
	mov eax, [rEipx]
	mov [r1.rEip], eax
	mov eax, [rEflx]
	mov [r1.rEfl], eax
if ?SWITCHSTACK
	mov eax, [rSSx]
	mov ecx, [rEspx]
	test [fException], FEXC_SAVESTACK
	jz saver1
	and [fException], not FEXC_SAVESTACK
	mov [r1.rSS], eax
	mov [r1.rEsp], ecx
endif
	jmp saver1
@@: 								;entry ueber PM-INT
	mov es, [r1.rSS]
	mov ebp, [r1.rEsp]
	mov ebx, ebp
	mov eax, es
	lar eax, eax
	test eax, 400000h
	jnz @F
	movzx ebp, bp
@@:
if ?32BIT eq 0
	test [fEntry], FENTRY_IDT
	jz @F
endif
	lea eax, [ebx+3*4]			; 32 Bit Entry
	mov [r1.rEsp], eax
	mov eax, es:[ebp+0*4]
	mov [r1.rEip], eax
	mov eax, es:[ebp+1*4]
	mov [r1.rCS], eax
	mov eax, es:[ebp+2*4]
	jmp savereg_1
@@:
	lea eax, [ebx+3*2]			; 16 Bit Entry
	mov [r1.rEsp], eax
	movzx eax, word ptr es:[ebp+0*2]
	mov [r1.rEip], eax
	mov ax, es:[ebp+1*2]
	mov [r1.rCS], eax
	mov ax, es:[ebp+2*2]
savereg_1:
	mov [r1.rEfl],eax
saver1:
if ?RESTRACEFLG
	call resettracemode
endif
if 0
	test [fMode], FMODE_STRICT
	jnz @F
endif
	str [rTR]
	sldt [rLDT]
	sgdt [rGDTR]
	sidt [rIDTR]
@@:
if ?FLOATREGS
	test [f80x87], F80X87_FPU_PRESENT
	jz savereg_3
	call savedebuggeefpustate
	jc @F
	call setdebuggerfpustate
@@:
	fnsave [frsave]
	fnclex
	or [f80x87], F80X87_STATE_SAVED
savereg_3:
endif
if ?MARKPSP
	test [fEntry], FENTRY_REAL
	jnz @F
	call getpsp
	push ds
	mov ds, eax
	or byte ptr ds:[4Fh], 80h	;mark this PSP as "protected mode"
	pop ds
@@:
endif
	ret
savereg endp

if ?SETEXCVECS

;--- dont let debuggee handle exceptions while in debugger

;--- if ?SAVEINT08 is on, check if int 08 and 1C are modified
;--- and if yes, set them as well so debuggee won't get control.

if ?SAVEINT08
myint08 proc
	jmp cs:[oldint08]
myint08 endp
myint1C proc
	jmp cs:[oldint1C]
myint1C endp
endif

setdebuggervecs proc
	pushad
	@tprintf <"setdebuggervecs enter",lf>
	mov word ptr saveCurExc0D+?SEGOFFS,0
	mov word ptr saveCurExc0E+?SEGOFFS,0
if ?SAVEINT08
	mov word ptr saveCurInt08+?SEGOFFS,0
	mov word ptr saveCurInt1C+?SEGOFFS,0
endif
	cmp [cLoad], 0
	jz done
	mov bl, 0Dh
	@DpmiCall 202h
	movzx esi, bl
	shl esi, 2
	add esi, offset exc00h
ife ?32BIT
	movzx edx, dx
endif
if ?CS16ALIAS
	mov eax, [__cs16alias]
else
	mov eax, cs
endif        
	cmp ax, cx
	jnz @F
	cmp esi, edx
	jz no0d
@@:
	mov dword ptr saveCurExc0D, edx
	mov word ptr saveCurExc0D+?SEGOFFS, cx
	mov ecx, eax
	mov edx, esi
	@DpmiCall 203h
if ?WINDOWS
	jnc @F
	@DpmiCall 283h
@@:
endif
no0d:
	mov bl, 0Eh
	@DpmiCall 202h
	movzx esi, bl
	shl esi, 2
	add esi, offset exc00h
if ?CS16ALIAS
	mov eax, [__cs16alias]
else
	mov eax, cs
endif        
	cmp ax, cx
	jnz @F
	cmp esi, edx
	jz no0e
@@:
	mov dword ptr saveCurExc0E, edx
	mov word ptr saveCurExc0E+?SEGOFFS, cx
	mov ecx, eax
	mov edx, esi
	@DpmiCall 203h
if ?WINDOWS
	jnc @F
	@DpmiCall 283h
@@:
endif
no0e:
if ?SAVEINT08
	cmp fIrq, 0
	jz no1c
	mov  bl, 08h
	@DpmiCall 204h
if ?32BIT
	cmp edx, dword ptr [oldint08+0]
else
	cmp dx, word ptr [oldint08+0]
endif
	jnz @F
	cmp cx, word ptr [oldint08+?SEGOFFS]
	jz no08
@@:
	mov dword ptr saveCurInt08, edx
	mov word ptr saveCurInt08+?SEGOFFS, cx
	mov ecx, cs
	mov edx, offset myint08
	@DpmiCall 205h
no08:
	mov  bl, 1Ch
	@DpmiCall 204h
if ?32BIT
	cmp edx, dword ptr [oldint1C+0]
else
	cmp dx, word ptr [oldint1C+0]
endif
	jnz @F
	cmp cx, word ptr [oldint1C+?SEGOFFS]
	jz no1c
@@:
	mov dword ptr saveCurInt1C, edx
	mov word ptr saveCurInt1C+?SEGOFFS, cx
	mov ecx, cs
	mov edx, offset myint1C
	@DpmiCall 205h
no1c:
endif
	call kbd_setdebuggervecs
done:
	popad
exit:        
	ret
setdebuggervecs endp

;--- restore debuggee exc vecs

setdebuggeevecs proc
	pushad
	@tprintf <"setdebuggeevecs enter",lf>
	mov cx, word ptr saveCurExc0D+?SEGOFFS
	jcxz noexc0d
	mov edx, dword ptr saveCurExc0D
	mov bl, 0Dh
	@DpmiCall 203h
if ?WINDOWS
	jnc @F
	@DpmiCall 283h
@@:
endif
noexc0d:
	mov cx, word ptr saveCurExc0E+?SEGOFFS
	jcxz noexc0e
	mov edx,dword ptr saveCurExc0E
	mov bl, 0Eh
	@DpmiCall 203h
if ?WINDOWS
	jnc @F
	@DpmiCall 283h
@@:
endif
noexc0e:

	mov ax, 205h
if ?SAVEINT08
	mov cx, word ptr saveCurInt1C+?SEGOFFS
	jcxz @F
	mov edx, dword ptr saveCurInt1C
	mov bl, 1Ch
	@DpmiCall
@@:
	mov cx, word ptr saveCurInt08+?SEGOFFS
	jcxz @F
	mov edx, dword ptr saveCurInt08
	mov bl, 08h
	@DpmiCall
@@:
endif
	call kbd_setdebuggeevecs
	popad
	ret
setdebuggeevecs endp

endif

;--- restore debuggee environment
;--- changes no registers

restoreenvironment proc uses eax
	mov ax, wPicValue
	xchg al, ah
	out 0A1h, al
	mov al, ah
	out 21h, al
	and [fMode], not FMODE_INDEBUG
	call setctrlctrap
if ?SETEXCVECS
	call setdebuggeevecs
endif
	ret
restoreenvironment endp

;--- save debuggee state and
;--- set debugger state

setmyenvironment proc uses eax

	in al, 0A1h
	mov ah, al
	in al, 21h
	mov wPicValue, ax

	cmp [fIrq], 0
	jz @F
	mov ax, wPicValue
	or ax, wPicOff
	push ecx
	mov cx, wPicOn
	xor cx, -1
	and ax, cx
	pop ecx
	out 21h, al
	mov al, ah
	out 0A1h, al
	mov al, 0Bh
	out 20h, al
	in al, 20h
	test al, 3
	jz @F
	@errorout WRN_IRQ0_OR_IRQ1_INSERVICE
@@:
	or [fMode], FMODE_INDEBUG
	call resetctrlctrap		; for COM in/out, no register changes
if ?SETEXCVECS
	call setdebuggervecs
endif
	ret
setmyenvironment endp

;*** registerzustand restore ***

ReadReg proc near

if ?FLOATREGS
	test [f80x87], F80X87_STATE_SAVED
	jz readreg_1
	frstor [frsave]
	call restoredebuggeefpustate
	and [f80x87], not F80X87_STATE_SAVED
readreg_1:
endif
	test [fEntry], FENTRY_REAL
	jz @F
	push ds
	pop es
	mov edi, offset rmcsi0y
	mov esi, offset r1
	mov ecx, 8
	rep movsd
	mov esi, offset r1r
	mov cl, 9
	rep movsw
	jmp rr_1
@@:
	mov esi,[r1.rEsi]
	mov edi,[r1.rEdi]
	mov ebp,[r1.rEbp]
	mov eax,[r1.rEax]
	mov ebx,[r1.rEbx]
	mov ecx,[r1.rEcx]
	mov edx,[r1.rEdx]
rr_1:    
	mov es,[r1.rES]
	mov fs,[r1.rFS]
	mov gs,[r1.rGS]
	ret
ReadReg endp

eaxout proc stdcall public
	@dwordout eax
	ret
eaxout endp

FloatToStr proto stdcall :dword, :dword

;*** display value of a symbol (EBX=>)
;*** typ in cl:
;***  ESI=> eintrag in symboltabelle
;*** aufgerufen von: symtout, _regsout, _show

symout proc stdcall public
	and cl, 0DFh		; reset read/only flag
	test cl, _FNCALL_	; funktionsaufruf?
	jz symout_1
	and cl, -_FNCALL_-1
	mov dword ptr [tmpvar], esp
	test byte ptr [esi.SYMBOL.bType2],_PARMOK_
	jz @F
	mov eax, [esi.SYMBOL.dwAddr]
	and eax, eax
	jz @F
	push [eax]
@@:
	mov al, 2		  ;modus: alle werte holen
	call [esi.SYMBOL.dwProc] ;esi=parameter (normalerweise symboladresse)
	mov ebx, offset tmpvar
	mov esp, [ebx]
	jc symout_void
	mov [ebx+0], eax
	mov [ebx+4], edx
symout_1:
	cmp cl, __LIST__
	jz symout_ls
	cmp cl, __STRING__
	jz symout_str
	cmp cl, __LSTRING__
	jz symout_lstr
	cmp cl, __CONST__
	jz symout_dw
	cmp cl, __DWORD__
	jz symout_dw
	cmp cl, __WORD__
	jz symout_w
	cmp cl, __BYTE__
	jz symout_b
	cmp cl, __FWORD__
	jz symout_fw
	cmp cl, __FPTR__
	jz symout_fptr
	cmp cl, __QWORD__
	jz symout_qw
	cmp cl, __TBYTE__
	jz symout_tbyte
if ?XMMREGS        
	cmp cl, __QWORD__
	jz symout_ow
endif        
	cmp cl, __CHAR__
	jz symout_char
	cmp cl, __BOOL__
	jz symout_bool
	cmp cl, __LPTR__
	jz symout_lptr
	cmp cl, __RMLPTR__
	jz symout_rmptr
	cmp cl, __VOID__
	jz symout_void
	ret
symout_ls:
	mov cl, [ebx+4]
	and cl, cl
	jz symout_lsex
	mov ebx, [ebx]
@@:
	mov al, [ebx]
	invoke _hexout
	@putchr ' '
	inc ebx
	dec cl
	jnz @B
symout_lsex:
	ret
symout_str:
	cmp dword ptr [ebx], 0
	jz @F
	@strout [ebx]
@@:
	ret
symout_dw:
	@dwordout [ebx]
	ret
symout_w:
	@wordout [ebx]
	ret
symout_b:
	mov al, [ebx]
	invoke _hexout
	ret
symout_fw:
symout_fptr:
	@wordout [ebx+4]
	@putchr ':'
	@dwordout [ebx+0]
	ret
symout_lptr:
symout_rmptr:
	@wordout [ebx+2]
	@putchr ':'
	@wordout [ebx+0]
	ret
symout_lstr:			; ausgabe Byteliste ^EBX mit laenge CH
	push ecx			; wird z.Z. nicht verwendet (stattdessen __LIST__)
	mov al, [ebx]
	invoke _hexout
	@putchr ' '
	pop  ecx
	inc ebx
	dec ch
	jnz symout_lstr
	retn
symout_tbyte:
if 0
	@wordout [ebx+8]
	@putchr '.'
	@dwordout [ebx+4]
	@dwordout [ebx+0]
else
	sub esp, 32
	invoke FloatToStr, ebx, esp
	invoke printf, CStr("%-22s"),esp
	add esp, 32
endif
	retn
symout_qw:
	@dwordout [ebx+4]
	@putchr '-'
	@dwordout [ebx+0]
	retn
if ?XMMREGS        
symout_ow:
	push ebx
	add ebx, 8
	call symout_qw
	pop ebx
	@putchr '-'
	call symout_qw
	retn
endif        
symout_char:
	@putchr [ebx]
	retn
symout_bool:
	mov al, [ebx]
	and al, al
	jz @F
	@stroutc "true"
	retn
@@:
	@stroutc "false"
	retn
symout_void:
	@stroutc "(void)"
	retn

symout endp


allocflat proc stdcall public bytes:dword

	mov eax, [flatheap]
	and eax, eax
	stc
	jz exit
	mov ecx,bytes
	add ecx,eax
	mov [flatheap],ecx
exit:
	ret
allocflat endp

freeflat proc stdcall public pMem:dword
	mov eax, pMem
	mov [flatheap], eax
	ret
freeflat endp

;*** status sichern ***
;--- then switch to debuggee screen!

SaveState proc stdcall public

local	myaddr:dword

	mov ecx, [sdalen1]
	add ecx, lclientarea+8*4
	add ecx, ?DBSTACK
if ?SAVECURDIR        
	add ecx,72				;current drive + dir
endif        
	invoke allocflat, ecx
	jc error
	mov edi,eax
	mov myaddr,eax
	call MarkDOSused 		;DOS Status mitsichern, deb16-PSP setzen
ife ?FLAT		 
	mov es,[__flatsel]
endif
	mov eax,edi

	xchg eax,[pSaveState]
	stosd
	lea eax,[ebp+2*4]		;das ist esp
	stosd
	mov eax,[ebp]
	stosd

	mov esi,offset top_of_stack - ?DBSTACK
	mov ecx,?DBSTACK/4
	rep movsd

	mov esi,offset clientarea
	mov ecx,lclientarea/4
	rep movsd
	call getpsp
	@tprintf <"SaveState: save psp=%X",lf>, eax
	stosd
	call getdta
	@tprintf <"SaveState; save dta=%X:%X",lf>, edx, eax
	stosd
	mov eax,edx
	stosd
if ?SAVECURDIR
	mov ah, 19h
	@DosCall
	stosd
	push ds
	mov al,'\'
	stosb
if ?32BIT
	mov esi, edi
	push es
	pop ds
else
	invoke setworkselbase, edi
	xor esi, esi
	mov ds, worksel
endif
	mov dl, 0			;for default drive only
	mov ah, 47h
	@DosCall
if _TRACE_
	pushfd
	pop ecx
	@tprintf <"save curdir=%ls, fl=%X",lf>, esi, ds, ecx
endif
	add edi, 68-1
	pop ds
endif
	mov al, [fDOSused]
	stosd
	test al,1				;muss SDA auch gesichert werden?
	jz savestate_1

	mov ecx,[sdalen1]
	mov esi,[pSdaSave]
	and esi,esi
	jz savestate_1
ife ?FLAT
	push ds
	mov ds,[__flatsel]
	rep movsb
	pop ds
else
	rep movsb
endif
savestate_1:
	mov eax,myaddr			;adresse flat heap (fuer freigabe)
	stosd
	inc [cLoad]
if ?RESETVIDEOONSAVESTATE
	call SwitchToDebuggeeScreen
endif
	call restoreenvironment
	call restoredebuggeefpustate

	and [fDOSused], not (FDU_SAVED or FDU_PSPSWITCHED)
	mov [fEntry], 0

	mov [dwCurPsp],0

	push ds
	pop es
if ?WINDOWS
	@tprintf <"SaveState exit",lf>
endif
	clc
exit:
	ret
error:
	invoke printf, CStr("out of memory",10)
	stc
	ret
SaveState endp

;--- dont change eax, edx, ebx!

switch2hoststack proc public
if ?USEHOSTSTACK
	pop ecx
	lss esp,[hoststack]
	sub dword ptr [hoststack], ?HSTACKCORR
	jmp ecx
else        
	ret
endif
        
switch2hoststack endp

;--- dont change eax and efl!

switch2debuggerstack proc public
if ?USEHOSTSTACK
	pop ecx
	push ds
	pop ss
	mov esp,[myesp]
	push ecx
	pushfd
	add dword ptr [hoststack], ?HSTACKCORR
	popfd
endif
	ret
        
switch2debuggerstack endp

;*** loadstate: restore a state stored with SaveState

LoadState proc stdcall public

	@tprintf <"LoadState: enter",lf>
	mov esi, [pSaveState]
	and esi, esi
	stc
	jz exit
	and [fDOSused], not FDU_PSPSWITCHED
	call MarkDOSnotused
	mov [dwCurPsp],0
	call setmyenvironment
	call setdebuggerfpustate
ife ?FLAT
	push ds
	pop es
	mov ds,[__flatsel]
endif
	lodsd
	mov es:[pSaveState], eax
	pop ebx					; get return addr into ebx
	lodsd
	mov esp, eax
	lodsd
	mov ebp, eax

	mov edi, offset top_of_stack - ?DBSTACK
	mov ecx, ?DBSTACK/4
	rep movsd

	mov edi, offset clientarea
	mov ecx, lclientarea/4
	rep movsd

	push ebx 				; push return on new esp

	lodsd					; PSP
	verw ax
	jnz @F
	mov es:[dwCurPsp], eax
	@tprintf <"LoadState: load psp=%X",lf>,eax
	mov ebx, eax
	mov ah, 50h
	@DosCall
@@:
	lodsd					; DTA
	mov edx, eax
	lodsd
	mov dword ptr es:[dfCurDta+0],edx
	mov word  ptr es:[dfCurDta+?SEGOFFS], ax
	@tprintf <"LoadState: load dta=%X:%X",lf>, eax, edx
if ?SAVECURDIR
	lodsd
	mov dl, al
	mov ah, 0eh
	@DosCall
 if ?32BIT
	mov edx, esi
 else
	push ds
	mov ds, cs:[__csalias]
	invoke setworkselbase, esi
	mov ds, [worksel]
	xor edx, edx
 endif
	mov ah, 3bh
	@DosCall
 if _TRACE_
	pushfd
	pop ecx
 endif
	@tprintf <"LoadState: restore curdir=%ls, fl=%X",lf>, edx, ds, ecx
	add esi, 68
 ife ?32BIT
	pop ds
 endif
endif        
	lodsd
	mov es:[fDOSused], al
	test al, 1			 ;muss SDA auch restauriert werden?
	jz loadstate_1

	mov ecx, cs:[sdalen1]
	mov edi, cs:[pSdaSave]
	and edi, edi
	jz loadstate_1
ife ?FLAT
	push es
	push ds
	pop es
	rep movsb
	pop es
else
	rep movsb
endif
loadstate_1:
	lodsd
ife ?FLAT
	push es
	pop ds
endif
	invoke freeflat, eax
	dec [cLoad]
	clc
exit:
	ret
LoadState endp

;--- check if address in cx:edx points to free dos mem
;--- CX may be null, then edx is a linear address

IsAddrInFreeDosMem proc stdcall uses ebx esi edi

	mov esi, edx
	jcxz @F
	mov ebx, ecx
	invoke getbaser, ebx
	cmc
	jnc exit
	add esi, eax
@@:
	cmp esi, 110000h	;is address in dos memory?
	jnc exit
	xor edi, edi
	.while (1)
		invoke GetNextMcb, edi
		.break .if (CARRY?)
		mov edi, eax
		lea edx, [edx+eax+10h]
		.break .if (esi < eax)
		.if (esi < edx)
			.if (word ptr @flat:[eax+1] == 0)
				stc
				jmp exit
			.endif
		.endif
	.endw
	clc
exit:
	ret
IsAddrInFreeDosMem endp

;--- checkpmints is called by _loadpgm
;--- scan all pm ints if they are still valid

checkpmints proc stdcall public

local intval:FWORD

	push [excexit]
	mov [excexit], offset error
	mov bl, 0
nextitem:
	pushad
	xor edx, edx
	@DpmiCall 204h
	jc error
ife ?32BIT
	movzx edx, dx
endif
	mov dword ptr [intval], edx
	mov word ptr [intval+4], cx
	lsl eax, ecx
	jnz error
	cmp eax, edx
	jc error
	test [fStat], FSTAT_ISNT	; doesnt work on NT platforms
	jnz @F
	mov fs, ecx
	mov al, fs:[edx]
@@:
	call IsAddrInFreeDosMem
	jnc skipitem
	mov eax, WRN_PMINT_IN_FREE_DOSMEM
	jmp errorx
error:
	mov eax, MSG_INVALID_PMINT
errorx:
	movzx ebx, bl
	push ebx
	@errorout eax
	add esp, 1*4

	mov eax, dword ptr [intval+0]
	mov ax, word ptr [intval+4]
ife ?WINDOWS
	.if ([cLoad] == 0)
		xor ecx, ecx
		.if (bl == 31h)
 if ?WATCHI31
  if ?CS16ALIAS
			mov ecx, [__cs16alias]
  else
			mov ecx, cs
  endif                
			mov edx, myint31
 else
			mov cx, word ptr oldint31+?SEGOFFS
			mov edx, dword ptr oldint31+0
 endif
		.elseif (bl == 21h)
 if ?WATCHI21
  if ?HIDEINT21
			mov ecx, dword ptr oldint21
			mov dword ptr oldi21p, ecx
   if ?32BIT
			mov cx, word ptr oldint21+4
			mov word ptr oldi21p+4, cx
   endif                
  endif
  if ?CS16ALIAS
			mov ecx, [__cs16alias]
  else
			mov ecx, cs
  endif                
			mov edx, myint21
 else
			mov cx, word ptr oldint21+?SEGOFFS
			mov edx, dword ptr oldint21+0
 endif
 if ?SAVEINT08
		.elseif (bl == 8h)
			mov edx, dword ptr oldint08+0
			mov cx, word ptr oldint08+?SEGOFFS
		.elseif (bl == 1Ch)
			mov edx, dword ptr oldint1C+0
			mov cx, word ptr oldint1C+?SEGOFFS
 endif
		.elseif (bl == 9h)
			call kbd_checkpmints
		.endif
		.if ecx
			mov ax, 205h
			@DpmiCall
			mov edx, dword ptr intval
			movzx ecx, word ptr intval+4
			invoke printf, CStr("int %02X repaired (invalid value was %X:%X) ",10), ebx, ecx, edx
		.endif
	.endif
endif ; ife ?WINDOWS
skipitem:
	popad
	inc bl
	jnz nextitem
	pop [excexit]
	call checkrmints
	ret
checkpmints endp

checkrmints proc stdcall public
	mov bl,0
	.while (bl < 30h)
		mov ax,0200h
		@DpmiCall
		movzx ecx, cx
		shl ecx, 4
		movzx edx, dx
		add edx, ecx
		xor ecx,ecx
		call IsAddrInFreeDosMem
		.if (CARRY?)
			movzx ebx, bl
			push ebx
			@errorout WRN_RMINT_IN_FREE_DOSMEM
			add esp,1*4
		.endif
		inc bl
	.endw
	ret
checkrmints endp

_cleardos proc c pb:PARMBLK
	xor edi, edi
	.while (1)
		invoke GetNextMcb, edi
		.break .if (CARRY?)
		mov edi, eax
		mov ax,@flat:[edi+1]
		.if (!ax)
			push edi
			lea edi, [edi+10h]
			mov ecx, edx
			shr ecx, 2
			mov eax, ebx
ife ?FLAT
			push es
			push @flat
			pop es
			rep stosd
			pop es
else
			rep stosd
endif
			pop edi
		.endif
	.endw
	ret
_cleardos endp

preparecmdline proc stdcall public
	mov ebx,offset cmdline
	cmp cl,__STRING__
	jnz preparecmdline_ex
if 0
	@stroutc "cmdline:"
	mov ebx,esi
	invoke __stroutebx		; modifies ebx, eax
	invoke _crout
endif
	mov edi,[pNearHeap]
	mov ebx,edi
	inc edi
	mov cl,-1
@@:
	lodsb
	stosb
	inc cl
	and al,al
	jnz @B
	inc edi
	mov [pNearHeap],edi
if ?USELOADMODULE
	inc ebx
else
	mov word ptr [edi-2],cr
	mov [ebx],cl
endif
preparecmdline_ex:
	ret
preparecmdline endp

addnotifyactive proc stdcall bit:dword
	push eax
	mov eax, bit
	bts notifyactive, eax
	pop eax
	ret
addnotifyactive endp
delnotifyactive proc stdcall bit:dword
	push eax
	mov eax, bit
	btr notifyactive, eax
	pop eax
	ret
delnotifyactive endp

;*** Load cmd

_loadpgm proc c pb:PARMBLK

local	cmdshow:dword
local	tmplmode:dword

	mov al,1								; default: stop at program, but not at dll entries
	cmp pb.p3.bType, __CONST__
	jnz @F
	mov al,byte ptr pb.p3.dwOffs
@@:
	mov tmplmode, eax
	test al, 1
	jz @F
	invoke addnotifyactive, DS_StartTask	; trap programm entry
@@:
	test al, 2
	jz @F
	invoke addnotifyactive, DS_LOADDLL		; trap dll entry
@@:
	test al, 4
	jz @F
	invoke addnotifyactive, DS_DELMODULE	; trap module delete
@@:
ife ?WINDOWS
	push eax
	test al,10h
	setz al
	movzx eax,al
	call rmbreak	; set flag NOTACTIVE in rmdbghlp
	pop eax
endif
	cmp cl,__STRING__
	jz @F
	@errorout ERR_PGMNAME_EXPECTED
	jmp exit
@@:
if ?WINDOWS
	cmp [cLoad], 00
	jz @F
	@errorout ERR_DEBUGGER_NOT_CURRENT_TASK
	jmp exit
@@:
endif
	call install_i41

	or [fExit], FEXIT_LOADPGM

	call SetTheBreaks
	call ActAllHWBreaks

	call SaveState			; this should activate debuggee screen
	jc loadpgm_er1
if 1
	test [f80x87], F80X87_STATE_SAVED
	jz @F
	frstor [frsave]
@@:
endif
;-----------------------------------------
ife ?WINDOWS
 if ?MARKPSP
	push ds
	mov ds, [_psp]
	and byte ptr ds:[4Fh], not 80h	; isn't this a hack for dkrnl32?
	pop ds
 endif
	cmp bLdr,0
	jz @F
	mov cx, [wLdrMask]		;mask
	mov dx, [wLdrFlgs]		;set DPMILDR=
	mov ax, 4b94h			;set loader flags
	@DosCall
	xor dx, dx
	mov ax, 4b93h			;cause the loader to display load errors
	@DosCall
@@:        
 ife ?LDRDISABLE
	call setloaderstate
 endif
endif
	mov al, byte ptr tmplmode
	mov fLoadMode, al

	mov esi, pb.dwOffs2
	mov cl, pb.p2.bType
	call preparecmdline
	mov edi, offset params
if ?32BIT
;--- 32-bit EXECPGM struct ( without environment )
	mov eax, ebx 			; parameter
	stosd
	mov eax, ds
	stosd
	mov eax, offset fcb		; 1. FCB
	stosd
	mov eax, ss
	stosd
	mov eax, offset fcb		; 2. FCB
	stosd
	mov eax, ss
	stosd
else
  if ?EXPLENV
	mov eax,[_environ]
  else
	xor eax,eax
  endif
	stosw					; no environment
	mov eax, ebx
	stosw					; cmdline
	mov eax, ds
	stosw
  if ?USELOADMODULE
	mov eax, offset cmdshow
	mov dword ptr cmdshow, 00000002
	stosw
	mov eax, ss
	stosw
	xor eax, eax
	stosd
  else
	mov eax, offset fcb
	stosw
	mov eax, ss
	stosw
	mov eax, offset fcb
	stosw
	mov eax, ss
	stosw
  endif
endif
if ?TRAPRM214B
	cmp fLoadMode, 0
	jz @F
	call traprm21
@@:
endif
	mov edx, pb.dwOffs1
	mov ebx, offset params
	push ds
	pop es
	mov fs, r1.rFS
	mov gs, r1.rGS
	call switch2hoststack
if ?USELOADMODULE
	@savewinsegregs
	mov eax,ds
	push ax
	push dx
	mov eax,es
	push ax
	push bx
	call _LoadModule
	@restorewinsegregs
else
	or [fExit], FEXIT_TRACE		; kein protokoll bei rm entry
	mov ax,4B00h
;	 @DosCall
	int 21h 					; hier int 21h verwenden!

endif
;----------------------------------------------------------

	mov ds,cs:[__csalias]
	push ds
	pop es
	@loadflat
	call switch2debuggerstack
if ?USELOADMODULE
	cmp ax,0020h
endif
	jnc @F
	movzx eax,ax
	push eax
	@errorout ERR_UNSUCCESSFUL_LOAD
	jmp loadpgm_1
@@:
if ?WINDOWS eq 0
	push eax
	mov ah,4Dh
	@DosCall
	movzx eax,ax
	mov [dwLastRC],eax
	push eax
	@errorout MSG_PGM_TERMINATED
	pop eax
else
	cmp hInstDbgee,0
	jz @F
	invoke addnotifyactive, DS_EXITCALL	;end task
	invoke delnotifyactive, DS_DELMODULE
	invoke delnotifyactive, DS_LOADDLL
	invoke delnotifyactive, DS_StartTask
	call LoadState
	call deinstall_i41
	jmp _reset
@@:        
	movzx eax,ax
	push eax
	@errorout MSG_DLL_LOADED
	pop eax
endif
loadpgm_1:

if ?FLOATREGS
ife ?WINDOWS
	test [f80x87], F80X87_FPU_PRESENT
	jz @F
	fnstsw ax
	fnclex
	test ax, 03Fh
	jz @F
	movzx eax, ax
	push eax
	@errorout MSG_FPU_EXCEPTION
	pop eax
@@:
endif
endif
if ?TRAPRM214B
	cmp fLoadMode,0
	jz @F
	call untraprm21
@@:
endif
	call LoadState
loadpgm_er1:
	invoke delnotifyactive, DS_DELMODULE
	invoke delnotifyactive, DS_LOADDLL
	invoke delnotifyactive, DS_StartTask

	call DeactAllHWBreaks
	call ResetBreaks
	call ClearAllAutoBreaks

	and [fMode], not FMODE_EXEACTIVE

	call deinstall_i41

	call checkpmints

	jmp mains
exit:
	ret
_loadpgm endp

;*** install int 41 handler (proc myint41)
;*** in Windows myint41 is no interrupt proc but a normal proc
;*** that is called by either ToolHelp.Dll or the kernel (if TOOLHELPHOOK is active)
;*** might be changed by setting "?USEINT41 equ 1".

install_i41 proc stdcall

	cmp cVisible_i41, 0
	jz exit
if ?WINDOWS
	test [fToolHelp],FTH_REGISTERED		; NotifyRegister aktiv?
	jnz exit							; dann nix tun
endif
	inc byte ptr cInstalled_i41
	cmp byte ptr cInstalled_i41,1
	jnz exit
if ?USEINT41        
	mov bl, 41h
	mov ax, 204h
	int 31h
	mov dword ptr oldint41+0,edx
	mov word ptr oldint41+?SEGOFFS,cx
  if ?CS16ALIAS
	mov ecx, [__cs16alias]
	mov edx, offset myint41_16
  else
	mov ecx, cs
	mov edx, offset myint41
  endif        
	mov ax, 205h
	int 31h
else

  if ?WINDOWS
	@savewinsegregs
    if ?CS16ALIAS        
	mov eax, __cs16alias
    else
	mov eax, cs
    endif
	shl eax, 16
	mov ax, lowword offset thhookcb
	push eax
	call _ToolhelpHook
	@restorewinsegregs

	mov dword ptr oldint41+0, eax
	mov word ptr oldint41+?SEGOFFS, dx
  endif
endif  
if ?SUPI4112
	invoke addnotifyactive, DS_Out_Str16	; 0012: 16bit string in ds:si
endif
	invoke addnotifyactive, DS_ForcedGO16	; forced GO for 16-bit
if ?WINDOWS
	invoke addnotifyactive, 63h				; get ctrl-alt-sysreq events
endif
exit:
	ret
install_i41 endp

;*** int 41 handler deinstallieren ***

deinstall_i41 proc stdcall

	cmp cVisible_i41, 0
	jz exit
if ?WINDOWS
	test [fToolHelp],FTH_REGISTERED		; NotifyRegister aktiv?
	jnz exit							; dann nix tun
endif
	cmp byte ptr cInstalled_i41,0
	jz exit
	dec byte ptr cInstalled_i41
	jnz exit
if ?USEINT41        
	mov cx,word ptr oldint41+?SEGOFFS
	mov edx,dword ptr oldint41+0
	mov bl,41h
	mov ax,0205h
	int 31h
else        
  if ?WINDOWS
	@savewinsegregs
	push word ptr oldint41+2
	push word ptr oldint41+0
	call _ToolhelpHook
	@restorewinsegregs
  endif
endif  
	xor eax,eax
	mov word ptr oldint41+?SEGOFFS,ax
exit:
	ret
deinstall_i41 endp

;*** interrupt 41 handler
;*** in al eventtyp
;*** hier interessieren:
;*** 40=breakpoint cx:bx
;*** 50/150=load segments (si=type, dx:ebx=params)
;*** 52/152=free segments (152=bx=segment#, dx:edi=module name)
;*** 59=Load App
;*** 63=Ctrl-Sysreq pressed
;*** 64/164=Load Dll, entry cx;e/bx 
;*** F003=breakpoint cx:ebx

;--- please note that SS isnt necessarily FLAT!

;--- for windows this is ToolhelpHook callback
;--- thats why a 16bit retf has to be used
;--- Toolhelphook is NOT active if NotifyRegister was called

thhookcb proc
if 0
	mov ah, 0eh
	mov al, 07h
	int 10h
endif
thhookcb endp

myint41_16:
	@switch32bit
myint41 proc

	push gs
	push fs
	push es
	push ds
	pushad

	@loadesp ebp

INT41FR struct
	PUSHADS <>
rDS dd ?
rES dd ?
rFS dd ?
rGS dd ?
ife ?32BIT
  ife ?WINDOWS  ; don't use for deb16fw!
rIP dw ?
rCS dw ?
rFlags  dw ?
  endif
else
rEIP   dd ?
rCS    dd ?
rFlags dd ?
endif
INT41FR ends

	mov ds,cs:[__csalias]
	push ds
	pop es
	@loadflat

	or [fVideo], FVIDEO_NOSWITCH
if ?32BIT
 if 0
	cmp ax, 0F003h			; force 32bit GO?
	jz intr41_F003
 endif
	cmp ax, DS_CheckFault	; check fault
	jz intr41_007F
	cmp ax, DS_TrapFault	; fault
	jz intr41_0083
endif        
	cmp ax,004Fh			; debugger loaded?
	jz intr41_004F
        
	push eax
	movzx eax,al			; clear bit 8 (so the 32-bit versions are covered as well)
	bt notifylog,eax
	pop eax
	jnc printdone
	mov [cLines], 1			; don't stop in video out!

	cmp ax, DS_Out_Str
	jz intr41_0002
if ?SUPI4112
	cmp ax, DS_Out_Str16
	jz intr41_0012
endif
;if ?32BIT
	cmp ax, DS_LoadSeg_32	; load 32bit segments
	jnz func150
	mov fs, edx
	assume ebx:ptr D386_Device_Params
	movzx eax, fs:[ebx].DD_logical_seg
	mov ecx, fs:[ebx].DD_base
	mov edx, fs:[ebx].DD_length
	mov esi, dword ptr fs:[ebx].DD_sym_name
	mov di, word ptr fs:[ebx].DD_sym_name+4
	invoke printf, CStr("Segment load(32): %2X %8X %8X %ls",10), eax, ecx, edx, edi::esi
	assume ebx:nothing
	jmp printdone
func150:
	cmp ax, DS_FreeSeg_32	; free 32bit segments
	jnz func152
	movzx ebx,bx
	invoke printf, CStr("Segment free(32): %2X %ls",10), ebx, edx::edi
	jmp printdone
func152:
if ?32BIT        
	cmp ax, DS_LOADDLL+100h
	jnz func164
	movzx ecx, cx
	invoke printf, CStr("Load Dll(32): Module=%X, CS:EIP=%X:%X",10), edx, ecx, ebx
	jmp printdone
func164:        
endif
	cmp ax, DS_LoadSeg
	jnz func50
	movzx ebx, bx
	movzx ecx, cx
	movzx esi, si
	movzx edi, di
	mov edx, [ebp].INT41FR.rES
	invoke printf, CStr("Segment load(16): Segm#=%2u Sel=%4X Flags=%4X %.12ls",10), ebx, ecx, esi, edx::edi
	jmp printdone
func50:        
	cmp ax, DS_FreeSeg
	jnz func52
	movzx ebx,bx
	invoke printf, CStr("Segment free(16): Sel=%4X",10), ebx
	jmp printdone
func52:        
	cmp ax, DS_StartTask
	jnz func59
	movzx edx, word ptr [ebp].INT41FR.rES
	movzx ecx, cx
	movzx ebx, bx
	invoke printf, CStr("Start Task(16): Module=%X, CS:IP=%X:%X",10), edx, ecx, ebx
	jmp printdone
func59:        
	cmp ax, DS_LOADDLL
	jnz func64
	movzx edx, word ptr [ebp].INT41FR.rES
	movzx ecx, cx
	movzx ebx, bx
	invoke printf, CStr("Load Dll(16): Module=%X, CS:IP=%X:%X",10), edx, ecx, ebx
	jmp printdone
func64:        
	cmp ax, DS_DELMODULE
	jnz func65
	movzx ecx, word ptr [ebp].INT41FR.rES
	mov fs, ecx
	movzx edx, word ptr fs:[0026h]
	inc edx
	invoke printf, CStr("Delete Module(16): Handle=%4X %.8ls",10), ecx, ecx::edx
	jmp printdone
func65:        
if ?WINDOWS
	cmp ax, DS_EXITCALL			; exit task
	jnz func62
	call _GetCurrentTask
	mov edx, [ebp].INT41FR.rES
	movzx eax, ax
	movzx ebx, word ptr [ebp].INT41FR.rEbx
	invoke printf, CStr("Task terminated: hTask=%X hInst=%X, ExitCode=%X",10), eax, edx, ebx
	jmp printdone
func62:
endif
if ?32BIT
	cmp ax, DS_StartTask+100h
	jnz @F
	movzx edx, word ptr [ebp].INT41FR.rES
	movzx ecx, cx
	invoke printf, CStr("Start Task(32): Module=%X, CS:EIP=%X:%X",10), edx, ecx, ebx
	jmp printdone
@@:
endif
	mov word ptr [ebp].INT41FR.rDS+2, 0
	mov word ptr [ebp].INT41FR.rES+2, 0
	invoke printf, CStr("notify: ax=%X, bx=%X, cx=%X, dx=%X, ds=%X, es=%X",10),\
		eax, ebx, ecx, edx, [ebp].INT41FR.rDS, [ebp].INT41FR.rES

printdone:

	popad
	pushad
	@loadesp ebp

if ?32BIT        
 if 0
	cmp ax, DS_StartTask+100h; Load 32bit app CX:EBX
	jz intr41_0159
 endif
	cmp ax, DS_LOADDLL+100h	; Load 32bit DLL CX:EBX
	jz intr41_0164
	cmp ax, DS_LoadSeg_32	; load 32bit segment
	jnz @F
	test fLoadMode, 8
	jz @F
	or byte ptr [ebp].INT41FR.rFlags+1, 1
	or [fExit], FEXIT_TRACE
@@:
endif

	movzx eax, ax
	cmp ax, 4*32			; bitfield is 4 dwords
	jnc intr41_ex
	bt notifyactive, eax
	jnc intr41_ex

if ?WINDOWS
	cmp ax, DS_StartTask	; Load App
	jz intr41_0059
endif
	cmp ax, 40h				; 16-bit breakpoint CX:BX
	jz intr41_0040
	cmp ax, DS_LOADDLL		; Load 16bit DLL CX:BX
	jz intr41_0064
if ?WINDOWS
	cmp ax, DS_EXITCALL		; exit task
	jz intr41_0062
	test fKeybrd, FKB_SYSREQ
	jz @F
	cmp ax, 63h				; Ctrl-Alt-SysReq (not toolhelp)
	jz intr41_0063
@@:        
endif
	jmp intr41_ex

intr41_004F:
	mov word ptr [ebp].INT41FR.rEax, 0F386h
	jmp intr41_ex
if ?SUPI4112
intr41_0012:
	movzx esi, si
endif
intr41_0002:
	mov fs,[ebp].INT41FR.rDS
	.while (1)
		lodsb fs:[esi]
		.break .if (!al)
		@putchr al
	.endw
	jmp done

if ?WINDOWS        
intr41_0063:
	and [fVideo], not FVIDEO_NOSWITCH
	call SwitchToDebuggeeScreen
	mov gs, [ebp].INT41FR.rGS
	pushfd
	pop eax
	or ah, 1	; set TF
	mov [ebp].INT41FR.rGS, eax
	popad
	pop ds
	pop es
	pop fs
	popfd
	db 66h
	retf
intr41_0062:
	call _GetTaskDS
	cmp ax, hInstDbgee
	jnz intr41_ex
	mov ax, word ptr [ebp].INT41FR.rEbx
	mov wRCDbgee, ax
	call debuggee_terminated
	jmp intr41_ex
endif
        
if ?32BIT
intr41_007F:
	test cx,DEBUG_FAULT_TYPE_FIRST
	jz intr41_ex
	mov word ptr [ebp].INT41FR.rEax,0
	jmp intr41_ex
intr41_0083:
	test [fMode], FMODE_SKIP
	setnz al
	and [fMode], not FMODE_SKIP
	and al,al
	jnz intr41_ex
	or [fEntry], FENTRY_EXCEPTION
	mov [rCSx],ecx
	mov [rEipx],edx
	mov [rEflx],edi
	mov [ebp].INT41FR.rEdx, offset debug_entry
	mov [ebp].INT41FR.rEcx, cs
	movzx ebx,bl
	call excout
	@stroutc " occured - last chance",lf
	or [fException], FEXC_USEXREGS	;restore regs!
	jmp intr41_ex
endif

intr41_0059:		; start 16bit app

if ?WINDOWS
	invoke delnotifyactive, DS_StartTask
	push ecx
	push ebx
	@savewinsegregs
	call _GetTaskDS
	@restorewinsegregs
	mov hInstDbgee, ax
	pop ebx
	pop ecx
endif
intr41_0040:		; 16bit breakpoint
intr41_0064:		; load 16bit dll
	movzx ebx,bx
if ?32BIT        
	jmp intr41_setbreak
intr41_0164:		; load 32bit dll
	test fLoadMode, 2+4	;load/delete?
	jz intr41_ex
 if 0
intr41_0159:		; start 32bit task
 endif
intr41_setbreak:	; 32bit breakpoint
endif
					; CX:(E)BX=CS:(E)IP
	call setmyenvironment	; no register change

	mov eax, ecx
	mov fs, eax				; falls NP -> schon mal einlesen
	mov dl, 00				; typ (prot mode)
	mov cl, FBRP_AUTO or FBRP_CMDSET
	test fLoadMode, 4
	jz @F
	and cl, not FBRP_AUTO	; clear auto reset
@@:        
	test [fStat], FSTAT_ISNT; in NT dont use hwbreaks
	jnz @F					; they cannot be freed anymore
;----------------------------------- try a HW break if available
	or cl, FBRP_USEHW
	mov ch, 00				; for hw breaks specify type "execute"
@@:
	mov edi, offset tModout
	call insertbrkpnt

	call SetTheBreaks

ife ?WINDOWS        
	jnc @F
	or byte ptr [ebp].INT41FR.rFlags+1, 1
	or [fExit], FEXIT_TRACE
@@:
endif
	call ActAllHWBreaks
	call restoreenvironment

intr41_ex:				; standard exit
	and [fVideo], not FVIDEO_NOSWITCH
	call SwitchToDebuggeeScreen
	popad
	pop ds
	pop es
	pop fs
	pop gs
if ?WINDOWS        
 ife ?USEINT41        
	test cs:[fToolHelp], FTH_REGISTERED
	jz @F
	ret
@@:
	cmp word ptr cs:[oldint41+?SEGOFFS], 0
	jnz @F
	db 66h
	retf
@@:
 endif
endif
	jmp cs:[oldint41]

;--- jumped to by functions 0002/0012 (print string)
;--- why???

done:

	and [fVideo], not FVIDEO_NOSWITCH
	call SwitchToDebuggeeScreen		; still required (for page flip)
doneX:
	popad
	pop ds
	pop es
	pop fs
	pop gs
ife ?USEINT41
 if ?WINDOWS
	db 66h		; we are called as ToolhelpHook proc!
	retf
 else
	@iret
 endif        
else
	@iret
endif        
myint41 endp

_i41 proc c pb:PARMBLK
	.if (ebx)
		mov cVisible_i41, 1
		.if (!cInstalled_i41)
			invoke install_i41
		.endif
	.else
		.while (cInstalled_i41)
			invoke deinstall_i41
		.endw
		mov cVisible_i41, 0
	.endif
	ret
_i41 endp

if ?WINDOWS
debuggee_terminated proc        
	mov hInstDbgee,0
	invoke delnotifyactive, DS_EXITCALL
if ?STOPONEXIT
	@savewinsegregs
	push [hMyTask]
	push word ptr 0C002h
	push word ptr 0
	push dword ptr 0
	call _PostAppMessage  
	@restorewinsegregs
endif
	movzx eax, wRCDbgee
	invoke printf, CStr("debuggee terminated, exitcode=%u",10),eax
	call DeactAllHWBreaks
	call ResetBreaks
	call ClearAllAutoBreaks
	ret
debuggee_terminated endp        
endif

if ?USETOOLHELP

_settoolhelp proc c pb:PARMBLK

	mov eax,pb.dwOffs1
	and eax,eax
	setnz al
	mov fUseTH, al
	and eax,eax 				; ausschalten?
	jz @F
	test fToolHelp, FTH_REGISTERED
	jnz exit					; ist bereits ein, also nix tun
	mov  byte ptr cInstalled_i41, 1
	call deinstall_i41			; int 41 handler deinstall falls da
	call toolhelpregister
	jmp exit
@@:
	test fToolHelp, FTH_REGISTERED
	jz exit					; ist bereits aus, also nix tun
	call toolhelpunregister
	call install_i41 		; dafuer int 41 handler installieren
exit:
	ret
_settoolhelp endp

;*** NotifyRegister callback
;*** diese routine wird von Toolhelp aufgerufen
;*** das format der benachrichtigungen ist nicht immer kompatibel
;*** deshalb brauchen einige eine aufbereitung

notifycb proc

	@switch32bit

NFYP struct
dwRet  dd ?
dwData dd ?
wID    dw ?
NFYP ends

;nfy_parm1 equ <[bp+10]> 		;wID
;nfy_parm2 equ <[bp+8]>			;HIWORD(dwData)
;nfy_parm3 equ <[bp+6]>			;LOWORD(dwData)

	push ebp
	movzx ebp, sp

	mov ax, [ebp+4].NFYP.wID
	cmp ah, 0
	jnz exit
	cmp al, NFY_STARTTASK
	jz notifycb_1
	cmp al, NFY_STARTDLL
	jz notifycb_2
	cmp al, NFY_DELMODULE
	jz notifycb_3
	cmp al, NFY_EXITTASK
	jz notifycb_4
	cmp al, NFY_LOADSEG
	jz notifycb_5
	cmp al, NFY_FREESEG
	jz notifycb_6
if 0
	mov cx, [ebp+4].NFYP.dwData+0
	mov bx, [ebp+4].NFYP.dwData+2
	call myint41
endif
	jmp exit
notifycb_1:                 ; STARTTASK
	mov bx, word ptr [ebp+4].NFYP.dwData+0	; IP
	mov cx, word ptr [ebp+4].NFYP.dwData+2	; CS
	mov al, DS_StartTask
	call myint41
	jmp exit
notifycb_2:                 ; STARTDLL
	push es
	les bx, [ebp+4].NFYP.dwData
	mov cx, es:[bx+6]		; CS
	mov bx, es:[bx+8]		; IP
	pop es
	mov al, DS_LOADDLL
	call myint41
	jmp exit
notifycb_3:                 ; DELMODULE
	push es
	mov es, word ptr [ebp+4].NFYP.dwData	; module handle
	mov al, DS_DELMODULE
	call myint41
	pop es
	jmp exit
notifycb_4:                 ; EXITTASK
	mov bx, word ptr [ebp+4].NFYP.dwData	; exit code
	mov al, DS_EXITCALL
	call myint41
	jmp exit
notifycb_5:                 ; LOADSEG
	push si
	push di
	push es
	les bx, [ebp+4].NFYP.dwData
	mov cx, es:[bx+4]		; wSelector
	mov ax, es:[bx+6]		; wSegNum
	mov si, es:[bx+8]		; wType
	mov dx, es:[bx+10]		; wInstance
	les di, es:[bx+12]		; module name
	mov bx, ax
	mov ax, DS_LoadSeg
	call myint41
	pop es
	pop di
	pop si
	jmp exit
notifycb_6:                 ; FREESEG
	mov bx, word ptr [ebp+4].NFYP.dwData	; selector
	mov ax, DS_FreeSeg
	call myint41
exit:
	xor ax,ax
	pop ebp
	db 66h
	retf 6
        
notifycb endp

endif

if ?WINDOWS or ?32BIT

if ?WINDOWS
krnlprocs label word
	dw 513
_LoadLibraryEx32W	PF16 0
	dw 514
_FreeLibrary32W		PF16 0
	dw 515
_GetProcAddr32W		PF16 0
	dw 517
_CallProc32W		PF16 0
	dw 0

getkrnlprocs32 proc stdcall

	cmp _CallProc32W,0
	jnz exit
	@savewinsegregs
	mov eax, cs				   ;get krnl386 handle
	movzx eax, ax
	push eax
	call _GetModuleHandle	;returns kernel module in DX
	push esi
	push edi
	mov edi, edx
	mov esi, offset krnlprocs
nextitem:        
	lodsw
	movzx eax, ax
	and eax, eax
	jz done
	push di
	push eax
	call _GetProcAddress
	mov word ptr [esi+0], ax
	mov word ptr [esi+2], dx
	or ax, dx
	jz done
	add esi, 4
	jmp nextitem
done:
	pop edi
	pop esi
	@restorewinsegregs
	or ax, dx
	cmp ax, 1
exit:        
	ret
getkrnlprocs32 endp

endif

loaddll32 proc stdcall pName:dword

if ?WINDOWS
	call getkrnlprocs32
	jc exit
	@savewinsegregs
	mov eax,ds
	push ax
	mov eax,pName
	push ax
	push 0						; dword 0
	push 0						; dword 0
	call _LoadLibraryEx32W		; LoadLibraryEx32W(pName,0,0)
	push dx						
	push ax
	pop eax 					; return in EAX
	@restorewinsegregs
	ret
exit:
	@errorout ERR_LOADLIBRARY32W
	ret
else
 if ?LDRDISABLE
	call chkloader
	jc error
 endif
	mov fs, r1.rFS
	mov edx, pName
	mov ax, 4b00h
	@DosCall
	jc error
	ret
error:
	xor eax, eax
	ret
endif
loaddll32 endp


freedll32 proc stdcall public hModul:dword

if ?WINDOWS
	call getkrnlprocs32
	jc exit
	@savewinsegregs
	push hModul
	call _FreeLibrary32W			; FreeLibrary32W(handle)
	@restorewinsegregs
	ret
exit:
	@errorout ERR_FREELIBRARY32W
	ret
else
 if ?LDRDISABLE
	call chkloader
	jc exit
 endif
	mov fs, r1.rFS
	mov edx, hModul
	mov ax, 4b80h
	@DosCall
	jc exit
	ret
exit:
	ret
endif
freedll32 endp

;--- getprocaddress (32bit)
;--- dpmildr32 expects a linear address in edx!

getproc32 proc stdcall hModule32:dword, procname:dword, bByNumber:dword

if ?WINDOWS
	call getkrnlprocs32
	jc exit
	@savewinsegregs
	push hModule32
	cmp byte ptr bByNumber, 1
	jnz @F
	mov eax,procname
	push eax
	jmp label1
@@:
	mov eax, ds
	push ax
	mov eax, procname
	push ax
label1: 
	call _GetProcAddr32W	   ;GetProcAddr32W(DWORD hModule32,DWORD procnr)
	push dx
	push ax
	pop eax
	@restorewinsegregs
	ret
exit:
	@errorout ERR_GETPROCADDR32W
	ret
else
 if ?LDRDISABLE
	call chkloader
	jc exit
 endif
	mov ebx, hModule32
	mov edx, procname
	mov ax, 4b81h
	@DosCall
	jc exit
	ret
exit:
	ret
endif
getproc32 endp

;--- call a proc in a win32 dll
;--- CallProc32W is of FAR PASCAL type!

callproc32 proc stdcall public procaddr:dword,param:dword

if ?WINDOWS
	call getkrnlprocs32
	jc exit
	@savewinsegregs
	push param			; parameter
	push procaddr		; procaddr
	push 0				; convert?
	push 1				; number of parameters
	call _CallProc32W	; CallProc32W(DWORD parm,procadr,0,1)
	push dx
	push ax
	pop eax
	@restorewinsegregs
	ret
exit:
	@errorout ERR_CALLPROC32W
	ret
else
 if ?LDRDISABLE
	call chkloader
	jc exit
 endif
	mov edx, procaddr
	mov ebx, param
	mov ax, 4b84h
	@DosCall
exit:
	ret
endif
callproc32 endp

if ?WINDOWS

;--- call a 32bit proc with 2 parameters
;--- CallProc32W is of FAR PASCAL type!

callproc32_2 proc stdcall public procaddr:dword,param1:dword,param2:dword

	call getkrnlprocs32
	jc exit
	@savewinsegregs
	push param1
	push param2
	push procaddr		; procaddr
	push 0				; convert?
	push 2				; number of parameters
	call _CallProc32W	; CallProc32W(DWORD parm,procadr,0,1)
	push dx
	push ax
	pop eax
	@restorewinsegregs
	ret
exit:
	@errorout ERR_CALLPROC32W
	ret
callproc32_2 endp
endif

;*** 32bit-DLL laden ***

_loaddll32 proc c pb:PARMBLK

	invoke loaddll32,pb.dwOffs1
	invoke printf, CStr("Handle: %X",lf), eax
	ret
_loaddll32 endp

;*** display address of a 32Bit proc

_getproc32 proc c pb:PARMBLK

	mov edx, pb.dwOffs2
ife ?WINDOWS
ife ?FLAT
	add edx, MyBase
endif
endif
	invoke getproc32, pb.dwOffs1, edx, 0
if 0
	push eax
	@dwordout pb.dwOffs1
	invoke _crout
	mov ebx,pb.dwOffs2
	invoke __stroutebx		; modifies ebx, eax
	invoke _crout
	pop eax
endif
	invoke printf, CStr("%X",lf), eax
	ret
_getproc32 endp

endif

ife ?WINDOWS
 if ?LDRDISABLE
chkloader proc public
	cmp bLdr, 0
	jz @F
	ret
@@:
	@errorout ERR_NOLOADER
	stc
	ret
chkloader endp
 endif
endif

;*** function MYDS

getds proc near
	mov eax, ds
	ret
getds endp

;*** function MYCS

getcs proc near
	mov eax, cs
	ret
getcs endp

;*** function DOS_Version

getdosversion proc near
	mov ax,wDosVersion
	xchg ah,al
	ret
getdosversion endp

;*** function CUR_DRive

getcurdrive proc near
	mov ah, 19h
	@DosCallx
	add al, 'A'
	ret
getcurdrive endp

notifyout proc c index:dword

	movzx edx, byte ptr index
	cmp al, 0
	jnz @F
	xor eax, eax
	bt notifylog, edx
	adc eax, eax
	jmp notifyout_ex
@@:
notifyout_1:
	mov ebx, [pNearHeap]
	xor eax, eax
	mov dl, 00
notifyout_2:
	bt notifylog,eax
	jnc @F
	mov [ebx], al
	inc ebx
	inc dl
@@:
	inc al
	cmp al, 00h
	jnz notifyout_2
	mov eax, ebx
	xchg eax, [pNearHeap]
	mov cl, __LIST__
notifyout_ex:
	ret
notifyout endp

setnotify proc c value:dword,index:dword

	movzx edx,byte ptr index
	mov eax,value
	and eax,eax
	jz @F
	bts notifylog,edx
	clc
	jmp setnotify_ex
@@:
	btr notifylog,edx
	clc
setnotify_ex:
	ret
setnotify endp

idtuse proc c index:dword

	push esi
	mov esi, offset oldidtvecs
	cmp al,2
	jnz idtuse_2
	mov ebx, [pNearHeap]
	xor edx, edx
	mov cl, _IDTVECS_
	mov dh, 0
idtuse_3:
	mov ax, word ptr [esi+2]
	and ax, ax
	jz @F
	mov byte ptr [ebx], dh
	inc ebx
	inc dl
@@:
	add esi, 8
	inc dh
	dec cl
	jnz idtuse_3
	mov eax, ebx
	xchg eax, [pNearHeap]
	mov cl, __LIST__
	jmp idtuse_ex
idtuse_2:
	mov ebx, index
	cmp ebx, _IDTVECS_
	jnc idtuse_1
	mov ax, word ptr [ebx*8+esi+2]
	cmp ax, 1
	cmc
	sbb al, al
	clc
	jmp idtuse_ex
idtuse_1:
	mov cl, __VOID__
idtuse_ex:
	pop esi
	ret
idtuse endp

;--- TIDT()

setidttrap proc c tvalue:dword,mindex:dword

	invoke set_reset_idtvec, mindex, tvalue
	ret
setidttrap endp

;---

setetrapval:
	btr eStops, ebx
	or al, 1
	call setexception
	jc setetrapval_1
	bts eTraps, ebx
	test al, 2
	jz @F
	bts eStops, ebx
@@:
	clc
	ret
setetrapval_1:
	ret

;--- function EXC()

etrapslastout proc c index:dword

	cmp al, 2
	jz etrapsout_0
	mov ebx, index
	cmp ebx, 20h
	jnb etrapsout_2
	xor eax, eax
	bt eTraps, ebx
	adc eax, eax
	jmp exit
etrapsout_0:
	mov ebx, [pNearHeap]
	xor edx, edx
	mov al,0
etrapsout_1:
	bt eTraps, edx
	jnc @F
	mov [ebx], dl
	inc ebx
	inc al
@@:
	inc dl
	cmp dl, 20h
	jnz etrapsout_1
	mov dl, al
	mov dh, 0
	mov eax, ebx
	xchg eax, [pNearHeap]
	mov cl, __LIST__
	jmp exit
etrapsout_2:
	mov cl, __VOID__
exit:
	ret
etrapslastout endp

;--- function EXCE()

etrapsfirstout proc c index:dword

	cmp al, 2
	jz etrapsout_0
	mov ebx, index
	cmp ebx, 20h
	jnb etrapsout_2
	xor eax, eax
	bt eFirst, ebx
	adc eax, eax
	jmp exit
etrapsout_0:
	mov ebx, [pNearHeap]
	xor edx, edx
	mov al,0
etrapsout_1:
	bt eFirst, edx
	jnc @F
	mov [ebx], dl
	inc ebx
	inc al
@@:
	inc dl
	cmp dl, 20h
	jnz etrapsout_1
	mov dl, al
	mov dh, 0
	mov eax, ebx
	xchg eax, [pNearHeap]
	mov cl, __LIST__
	jmp exit
etrapsout_2:
	mov cl, __VOID__
exit:
	ret
etrapsfirstout endp

;--- function TEXC()

setextrap proc c value:dword, index:dword

	mov ebx, index
	cmp ebx, 20h
	jnb setextrap_1
	mov eax,value
	and eax,eax
	jz @F
	call setetrapval
	jc setextrap_1
	jmp exit
@@:
	btr eStops,ebx
	btr eTraps,ebx
	jnc exit
	call resetexception
	jmp exit
setextrap_1:
	mov cl,__VOID__
exit2:        
	clc
exit:
	ret
setextrap endp

ife ?WINDOWS
;--- function PMInt()

pmtrapsout proc c index:dword

	mov ebx, index
	cmp al, 0
	jnz @F
	cmp ebx, SIZEOLDPMVECS
	jnc pmtrapsout_2
	xor eax, eax
	bt pmtraps, ebx
	adc eax, eax
	jmp exit
@@:
pmtrapsout_3:
	mov ebx, [pNearHeap]
	xor edx, edx
	mov al, 0
pmtrapsout_0:
	bt pmtraps, edx
	jnc @F
	mov [ebx], dl
	inc al
	inc ebx
@@:
	inc dl
	cmp dl, 20h
	jnz pmtrapsout_0
	mov dh, 0
	mov dl, al
	mov eax, ebx
	xchg eax, [pNearHeap]
	mov cl, __LIST__
	jmp exit
pmtrapsout_2:
	mov cl,__VOID__
exit:
	ret
pmtrapsout endp

;--- function TPMI()

setpmtrap proc c value:dword,index:dword

	mov ebx, index
	cmp ebx, SIZEOLDPMVECS
	jnc setpmtraps_er
	mov eax, value
	and eax, eax
	jz @F
	bts pmtraps, ebx
	cmc
	jnc exit
	call setpmvec
	jmp exit
@@:
	btr pmtraps, ebx
	jnc exit
	call resetpmvec
	jmp exit
setpmtraps_er:
	mov cl, __VOID__
exit:
	ret
setpmtrap endp

endif

;--- function RMI()

rmtrapsout proc c index:dword

	mov ebx, index
	cmp al, 0
	jnz @F
	cmp ebx, 8h
	jnc rmtrapsout_er
	xor eax, eax
	bt rmtraps, ebx
	adc eax, eax
	jmp exit
@@:
rmtrapsout_3:
	mov ebx, [pNearHeap]
	xor edx, edx
	mov al,0
rmtrapsout_0:
	bt rmtraps, edx
	jnc @F
	mov [ebx], dl
	inc al
	inc ebx
@@:
	inc dl
	cmp dl, 20h
	jnz rmtrapsout_0
	mov dh, 0
	mov dl, al
	mov eax, ebx
	xchg eax, [pNearHeap]
	mov cl, __LIST__
	jmp exit
rmtrapsout_er:
	mov cl, __VOID__
exit:
	ret
rmtrapsout endp

;--- function TRMI()

setrmtrap proc c value:dword,index:dword

	mov ebx, index
	cmp ebx, 8h
	jnc setrmtraps_er
	mov eax, value
	and eax, eax
	jz @F
	bts rmtraps, ebx
	cmc
	jnc exit
	call setrmvec
	jmp exit
@@:
	btr rmtraps, ebx
	jnc exit
	call resetrmvec
	jmp exit
setrmtraps_er:
	mov cl, __VOID__
exit:
	ret
setrmtrap endp

;--- function MSW()

getmsw proc c value:dword
	smsw ax
	ret
getmsw endp

;--- function CR0()

getcr0 proc c value:dword

	pushad
	call restoredebuggeefpustate 
	popad
	cmp al,1
	jz @F
	@ring0call getmycr0
	jmp getcr0_ex
@@:
	mov ebx,value
	@ring0call setmycr0
getcr0_ex:
	pushad
	call setdebuggerfpustate 
	popad
	ret
getcr0 endp

getmycr0:
	mov eax, cr0
	ret
setmycr0:
	mov edx, 7FFFFFFEh
	and ebx, edx
	xor edx, -1
	mov eax, cr0
	and eax, edx
	or eax, ebx
	mov cr0, eax
	ret

;--- function CR2()

getcr2 proc c value:dword

	cmp al, 1
	jz @F
	@ring0call getmycr2
	jmp getcr2_ex
@@:
	mov eax, value
	@ring0call setmycr2
getcr2_ex:
	ret
getmycr2:
	mov eax, cr2
	retn
setmycr2:
	mov cr2, eax
	retn
getcr2 endp

;--- function CR3()

getcr3 proc c value:dword

	cmp al, 1
	jz @F
	@ring0call getmycr3
	jmp getcr3_ex
@@:
	mov eax, value
	@ring0call setmycr3
getcr3_ex:
	ret
getmycr3:
	mov eax, cr3
	retn
setmycr3:
	mov cr3, eax
	retn

getcr3 endp

;--- function CR4()

getcr4 proc c value:dword

	cmp al, 1
	jz	@F
	@ring0call getmycr4
	jmp getcr4_ex
@@:
	mov eax,value
	@ring0call setmycr4
getcr4_ex:
	ret
getmycr4:
	@moveaxcr4
	retn
setmycr4:
	@movcr4eax
	retn

getcr4 endp

;*** kopiert aus GETCURD.ASM

GetCurrentDirectory proc stdcall uses esi edi maxlen:dword,buffer:dword

local	tmpbuf[MAXPATH]:byte

	mov eax,maxlen
	cmp eax,4
	jb getcwd_er
	mov ah,19h			   ;get drive
	@DosCall
	mov dl,al
	inc dl
	add al,'A'
	mov edi,buffer
	mov ah,':'
	stosw
	mov ax,'\'
	stosw
	dec edi

	lea esi,tmpbuf
;	mov		byte ptr [esi],0FFh	;OS should change this byte or function failed!!
	mov ax,7147h
	stc
	@DosCall
	jc getcwd_1
;	cmp	byte ptr [esi],0FFh
;	jz getcwd_1
	cmp ax,7100h			;function supported?
	jnz getcwd_2
getcwd_1:
	mov ah,47h
	@DosCall
	jc getcwd_er
getcwd_2:
	mov ecx,maxlen
	sub ecx,3
	cmp byte ptr [esi],'\'
	jnz @F
	inc esi
@@:
	lodsb
	stosb
	and al,al
	loopnz @B
	mov eax,buffer
	jmp getcwd_ex
getcwd_er:
	xor eax,eax
getcwd_ex:
	ret
GetCurrentDirectory endp

;--- function CUR_DIR()

getcurdir proc stdcall public
	push ecx
	invoke	alloctmpheap, MAXPATH
	invoke	GetCurrentDirectory, MAXPATH, eax
	pop ecx
	ret
getcurdir endp

;--- function CPUID_MAnuf()

getmanu proc near
	push ecx
	invoke alloctmpheap, 13
	mov byte ptr [eax],0
	test fCPUID,1
	jz @F
	push eax
	mov eax,0
	@cpuid
	pop eax
	mov [eax+0], ebx
	mov [eax+4], edx
	mov [eax+8], ecx
	mov byte ptr [eax+12], 0
@@:
	pop ecx
	ret
getmanu endp

;--- function MSR()

_getmsr proc c pb:PARMBLK
	test byte ptr idflags, ID_MSR
	jz getmsr_er
	push ebx
	push esi
	push edi
	mov ecx, ebx
	cmp pb.wArgc, 3
	jnc setmsr
	@mov ebx, 20h
	cmp pb.wArgc, 2
	jb @F
	mov ebx, pb.dwOffs2
@@:        
getmsr_1:
	@ring0call r0getmsr
	jc next
	@dwordout ecx
	@putchr '='
	@dwordout edx
	@putchr '.'
	call eaxout
	invoke _crout
next:
	inc ecx
	dec ebx
	jnz getmsr_1
exit:
	pop edi
	pop esi
	pop ebx
	ret
setmsr:        
	mov eax, pb.p2.dwOffs
	mov edx, pb.p3.dwOffs
	@ring0call r0setmsr
	jnc exit
	@errorout 4Ch	;cannot write
	jmp exit
getmsr_er:
	@errorout ERR_MSR_NOTSUPPORTED
	ret
if 1
saveexc0d:
	pop esi
	sub  esp,6
	mov edi,ss
	lar edi,edi
	test edi,400000h
	jnz @F
	movzx esp,sp
@@:
	sidt [esp]    ;should be ok to use ESP in 16-bit here
	pop di
	pop edi
	push dword ptr @flat:[edi+8*0Dh+4]
	push dword ptr @flat:[edi+8*0Dh+0]
	push eax
	mov  eax,offset r0exc0d
	add  eax,MyBase
	mov  @flat:[edi+8*0Dh+0],ax
	mov  word ptr @flat:[edi+8*0Dh+2],cs
	mov  word ptr @flat:[edi+8*0Dh+4],0EE00h
	shr  eax,16
	mov  @flat:[edi+8*0Dh+6],ax
	pop eax
	jmp esi
restexc0d:
	pop esi
	pop dword ptr @flat:[edi+8*0Dh+0]
	pop dword ptr @flat:[edi+8*0Dh+4]
	jmp esi
endif
r0getmsr:
	call saveexc0d
	clc
	@rdmsr
	call restexc0d
	retn
r0setmsr:
	call saveexc0d
	clc
	@wrmsr
	call restexc0d
	retn
_getmsr endp

r0exc0d:
	add esp,4
	add dword ptr [esp+0], 2	; ok to use [esp]
	or byte ptr [esp+2*4], 1	; set carry flag, ok to use [esp]
	iretd

;--- functions PSP() : return psp (selector) in eax

getpsp proc stdcall public
	mov eax,[dwCurPsp]
	and eax, eax
	jnz @F
	push ebx
if ?WINDOWS        
	mov ah,51h
	int 21h					;use int 21h here!
else
	@DosCall 51h				;getpsp ist immer moeglich
endif        
	movzx ebx, bx
	mov [dwCurPsp],ebx
	mov eax,ebx
	pop ebx
@@:
	ret
getpsp endp

if 0
;--- getpspseg

getpspseg proc stdcall public uses ebx
	call getpsp
	mov ebx, eax
	mov ax,6
	@DpmiCall
	push cx
	push dx
	pop eax
	shr eax, 4
	ret
getpspseg endp

endif

;--- function PSPNAME()

getpspname proc
	call getpsp
	push ebx
	push ecx
	push edi
	invoke getbaser, eax
	lea ebx,[eax-08h]
	mov ecx,eax
	shr ecx,4
	invoke alloctmpheap, 9
	push eax
	mov edi,eax
	cmp cx,@flat:[ebx-7]
	jnz getpspname_1
	mov ecx,8
@@:
	mov al,@flat:[ebx]
	stosb
	inc ebx
	loop @B
getpspname_1:
	xor al,al
	stosb
	pop eax
	pop edi
	pop ecx
	pop ebx
	ret
getpspname endp

;--- getdta

getdta proc
	test [fStat], FSTAT_DTAREAD
	jnz @F
	push es
	push ebx
	@DosCall 2Fh
	or [fStat], FSTAT_DTAREAD
	mov dword ptr [dfCurDta+0],ebx
	mov word ptr [dfCurDta+?SEGOFFS],es
	@tprintf <"getdta: dta read from dos %X:%X",lf>, es, ebx
	pop ebx
	pop es
@@:
	mov dx,word ptr [dfCurDta+?SEGOFFS]
	mov eax,dword ptr [dfCurDta+0]
	@tprintf <"getdta: dta is %X:%X",lf>,edx,eax
	ret
getdta endp

;--- getindosflag

getindosflag proc
	push es
	push ebx
;	les bx,[dwIndos]
	mov ebx, [dwIndos]
if 0;?CHECKINDOSVALID
	movzx ebx,bx
	mov eax,es
	lsl eax,eax
	cmp eax,ebx
	jnb @F
	@errorout ERR_INVALID_INDOS_ADDR
	mov al,0
	jmp getindosflag_1
@@:
endif
	mov al, @flat:[ebx]
;	movzx ebx, bx
;	mov al,es:[ebx]
getindosflag_1:
	pop ebx
	pop es
	ret
getindosflag endp

getvmid proc
	mov bx,0000h
	mov ax,1683h				;VM id holen
	@callint2F
	mov eax,ebx
	ret
getvmid endp

getfEMU proc c value:dword
	cmp al, 1
	jz @F
	mov al,[fEMU]
	clc
	ret
@@:
	pushad
	call restoredebuggeefpustate
	jc @F
	mov al,byte ptr value
	mov [fEMU],al
	call savedebuggeefpustate
	jc @F
	call setdebuggerfpustate
@@:
	popad
	ret
getfEMU endp

ife ?WINDOWS

;--- function SCREENSwap()
;--- get/set variable fSwap

getswap proc c value:dword
	cmp al, 1
	jz @F
	mov al, fSwap
	clc
	ret
@@:
	mov al, byte ptr value
	and al, al
	setne al
	pushad
	invoke SetScreenSwap, eax
	popad
	clc
	ret
getswap endp

endif

;--- function STRICTMode()

getstrictmode proc c value:dword

	cmp al, 1
	mov al, [fMode]
	jz @F
	and al, FMODE_STRICT
	setne al
	clc
	jmp exit
@@:
	mov edx, value
	cmp edx, 1
	cmc
	sbb dl, dl
	and dl, FMODE_STRICT
	and al, not FMODE_STRICT
	or al, dl
	mov [fMode], al
exit:
	ret
getstrictmode endp

;--- function DIRECTMode()

getdirectmode proc c value:dword

	cmp al, 1
	mov al, [fMode]
	jz @F
	and al, FMODE_LDTDIRECT
	setne al
	clc
	jmp exit
@@:
	mov edx, value
	cmp edx, 1
	cmc
	sbb dl, dl
	and dl, FMODE_LDTDIRECT
	and al, not FMODE_LDTDIRECT
	or al, dl
	mov [fMode], al
exit:
	ret
getdirectmode endp

;--- get the state of the virtual interrupt flag
;--- out: Z if interrupts disabled

GetVIF proc stdcall public
	test cs:[fMode], FMODE_STRICT
	jnz @F
	pushfd
	pop eax
	mov al, ah
	shr al, 1
	and al, 1
	ret
@@:
	mov ax, 902h
	@DpmiCall
	test al, 1
	ret
GetVIF endp        

;--- set the state of the virtual interrupt flag

SetVIF proc stdcall public
	test cs:[fMode], FMODE_STRICT
	jnz sv1
	and al,al
	jnz @F
	cli
	ret
@@:
	sti
	ret
sv1:
	mov ah,09h
	@DpmiCall
	ret
SetVIF endp        

getfIrq proc c value:dword

	cmp al,1
	mov al,fIrq
	jnz exit
	mov al,byte ptr value
	and al,al
	setne al
	@tprintf <"getfIrq: AL=%X",lf>,eax
	mov dl,al
	invoke SetVIF
	invoke GetVIF
	cmp al,dl
	jnz error
	mov fIrq,al
exit:
	clc
	ret
error:
	@stroutc "error: (V)IF cannot be changed",lf
	clc
	ret
getfIrq endp

getrealmode:
	mov   al, [fEntry]
	and   al,FENTRY_REAL
	setne al
	ret

getvmhandle proc stdcall

if ?32BIT
local	shellapi:PF32
else
local	shellapi:PF16
endif

	push es
	pushad
	xor edi, edi
	mov es, edi
	mov ax, 1684h				 ;shell API holen
	mov bx, 0017h
	@callint2F
	mov  dword ptr [shellapi+0],edi
	mov  word ptr [shellapi+?SEGOFFS],es
	mov  eax,es
	and  ax,ax
	stc
	jz @F
	xor  edx,edx
	xor  ebx,ebx
	call [shellapi]
	mov dword ptr shellapi,ebx
@@:
	popad
	pop  es
	mov  eax,dword ptr shellapi
	ret
getvmhandle endp

;--- DOSEMU has a LDT alias, but doesn't allow to get the base of it

getldtaddr proc
	and ebx, 0000FFF8h
	push ebx
	mov bx,[rLDT]
	call getlimitr
	jc error
	pop ebx
	cmp ebx, eax
	jnc error
	movzx eax, [rLDT]
	invoke getbaser, eax
	jc error
	add ebx, eax
	ret
error:
	stc
	ret
getldtaddr endp

;--- in: selector in EBX
;*** out: NC, eax=accessrights 
;***       C if error
;--- modifies ebx!

getaccr proc stdcall public
	lar eax, ebx
	jz getaccr3
	test bl,4
	jnz getaccr4
	and ebx,0000FFF8h
	jz getaccr_er
	cmp bx,word ptr [rGDTR]
	jnc getaccr_er
	add ebx,dword ptr [rGDTR+2]
getaccr2:
	test [fMode], FMODE_STRICT
	jnz getaccr_er
	mov eax,@flat:[ebx+4]
getaccr3:
	and eax,00F0FFFFh
	ret
getaccr4:
	test [fMode], FMODE_LDTDIRECT
	jz getaccr_er
	call getldtaddr
	jnc getaccr2
getaccr_er:
	stc
	ret
getaccr endp

;*** limit von ebx -> eax holen ***

getlimitr proc stdcall public
	lsl eax,ebx
	jz getlimit2
	test bl,4
	jnz getlimit3
	and ebx,0000FFF8h
	jz getlimit_er
	cmp bx,word ptr [rGDTR]
	jnc getlimit_er
	add ebx,dword ptr [rGDTR+2]
getlimit5:
	test [fMode], FMODE_STRICT
	jnz getlimit_er
	mov al,@flat:[ebx+6]
	mov cl,al
	and ax,0fh
	shl eax,16
	mov ax,@flat:[ebx+0]
	test cl,80h
	jz @F
	shl eax,12
	or eax,00000FFFh
@@:
getlimit2:
	clc
	ret
getlimit3:
	test [fMode], FMODE_LDTDIRECT
	jz getlimit_er
	call getldtaddr
	jnc getlimit5
getlimit_er:
	stc
	ret
getlimitr endp

;*** return in EAX base of a descriptor

getbaser proc stdcall public uses ebx ecx edx selector:dword

restartproc:
	mov ebx, selector
	test bl,04
	jnz localbase
	and ebx, 0000FFF8h
	jz error
	cmp bx, word ptr [rGDTR]
	jnc error
	add ebx, dword ptr [rGDTR+2]
getbaser_1:
	test [fMode], FMODE_STRICT
	jnz error
	mov [excexit], offset forcestrict
	mov al, @flat:[ebx+7]
	shl eax, 24
	mov edx, @flat:[ebx+2]
	and edx, 00FFFFFFh
	or eax, edx
	jmp exit
localbase:
	test [fMode], FMODE_LDTDIRECT
	jnz @F
	mov ax, 6
	@DpmiCall
	jc error
	push cx
	push dx
	pop eax
	jmp exit
@@:
	call getldtaddr
	jnc getbaser_1
error:
	stc
exit:
	ret
forcestrict:
	or [fMode], FMODE_STRICT
	jmp restartproc
getbaser endp

getbase proc c sel:dword

	push ecx
	invoke getbaser,sel
	pop ecx
	jnc @F
	mov cl,__VOID__
	clc
@@:
	ret
getbase endp

getbasex:
	lodsd
	mov cl,__DWORD__
	push [eax]
	call getbase
	pop ebx
	ret

setbase proc c wert:dword,msel:dword

	push ecx
	mov cx,word ptr wert+2
	mov dx,word ptr wert+0
	mov ebx,msel
	mov ax,7
	int 31h
	mov eax,wert
	pop ecx
	jnc @F
	mov cl,__VOID__
	clc
@@:
	ret
setbase endp

getlimit proc c sel:dword

	push ecx
	mov  ebx,sel
	call getlimitr
	pop  ecx
	jnc  @F
	mov  cl,__VOID__
	clc
@@:
	ret
getlimit endp

getlimx:
	lodsd
	mov  cl,__DWORD__
	push [eax]
	call getlimit
	pop  ebx
	ret

setlimit proc c wert:dword,sel:dword

	push ecx
	mov ebx,sel
	mov cx,word ptr wert+2
	mov dx,word ptr wert+0
	mov ax,8
	int 31h
	mov eax,wert
	pop ecx
	jnc @F
	mov cl,__VOID__
	clc
@@:
	ret
setlimit endp

getattr proc c msel:dword

	push ecx
	mov ebx,msel
	call getaccr
	jc getattrx_ex
	shr eax,8
	clc
getattrx_ex:
	pop ecx
	jnc @F
	mov cl,__VOID__
	clc
@@:
	ret
getattr endp

getattrx:
	lodsd
	mov  cl,__WORD__
	push [eax]
	call getattr
	pop  ebx
	ret

setattr proc c wert:dword,sel:dword

	push ecx
	mov ebx,sel
	mov cx,word ptr wert+0
	mov ax,9
	int 31h
	mov eax,wert
	pop ecx
	jnc @F
	mov cl,__VOID__
	clc
@@:
	ret
setattr endp

getseg proc c value2:dword,value1:dword

	movzx  eax,word ptr value1
	cmp ch,__FPTR__
	jz @F
	cmp ch,__LPTR__
	jz @F
	cmp ch,__RMLPTR__
	jz @F
	stc
@@:
	ret
getseg endp

getlptr proc c value2:dword,value1:dword

	mov eax,value2
	mov edx,value1
if 0
	pushad
	invoke printf,CStr("getlptr: eax=%x, edx=%x, ecx=%x",lf),eax,edx,ecx
	popad
endif
	cmp ch,__FPTR__
	jz @F
	cmp ch,__LPTR__
	jz getlptr_2
	cmp ch,__RMLPTR__
	jz getlptr_2
	cmp ch,__DWORD__
	jz getlptr_1
	stc
	jmp getlptr_ex
getlptr_2:
	push dx
	push ax
	pop eax
	jmp @F
getlptr_1:
	mov edx,eax 		;nur bei typ DWORD
	shr edx,16
	movzx eax,ax
	clc
@@:
getlptr_ex:
	ret
getlptr endp

getoff proc c xvalue:dword

	mov eax,xvalue
	ret
getoff endp

getcompl proc c xvalue:dword

	mov eax,xvalue
	xor eax,-1
	ret
getcompl endp

inportb proc c port:dword

	mov  edx,port
	in al,dx
	movzx eax,al
	ret
inportb endp

outportb proc c wert:dword,port:dword

	mov edx,port
	mov al,byte ptr wert
	out dx,ax
	ret
outportb endp

inportw proc c mport:dword

	mov edx,mport
	in ax,dx
	movzx eax,ax
	ret
inportw endp

outportw proc c wert:dword,port:dword

	mov edx, port
	mov eax, wert
	out dx,ax
	ret
outportw endp

_highword proc pascal wert:dword

	mov ax, word ptr wert+2
	ret
_highword endp

_lowword proc pascal wert:dword

	mov ax, word ptr wert+0
	ret
_lowword endp

_highbyte proc pascal wert:dword

	mov al, byte ptr wert+1
	ret
_highbyte endp

_lowbyte proc pascal wert:dword

	mov al, byte ptr wert
	ret
_lowbyte endp

gettype proc c

	movzx eax,ch		   ;in ch ist typ des letzten funktionsarguments
	and al,0Fh
	ret
gettype endp

;*** unterroutine fuer help ***
;*** kapitel/section suchen ***
;*** falls gefunden in eax zurueck ***

searchkap proc stdcall uses ds esi edi sbereich:dword,searchstr:near ptr byte

	mov edi, sbereich
	and edi, edi
	jz  searchkap1
searchkap3:
	mov al, @flat:[edi]
	and al, al
	jz  searchkap1
	cmp al, '['
	jz   searchkap2
searchkap4:
	mov al, @flat:[edi]
	inc edi
	and al, al
	jz  searchkap1	 ;fertig
	cmp al, lf
	jnz searchkap4
	jmp searchkap3
searchkap2:
	mov esi, searchstr
	inc edi
@@:
	mov al, @flat:[edi]
	inc edi
	call kl2gr
	mov ah, al
	lodsb
	cmp al, ah
	jz @B
	and al, al
	jz searchkap5
	cmp al, ' '		 ;ist parameter zuende?
	jz searchkap5
	jmp searchkap4
searchkap5: 				 ;eingegebener parameter ist zuende
	cmp ah, ']'		 ;wie siehts mit fileparameter aus?
	jz searchkap6
	mov al, @flat:[edi-1]
	call tstkl		 ;nur noch kleinbuchstaben?
	jnc searchkap4
searchkap6:
	mov al, @flat:[edi]
	inc edi
	and al, al
	jz @F
	cmp al, lf
	jnz searchkap6
@@:
	mov eax, edi
	jmp searchkapex
searchkap1:
	xor eax, eax
searchkapex:
	ret
searchkap endp

;*** unterroutine fuer help; kapitel ausgeben ***

TABEXPAND equ 0

kapout proc stdcall uses esi kbereich:dword

	mov esi,kbereich
	mov ebx,esi
	mov al,lf
kapout1:
	mov ah,al
	mov al,@flat:[esi]
	inc esi
	and al,al
	jz kapout2
	cmp al,'['
	jnz kapout1
	cmp ah,lf
	jnz kapout1
	dec esi
kapout2:
	mov ecx, esi
	sub ecx, ebx
	dec ecx
	mov esi, ebx
	xor edx, edx
	.while (ecx)
if TABEXPAND
		mov al,@flat:[esi]
		.if (al == 9)			;expand TABS!
			.repeat
				@putchr ' '
				inc edx
				test dl,03		;expand tab to max 4 spaces
			.until (ZERO?)
		.else
			push eax
			@putchr al
			pop eax
			inc edx				;inc tab counter
			.if (al == cr)
				xor edx,edx
			.endif
		.endif
else
		@putchr @flat:[esi]
endif
		inc esi
		dec ecx
	.endw
	invoke _crout
	ret
kapout endp

hindexout proc stdcall uses esi hbereich:dword

	mov esi, hbereich
	and esi, esi
	jz exit
	mov ebx, esi
	mov al, lf
	mov dl, 4
hindexout1:
	mov ah, al
	mov al, @flat:[esi]
	inc esi
	and al, al
	jz exit
	cmp al, '['
	jnz hindexout1
	cmp ah, lf
	jnz hindexout1
	mov ecx, 19
hindexout3:
	mov al, @flat:[esi]
	inc esi
	cmp al, ']'
	jz @F
	cmp al, cr
	jz @F
	@putchr al
	dec ecx
	jmp hindexout3
@@:
	invoke _spout, ecx
	mov al, cr
	dec dl
	jnz hindexout1
	invoke _crout
	mov dl, 4
	jmp hindexout1
exit:
	invoke _crout
	ret
hindexout endp

ife ?32BIT

?CHKSIZE equ 8000h

	option prologue:none
        
__write proc stdcall public handle:dword, pBuffer:qword, numBytes:dword
	mov al,40h
	jmp _rdwr
__write endp

__read proc stdcall public handle:dword, pBuffer:qword, numBytes:dword
	mov al,3Fh
__read endp

	option prologue:@prologue

_rdwr proc stdcall uses ds ebx handle:dword, pBuffer:qword, numBytes:dword

local base:dword
local flag:dword
local dwRdWr:dword

	mov byte ptr flag,al
	mov ebx,dword ptr pBuffer+4
	mov ax,6
	int 31h
	jc exit
	push cx
	push dx
	pop eax
	add eax,dword ptr pBuffer+0
	mov base,eax 		;linear address of read/write buffer

	sub eax,eax
	mov dwRdWr,eax
        
	mov cx,1
	mov ax,0
	int 31h				;get a scratch selector
	jc exit
	mov ebx, eax
	mov ds,eax

	mov dx, ?CHKSIZE-1
	xor ecx,ecx
	mov ax,8
	int 31h
	jc sm1_er_pm4
        
nextblock:
							;1. set new base of DS
	mov ebx,ds
	mov eax,base
	add eax,dwRdWr
	push eax
	pop dx
	pop cx
	mov ax,7
	int 31h
	jc sm1_er_pm3
	push ds				;make sure ds has new values
	pop ds

	mov eax,numBytes	;2. ecx = _min(laenge,?CHKSIZE)
	and eax,eax
	jz sm1
	mov ecx,?CHKSIZE
	sub eax,ecx
	jnc @F				;restlaenge ist groesser
	add ecx,eax
	sub eax,eax
@@:
	mov numBytes,eax
	xor edx,edx
	mov ebx,handle
	mov ah,byte ptr flag
	@DosCall
	jc sm1_er
	movzx eax,ax
	add dwRdWr,eax
	cmp ax,?CHKSIZE
	jz nextblock		;---->
sm1:
	mov eax,dwRdWr
	clc
sm1_er:
sm1_er_pm3:
sm1_er_pm4:
	pushfd
	push eax
	mov ebx,ds
	push 0
	pop ds
	mov ax,1
	int 31h		;free descriptor
	pop eax
	popfd
exit:        
	ret
_rdwr endp

endif

_fileread proc stdcall public uses ebx handle:dword,buffer:qword,laenge:dword,addrinfile:dword

	call	MarkDOSused
	mov 	cx,word ptr addrinfile+2
	mov 	dx,word ptr addrinfile+0
	mov 	ebx,handle
	mov 	ax,4200h
	@DosCall
	jc		err1
ife ?32BIT
	invoke	__read, ebx, buffer, laenge
	jc		err2
else
	push	ds
	lds 	edx,fword ptr buffer
	mov 	ecx,laenge
	mov 	ah,3fh
	@DosCall
	pop		ds
	jc		err2
endif
	jmp		done
err1:
	mov 	ecx, ERR_FILE_LSEEK
	jmp 	@F
err2:
	mov 	ecx, ERR_FILE_READ
@@:
	push	handle
	movzx	eax,ax
	push	eax
	@errorout ecx
	pop		eax
	pop		eax
	@mov 	eax,-1
done:        
	ret
_fileread endp


;--- REAd command: read a file
;--- into dynamically allocated memory

_readfile proc c pb:PARMBLK

local	xhandle:dword

	call MarkDOSused
	mov edx,pb.dwOffs1
	mov ax,3d20h			;r/o, deny write
	@DosCall
	jnc readfile1
	movzx eax,ax
	push eax
	push edx
	@errorout ERR_FILE_OPEN
	add esp,2*4
	jmp exit
readfile1:
	mov xhandle,eax

	mov ebx,pb.p3.dwOffs
	cmp pb.p3.bType, __VOID__
	jnz @F
	call GetFileSize					;handle in eax
	mov ebx,eax						;max. laenge
@@:
	mov esi, pb.wSeg2
	mov eax,pb.dwOffs2
	cmp pb.p2.bType,__RMLPTR__
	jnz @F
	shl esi,4
	add eax, esi
	mov esi, @flat
@@:
	cmp pb.p2.bType, __VOID__		; adresse angegeben?
	jnz @F
	mov eax, ebx					; kB allokieren
	call allocdpmimem				; returns flat address in EBX
	jc	exit
	mov esi, @flat
@@:
	xor ecx, ecx
	cmp pb.p4.bType, __CONST__
	jnz @F
	mov ecx,pb.p4.dwOffs
@@:
	verw si
	jz @F
	push esi
	@errorout ERR_INVALID_POINTER
	pop esi
	jmp exit
@@:
	invoke	_fileread, xhandle, esi::eax, ebx, ecx

	push eax
	@close xhandle
	pop eax
	cmp eax,-1
	jz exit
	invoke printf, CStr("%X Bytes read",lf),eax
exit:
	ret
_readfile endp

_filewrite proc stdcall uses ebx handle:dword,buffer:qword,laenge:dword,addrinfile:dword

	call MarkDOSused
	mov cx,word ptr addrinfile+2
	mov dx,word ptr addrinfile+0
	mov ebx,handle
	mov ax,4200h
	@DosCall
	jc err1
ife ?32BIT
	invoke __write, ebx, buffer, laenge
	jc err2
else
	push ds
	lds edx,fword ptr buffer
	mov ecx,laenge
	mov ah,40h
	@DosCall
	pop ds
	jc err2
endif
	jmp done
err1:
	mov ecx,ERR_FILE_LSEEK
	jmp @F
err2:
	mov ecx,ERR_FILE_WRITE
@@:
	push handle
	movzx eax,ax
	push eax
	@errorout ecx
	add esp,2*4
	mov eax,-1
done:
	ret
_filewrite endp

;*** write file ***

_writefile proc c pb:PARMBLK

local	xhandle:dword

	call MarkDOSused
	mov	esi, pb.wSeg2
	mov cl, pb.p2.bType
	cmp cl,__FPTR__						;memory adresse ok?
	jz	parm2ok
	cmp cl,__RMLPTR__			
	jz	isrm
	cmp cl,__CONST__
	jnz writefile_err
	mov esi,[r1.rDS]
	test [fEntry],FENTRY_REAL
	jz	parm2ok
	movzx esi,[r1r.rDS]
isrm:
	shl	esi, 4
	add	pb.dwOffs2, esi
useflat:
	mov	esi, @flat
parm2ok:
	cmp pb.p3.bType, __CONST__	;laenge ok?
	jz	@F
writefile_err:
	@errorout ERR_WRONG_PARAM
	jmp exit
@@:
	mov	edx,pb.dwOffs1
	cmp pb.p4.bType, __CONST__
	jnz	@F
	mov	ax,3D01h	;open file for writing
	@DosCall
	jnc	writefile1
@@:
	mov	ah,3ch		;create file?
	xor	ecx,ecx
	@DosCall
	jnc writefile1
	movzx	eax,ax	
	push eax
	push pb.dwOffs1
	@errorout ERR_FILE_CREATE
	add esp,2*4
	jmp exit
writefile1:
	mov	xhandle,eax
	xor ecx,ecx						;offset in file
	cmp pb.p4.bType, __CONST__
	jnz @F
	mov ecx,pb.p4.dwOffs
@@:
	verr si
	jz	@F
	push esi
	@errorout ERR_INVALID_POINTER
	pop	esi
	jmp	 exit
@@:
	mov	edi, pb.dwOffs2
	invoke	_filewrite, xhandle, esi::edi, pb.p3.dwOffs, ecx

	push eax
	@close	xhandle
	pop eax
	invoke printf, CStr("%X Bytes written from %X:%X",lf), eax, esi, edi
exit:
	ret
_writefile endp

if 0

searchentry proc stdcall kapitel:dword,entry:dword

local	entrylen:dword

	push esi
	push edi
	xor eax, eax
	mov esi, kapitel
	mov edi, entry
	mov ecx, -1
	mov al, 0
	repnz scasb
	not ecx
	dec ecx
	mov entrylen, ecx
searchentry_1:
	mov ecx, entrylen
	mov edi, entry

if ?WINDOWS eq 0
	cmp byte ptr [esi],'-'
	jnz @F
	inc esi
@@:
else
	cmp byte ptr [esi],'+'
	jnz @F
	inc esi
@@:
endif

searchentry_2:
	lodsb
	mov ah, es:[edi]
	or al, 20h
	or ah, 20h
	cmp al, ah
	jnz @F
	inc edi
	loop searchentry_2
	cmp byte ptr [esi], '='
	jnz @F
	mov eax, esi
	inc eax
	jmp searchentry_ex
@@:
	lodsb
	and al, al
	jz searchentry_ex
	cmp al, lf
	jnz @B
	cmp byte ptr [esi],'['
	jnz searchentry_1
	xor eax, eax
searchentry_ex:
	pop edi
	pop esi
	ret
searchentry endp

endif

ife ?WINDOWS
GetDebuggerName proc stdcall uses es edi esi pBuffer:ptr byte
	mov es,_psp
	mov es,es:[2ch]
	xor edi,edi
	mov al,0
	mov ecx,-1
@@:
	repnz scasb
	scasb
	jnz @B
	mov eax, ds
	inc edi
	inc edi
	mov esi, edi
	mov edi, pBuffer
	push es
	pop ds
	push ss
	pop es
@@:
	lodsb
	stosb
	and al,al
	jnz @B
	push ss
	pop ds
	sub edi, pBuffer
	mov eax, edi
	dec eax
	ret

GetDebuggerName endp
endif

;--- create a full path name file name (path is debugger path)
;--- output is on near heap
;--- out: eax -> filename
;--- out: edx = length (including terminating 0)

_createfilename proc stdcall uses edi fname:dword

	mov edi,[pNearHeap]

ife ?WINDOWS
	invoke GetDebuggerName, edi
else
	push ds
	lds si,pszDebuggerName
	movzx esi,si
	mov edx, edi
@@:
	lodsb
	stosb
	and al,al
	jnz @B
	pop ds
	sub edi, edx
	dec edi
	mov eax, edi
	mov edi, edx
endif
	add edi,eax
	mov ecx, eax
@@:
	mov al,[edi-1]
	cmp al,'\'
	jz createfilename_1
	cmp al,':'
	jz createfilename_1
	dec edi
	loop @B
createfilename_1:
	mov esi,fname
@@:
	lodsb
	stosb
	and al,al
	jnz @B
	mov eax,pNearHeap
	mov edx, edi
	sub edx, eax
	ret

_createfilename endp

	@cmdproc

_listmacros proc c pb:PARMBLK

	mov esi, pMacros
nextitem:
	and esi, esi
	jz done
	add esi, 4
	.if (cl == __STRING__)
		push ecx
		invoke strcmp, [esi.SYMBOL.pText], ebx
		pop ecx
		.if (!eax)
			call dispmacro
			jmp exit
		.endif
	.else
		call dispmacro
	.endif
	mov esi, [esi-4]
	jmp nextitem
done:
	.if (cl == __STRING__)
		@errorout ERR_MACRO_NOT_FOUND
	.endif
exit:
	ret
dispmacro:
	pushad
	@strout [esi.SYMBOL.pText]
	@putchr '='
	@strout [esi.SYMBOL.dwProc]
	invoke _crout
	popad
	retn

_listmacros endp

	@cmdprocend

;--- add macros found in debxxf.mac

ReadMacros proc c

local	key:dword
local	tvalue:dword
local	fIgnore:byte

	invoke _createfilename,CStr("debxxf.mac")
	mov edx, eax
	mov ax, 3d00h			;r/o
	@DosCall
	jnc @F
	movzx eax,ax
	push eax
	push edx
	@errorout ERR_FILE_OPEN
	add esp,2*4
	jmp exit
@@:
	mov ebx, eax
	mov edx, @flat
	mov ecx, flatheap
	invoke _fileread, ebx, edx::ecx, 4000h, 0
	push eax
	@close ebx
	pop eax
	cmp eax,-1
	jz error
	mov esi,flatheap
	mov byte ptr @flat:[esi+eax],0
addmacro_00:
	mov al,@flat:[esi]
	and al,al
	jz exit

	mov fIgnore, 0
	cmp al, '#'
	jnz @F
	mov fIgnore, 1
@@:        
	cmp al,MYMAC
	jnz @F
	inc esi
	jmp addmacro_01
@@:
	cmp al,IGNMAC
	jnz addmacro_01
	mov fIgnore,1		;macro will be ignored!
addmacro_01:
	mov edi,[pNearHeap]
	mov key,edi
nextchar1:
	mov al,@flat:[esi]
	inc esi
	cmp al,'='
	jz addmacro_1		;now comes value
	cmp al,cr
	jz nextchar1		;skip CRs
	cmp al,lf
	jz addmacro_00		;no value, ignore this line
	and al,al
	jz addmacro_2		;file end
	stosb
	jmp nextchar1
addmacro_1:
	xor al,al			;finish macro name
	stosb
	mov tvalue,edi
nextchar2:
	mov al,@flat:[esi]
	inc esi
	cmp al,cr
	jz nextchar2		;skip CRs
	cmp al,lf
	jz islf
	dec esi
	and al,al
	jz @F
	inc esi
	stosb
	mov ah,al
	jmp nextchar2
islf:
	cmp ah,'\'			;continuation character?
	jnz @F
	dec edi				;skip the '\'
	jmp nextchar2
@@:
	mov al, 0
	stosb

	cmp fIgnore, 0
	jnz addmacro_00		;skip this macro

	invoke PutStringInHeap, key
	mov key, eax
	invoke PutStringInHeap, tvalue
	mov tvalue, eax
	invoke malloc, sizeof SYMBOL+4
	and eax, eax
	jz addmacro_00
	mov ebx, eax
	mov eax, tvalue
	mov [ebx+4].SYMBOL.dwProc, eax
	mov eax, key
	mov [ebx+4].SYMBOL.pText, eax
	mov byte ptr [ebx+4].SYMBOL.bType,0C0h  ;als textmacro kennzeichnen
	mov eax,pMacros
	mov [ebx+0], eax
	mov pMacros, ebx
if 0
	@stroutc "macro added: "
	mov ebx, key
	invoke __stroutebx		; modifies ebx, eax
	@stroutc "  "
	mov ebx, tvalue
	invoke __stroutebx		; modifies ebx, eax
	invoke _crout
endif
	jmp addmacro_00
addmacro_2:
error:
exit:
	ret
ReadMacros endp


malloc proc stdcall public nBytes:dword

	mov eax, nBytes
	add eax, pHeap
	cmp eax, pHeapMax
	jnc error
	xchg eax, pHeap
	ret
error:
	xor eax, eax
	ret
malloc endp

PutBytes2Heap proc stdcall public uses edi esi varadr:dword,varlen:dword

	invoke malloc, varlen
	and eax, eax
	jz @F
	mov edi, eax
	mov ecx, varlen
	mov esi, varadr
	rep movsb
@@:
	ret
PutBytes2Heap endp

PutStringInHeap proc stdcall public pString:dword

	invoke strlen, pString
	inc eax
	invoke PutBytes2Heap, pString, eax
	ret
PutStringInHeap endp

alloctmpheap proc stdcall nbytes:dword

	mov eax, [pNearHeap]
	add eax, nbytes
	xchg eax, [pNearHeap]
	ret
alloctmpheap endp

CopyString2TmpHeap proc stdcall public pString:dword

	pushad
	invoke strlen, pString
	inc eax
	invoke alloctmpheap, eax
	xchg eax, pString
	invoke strcpy, pString, eax
	popad
	mov eax, pString
	ret
CopyString2TmpHeap endp


;*** help file in flat heap einlesen

readhelpfile proc stdcall

local	fhandle:dword

	pushad
	push ds
	pop es
	mov eax,[flatheap]
	mov [pHlpFile],eax

	@tprintf <"create help file path",lf>
	invoke _createfilename, CStr("debxxf.hlp")
	and eax, eax
	jnz @F
	invoke printf, CStr("can't create help file path",lf)
	jmp readhelpfile_err
@@:
	@tprintf <"open help file, addr path=%X, esp=%X",lf>, eax, esp
	mov edx, eax
	mov ax, 3d00h
	@DosCall
	jnc @F
	movzx eax, ax
	invoke printf, CStr(lf,"cannot open '%s' [%X]",lf), pNearHeap, eax
	xor eax, eax
	jmp readhelpfile_err
@@:
	@tprintf <"read help file",lf>
	movzx eax, ax
	mov [fhandle], eax
	mov edx, @flat
	mov ecx, pHlpFile
	invoke _fileread, eax, edx::ecx, 20000h, 0
	cmp eax, -1
	jnz @F

;--- get last error from dos here

	invoke printf, CStr("error %u reading help file",lf), eax
	xor eax, eax
@@:
	@tprintf <"close help file",lf>
	push eax
	@close fhandle
	pop eax
readhelpfile_err:
	@tprintf <"mark eof in memory",lf>
	add eax, [pHlpFile]
	mov byte ptr @flat:[eax], 00
	inc eax
	mov [flatheap],eax
	popad
	ret
readhelpfile endp

;*** help function

_help proc c pb:PARMBLK

	mov ebx, offset tkaphelp
	mov al, pb.p1.bType
	cmp al, __VOID__
	jz @F
	mov ebx,pb.dwOffs1
@@:
	mov ax,[ebx]
	cmp ax,'?'
	jnz @F
	invoke hindexout,[pHlpFile]
	jmp exit
@@:
	invoke searchkap,[pHlpFile],ebx
	and eax,eax
	jz nohelp
	invoke kapout,eax
	jmp exit
nohelp:
	@stroutc "no help available, type '? ?' for help index.",lf
exit:
	ret
_help endp

;*** skip next exception
;--- might be dangerous if the DPMI host terminates the client
;--- and the debugger doesn't run as a separate client ( true
;--- if debuggee is loaded via DPMILD16/32 and .LDR 0 isn't set ).

_skip proc c pb:PARMBLK

	xor [fMode], FMODE_SKIP
	test [fMode], FMODE_SKIP
	jnz @F
	@errorout MSG_SKIPNOTACTIV
	ret
@@:
	@errorout MSG_SKIPACTIV
	ret
_skip endp


_testring0 proc stdcall

	push es
	@ring0call int20test
	pop es
	ret

int20test:
	push ds
	mov eax, ss
	mov ds, eax
	mov es, eax
	int 20h
	dw 1
	dw 0
	pop ds
	ret
_testring0 endp

;*** switch in ring0 ueber call gate ***
;*** wichtig ist, das im 32-Bit Modus auch ein 386 Call Gate
;*** allokiert wird und entsprechend zurueckgekehrt wird.
;*** alles andere stuerzt ab.
;*** das frher ebenfalls zusammengebastelte 32-Bit Stacksegment
;*** ist nicht mehr notwendig.

ring0 proc stdcall public routine:dword

	push eax
	mov eax,routine
	mov [ring0proc],eax
	test [fMode], FMODE_STRICT
	jnz ring02
	cmp ring0sel,0
	jz ring02				;wir haben kein call gate
if ?USEFLATR0CS
	mov eax,MyBase
	add [ring0proc],eax
endif
	pushfd
	pop eax
	test ah, 2
	pop eax
	jnz @F
	call fword ptr [ring0switch] 	;call gate aufrufen
	jmp exit
@@:
	cli
	call fword ptr [ring0switch] 	;call gate aufrufen
	sti
exit:
	ret
ring02:
	pop eax
	@errorout ERR_NORING0CALL_POSSIBLE
	jmp mains
ring0 endp

;*** entry routine in ring0 ***
;*** SS:ESP ist veraendert! ***

ring0rou proc stdcall

	call [ring0proc]
	retf

ring0rou endp

;--- display 1 symbol, name + value + CRLF
;--- inp: symbol in ESI
;--- inp: flags in CH

DisplaySymbolLine proc stdcall public      
	@strout [esi.SYMBOL.pText]
	test byte ptr [esi.SYMBOL.bType],_RDONLY_
	jz @F
	test ch,_RDONLY_
	jnz @F
	@stroutc " (r/o)"
@@:
	@stroutc ": "
	mov cl,[esi.SYMBOL.bType]
	mov al,cl
	and al,1fh
	jz @F
	mov ebx,[esi.SYMBOL.dwProc]  ;addr holen
	pushad
	call symout
	popad
@@:
	invoke _crout
	ret
DisplaySymbolLine endp        

;*** display a list of symbols
;*** ebx -> start in symbol table
;*** eax -> end in symbol table
;*** ch=flags

symtout proc public uses esi
	mov esi,ebx
nextitem:
	cmp esi,eax
	jnb exit
	cmp byte ptr [esi.SYMBOL.pNext],00
	jz exit
	push eax
	invoke DisplaySymbolLine
	movzx eax,byte ptr [esi.SYMBOL.pNext]
	add esi,eax
	pop eax
	jmp nextitem
exit:
	ret
symtout endp

;*** 1 disassemblierte zeile ausgeben
;*** will use CS:EIP (=unassarg)
;--- called by setcmddefaults (which is called by debug_entry)

singlprot proc uses esi

	inc [fSkipLF]			; kein LF am ende
	push dword ptr [unasarg+0]

	push __CONST__			;Type2
	push 0					;dwSeg2
	push 1                  ;dwOffs2 (=1 line)
	mov  cl,__VOID__
	push ecx				;Type
	push 0					;dwSeg
	push 0					;dwOffs
	push 2					;wArgC
	call _unass
	jc sp_1
	test fUnass, FUNAS_DISPLAYAX
	jz sp_1

	push fs
	mov fs, esi
	mov cx, fs:[ebx]		; ebx has old offset
	pop fs

	cmp cx, 21CDh
	jz @F
	cmp cx, 31CDh
	jnz sp_1
@@:
	mov eax, r1.rEax
;	test [fEntry], FENTRY_REAL
;	jz @F
;	mov eax, r1r.rEAX
;@@:
	movzx eax, ax
	invoke printf, CStr(9,9,"'ax=%04X'"), eax
sp_1:        
	add esp,7*4
	pop dword ptr [unasarg+0]
	call croutx
	ret
singlprot endp

;--- called by "perform"

calcopsize proc uses esi
	or [fMode], FMODE_NODISP
	call getactcseip					 ;CS:EIP -> AX:EBX
	mov cl,__FPTR__
	test dl,1
	jz @F
	mov [a1.ARGDESC.dwSeg],eax
	mov cl,__RMLPTR__
@@:
	push __CONST__
	push 0
	push 1

	push ecx
	push eax
	push ebx

	push 2
	call _unass
	add esp,7*4
	sub eax,ebx
	and [fMode], not FMODE_NODISP
	ret
calcopsize endp

;*** CPU: symbole in registertabelle ausgeben ***

_xregsout proc c

;	mov esi,offset xtstab		  ;gdtr,idtr,ldtr,tr ausgeben
;	call regsout_2
;	invoke _crout

	@ring0call savexregs			 ;protected register ausgeben
	mov esi, offset ptstab
	jmp regsout_2
_xregsout endp

regsout_tss proc c public				;aktuelles tss ausgeben
	mov esi, offset tsstab
	mov edi, [pNearHeap]
	jmp regsout_3
regsout_tss endp

if ?MMXREGS
_mmxregsout:
	test idflags,800000h			;MMX implemented?
	jnz @F
	ret
@@:
	xor ecx, ecx
	mov esi, offset mmxtab
nextmmxreg:
	push ecx
	invoke printf, CStr("mm%u="),ecx
	lodsd
	push esi
	mov esi, eax
	mov ebx, [esi].SYMBOL.dwProc
	mov cl, [esi].SYMBOL.bType
	call symout
	call _crout
	pop esi
	pop ecx
	inc ecx
	cmp ecx, 8
	jnz nextmmxreg
	ret
endif

if ?XMMREGS
_xmmregsout:
	test idflags,1000000h		; FXSAVE FSRSTOR supported?
	jnz @F
	ret
@@:
	push ebp
	@loadesp ebp
	sub esp, 512+16
	and esp, not 16-1

;	fxsave [esp]
	db 0fh, 0AEh, 04h, 24h

	add esp,160
	xor ecx,ecx
	@loadesp esi
nextxmmreg:
	push ecx
	lodsd
	mov edi,eax
	lodsd
	mov edx,eax
	lodsd
	mov ebx,eax
	lodsd
	invoke printf, CStr("xmm%u=%08X-%08X-%08X-%08X",10), ecx, eax, ebx, edx, edi
	pop ecx
	inc ecx
	cmp ecx, 8
	jnz nextxmmreg
	mov esp,ebp
	pop ebp
	ret
endif

_fpregsout:								; display FP regs
	test [f80x87], F80X87_STATE_SAVED
	jnz @F
	@errorout ERR_NO_FPUREGS_SAVED
	ret
@@:
	mov esi,offset ftstab
	call regsout_2

	mov edi,ftw
	mov ecx,fsw
	shr ecx, 11
	and cl, 7
	shl cl, 1
	ror di, cl
	xor ecx,ecx
	mov esi,offset ftstab2
nextfreg:
	push ecx
	invoke printf, CStr("st%u="),ecx
	lodsd
	push esi
	mov esi,eax
	mov eax, edi
	mov ebx,[esi].SYMBOL.dwProc
	ror di,2
	and al,3
	mov ecx, CStr("<empty>")
	cmp al,3
	jz isEmpty
	mov ecx, CStr("<NaN>")
	cmp al,2
	jz isNaN
	mov cl,[esi].SYMBOL.bType
	call symout
	jmp fregdone
isNaN:
	test byte ptr [ebx+9],80h
	jz isEmpty
	invoke printf, CStr("-%-21s"), ecx
	jmp  fregdone
isEmpty:
	invoke printf, CStr("%-22s"), ecx
fregdone:
	pop esi
	pop ecx
	test cl,1
	jz @F
	invoke _crout
@@:
	inc ecx
	cmp ecx, 8
	jnz nextfreg
	ret

_regsout proc c 					; current register
	test [fEntry], FENTRY_REAL
	jz _regsoutp
_regsoutr:: 						; real mode register
	mov esi,offset rtstab
	jmp regsout_2
_regsoutp:: 						; protected mode register
	mov esi,offset tstab
regsout_2::
	xor edi,edi
regsout_3::
	call symbolout
	jnc regsout_3
	invoke _crout
	ret
_regsout endp

symbolout:							; esi -> symbol descriptor
	lodsd							; edi = offset (normal = 0)
	cmp eax,-1
	jz symbolout_ex
	and eax,eax
	jz @F
	@strout eax
@@:
	lodsd
	and eax,eax
	jz @F
	push esi
	mov esi,eax
	mov cl,[esi.SYMBOL.bType]
	mov ebx,[esi.SYMBOL.dwProc]
	add ebx,edi
	call symout
	pop esi
	clc
	ret
@@:
	lodsd
	call eax
	mov ebx,offset tmpvar
	mov [ebx+0],eax
	mov [ebx+4],edx
	call symout
	clc
	ret
symbolout_ex:
	stc
	ret

if ?SYMTRACE

_symtrace proc c

	mov esi,[pSymtab]
symtrace_1:
	cmp byte ptr [esi.SYMBOL.pNext],00  ;tabelle zuende?
	jz symtrace_ex
	@dwordout esi
	@putchr ' '
	mov al, [esi.SYMBOL.pNext]
	invoke _hexout
	@putchr ' '
	mov al, [esi.SYMBOL.bType]
	invoke _hexout 
	@putchr ' '
	mov al, [esi.SYMBOL.bType2]
	invoke _hexout 
	@putchr ' '
	@dwordout [esi.SYMBOL.dwProc]
	@putchr ' '
	cmp byte ptr [esi.SYMBOL.bType],-1
	jz @F
	@strout [esi.SYMBOL.pText]
@@:
	invoke	_crout
	movzx eax,byte ptr [esi.SYMBOL.pNext]
	cmp al,0FFh
	jz @F
	add esi,eax
	jmp symtrace_1
@@:
	mov esi,[esi.SYMBOL.pText] 	  ;???
	jmp symtrace_1
symtrace_ex:
	ret
_symtrace endp
endif

;*** unassemble routines

if ?VMMINT20

;--- in: esi=selector for mem access
;---     eax=next address for unassemble
;---     ebx=curr address for unassemble

checkvmmfunc proc        
	cmp byte ptr wWinVersion, 0
	jz @F
	push ecx
;	mov ecx, fs
	lsl ecx, esi
	sub ecx, eax
	cmp ecx, 4
	pop ecx
	jb @F
	push ds
	mov ds, esi
	cmp word ptr [ebx],20CDh
	pop ds
	jnz @F
	pushad
	push ds
	mov ds, esi
	movzx ecx,word ptr [eax+0]
	movzx edx,word ptr [eax+2]
	pop ds
	invoke printf, CStr(" [%04X %04X]"), ecx, edx
	popad
	add eax,4
@@:
	ret
checkvmmfunc endp
endif

;--- edx = current selector for unassemble
;--- ebx = current offset
;--- eax = next offset
;--- cl = segment type (1=32bit)
;--- esi = access selector

prefixOut proc

	test byte ptr fUnass,FUNAS_TRANSLATECS
	jz @F
	push eax
	invoke ownerout, edx
	pop eax
	jmp prefixout_2
@@:
	test byte ptr fUnastype,FUT_RMADDR
	jz @F
	@putchr '&'
@@:
	cmp  edx, __flatsel
	jnz @F
	@putchr '%'
	jmp offsetOut
@@:        
	@wordout edx
prefixout_2:
	@putchr ':'
	test cl,1
	jnz @F
	@wordout ebx
	jmp prefixout_1
@@:
offsetOut:
	@dwordout ebx
prefixout_1:
	@putchr ' '
	mov ecx, eax
	sub ecx, ebx
	cmp ecx, 8
	jb @F
	mov ecx, 8
@@:
	mov eax, 8
	sub eax, ecx
	push eax
@@:
;	mov eax,fs
;	lsl eax,eax
	lsl eax, esi
	cmp eax, ebx
	jc @F

	push ds
	mov ds, esi
	mov al, [ebx]
	pop ds

	invoke _hexout
	inc ebx
	loop @B
@@:        
	pop ecx
	jecxz exit
	add ecx,ecx
	invoke printchars, ' ', ecx, 0
exit:
	@putchr ' '
	ret
prefixOut endp

;--- Unassemble command
;--- is expected, if NC, to return:
;---  EAX: new offset
;---  EBX: old offset
;---  ESI: selector

_unass proc c pb:PARMBLK

local	segtype:dword
local	xbuffer[80]:byte

	mov excexit, offset error1
	mov dwExcEbp, ebp
	xor eax, eax
	mov ah, fUnass
	mov segtype, eax
	cmp cl, __FPTR__
	jnz nopmfar
	mov eax, pb.wSeg1
	mov dword ptr [unasarg+4], eax
@@:        
	and byte ptr fUnastype, not FUT_RMADDR
	mov cl, __CONST__
nopmfar:
	cmp cl, __RMLPTR__
	jnz @F
	mov eax, [a1.ARGDESC.dwSeg]
	mov dword ptr [unasarg+4], eax
	or byte ptr fUnastype, FUT_RMADDR
@@:
	cmp cl, __VOID__
	jnz @F
	mov ebx, dword ptr [unasarg+0]
@@:
	mov eax, ebx
	mov edx, dword ptr [unasarg+4]
	test byte ptr fUnastype,FUT_RMADDR
	jz @F
	movzx ebx, dx
	shl ebx, 4
	invoke setworkselbase, ebx
	jmp unass_2
@@:
	verr dx
	jnz exit
	lar ebx, edx
	test ebx, 400000h			;32Bit segment?
	jnz is32bit
	cmp edx, [__flatsel]
	jnz @F
is32bit:
	or byte ptr segtype, 3
@@:        
unass_2:
	mov ecx, [unaslines]
	cmp pb.p2.bType, __CONST__
	jnz @F
	mov ecx, pb.dwOffs2
	cmp ecx, 2
	jb @F
	mov [unaslines], ecx
@@:
nextline:
if ?LOADVDD
	cmp [hVDD], -1
	jz @F
	invoke VerifyAddress, edx::eax, 1
	jc error1
@@:        
endif
	push ecx 			; anzahl zeilen
	push edx 			; adresse (segment/selector)
	push eax 			; adresse (offset)

	test byte ptr fUnastype, FUT_RMADDR
	jz @F
	mov edx, [worksel]
@@:
	push edx
if ?WINDOWS
	invoke UnAssemble, addr xbuffer, edx::eax, segtype, _ReadAccProc
else
	invoke UnAssemble, addr xbuffer, edx::eax, segtype
endif
	pop esi				; selector used
	pop ebx				; get old offset
	pop edx
	pop ecx
	test [fMode], FMODE_NODISP
	jnz unass_4
	pushad
	mov ecx, segtype
	call prefixOut
	lea ebx, xbuffer
	invoke __stroutebx		; modifies ebx, eax
	popad
if ?VMMINT20
	test byte ptr segtype, 3
	jz @F
	call checkvmmfunc
@@:        
endif
unass_4:
	call croutx
	loop nextline
	mov dword ptr [unasarg+0], eax
	mov word ptr [unasarg+4], dx
	clc
	jmp exit
error1:
	mov ebp, dwExcEbp
	mov eax, ERR_ACCESS_CAUSED_EXCEPTION
error:        
	@errorout eax
	stc
exit:
	mov excexit, 0
	ret
_unass endp

;--- callback for disassembler
;--- dwAddress may be:
;--- 1. a flat address (high dword is 0)
;--- 2. a far32 address
;--- 3. a far16 address

SearchSymbol proc stdcall public pszOut:DWORD, qwAddress:QWORD

	test byte ptr fUnastype,FUT_RMADDR
	jnz error
;--- no flat addresses currently
	cmp dword ptr qwAddress+4, 0
	jz error
;--- no far32 addresses currently
	cmp word ptr qwAddress+2, 0
	jnz error
	.if (bTranslateSyms)
ife ?32BIT
		@savewinsegregs
		mov eax, dword ptr qwAddress+4
		push ax
		mov eax, dword ptr qwAddress+0
		push ax
		mov eax, ds
		push ax
		mov eax, pszOut
		push ax
		push word ptr 40h
		call _GetSymbolName
		@restorewinsegregs
		movzx eax, ax
else
  if ?FLAT
		push workselbase
		invoke setworkselbase, pszOut
		mov ecx, worksel
		shl ecx, 16
		xor cx,cx
  else
		mov ecx, ds
		shl ecx, 16
		mov cx, word ptr pszOut
  endif
		mov eax, dword ptr qwAddress+4
		push ax
		mov eax, dword ptr qwAddress+0
		push ax
		push ecx
		push word ptr 40h
		mov ebx, esp
		mov edx, _GetSymbolName
		mov cx, 5
		mov ax, 4B87h	; call GetSymbolName
		@DosCall
		add esp,2*5
  if ?FLAT
		pop ecx
		push eax
		invoke setworkselbase, ecx
		pop eax
  endif
endif
	.else
error:
		xor eax, eax
	.endif
exit:
	ret
SearchSymbol endp


;--- get debuggee CS in AX, type in DL

GetCurrentCS proc stdcall public
	test [fEntry], FENTRY_REAL
	jz @F
	mov ax,[r1r.rCS]
	mov dl,01
	ret
@@:
	mov eax,[r1.rCS]
	mov dl,00
	ret
GetCurrentCS endp

;--- get current CS:EIP in AX:EBX, in real-mode AX will be a segment

getactcseip proc stdcall public
	call GetCurrentCS
GetCurrentEIP::
	test [fEntry], FENTRY_REAL
	jnz getacteip_1
	mov ebx,[r1.rCS]
	lar ebx,ebx
	test ebx,400000h
	mov ebx,[r1.rEip]
	jnz @F
	movzx ebx,bx
@@:
	ret
getacteip_1:
	movzx ebx,[r1r.rIP]
	ret
getactcseip	endp

;--- get current CS:EIP in AX:EBX, type in DL
;--- in real-mode a flat address will be returned

getactcseipx proc
	call getactcseip
	test dl,1			;real mode?
	jz @F
	movzx eax,ax
	shl eax,4
	add ebx,eax
	mov eax,@flat
@@:
	ret
getactcseipx endp

;--- check if next instruction is a int1 or int3
;--- if yes, skip it

checknextinstruction proc stdcall

local eipinh:dword

	test [fMode], FMODE_SKIP	;dont skip int1/3 if skip is active
	jnz exit
	call getactcseipx
	mov esi,ebx
	lea edi,eipinh

	call getmemory			;get AX:ESI to ES:EDI
	cmp bl,1
	jb eiperror

	mov al,byte ptr eipinh

	xor ecx,ecx
	cmp al,0CCh 	  ;INT 3?
	jz cni_1
	cmp al,0CDh
	jz @F
	clc
	jmp exit
@@:
	cmp bl,2
	jb eiperror
	mov al,byte ptr eipinh+1
	cmp al,3		  ;INT 3?
	jz cni_2
	cmp al,1		  ;INT 1?
	jz cni_2
	clc
	jmp exit
eiperror:
	@errorout ERR_CSEIP_INVALID
	jmp mains
cni_2:
	inc ecx
cni_1:
	inc ecx
	test [fEntry], FENTRY_REAL
	jnz @F
	add [r1.rEip],ecx
	jmp cni_3
@@:
	add [r1r.rIP],cx
cni_3:
	call gettracemode  ;im tracemodus dann gleich in debugger gehen
	jz exit
	stc
exit:
	ret
checknextinstruction endp

;--- MarkDOSused will do 2 things
;--- 1. if we are in dos, SDA will be saved to pSdaSave if not already done
;--- 2. switch to debugger's PSP if not already done

MarkDOSused proc stdcall public

	push es
	push ds
	pushad
	mov ds,cs:[__csalias]
	@tprintf <"enter MarkDOSused",lf>
	test [fDOSused], FDU_SAVED
	jnz MarkDOSused_1
if ?CHECKINDOS
	push ds
;	lds bx,dwIndos
;	movzx ebx,bx
	mov ebx, [dwIndos]
ife ?FLAT
	mov ds, cs:[__flatsel]
endif
if 0;?CHECKINDOSVALID
	mov eax,ds
	lsl eax,eax
	cmp eax,ebx
	jb @F
endif
	cmp byte ptr [ebx],0
;	cmp byte ptr [ebx],0		;are we in DOS currently?
@@:
	pop ds
	jz MarkDOSused_1
endif
	mov ecx,[sdalen1]
	mov edi,[pSdaSave]
	and edi,edi
	jz @F
	mov es,[__flatsel]
	push ds					  ;in SDA ist PSP und DTA enthalten!
	movzx esi,word ptr [sdaadr+0]
	movzx eax,word ptr [sdaadr+2]
	shl eax,4
	add esi,eax
	push es
	pop ds
	mov dl,cl
	shr ecx,2
	rep movsd
	mov cl,dl
	and cl,3
	rep movsb
	pop ds
	@tprintf <"MarkDOSused: SDA saved",lf>
@@:
	or [fDOSused], FDU_SAVED
MarkDOSused_1:
	test [fDOSused], FDU_PSPSWITCHED
	jnz MarkDOSused_2
	or [fDOSused], FDU_PSPSWITCHED
	call getpsp
	call getdta
	mov ebx,[_psp]			;debugger PSP
	@DosCall 50h
	push ds
	mov edx,0080h
	mov ds,[_psp]
	@DosCall 1Ah				;reset DTA
	pop ds
	@tprintf <"MarkDOSused: PSP switched to debugger",lf>
MarkDOSused_2:
	@tprintf <"exit MarkDOSused",lf>
	popad
	pop ds
	pop es
	ret
MarkDOSused endp

;--- MarkDOSnotused will do 2 things
;--- 1. if PSP has been switched to debugger, switch it back
;--- 2. if SDA has been saved, restore it

MarkDOSnotused proc stdcall

	@tprintf <"enter MarkDOSnotused",lf>
	test [fDOSused], FDU_PSPSWITCHED
	jz MarkDOSnotused_1
	mov ebx,[dwCurPsp]
	verw bx
	jnz @F
	@DosCall 50h
@@:
	push ds
if ?32BIT
	lds edx, fword ptr [dfCurDta]
else
	lds dx, dword ptr [dfCurDta]
endif
	@tprintf <"MarkDOSnotused: restore DTA to %X:%X",lf>,ds,edx
	@DosCall 1Ah
	pop ds
	and [fDOSused], not FDU_PSPSWITCHED
MarkDOSnotused_1:
	test [fDOSused], FDU_SAVED
	jz exit

	mov ecx,[sdalen1]
	mov esi,[pSdaSave]
	and esi,esi
	jz @F
	movzx edi,word ptr [sdaadr+0]
	movzx eax,word ptr [sdaadr+2]
	shl eax,4
	add edi,eax
ife ?FLAT
	push  ds
	push  es
	mov   ds,[__flatsel]
	push  ds
	pop   es
	rep   movsb
	pop  es
	pop  ds
else
	rep   movsb
endif
	@tprintf <"MarkDOSnotused: SDA restored",lf>
@@:
	and [fDOSused], not FDU_SAVED
exit:
	ret
MarkDOSnotused endp

;-- IF command

_if proc c

	lea esi,fNotActive
	inc byte ptr [ifcount]
	cmp byte ptr [esi],0
	jnz @F
	and ebx,ebx
	jnz @F
	mov al,[ifcount]
	mov [esi],al
@@:
	ret
_if endp

_else proc c
	mov al,[ifcount]
	cmp al,[elsecnt]
	mov [elsecnt],al
	jnz else_1
	cmp al,2
	jnb @F
	@errorout ERR_ELSE_WITHOUT_IF
	ret
@@:
	call _endif
	jmp _else
else_1:
	lea esi,fNotActive
	cmp byte ptr [esi],0		;augenblicklich deaktiv?
	jnz @F						;dann sprung
	mov [esi],al				;ansonsten deaktivieren
	jmp else_ex
@@:
	cmp al,[esi]				;aktueller level?
	jnz else_ex
	mov byte ptr [esi],0		;aktivieren
else_ex:
	ret
_else endp

_endif proc c
	mov byte ptr [elsecnt],0
	mov al,[ifcount]
	cmp al,0
	jnz @F
	@errorout ERR_ENDIF_WITHOUT_IF
	ret
@@:
	cmp al,[fNotActive]
	jnz @F
	mov byte ptr [fNotActive],0
@@:
	dec al
	mov [ifcount],al
	ret
_endif endp

;--- Z command

_breakremove proc C

	mov [excexit], offset error
	call getlinearcseip
	mov ebx,eax
	cmp byte ptr @flat:[ebx],0CCh
	jnz @F
	mov byte ptr @flat:[ebx],90h
	ret
@@:
	cmp word ptr @flat:[ebx-2],01CDh
	jnz @F
	mov word ptr @flat:[ebx-2],9090h
	ret
@@:
	@stroutc "no break found",lf
	ret
error:
	@errorout ERR_ACCESS_CAUSED_EXCEPTION
	ret

_breakremove endp

checkifactive proc stdcall
	cmp byte ptr [fNotActive],0
	jz checkifactive_ex
	mov eax,[esi.SYMBOL.dwProc]
	cmp eax,offset _endif
	jnz @F
	call _endif
	jmp checkifactive_er
@@:
	cmp eax,offset _if
	jnz @F
	call _if
	jmp checkifactive_er
@@:
	cmp eax,offset _else
	jnz @F
	call _else
checkifactive_er:
@@:
	stc
	ret
checkifactive_ex:
	clc
	ret
checkifactive endp

;*** trace, perform, animate, jump, go ***

prefixopctab label byte
	db 26h,2eh,36h,3eh
	db 64h,65h,66h,67h
lprefixopctab equ $ - prefixopctab

jmpopctab label byte
	db 70h,71h,72h,73h,74h,75h,76h,77h	  ;jmp relativ
	db 78h,79h,7Ah,7Bh,7Ch,7Dh,7Eh,7Fh
	db 0E3h 							  ;jcxz
	db 0E9h,0EAh,0EBh					  ;jmp near/jmp far/jmp short
	db 0C2h,0C3h,0CAh,0CBh				  ;ret x,ret,retf x,retf
ljmpopctab equ $ - jmpopctab

jmpopctab2 label byte
	db 80h,81h,82h,83h,84h,85h,86h,87h
	db 88h,89h,8Ah,8Bh,8Ch,8Dh,8Eh,8Fh
ljmpopctab2 equ $ - jmpopctab2


setdl2rmpm:
	mov dl,00			  ;typ des breakpoints
	test [fEntry], FENTRY_REAL
	jz @F
	inc dl
@@:
	ret

;--- CS:EIP -> INT 21, check if app terminates

checkterminate proc stdcall
	mov cl,byte ptr [r1.rEax+1]
	cmp cl,4Ch
	jz isterm
	cmp cl,31h
	jz isterm
exit:
	ret
isterm:
	cmp tracesteps,0
	jz exit
	invoke _crout
	@errorout WRN_PROGRAM_WILL_TERMINATE
	jmp mains
        
checkterminate endp

;--- set a break at CS:E/IP on [ss:e/sp]

setbrkforsssp proc        
	test dl,1
	jz @F
	movzx ebx,[r1r.rSS]	 		;in real mode break an
	shl ebx,4					;rueckkehradresse
	movzx ecx,[r1r.rSP]
	add ebx,ecx
	mov ax,word ptr @flat:[ebx+2]	;real mode CS into AX
	mov bx,word ptr @flat:[ebx+0]	;real mode IP into BX
	and byte ptr [r1r.rFlags+1],not FL_TRACE
	ret
@@:
	mov eax, r1.rSS
	mov fs, eax
	mov ebx, r1.rEsp
	lar ecx, eax
	test ecx,400000h
	jnz @F
	movzx ebx,bx
@@:
	lsl ecx, eax
if ?32BIT
	lea eax,[ebx+5]
	cmp ecx, eax
	jc exit
	mov ax, fs:[ebx+4] 
	mov ebx, fs:[ebx+0] 
else
	lea eax,[ebx+3]
	cmp ecx, eax
	jc exit
	mov ax, fs:[ebx+2] 
	movzx ebx, word ptr fs:[ebx+0] 
endif
	verr ax
	jnz error
	and byte ptr [r1.rEfl+1],not FL_TRACE
exit:
	ret
error:
	test [fTMode], FTMODE_STOPIFCTRLLOST
	jz exit2				;silent exit, continue trace mode
	pushad
	mov eax, MSG_UNKNOWN_PM_CONTINUE
	call askuser
	popad
	jnc exit2
	@errorout ERR_PMBREAK_UNKNOWN
	jmp mains
exit2:
	stc
	ret
setbrkforsssp endp

;--- check if CS:EIP = dpmi raw switch address

checkrawswitch proc        
	test dl,1
	jz isprotmode
	cmp bx,word ptr dpmirm2pm+0	;check for raw switch to prot mode
	jnz norawswitch
	cmp ax,word ptr dpmirm2pm+2
	jnz norawswitch
	and byte ptr [r1r.rFlags+1],not FL_TRACE
	movzx eax,word ptr [r1.rEsi]	;SI=prot mode CS
if ?32BIT
	mov ebx,[r1.rEdi]
else
	movzx ebx,word ptr [r1.rEdi]	;E/DI=prot mode E/IP
endif
	mov dl,0					;is a protected mode address
	clc
	ret
isprotmode:								;check for raw switch to real mode
if ?32BIT
	cmp ebx,dword ptr dpmipm2rm+0
	jnz norawswitch
	cmp ax,word ptr dpmipm2rm+4
	jnz norawswitch
else
	cmp bx,word ptr dpmipm2rm+0
	jnz norawswitch
	cmp ax,word ptr dpmipm2rm+2
	jnz norawswitch
endif
	and byte ptr [r1.rEfl+1],not FL_TRACE
	mov ax,word ptr [r1.rEsi]	;SI=real mode CS
	mov bx,word ptr [r1.rEdi]	;DI=real mode IP
	mov dl,1					;is a real-mode address
	clc
	ret
norawswitch:
	stc
	ret
checkrawswitch endp

;--- check if CS:EIP = dpmi save/restore task state address

checksaverestore proc        
	test dl,1				;make it a valid address
	jz pmsrts
	cmp bx, word ptr dpmisrtsRM+0
	jnz @F
	cmp ax, word ptr dpmisrtsRM+2
	jz issrts
@@:
	stc
	ret
pmsrts:        
if ?32BIT
	cmp ebx,dword ptr dpmisrtsPM+0
	jnz @F
	cmp ax,word ptr dpmisrtsPM+4
	jz issrts
else
	cmp bx,word ptr dpmisrtsPM+0
	jnz @F
	cmp ax,word ptr dpmisrtsPM+2
	jz issrts
endif
@@:
	stc
issrts:
	ret
checksaverestore endp

if ?32BIT
?SUPP32RTM	equ 1
else
?SUPP32RTM	equ 0
endif

;--- out AX:EBX = CS:EIP

checkint proc        
	cmp byte ptr fs:[ebx],0CDh		;INT XX?
	jnz notint
	cmp byte ptr fs:[ebx+1],21h		;INT 21h?
	jnz notint21
	call checkterminate
if ?SUPPDOS4GW
	test dl,1
	jnz notint21
	test [fTMode], FTMODE_ISDOS4GW
	jz notint21
	cmp byte ptr r1.rEax+1, 0FFh	;DOS4/GW function?
	jnz notint21
	cmp byte ptr r1.rEdx+1, 10h		;DOS4/GW program exec?
	jnz notint21
@@:
	mov ax,word ptr r1.rEbx
	movzx ebx,word ptr r1.rEcx
	jmp done
endif
notint21:
if ?SUPP32RTM
	cmp byte ptr fs:[ebx+1],2Fh		;INT 2Fh?
	jnz notint2F
	test dl,1
	jz notint2F
	cmp word ptr r1.rEax, 0FB42h	;32RTM call?
	jnz notint2F
	cmp word ptr r1.rEbx, 03233h	;???
	jnz notint2F
	mov bl,2Fh
	@DpmiCall 200h
	movzx ebx,cx
	shl ebx,4
	movzx edx,dx
	add ebx,edx
	mov eax,@flat
	mov dl,00
	jmp done
notint2F:
endif
	inc ebx
	inc ebx
	test [fTMode], FTMODE_NOTRACEFORINT
	jz done
	call resettracemode
done:
	clc
	ret
notint:
	stc
	ret
checkint endp        

checkrep proc
	mov cl,fs:[ebx]
	and cl,0FEh
	cmp cl,0F2h
	jnz norep
	mov cl,fs:[ebx+1]
	cmp cl,0A4h
	jb norep
	and cl,0FEh
	cmp cl,0A8h
	jz norep
	cmp cl,0B0h
	jnb norep
	call resettracemode
	inc ebx				;is opcode size always 2?
	inc ebx
	clc
	ret
norep:
	stc
	ret
checkrep endp        

;--- settracemode
;--- called by settracevars
;--- check some special conditions like
;--- - vm breakpoints 
;--- - INT xx instructions
;--- - int 21h, ah=4ch

settracemode proc

	test [fEntry], FENTRY_REAL
	jz @F
	or byte ptr [r1r.rFlags+1],FL_TRACE
	jmp settracemode_0
@@:
	or byte ptr [r1.rEfl+1],FL_TRACE
settracemode_0:
	pushad
	call getactcseip			;CS:E/IP to AX:E/BX

	call checkrawswitch		;is a raw switch?
	jnc setbreak

;	call checksaverestore	;is a save/restore task state?
;	jnc setstacktrace

	test dl,1
	jz isprotmode
	cmp wRMStop,0
	jz @F
	cmp ax, wRMStop
	jz setstacktrace
@@:
	cmp bx, word ptr dpmiPMentry+0
	jnz @F
	cmp ax, word ptr dpmiPMentry+2
	jz setstacktrace
@@:
	movzx eax, ax				;convert AX:BX to a protected mode addr
	shl eax, 4
	add ebx, eax
	mov eax, @flat
	mov fs, eax
	mov cx,wRMBreak
	and cl, cl
	jz protandreal
	cmp cl, fs:[ebx]		;RM-Breakpoint?
	jnz protandreal
	and ch, ch
	jz setstacktrace
	cmp ch, fs:[ebx+1]
	jz setstacktrace
	jmp protandreal
isprotmode:
if ?CS16ALIAS
	cmp ax, word ptr __cs16alias
	jz setstacktrace
endif
	cmp ax, wPMStop
	jz setstacktrace
	mov fs, eax
	mov cx, wPMBreak
	and cl, cl
	jz @F
	cmp cl, fs:[ebx]		;RM-Breakpoint?
	jnz @F
	and ch, ch
	jz setstacktrace
	cmp ch, fs:[ebx+1]
	jz setstacktrace
@@:        
	test al, 4				;CS in LDT?
	jnz @F
	test [fTMode], FTMODE_NOTRACEIFCSINGDT
	jnz setstacktrace		;this may not work in all cases
	jmp exit		 		;don't set auto break
@@:        
protandreal:
	test [fTMode], FTMODE_CHECKINT
	jz @F
	call checkint
	jnc setbreakEx
@@:
	test [fTMode], FTMODE_CHECKREP
	jz @F
	call checkrep
	jnc setbreakEx
@@:
	jmp exit
setstacktrace:
	call setbrkforsssp
	jc exit
	jmp setbreak
setbreakEx:
	mov dl,00
setbreak:
	mov cl, FBRP_AUTO or FBRP_SILENT
	call insertbrkpnt
	and [fExit], not FEXIT_NOSWITCH
exit:
	popad
	ret
settracemode endp

gettracemode proc
	test [fEntry], FENTRY_REAL
	jz @F
	test byte ptr [r1r.rFlags+1],FL_TRACE
	ret
@@:
	test byte ptr [r1.rEfl+1],FL_TRACE
	ret
gettracemode endp

resettracemode proc
	test [fEntry], FENTRY_REAL
	jz @F
	and byte ptr [r1r.rFlags+1],not FL_TRACE
	ret
@@:
	and byte ptr [r1.rEfl+1],not FL_TRACE
	ret
resettracemode endp

checkifappactive:

	cmp [cLoad], 0
	jz @F
	ret
@@:        
	@errorout MSG_NO_PROGRAM_LOADED
	jmp mains

;--- settracevars
;--- called by Trace command
;--- inp: AL=-1/00
;--- out: AL=0 -> trace will be logged
;---      AL!=0 -> trace will not be logged

settracevars proc stdcall public
	or [fExit], FEXIT_TRACE
	call settracemode
	cmp al,-1
	jnz @F
	mov al,[bTraceFlags]
@@:
	and al,FTRAC_LOG
	cmp al,1
	sbb al,al			;0->FF,1->0
	and [fMode], not FMODE_NODISP
	and al, FMODE_NODISP
	or [fMode], al
	ret
settracevars endp

;--- command Trace

_traceproc proc c pb:PARMBLK
	call checkifappactive
trace1::							 ;<--- entry von perform und jump
	mov tracesteps,0
	mov al,1
	cmp pb.p2.bType, __CONST__
	jnz @F
	mov al,byte ptr pb.dwOffs2
@@:
	mov bTraceFlags,al
	test [fTMode], FTMODE_SWAPALWAYS
	jnz @F
	or [fExit], FEXIT_NOSWITCH
@@:        
	mov al,-1
	call settracevars
	cmp pb.p1.bType, __DWORD__
	jnz _reset
	mov eax,pb.dwOffs1
	mov [tracesteps],eax
	or [fMode], FMODE_EXEACTIVE
	mov [exitproc],offset nexttrace
	jmp _reset
nexttrace:
	test [fEntry], FENTRY_INT01 or FENTRY_SILENTBRK
	jz exit
	dec [tracesteps]
	jz exit
	test [fTMode], FTMODE_SWAPALWAYS
	jnz @F
	or [fExit], FEXIT_NOSWITCH
@@:        
	mov al, -1
	call settracevars
if 1
	cmp fIrq,0					;interrupts enabled?
	jnz nt_1
	call GetInpStatus			;is a key pressed?
	jz nt_1
	call getcharex
	cmp al,4					;Ctrl-D?
	jz mains
endif        
nt_1:
	or [fMode], FMODE_EXEACTIVE
	jmp _reset
exit:
	retn
_traceproc endp

dpmimodechange proc
	cmp al,3
	jb dpmimodechange_ok
	stc
	ret
dpmimodechange_ok:
	cmp al,0
	jnz @F
	mov bl,byte ptr [r1.rEbx]
	@DpmiCall 0200h
	mov ax,cx
	movzx ebx,dx
	jmp dpmimodechange_1
@@:
	mov ebx,[r1.rEdi]
if ?32BIT eq 0
	movzx ebx,bx
endif
	mov fs,[r1.rES]
	mov ax,fs:[ebx.RMCS.rCS]
	movzx ebx,word ptr fs:[ebx.RMCS.rIP]
dpmimodechange_1:
	mov dl,1
	mov cl,FBRP_AUTO
	call insertbrkpnt
	clc
	ret
dpmimodechange endp

;--- IRB command ???

_setrmbreak proc c pb:PARMBLK

	mov bl,byte ptr pb.dwOffs1
	cmp pb.p1.bType, __CONST__
	jz @F
	call getactcseipx		;CS:EIP->AX:EBX
	mov fs, eax
	mov al, fs:[ebx]
	cmp al, 0CDh
	jnz exit
	mov bl, fs:[ebx+1]
@@:
							;get real mode int
	@DpmiCall 0200h
	movzx ebx,dx
	mov eax,ecx
	mov dl,1
	mov cl,FBRP_AUTO
	call insertbrkpnt
	@stroutc "RM breakpoint set",lf
exit:
	ret
_setrmbreak endp

;*** Jump command: step into int xx instruction ***

_jump proc c public pb:PARMBLK

	call checkifappactive
	call getactcseipx		;get linear address of cs:eip	
	mov fs, eax
	mov al, fs:[ebx]
	cmp al, 0CDh
	jnz trace1				;no INT xx, do normal trace
	mov bl,fs:[ebx+1]
	test dl, 1				;debuggee in real mode?
	jnz jump_1
							;debuggee in protected mode
	cmp bl, 31h				;dpmi int 31h?
	jnz @F
	mov eax,[r1.rEax]
	cmp ah, 03				;int 31h, ax=03xxh?
	jnz @F
	call dpmimodechange
	jmp jump_3				;trace flag immer setzen
@@:
	movzx edx,dx
	cmp pb.dwOffs1,1		;Jump with argument '1'?
	jz jump_11				;then forget any protmode int handlers
							;get pm int
	@DpmiCall 204h
	test cl,4				;is CS in GDT?
	jz jump_11
	mov ebx,edx
	mov eax,ecx
	mov dl,00
	jmp jump_2
jump_1: 					;debuggee is in real mode
	cmp bl, 21h				;Int 21 in real mode?
	jnz jump_11
	cmp byte ptr [r1.rEax+1], 4Ch	;int 21,AH=4C?
	jnz jump_11
	call getpsp
	invoke getbaser,eax
	mov bx, word ptr @flat:[eax+0Ah]
	mov ax, word ptr @flat:[eax+0Ch]	;get int 22 vector
	mov dl, 1
	jmp jump_2
jump_11:					;get real mode int vector
	@DpmiCall 200h
	movzx ebx,dx
	mov eax,ecx
	mov dl,1
jump_2:
	mov cl,FBRP_AUTO		;set breakpoint at ax:ebx
	call insertbrkpnt
jump_3:
	jmp trace1
_jump endp

;--- Perform command

_perform proc c public pb:PARMBLK

	call checkifappactive
	call calcopsize
	and eax,eax
	jnz @F
	ret
@@:
	mov esi,eax
	call GetCurrentEIP
	call GetCurrentCS
	test dl,1
	jz @F
	movzx eax,ax
	shl eax,4
	movzx ebx,word ptr [r1r.rIP]
	add ebx,eax
	mov eax,[__flatsel]
@@:
	mov fs,eax
@@:
	mov edi,offset prefixopctab
	mov ecx,lprefixopctab
	mov al,fs:[ebx]
	repnz scasb
	jnz @F
	inc ebx
	dec esi
	jmp @B
@@:
	cmp al,0FFh
	jnz @F
	mov al,fs:[ebx+1]	;indirekter sprung?
	and al,30h
	cmp al,20h
	jz perform_1
@@:
	mov edi,offset jmpopctab
	mov ecx,ljmpopctab
	cmp al,0Fh
	jnz @F
	mov edi,offset jmpopctab2
	mov ecx,ljmpopctab2
	mov al,fs:[ebx+1]
@@:
	repnz scasb
	jz perform_1
	add ebx,esi
	call GetCurrentCS	  ;CS-> AX, Typ -> DL
	test dl,1
	jz @F
	movzx esi,ax			  ;im real mode eip wieder korrigieren
	shl esi,4
	sub ebx,esi
@@:
	mov cl,FBRP_AUTO
	call insertbrkpnt
	jmp _reset
perform_1:
	jmp trace1
_perform endp

;--- Go command

_go proc c public pb:PARMBLK

ife ?WINDOWS
	call checkifappactive
endif        
	movzx ecx, byte ptr pb.wArgc
	jecxz go_1
	lea esi, pb.dwOffs1
nextitem:
	push ecx
	mov ebx, [esi.PARM.dwOffs]
	mov ch, byte ptr [esi.PARM.wType]
	call GetCurrentCS			;CS -> AX
	cmp ch, __FPTR__
	jnz @F
	mov eax, [esi.PARM.dwSeg]
	mov dl, 00
@@:
	cmp ch,__RMLPTR__
	jnz @F
	mov eax,[esi.PARM.dwSeg]
	mov dl,01
	cmp cl, byte ptr pb.wArgc	;argument 1 segment is converted!
	jnz @F
	mov eax, a1.dwSeg 
@@:
	mov cl,FBRP_AUTO
	@tprintf <"go break: %X:%X(%X)",lf>, eax, ebx, edx
	call insertbrkpnt			;AX:EBX wird breakpoint
	add esi,sizeof PARM
	pop ecx
	loop nextitem
go_1:
if ?RESTRACEFLG eq 0
	call resettracemode
endif
	jmp _reset
_go endp

;*** quit routine ***

	@cmdproc

_quit proc c pb:PARMBLK
	@stroutc "return ...",lf
	or [fExit], FEXIT_CANCEL
	and byte ptr [r1.rEfl+1], not FL_TRACE
	jmp _reset
_quit endp

	@cmdprocend

;*** debugger exit
;*** no parameter

_reset proc stdcall public

	call MarkDOSnotused
	@resetint24 				; for windows only
	call checknextinstruction	; skip int1/int3 on cs:eip
	jc debug_entryx				; if trace jmp into debugger
	test [fExit], FEXIT_CANCEL
	jnz @F
ife ?WINDOWS
 ife ?LDRDISABLE
	call setloaderstate
 endif
endif   
	call SetTheBreaks
	call ActAllHWBreaks
@@:
;	call CheckIrqSetting
	call SwitchToDebuggeeScreen
	call restoreenvironment
	call ReadReg 				; get orginal registers
	mov [fEntry], 00			; clear entry flags
	and [fExit], not FEXIT_CANCEL
if 0
	push [r1.rSS]
	pop  [r1.rEbx]	;is just behind rEsp member
	lss  esp, fword ptr [r1.rEsp]
else
	mov ss, [r1.rSS]
	mov esp, [r1.rEsp]
endif
	push dword ptr [r1.rEfl]
	push dword ptr [r1.rCS]
	push dword ptr [r1.rEip]
	test byte ptr [r1.rEfl+1], FL_INT ; interrupts disabled?
	mov ds,[r1.rDS]
	jz @F
	sti
	iretd
@@:
	cli
	iretd
_reset endp

resetpic proc stdcall

	mov dx,0020h
rp0:
	mov ecx,8
rp1:
	mov al, 0Bh		  ;ISR holen
	out dx, al
	nop
	nop
	in al, dx
	and al, al
	jz @F
	inc bl
	cmp bh, 0
	jz @F
	mov al, 20h
	out dx, al
	loop rp1
@@:
	cmp dl,20h
	mov dl,0A0h
	jz rp0
	ret
resetpic endp

checkpic proc stdcall

	xor ebx, ebx
	call resetpic
	and bl, bl
	jz exit
	mov eax, MSG_INISR
	call askuser
	jc exit
	mov bh, 1
	call resetpic
exit:
	ret
checkpic endp

closeall:
	mov ecx, edi
	mov ebx, esi
closeall_0:
	push ebx
	mov fs, [__flatsel]
	mov al, fs:[ebx]
	cmp al, 0FFh
	jz @F
	sub ebx, esi
	mov ax, 4400h
	@DosCall
	jnc @F
	pop ebx
	push ebx
	sub ebx, esi
	mov ah, 3Eh
	@DosCall
@@:
	pop ebx
	inc ebx
	loop closeall_0
	ret

closefiles:
	mov eax,MSG_OPENFILES
	call askuser
	jc @F
	call closeall
@@:
	ret

;--- display a message in eax, then ask user to continue
;--- out: C if not continue

askuser proc stdcall
	@errorout eax
@@:
	call _getchar
	or al,20h
	cmp al,'c'
	jz mains
	cmp al,'n'
	stc
	jz @F
	cmp al,'y'
	jnz @B
@@:
	pushfd
	invoke _crout
	popfd
	ret
askuser endp

checkopenfiles:
	verr ax
	jnz checkopenfiles_ex
	mov  fs,eax
	movzx ebx,word ptr fs:[0034h]
	movzx eax,word ptr fs:[0036h]
	movzx ecx,word ptr fs:[0032h]
	shl eax,4
	add ebx,eax
	mov esi,ebx
	mov edi,ecx
checkopenfiles_0:
	mov fs,[__flatsel]
	mov al,fs:[ebx]
	cmp al,0FFh
	jz @F
	push ebx
	sub ebx,esi
	mov ax,4400h
	mov dx,0000h
	@DosCall
	pop ebx
	jc @F
	test dh,80h
	jz checkopenfiles_1
@@:
	inc ebx
	loop checkopenfiles_0
checkopenfiles_ex:
	clc
	ret
checkopenfiles_1:
	stc
	ret

_cancel proc c pb:PARMBLK

if ?WINDOWS
	cmp byte ptr pb.wArgc,0
	jz @F
	@savewinsegregs
	call _GetCurrentTask
	@restorewinsegregs
	cmp ax,word ptr pb.dwOffs1	;is it current task?
	jz @F
	@savewinsegregs
	mov eax,pb.dwOffs1
	push ax
if ?CS16ALIAS
	mov eax,__cs16alias
else
	mov eax,cs
endif        
	push ax
	mov eax,offset cancel1
	push ax
	call _TaskSwitch
	@restorewinsegregs
	and ax,ax
	jnz cancel_ex
	@errorout ERR_UNSUCCESSFUL_CANCEL
	jmp cancel_ex
@@:
endif
	call MarkDOSnotused
if ?WINDOWS
	mov ah,62h
	int 21h
	movzx eax,bx
else
	call getpsp
endif        
	cmp eax,[_psp]			; ist aktuelle anwendung debugger selbst?
	jnz @F
	call LoadState			; LoadState versuchen
if ?WINDOWS
	jmp mains
else
	jmp mains				; aenderung 18.6.2005: zurueck
;	jnc mains				; falls ok, zurueck
endif        
@@:
	call checkopenfiles
	jnc @F
	call closefiles
@@:
	call checkpic
if ?MARKPSP
	call getpsp
	push ds
	mov ds, eax
	test byte ptr ds:[4Fh],80h
	pop ds
	jnz @F
endif
	test [fEntry], FENTRY_REAL
	jnz cancel_rm
@@:        
	or [fExit], FEXIT_CANCEL
	and byte ptr [r1.rEfl+1],not FL_TRACE
if ?WINDOWS
	mov eax, __cs16alias
	mov [r1.rCS],eax
else
	mov [r1.rCS],cs
endif        
	mov [r1.rSS],ss
	mov [r1.rDS],ss
	mov [r1.rEsp],esp
	mov [r1.rEip],offset cancel1
;;	xor eax,eax
;;	mov [r1.rDS],eax
;;	mov [r1.rES],eax
;;	mov [r1.rFS],eax	;FS is required for PE apps!
;;	mov [r1.rGS],eax
	jmp _reset

if ?WINDOWS
cancel1:
	db 0b8h, 0ffh, 4ch	; mov ax,4cffh
	int 21h
else
szByestring db "^C",13,10,"$"
cancel1:
	mov ah,09
	mov edx,offset szByestring
	@DosCall
	mov ax,4cffh
	@DosCall
endif        
	jmp mains
cancel_ex:
	ret
_cancel endp

cancel_rm:
	mov ax, __RmSeg
	shl eax, 16
	push ds
	mov ds, [__RmDS]
	assume ds:_TEXT16
	movzx ecx, ds:[DEBRMVAR.wCancelRM]
	pop ds
	assume ds:@data
	mov ax, cx
	mov dword ptr [r1r.rIP], eax
	jmp _reset

;--- .Quit command

	@cmdproc

_quitdeb proc c pb:PARMBLK
if ?WINDOWS
	@savewinsegregs
	push [hMyTask]
	push word ptr WM_CLOSE
	push word ptr 0
	push dword ptr 0
	call _PostAppMessage
	@restorewinsegregs
	jmp _quit
else
 if 0
	test [fStat], FSTAT_RESIDENT
	jz _quit

;--- to check if cLoad is > 0 is pretty useless. When the
;--- debugger is resident, it usually gets activated by
;--- a hard-coded breakpoint. In such cases cLoad is still 0.
	cmp [cLoad], 1
	jnz error
;--- just to activate the debugger PSP will hardly work. To modify
;--- the parent PSP is also rather hackish, since there's no documented
;--- way for a TSR to tell DOS at what address the parent is to be "continued".
	push cs
	pop ds
	call _disable
	mov ebx,[_psp]
	mov ah,50h
	@DosCall
	mov ah,4Ch
	@DosCall
error:
	@errorout ERR_CANNOT_QUIT
 endif
endif
	ret
_quitdeb endp

ife ?WINDOWS 
_resident proc c pb:PARMBLK
	call getpsp
	cmp eax,[_psp]
	jnz resident2
	inc [cLoad] 					; no savestate, just increment
	mov [r1.rCS], cs
	mov [r1.rEip], offset resident1
	or [fStat], FSTAT_RESIDENT
	jmp _reset
resident1:
 ife ?LDRDISABLE
	call enableloader
 endif
	mov ax, 3100h				; DPMI loader should care for the rest...
	@DosCall
resident2:
	@errorout ERR_DBGPSP_NOT_ACTIVE
	ret
_resident endp
endif

	@cmdprocend

;--- set base of working selector, limit to 64 kB

setworkselbase proc stdcall public dwBase:dword

	pushad
	mov ebx,worksel
	mov eax,dwBase
	cmp eax,[workselbase]
	jz @F
	mov [workselbase],eax
	push eax
	pop dx
	pop cx
	mov ax, 7
	int 31h
@@:
	lsl eax, ebx
	cmp eax, 0FFFFh
	jz @F
	@mov edx, -1
	xor ecx, ecx
	mov ax, 8
	int 31h
@@:
	popad
	ret
setworkselbase endp

setworksellimit proc stdcall public dwLimit:dword

	pushad
	mov dx, word ptr dwLimit+0
	mov cx, word ptr dwLimit+2
	mov ebx, worksel
	mov ax, 8
	int 31h
	popad
	ret
setworksellimit endp

;*** diverse subroutinen ***

if ?USETOOLHELP

;--- toolhelp InterruptRegister callback

THICB struct
dwTHret dd ?	; return to toolhelp lib
wAX     dw ?
wExc    dw ?	; exception/interrupt (bit 7: 1=dwSSSP is valid)
wHandle dw ?
wIP     dw ?
wCS     dw ?
wFlags  dw ?
dwSSSP  dd ?	; for stack exceptions only
THICB ends

intrcb proc c

	@switch32bit
	push ebp
	movzx ebp, sp
	mov ax, [ebp+4].THICB.wExc
	cmp ah,00h
	jz @F
	mov al, 1Fh		;for ctrl-alt-sysreq use 1Fh
	mov ah, 00	
@@:
	and al,1Fh
	bt word ptr cs:[toolhlpvecs], ax
	jnc intrcb_01
	test cs:[fMode], FMODE_SKIP
	jz @F
	push ds
	mov ds,cs:[__csalias]
	and [fMode], not FMODE_SKIP
	pop ds
	jmp intrcb_01
@@:
	mov ax,[ebp+4].THICB.wExc
	cmp ax,100h		; ctrl-alt-sysreq
	jz intrcb_100
	cmp al, 0		; exception 0
	jz intrcb_0
	cmp al, 1		; interrupt 1
	jz intrcb_1
	cmp al, 3		; interrupt 3
	jz intrcb_3
	cmp al, 6		; exception 6
	jz intrcb_6
	cmp al,0Ch
	jz intrcb_C
	cmp al,0Dh
	jz intrcb_D
	cmp al,0Eh
	jz intrcb_E
intrcb_01:
	pop ebp
	db 66h
	retf
intrcb_0:
	push offset divbyzero
	jmp intrcb_x
intrcb_1:
	push offset singlstep
	jmp intrcb_x
intrcb_3:
	push offset breakpnt
	jmp intrcb_x
intrcb_6:
	push offset invalidopcode
	jmp intrcb_x
intrcb_C:
	push offset stackexc
	jmp intrcb_x
intrcb_D:
	push offset protexc
	jmp intrcb_x
intrcb_E:
	push offset pageexc
	jmp intrcb_x
intrcb_100:
	push ds
	mov ds,cs:[__csalias]
	or [fEntry], FENTRY_SYSREQ
	pop ds
	push offset sysrequest
	jmp intrcb_x

;*** DWORD [bp+00]=offset to jump to
;***  WORD [bp+04]=original bp
;*** DWORD [bp+06]=toolhlp ret
;***  WORD [bp+10]=ax saved by toolhelp
;***  WORD [bp+12]=exc#
;*** DWORD [bp+14]=handle

intrcb_x:
	pop dword ptr [ebp+4].THICB.wExc
;	mov ax, [bp+0]		; vektor sichern
;	mov [bp+6].THICB.wExc, ax
;	mov ax, [bp+2]
;	mov [bp+6].THICB.wHandle, ax
;	add esp, 4			; skip vector

	pop ebp				; restore ebp
	add esp, 4			; skip th cs:ip
	pop ax				; restore ax
	cmp cs:[excexit], 0
	jnz @F
	ret					; jump to offset saved at [esp]
@@:
	add esp, 4+6		; skip offset + IP,CS,FL
	mov ds, cs:[__csalias]
	push ds
	pop es
	jmp [excexit]		; and use excexit
intrcb endp

endif	;?usetoolhelp

;--- standard interrupt handlers
;--- int 00 divbyzero
;--- int 06 invalidopcode

divbyzero_16:
	@switch32bit
divbyzero:
	push MSG_DIVBYZERO
	jmp xxxexc
invalidopcode_16:
	@switch32bit
invalidopcode:
	push MSG_INVALID_OPCODE
	jmp xxxexc
stackexc:
	push MSG_STACK_EXC
	jmp xxxexc
protexc:
	push MSG_PROT_EXC
	jmp xxxexc
pageexc:
	push MSG_PAGE_EXC
	jmp xxxexc
sysrequest:
	push MSG_CTRL_SYSREQ
xxxexc:
	@errorout
	jmp xxxexcx
xxxexcx_16:
	@switch32bit

;--- here the following code will finally come
;--- toolhelp interrupt callback for exc 00, 06, 0C, 0D, 0E and sysreq
;--- handler for int 00, 06 
;--- handler for int 04, 05 

xxxexcx:
	push ds
	mov ds,cs:[__csalias]
	or [fEntry], FENTRY_EXCEPTION
	and [fMode], not FMODE_EXEACTIVE
	test [fMode], FMODE_INDEBUG
	pop ds
	jz debug_entry

	push gs
	push fs
	push ds
	push es
	pushad
	mov ds,cs:[__csalias]
	push ds
	pop es
	@loadesp ebp
	lea ebx, [ebp+12*4]
	mov al, 0
	call PutRegsOnHeap
	popad
	pop es
	pop ds
	pop fs
	pop gs
	jmp debug_entry

;--- std interrupt handler for int 08-0F

xxxirqx_16:
	@switch32bit
xxxirqx:
	push ds
	mov ds,cs:[__csalias]
	and [fMode], not FMODE_EXEACTIVE
;	@stroutc "irq received",lf
if 0
	push eax
	mov al,0Bh		 ;get interrupt service register
	out 20h,al
	in al,20h
	test al,02h		 ;tastatur interrupt in service?
	jz @F
	in al,60h
	mov al,20h
	out 20h,al
@@:
	pop eax
endif
	pop ds
	jmp debug_entry

;*** single step handler protected mode (INT 01) ***

singlstep_16:
	@switch32bit
singlstep:
	push ds
	mov ds, cs:[__csalias]
	or [fEntry], FENTRY_INT01
	pop ds
	jmp debug_entry

;*** int 02 (NMI) handler (INT 02) ***

intr02_16:
	@switch32bit
intr02:

	push ds
	mov ds, cs:[__csalias]
	and [fMode], not FMODE_EXEACTIVE
	or [fEntry], FENTRY_SYSREQ
	@stroutc lf,"Int 02 has occured, will break into debugger",lf
	pop ds
	jmp debug_entry

;*** breakpoint handler protected mode (INT 03) ***

breakpnt_16:
	@switch32bit
breakpnt:
	push ds
	mov ds, cs:[__csalias]
	and [fMode], not FMODE_INDEBUG ;falls breakpoint in debugger routine: reset
	or [fEntry], FENTRY_INT03
	pop ds
	jmp debug_entry

;*** entry callbacks real mode (int 00,01,02,03,06)
;*** der stack ist nur 100h bytes gross!
;*** auf real mode stack ist:
;*** SP+0:  cpumode
;*** SP+2:  original ax
;*** SP+4:  intnr
;*** SP+6:  original ip
;*** SP+8:  original cs
;*** SP+10: original fl

if ?RMCALLBACK

;*** einsprung aus real mode wenn rm-callback verwendet
;*** interrupts sind gesperrt
;*** ES:(E)DI zeigt auf RMCS
;*** DS:(E)SI zeigt auf real mode stack
;*** SS:(E)SP ist auf LPMS

rm2pmentry:
	push ds
	pushad
	cld
if ?32BIT eq 0
	movzx edi,di
	movzx esi,si
endif
	lodsw						 ;cpumode
	mov cl,al
	lodsw						 ;original ax
	mov word ptr es:[edi.rEAX],ax
	lodsw						 ;int#
	mov bl,al
	lodsd
	mov dword ptr es:[edi.rIP],eax
	lodsw
;	and ah,0FEh			  ;reset trace flag
	mov es:[edi.rFlags],ax
	mov word  ptr es:[edi.rSP],si
	mov ds,cs:[__csalias]
	assume ds:@data
	mov fCPUMode,cl
	cmp bl,0
	jz intxxx
	cmp bl,1
	jz intx01
	cmp bl,2
	jz intx02
	cmp bl,3
	jz intx03
	cmp bl,6
	jz intxxx
intrxx_ex:
	popad
	pop ds
	@iret
else	 ;!(?RMCALLBACK)

;*** hierher nach iret, raw mode switch to real mode ausfuehren ***

pm2rm:
if	?32BIT eq 0
	pop si						;2 bytes mehr bei 16 bit
endif
	mov [myesp],esp 			;debugger stack korrigieren
pm2rmback:
	mov esi, offset rmcsi0y
	mov edi, offset DEBRMVAR.rm
	mov ecx, sizeof RMCS/2
	mov es, [__RmDS]
	assume es:_TEXT16
	cld
	rep movsw
;pm2rm_x:
	mov dx, es:[DEBRMVAR.rm.rSS]		;ss
	mov bx, es:[DEBRMVAR.rm.rSP]		;sp
	mov di, es:[DEBRMVAR.wPM2RMEntry]	;ip

	mov ax,[__RmSeg]			;ds
	mov cx,ax					;es
	mov si,ax					;cs
	jmp fword ptr [pm2rmjump]
	assume es:nothing

;*** --- entry from real mode (raw mode) --- ***
;*** --- interrupts sind gesperrt?		 --- ***
;*** --- stack ist debugger stack		 --- ***
;*** --- DS,ES,SS,CS zeigen auf CSALIAS  --- ***

rm2pmentry:
if ?32BIT
	pushfd						;nach iret aus _reset an pm2rm
	push cs						;springen
	push offset pm2rm
else
	pushfd						;damit stack aligned bleibt
	mov eax,cs
	push ax
	mov eax,offset pm2rm
	push ax
endif
	mov [myesp],esp
	push ds
	pushad
	push ds
	pop es
	mov edi,offset rmcsi0y
	mov ds, [__RmDS]
	assume ds:_TEXT16

	mov esi, offset DEBRMVAR.rm
	mov ecx, sizeof RMCS/2
	cld
	push edi
	rep movsw
	pop edi
	mov cl,ds:[DEBRMVAR.bMode]
	mov bl,byte ptr ds:[DEBRMVAR.intno]
	push es
	pop ds
	assume ds:@data
	mov fCPUMode,cl
	cmp bl,0
	jz intxxx
	cmp bl,1
	jz intx01
	cmp bl,2
	jz intx02
	cmp bl,3
	jz intx03
	cmp bl,5
	jz intx05
	cmp bl,6
	jz intxxx
	cmp bl,7
	jz intxxx
intrxx_ex:
	popad
	pop ds
	@iret

endif  ;!(?RMCALLBACK)

;*** int handlers real mode callbacks (INT 01) ***
;*** die aktuellen register sind noch in rmcsi0y ***

intx01:
	and byte ptr es:[edi.RMCS.rFlags+1],0FEh
	test [fMode], FMODE_INDEBUG
	jz @F
	@stroutc lf,"debug exception in real mode at "
	@wordout dword ptr es:[edi.RMCS.rCS]
	@putchr ':'
	@wordout dword ptr es:[edi.RMCS.rIP]
	@stroutc " while in debugger",lf
	jmp intrxx_ex
@@:
	or [fEntry], FENTRY_REAL or FENTRY_INT01
	test [fExit], FEXIT_TRACE
	jz intxxx_1
	jmp intxxx_2
intx02:
	and byte ptr es:[edi.RMCS.rFlags+1],0FEh  ;reset trace flag
	test [fMode], FMODE_INDEBUG
	jnz intrxx_ex
	or [fEntry], FENTRY_REAL
	jmp intxxx_2

intx03:
	or [fEntry], FENTRY_REAL or FENTRY_INT03
	jmp intxxx_2
intx05:
	movzx eax,rmcsi0y.rCS
	shl eax,4
	movzx edx,rmcsi0y.rIP
	add edx,eax
	cmp byte ptr @flat:[edx],62h	;bound opcode?
	jz intxxx
if 1
	mov edx,oldi05r
	xchg edx,rmcsi0y.rCSIP
	movzx eax,rmcsi0y.rSS
	shl eax,4
	movzx ecx,rmcsi0y.rSP
	add eax,ecx
	mov @flat:[eax-6],edx
	mov cx,rmcsi0y.rFlags
	mov @flat:[eax-2],cx
	sub rmcsi0y.rSP,6
	and byte ptr rmcsi0y.rFlags+1,0FCh	;reset trace + interrupt flags
	jmp pm2rmback
endif
intxxx:
	or [fEntry], FENTRY_REAL
intxxx_1:
if ?ENTRYFROMRMPROT
	@loadflat
	movzx ebx,bl
	call excout
	@stroutc " occured in real mode at "
	@wordout dword ptr es:[edi.RMCS.rCS]
	@putchr ':'
	@wordout dword ptr es:[edi.RMCS.rIP]
	invoke _crout
endif
intxxx_2:
	popad
	pop ds
	jmp debug_entry


DebugEntry:
if ?32BIT
 if ?FLAT
	pop eax
	mov edx, cs
 else
	pop eax
	pop edx
 endif
	pushfd
	push edx
	push eax
else
	pop eax
	pop edx
	pushf
	push dx
	push ax
endif
if 0;if ?WINDOWS eq 0
	push ds
	mov ds,cs:[__csalias]
	or [fStat], FSTAT_SYNCENTRY
	pop ds
endif
	jmp debug_entry

if ?WATCHINT23
intr23 proc far c
	@switch32bit
	@stroutc lf,"Ctrl-C break",lf
	jmp debug_entry
intr23 endp
endif

if ?WATCHINT24
intr24 proc far c
	@switch32bit
	pushad
	@stroutc "DOS reports critical error",lf,"Type (AH)="
	push eax
	mov al, ah
	invoke _hexout
	pop eax
	@stroutc ", drive (AL)="
	invoke _hexout
	@stroutc lf,"DevDrv (BP:SI)="
	@wordout ebp
	@putchr ':'
	@wordout esi
	@stroutc lf,"ErrCode (DI)="
	mov bx, di
	mov al, bl
	invoke _hexout
	invoke _crout
	@stroutc "use '? DOSCRITERR' for description of ErrCode",lf
	popad
	mov al, 3
	test ah, 8	 ;fail erlaubt?
	jnz @F
	@stroutc "Fail not allowed, will terminate!",lf
	mov al, 2
@@:
	@iret

intr24 endp

endif

;--- display name of exception in BL

excout proc
	mov ah,bl
	add ebx,offset excstr
	mov al,[ebx]
	and al,al
	jnz @F
	@stroutc "Exception #"
	mov al, ah
	invoke _hexout
	ret
@@:
	movzx eax,al
	add ebx,eax
	invoke __stroutebx		; modifies ebx, eax
	ret
excout endp

@defexc macro xx
exc&xx:
	push xx
	jmp excproc
endm

	align 4

	@defexc 00h
	@defexc 01h
	@defexc 02h
	@defexc 03h
	@defexc 04h
	@defexc 05h
	@defexc 06h
	@defexc 07h
	@defexc 08h
	@defexc 09h
	@defexc 0Ah
	@defexc 0Bh
	@defexc 0Ch
	@defexc 0Dh
	@defexc 0Eh
	@defexc 0Fh
	@defexc 10h
	@defexc 11h
	@defexc 12h
	@defexc 13h
	@defexc 14h
	@defexc 15h
	@defexc 16h
	@defexc 17h
	@defexc 18h
	@defexc 19h
	@defexc 1Ah
	@defexc 1Bh
	@defexc 1Ch
	@defexc 1Dh
	@defexc 1Eh
	@defexc 1Fh

;*** exception handler ***

EXCSTR struct
		PUSHADS <>
rES		dd ?
rDS		dd ?
rFS		dd ?
rGS		dd ?
if ?32BIT
ExcNo   dd ?
else
ExcNo   dw ?
endif
		DPMIEXC <>
EXCSTR ends

?CHECKSEGREGS equ 0

excproc proc far

	@switch32bit
	push gs
	push fs
	push ds
	push es
	pushad
	@loadesp ebp
	assume ebp:ptr EXCSTR

	mov ds, cs:[__csalias]
	push ds
	pop es
	@loadflat
	push 0
	pop fs

	cld
if ?CHECKSEGREGS
	mov eax, [ebp].rDS
	mov ebx, [ebp].rES
	mov ecx, [ebp].rFS
	mov edx, [ebp].rGS
	call checksegregs
endif

	test [fMode], FMODE_SKIP	;Skip active?
	jz @F
	and [fMode], not FMODE_SKIP
	mov dl,00
	jmp excproc_5
@@:
	cmp bRC, 00
	jnz excproc_5					; debugger fully initialized?

	test [fStat], FSTAT_DOSOUT
	jz @F
	call MarkDOSused
@@:
	mov eax, [excexit]				;has debugger set an exit proc?
	and eax, eax
	jnz doexcexit
	cmp byte ptr [ebp].ExcNo, 1
	jz excproc_01
	cmp byte ptr [ebp].ExcNo, 3
	jz excproc_03
	invoke _crout
	movzx ebx, byte ptr [ebp].ExcNo
	call excout
	call exception_protokoll
	mov dl, FTRAP_STOP		; nur anhalten falls FTRAP_STOP gesetzt
	test [fMode], FMODE_INDEBUG
	jz excproc_5
	mov dl, 0FFh 			; if exc within debugger, always stop
excproc_5:
	mov esi, offset oldexcvecs
	movzx eax, byte ptr [ebp].ExcNo
	shl eax, 3
	add esi, eax
	test byte ptr [esi+6], dl
	jnz excproc2				;should we stop for this exception?
	call SwitchToDebuggeeScreen
	mov eax, [esi+0]
	mov cx, [esi+?SEGOFFS]
	mov dword ptr [tempjump+0], eax
	mov word ptr [tempjump+?SEGOFFS], cx
	popad
	pop es
	pop ds
	pop fs
	pop gs
	add esp, sizeof EXCSTR.ExcNo	; exceptionnr wegwerfen
	jmp cs:[tempjump]				; jmp to previous handler

;--- exception 1: this can result from the trace flag, and int 01
;--- or a debug exception (hardware)

excproc_01:
	test [fMode], FMODE_INDEBUG
	jz @F
if ?LOGINTDBGEXC        
	@stroutc lf, "debug exception "
	and byte ptr [ebp].rFL+1, not FL_TRACE	;reset trap flag
	call exception_protokoll
else
;	call DeactAllHWBreaks	;avoid loops!
	and byte ptr [ebp].rFL+1, not FL_TRACE	;reset trap flag
	jmp done
endif
@@:
	test [fEntry], FENTRY_SYSREQ	;a SYSREQ break is "unknown"
	jnz @F
	test [fTMode], FTMODE_SKIPUNKNOWNEXC01
	jz @F
	test [fExit], FEXIT_TRACE
	jnz @F
						;check if it is a known hw break
	call HWBreakHit
	mov dl, 0
	jnc excproc_5
@@:
	or [fEntry], FENTRY_INT01
	jmp excproc2
excproc_03:
	test [fMode], FMODE_INDEBUG
	jz @F
	@stroutc lf, "int 03 exception "
	call exception_protokoll
@@: 				   ;falls breakpoint in debugger routine: reset
;	and [fMode], not FMODE_INDEBUG or FMODE_EXEACTIVE
	or [fEntry], FENTRY_INT03
excproc2:

;--- now jump in debugger, but do this by returning to DPMI host
;--- so it will switch away from LPMS. For this to work
;--- save CS:EIP, EFL and SS:ESP here

	or [fEntry], FENTRY_EXCEPTION
	and byte ptr [ebp].rFL+1, not FL_TRACE	;reset trap flag
if ?32BIT
	mov eax, [ebp].rCS
	mov ecx, [ebp].rIP
	mov edx, [ebp].rFL
	mov [rCSx], eax
	mov [rEipx], ecx
	mov [rEflx], edx
	mov [ebp].rIP, offset debug_entry
	mov [ebp].rCS, cs
  if ?SWITCHSTACK
	mov eax, [ebp].rSS
	mov ecx, [ebp].rSP
	mov edx, [myesp]
	mov [rSSx], eax
	mov [rEspx], ecx
	mov [ebp].rSP, edx
	mov [ebp].rSS, ds
@@:
  endif
else
	movzx eax,[ebp].rCS
	movzx ecx,[ebp].rIP
	movzx edx,[ebp].rFL
	mov [rCSx], eax
	mov [rEipx], ecx
	mov [rEflx], edx
  if ?CS16ALIAS
	mov eax, offset debug_entry_16
	mov ecx, [__cs16alias]
	mov [ebp].rIP, ax
	mov [ebp].rCS, cx
  else
	mov eax, offset debug_entry
	mov [ebp].rIP, ax
	mov [ebp].rCS, cs
  endif
  if ?SWITCHSTACK
	movzx eax, [ebp].rSS
	movzx ecx, [ebp].rSP
	mov edx, [myesp]
	mov [rSSx], eax
	mov [rEspx], ecx
	mov [ebp].rSP, dx
	mov [ebp].rSS, ds
@@:
  endif
endif
	or [fException], FEXC_USEXREGS or FEXC_SAVESTACK	;restore regs!
done:
	popad
	pop es
	pop ds
	pop fs
	pop gs
	add esp,sizeof EXCSTR.ExcNo 		;exceptionnr wegwerfen
ife ?32BIT
	db 66h
endif
	retf
doexcexit:
if ?32BIT
 if 0
	mov tregs.rEdi, edi
	mov tregs.rEsi, esi
	mov tregs.rEcx, ecx
	mov ecx, [ebp].rDS
	mov edx, [ebp].rES
	mov tregs.rDS, ecx
	mov tregs.rES, edx
 endif
	xchg eax, [ebp].rIP
	mov tregs.rEip, eax
	mov eax, cs
	xchg eax, [ebp].rCS
	mov tregs.rCS, eax
	mov eax, [ebp].ExcNo
	mov tregs.rEax, eax
else
  if ?CS16ALIAS        
	mov eax, offset excexit_16
	mov ecx, [__cs16alias]
  else
	mov ecx, cs
  endif
	xchg ax, [ebp].rIP
	mov word ptr tregs.rEip, ax
	xchg cx, [ebp].rCS
	mov word ptr tregs.rCS, cx
	mov ax, [ebp].ExcNo
	mov word ptr tregs.rEax, ax
endif
	and byte ptr [ebp].rFL+1, not FL_TRACE
	jmp done

if ?CS16ALIAS
excexit_16:
	@switch32bit
	jmp cs:[excexit]
endif

excproc endp




exception_protokoll:
	test [fMode], FMODE_INDEBUG
	jnz x_exception_protokoll
exception_protokoll_1:
if ?32BIT
	movzx eax, word ptr [ebp].rCS
	movzx ecx, word ptr [ebp].rSS
	invoke printf, CStr(" errc=%X, cs:ip=%X:%X, fl=%X, ss:sp=%X:%X",lf),
		[ebp].ErrCode, eax, [ebp].rIP, [ebp].rFL, ecx, [ebp].rSP
else
	movzx edi, [ebp].ErrCode
	movzx esi, [ebp].rFL
	movzx eax, [ebp].rCS
	movzx edx, [ebp].rIP
	movzx ecx, [ebp].rSS
	movzx ebx, [ebp].rSP
	invoke printf, CStr(" errc=%X cs:ip=%X:%X fl=%X ss:sp=%X:%X",lf),
		edi, eax, edx, esi, ecx, ebx
endif
	invoke printf, CStr("eax=%X ebx=%X ecx=%X edx=%X esi=%X edi=%X",lf),
		[ebp].rEax, [ebp].rEbx, [ebp].rEcx, [ebp].rEdx, [ebp].rEsi, [ebp].rEdi
	movzx eax, word ptr [ebp].rDS
	movzx edx, word ptr [ebp].rES
	movzx ecx, word ptr [ebp].rFS
	movzx ebx, word ptr [ebp].rGS
	invoke printf, CStr("ebp=%X ds=%X, es=%X, fs=%X, gs=%X",lf),
		[ebp].rEbp, eax, edx, ecx, ebx
	ret

;--- create a MYREGSTR on near heap

x_exception_protokoll:
	@stroutc " - occured in debugger",lf
	inc byte ptr [excentry]
	cmp [excentry],1
	ja x_exception_protokoll_2
	lea ebx, [ebp].rIP
	mov al,1
	call PutRegsOnHeap
	ret

	assume ebp:nothing

;--- inp: ebx = IRETS, ebp=MYREGSTR 12 DWORDS (regs + ds,es,fs,gs)
;--- if al == 0, SS:ESP is not in IRETS
;--- out: [pNearHeap] = MYREGSTR

PutRegsOnHeap proc
	push ds
	mov edi,[pNearHeap]
	push edi
	push ss
	pop ds
	mov esi, ebp
	mov ecx, 12		;copy 8 std regs + 4 segment regs
	rep movsd
	pop edi
	pop ds
	push ebp
	mov ebp, ebx
if ?32BIT
	mov ecx,[ebp].IRETS.rIP
	movzx edx, word ptr [ebp].IRETS.rCS
else
	movzx ecx,[ebp].IRETS.rIP
	movzx edx,[ebp].IRETS.rCS
endif
	mov [edi].MYREGSTR.rEip, ecx
	mov [edi].MYREGSTR.rCS, edx
if ?32BIT
	mov ecx,[ebp].IRETS.rFL
else
	movzx ecx,[ebp].IRETS.rFL
endif
	mov [edi].MYREGSTR.rEfl, ecx

	lea ecx, [ebx].IRETS.rSP
	mov edx, ss
	movzx edx, dx
	and al,al
	jz @F
if ?32BIT
	mov ecx, [ebp].IRETS.rSP
	movzx edx, word ptr [ebp].IRETS.rSS
else
	movzx ecx, [ebp].IRETS.rSP
	movzx edx, [ebp].IRETS.rSS
endif
@@:
	mov [edi].MYREGSTR.rEsp, ecx
	mov [edi].MYREGSTR.rSS, edx

	pop ebp
	or [fMode], FMODE_REGSOUT
	ret
PutRegsOnHeap endp

	.const

szFatal db "exception while handling an internal exception",lf
		db "debugger will most likely be unstable now",lf
		db "irq set to false (disables interrupts)",lf
		db "press RETURN to go on or ESC to try a fatal exit",lf
		db 0

	.code

x_exception_protokoll_2:
	assume ebp:ptr MYREGSTR
	cli
	mov fIrq,0
	invoke printf, addr szFatal
	call exception_protokoll_1
@@:
	in al,64h
	test al,1
	jz @B
	in al,60h
	cmp al,1
	jz _fexit
	cmp al,1Ch
	jnz @B
	jmp mains

if ?CHECKSEGREGS
checksegregs:
	and ax,ax
	jz @F
	lar eax,eax
	jnz checkregs_1
@@:
	and bx,bx
	jz @F
	lar ebx,ebx
	jnz checkregs_1
@@:
	jcxz @F
	lar ecx,ecx
	jnz checkregs_1
@@:
	and dx,dx
	jz @F
	lar edx,edx
	jnz checkregs_1
@@:
	ret
checkregs_1:
	invoke printf, CStr("severe error: segment register invalid.",lf,"ds=%X es=%X fs=%X, gs=%X",lf),\
		dword ptr [ebp].rDS, dword ptr [ebp].rES, dword ptr [ebp].rFS, dword ptr [ebp].rGS
	ret
endif
	assume ebp:nothing

;--------------------------------------------------

checkifenhmode proc stdcall public
	pushad
	xor ebx,ebx
	mov ax,1683h
	@callint2F
	cmp bx,1
	popad
	jc @F
	ret
@@:
	@stroutc "enhanced mode windows not running",lf
	jmp mains
checkifenhmode endp


_getpic proc stdcall

	@stroutc "Master PIC:"
	mov dx,020h
	call getpic1
	@stroutc "Slave PIC:"
	mov dx,0A0h
	call getpic1
	ret
getpic1:
	@stroutc "IMR="
	inc dx
	in al,dx
	dec dx
	invoke _hexout
	@stroutc " IRR="
	mov al,0Ah
	call getpic2
	@stroutc " ISR="
	mov  al,0Bh
	call getpic2
;	@stroutc " SMM="
;	mov  al,0Fh
;	call getpic2
	invoke _crout
	ret
getpic2:
	out dx,al
	nop
	nop
	in al,dx
	invoke _hexout
	ret
_getpic endp

if ?SUPPVPICD

_vpicd proc c public pb:PARMBLK

local sysvm:dword

	call checkifenhmode
	@ring0call getsysvm
	mov  sysvm,ebx
	invoke printf, CStr("IRQ V CurVM    SysVM  ",lf)
	invoke printchars, '-', eax, 1
	mov  ecx,10h
	mov  al,0
	movzx eax,al
vpicd_1:
	push ecx
	push eax
	mov  esi,sysvm
	@ring0call getirqs
	pushfd
	invoke _hexout
	@stroutc ": "
	popfd
	jnc  @F
	@putchr 'v'
	jmp  vpicd_2
@@:
	@putchr ' '
vpicd_2:
	@putchr ' '
	@dwordout ecx
	@putchr ' '
	@dwordout edx
	invoke	_crout
	pop  eax
	pop  ecx
	inc  eax
	loop vpicd_1
	ret
_vpicd endp

endif

;--- this is a ring 0 proc

getsysvm proc
	push ds
	push es
	push ss
	pop  ds
	push ss
	pop  es
	VMMCall Get_Sys_VM_Handle
	pop  es
	pop  ds
	ret
getsysvm endp

;--- this is a ring 0 proc

getirqs proc
	push ds
	push es

	push ss
	pop  ds
	push ss
	pop  es
	push eax
	VxDCall VPICD_DEVICE_ID,VPICD_Get_IRQ_Complete_Status
	pop  eax

	push ecx
	mov  ebx,esi
	VxDCall VPICD_DEVICE_ID,VPICD_Get_IRQ_Complete_Status
	mov  edx,ecx
	pop  ecx
	pop  es
	pop  ds
	ret
getirqs endp

;-------------------------------------------------------------

;--- test if a string variable is a known module name
;--- inp: ebx=name to find
;--- out: ???

getmodulesym proc stdcall uses fs esi ebx

local	modname[9]:byte

	mov esi, ebx
	invoke getkrnlhandle, 0	;get handle of 16-bit kernel
	jc error
nextitem:
	lea ecx, modname
	invoke GetModuleName, eax, ecx
	jc error
	invoke strcmp, addr modname, esi
	and eax,eax
	jz found
	mov ax,fs:[0006]	;handle of next module
	and ax, ax
	jnz nextitem
error:
	xor eax, eax
	ret
found:
	mov eax, fs
	mov cl,__CONST__
	ret
getmodulesym endp

;--- search an address in tables loaded with .LS 
;--- called by the parser
;*** ebx= name of symbol to search

getmysymaddr proc stdcall public

	call getmodulesym
	and eax,eax			;is name a 16-bit module?
	jnz exit
	push es
	push edi
	push esi
	mov ecx,NUMNRITEMS
	mov esi,offset hdlNRes

nextitem:
	mov eax,[esi.NRNT.NRNTMOD]
	and eax,eax
	jz getmysymaddr_1
	push ecx
	push ebx
if ?32BIT
if ?FLAT
	invoke setworkselbase, ebx
	mov eax, worksel
	shl eax, 16
	xor ax,ax
	push eax
else
	mov eax,ds
	push ax
	push bx				   ;muss im 1. 64k segment liegen!
endif
	push word ptr [esi.NRNT.NRNTMOD]
	push word ptr [esi.NRNT.NRNTHDL]
	mov ebx, esp
	mov edx, _GetSymbolAddr
	mov cx, 4
	mov ax, 4B87h	; call GetSymbolAddr
	@DosCall
	add esp, 4*2
else
	@savewinsegregs
	mov eax,ds
	push ax
	push bx				   ;muss im 1. 64k segment liegen!
	push word ptr [esi.NRNT.NRNTMOD]
	push word ptr [esi.NRNT.NRNTHDL]
	call _GetSymbolAddr
	@restorewinsegregs
endif
	push dx
	push ax
	pop eax

	pop ebx
	pop ecx
	and eax,eax
	jnz getmysymaddr_ex
getmysymaddr_1:
	add esi,sizeof NRNT
	loop nextitem
getmysymaddr_ex:
	mov cl,__FPTR__
	pop esi
	pop edi
	pop es
exit:
	ret
getmysymaddr endp

;--- get name of a NE module

GetModuleName proc pascal public uses esi dwModule:dword, pModname:dword

	mov esi, pModname
	mov byte ptr [esi], 0
	mov eax, dwModule
	verr ax
	jnz error
	mov fs, eax
	cmp word ptr fs:[0],"EN"
	jnz error
	movzx edx,word ptr fs:[0026h]	;name (1. residenter Name)
	inc edx
	mov eax,fs:[edx+0]
	mov [esi+0],eax
	mov eax,fs:[edx+4]
	mov [esi+4],eax
	mov byte ptr [esi+8], 0
	clc
	ret
error:
	stc
	ret
GetModuleName endp

;---

setargdefaults proc
	xor eax,eax
	push edi
@@:
	mov byte ptr [edi.ARGDESC.dwType],__VOID__
	mov [edi.ARGDESC.dwOfs],eax
	add edi,sizeof ARGDESC
	cmp edi,offset argend
	jb @B
	pop edi
	ret
setargdefaults endp


if 0
strcat proc pascal string1:dword,string2:dword

	push esi
	push edi
	mov edi,[pNearHeap]
	mov esi,string1
@@:
	lodsb
	and al,al
	jz @F
	stosb
	jmp @B
@@:
	mov esi,string2
@@:
	lodsb
	stosb
	and al,al
	jnz @B
	mov eax,edi
	xchg eax,[pNearHeap]
	pop edi
	pop esi
	ret
strcat endp
endif

;--- display a linefeed if fSkipLF is 0

croutx proc stdcall public
	cmp [fSkipLF], 0
	jz @F
	dec [fSkipLF]
	stc
	ret
@@:
	invoke _crout
	clc
	ret
croutx endp

;*** aufgerufen aus parser ***
;*** nur edx darf geaendert werden ***

getdefseg proc stdcall public uses eax symptr:dword

		mov eax,symptr
		cmp [eax.SYMBOL.bType2],1
		jnz gds_1
		mov edx,[r1.rSS]
		test [fEntry], FENTRY_REAL
		jz exit
		mov dx,[r1r.rSS]
		jmp exit
gds_1:
		cmp [eax.SYMBOL.bType2],2
		jnz gds_2
		mov edx,[r1.rCS]
		test [fEntry], FENTRY_REAL
		jz exit
		mov dx,[r1r.rCS]
		jmp exit
gds_2:
		mov edx,[r1.rDS]
		test [fEntry], FENTRY_REAL
		jz exit
		mov dx,[r1r.rDS]
exit:
		ret
getdefseg endp

if 0
CheckIrqSetting proc
	mov al,[fIrq]
	and al,al
	jnz l1
	pushfd
	pop eax
	test ah,2
	jz l1
	@stroutc "error: ints are enabled ",lf
l1:
	ret
CheckIrqSetting endp
endif

;*** debugger main loop 

mains proc stdcall public

cmdptr equ <[ebp-4]>
cmdstr equ <[ebp-8]>
aktptr equ <[ebp-12]>
arganz equ <[ebp-16]>			;# of parameters

@@:
	@tprintf <"mains enter",lf>
main_2: 						;<----- comand loop
	mov ds, cs:[__csalias]
	mov eax, ds
	mov es, eax
	mov ss, eax
	mov esp,[myesp]
	@loadesp ebp
	sub esp,4*4			;make room for local vars
	@loadflat
if ?WINDOWS
	cmp [fEntry], 0
	setne cl
	mov [cLoad], cl
endif

;	call CheckIrqSetting
	test [fMode], FMODE_REGSOUT
	jz @F
	and [fMode], not FMODE_REGSOUT
	invoke myregsout,[dwFormStr],[pNearHeap]
@@:
	@tprintf <"mains first milestone reached",lf>

	and [fException], not (FEXC_USEXREGS or FEXC_SAVESTACK)
	mov [fExit], 0
	test [fMode], FMODE_EXEACTIVE
	jz @F
	@tprintf <"exit proc active",lf>
	and [fMode], not FMODE_EXEACTIVE
	and [fMode], not FMODE_NODISP
	call [exitproc]
	jmp main_2

@@:
	@setint24				   ;nur fuer windows
	cld
	mov [pNearHeap], offset startofheap
	test [fStat], FSTAT_BUFNOTEMPTY
	jnz @F
	mov [pKbdBuff], offset linebuffer
@@:
	mov excexit, 0
ife ?WINDOWS
 ife ?LDRDISABLE
	call enableloader
 endif
endif
if 1
;--- windows dosx changes PSP:[2Ch] in a segment
	push ds
	mov eax,[_environ]
	mov ds,[_psp]
	mov ds:[2Ch],ax
	pop ds
endif
	@tprintf <"mains getline reached",lf>
	call getline 		; get a line

	and [fMode], not FMODE_NODISP
	mov byte ptr excentry,0
	mov [cLines], 1
mains_0:
	mov cmdstr, eax
	invoke preparestring, eax
	mov cmdstr, eax

	mov edi,offset a0
	invoke GetStringToken, cmdstr, edi ;1. string holen (keine interpretation)
	and eax,eax
	jz main_2
	mov aktptr, edx
;;	call croutx
	push offset main_2
	mov byte ptr arganz, 0
	mov ebx, [eax.ARGDESC.dwPx]
	mov esi, ebx
	mov eax, offset functab
	invoke SearchStdSymbol, eax, ebx; search std command -> eax
	jnc mains_1 					; found
	mov eax, pMacros				; not a command
	and eax, eax					; are macros defined?
	jz @F
	invoke SearchUserSymbol, eax, esi; search macro
	jc @F
	mov eax, [eax.SYMBOL.dwProc]	; macro found, use as command
	call getmacroarguments
	jmp mains_0
@@:									; not a command, not a macro
	mov eax, cmdstr					; reset input string
	mov aktptr, eax					; and use expression eval
	mov eax, offset symDisp			; command "??"
mains_1:
	mov esi, eax
	mov ch, [esi.SYMBOL.bType2]
	and ch, 0Fh
	mov [anzarg], ch 				; benoetigte anzahl argumente
	mov eax, [esi.SYMBOL.dwProc]
	mov cmdptr, eax					; save cmd address
	call checkifactive				; if/else/endif
	jnc mains_11
	mov edx, aktptr
@@:
	invoke GetStringToken, edx, edi	; get all items of a logical line
	and edx, edx					; string left on this line?
	jnz @B
	jmp main_2
mains_11:
	@tprintf <"mains milestone mains_11 reached",lf>

	add edi, sizeof ARGDESC
	call setargdefaults
	mov cl, [esi.SYMBOL.bType]
	and cl, 7Fh
nextitem:
	@tprintf <"mains milestone mains_3 reached",lf>
	push ecx
	test cl, 01h					; is parameter supposed to have type "string"?
	jz @F
	invoke GetStringToken, aktptr, edi
	and eax, eax
	jz mains_31
	mov al, 1
	jmp mains_31
@@:
	@tprintf <"mains milestone mains_31b",lf>
	invoke GetParameter, aktptr, edi
mains_31:
	@tprintf <"mains milestone mains_31 reached",lf>
	pop ecx
	mov aktptr, edx
	shr cl, 1
	cmp eax, -1				;syntaxfehler?
	jz merr31
	add byte ptr arganz, al
	add edi, sizeof ARGDESC
	and edx, edx
	jnz nextitem

	@tprintf <"mains milestone mains_31x reached",lf>

	mov al, byte ptr [arganz]
	cmp al, [anzarg]
	jc merr2
	mov ecx, [a1.ARGDESC.dwType]
	cmp cl, __VOID__
	jz mains_5
	test byte ptr [esi.SYMBOL.bType2], 080h		;strings erlauben?
	jnz mains_5
	cmp cl, __STRING__
	jz merr3
mains_5:
	mov edx, [a1.ARGDESC.dwSeg]
	call checksegment
	movzx eax, byte ptr arganz
	cmp al,4
	jnb @F
	mov al, 4				; mindestens 4 argumente auf Stack
@@:
	dec al
	push edx
	push eax
	mov esi, sizeof ARGDESC
	mul esi
	mov esi, offset a1
	add esi, eax
	pop eax
	pop edx
@@:							; push arguments (12 bytes each)
	cmp al,0
	jz mains_8
if 0
	mov ebx,dword ptr [esi.ARGDESC.dwType]
	push bx								;wTypeX
	shr ebx,16
	push bx								;wresX
else
	push dword ptr [esi.ARGDESC.dwType]
endif
	push dword ptr [esi.ARGDESC.dwSeg]	;wSegX
	push dword ptr [esi.ARGDESC.dwOfs]	;dwOffsX
	sub esi,sizeof ARGDESC
	dec al
	jmp @B
mains_8:
	@tprintf <"mains milestone mains_8 reached",lf>

if 0
	mov ebx, ecx
	push bx					; wType1
	shr ebx,16
	push bx					; wres1
else
	push ecx
endif
	push edx 				; wSeg1
	mov ebx, [a1.ARGDESC.dwOfs]
	push ebx 				; dwOffs1
	movzx eax,byte ptr [arganz]
	push eax 				; wArgc
	push main_2
	push [cmdptr]
	mov ebp, esp			; to setup ebp for procs without prologue
	ret

merr31:
	@strout cmdstr
	invoke _crout
	mov eax,aktptr
	sub eax,cmdstr
	jbe merr3
	mov ecx,eax
@@:
	@putchr ' '
	loop @B
	@putchr '^'
	invoke _crout
merr3:
	@errorout ERR_PARM1_INVALID
	retn
merr2:
	@errorout ERR_MISSING_PARAM
	retn
merr1:
	@strout cmdstr
	@putchr '?'
	invoke _crout
	retn

checksegment:
	cmp cl,__LPTR__
	jz @F
	cmp cl,__FPTR__
	jz @F
	cmp cl,__RMLPTR__
	jnz checksegment_ex
	push ecx
	movzx edx,dx
	shl edx,4
	invoke setworkselbase,edx
	mov edx,[worksel]
	pop ecx
@@:
	lar eax,edx
	jnz @F
	test ah,80h			;is segment present?
	jnz checksegment_ex
	push edx
	@errorout ERR_SEGMENT_NOT_PRESENT
	jmp popret

@@:
	push edx
	@errorout ERR_INVALID_POINTER
popret:
	pop eax
	pop eax
checksegment_ex:
	retn

getmacroarguments:
	mov edi, pNearHeap
	mov esi, eax
	push edi
	.while (1)
		lodsb
		.if (al == '^')
			mov pNearHeap, edi
			invoke GetStringToken, aktptr, addr a1
			.if (eax)
				mov aktptr, edx
				invoke strlen, edi
				add edi, eax
			.else
				mov al, '?'
				stosb
			.endif
			.continue
		.endif
		stosb
		.break .if (!al)
	.endw
	mov pNearHeap, edi
	pop eax
if 1
	.if (fTrace)
		push eax
		invoke printf, CStr("%s"), eax
		pop eax
	.endif
endif
	retn

befptr equ <>
cmdstr equ <>
aktptr equ <>
arganz equ <>

mains endp

;*** convert logical address in CX:EBX to linear address
;*** type in DL (real Mode is <> 0)
;*** out: NC = ok, linear address in EAX

getlinearaddr proc stdcall public

	and dl, dl
	jz @F
	movzx eax, cx
	shl eax, 4
	jmp getlinearaddr_1
@@:
	invoke getbaser,ecx
	jc @F
getlinearaddr_1:					;jetzt lin. adresse EAX
	add eax, ebx
	clc
@@:
	ret
getlinearaddr endp

;*** lineare adresse cs:eip -> eax, cs:eip=cx:ebx ***

getlinearcseip proc stdcall public

	call getactcseip 			;cs:eip -> ax:ebx
	mov ecx, eax
	call getlinearaddr
	ret
getlinearcseip endp

;*** pruefen ob an aktuellem CS:EIP INT 1 oder INT 3 liegt
;*** Input: CS:EIP in CX:EBX
;*** das kann dann problematisch sein, wenn der breakpoint
;*** von einem fremden debugger gesetzt wurde. dann muss
;*** die Kontrolle an diesen Debugger weitergegeben
;*** werden.
;*** die eigenen Breakpoints (int 3) sind hier bereits
;*** zurueckgesetzt
;*** return: C wenn INT 01 oder INT 03 hart kodiert

;--- currently this proc is called only if FENTRY_INT03 is set!!!
;--- out: C: stop
;---     NC: continue execution

checkifbpreached proc stdcall

	mov dl, fEntry
	and dl, FENTRY_REAL
	call getlinearaddr		;CX:EBX -> EAX
	jc stop					;invalid CS???
	cmp ebx,1
	jb stop
	mov [excexit], offset stop
	@mov ecx,-1
	cmp byte ptr @flat:[eax+ecx], 0CCh
	jz cibpr1
	dec ecx
	cmp word ptr @flat:[eax+ecx], 03CDh
	jz cibpr1
	cmp word ptr @flat:[eax+ecx], 01CDh
	jz cibpr1
	clc
	ret
cibpr1: 						;hard coded "int 3"
	test dl, FENTRY_REAL
	jnz @F
	add [r1.rEip],ecx
	stc
	ret
@@:
	add [r1r.rIP],cx
stop:
	stc
	ret
checkifbpreached endp

;--- called by debug_entry

setcmddefaults proc
	@tprintf <"setting unass defaults",lf>
	mov eax, [r1.rCS]
	verr ax
	jz @F
	@stroutc "can't read CS segment!!!",lf
	xor eax, eax
@@:
	call GetCurrentEIP
	mov ecx, offset symtab
	mov edx, [r1.rDS]

	and byte ptr fUnastype, not FUT_RMADDR
	and [fDump], not FDT_RMADDR

	test [fEntry], FENTRY_REAL
	jz @F
	or byte ptr fUnastype, FUT_RMADDR
	or [fDump], FDT_RMADDR
	mov ax, [r1r.rCS]
	mov dx, [r1r.rDS]
	mov ecx, offset rregtab
@@:
	mov [pSymtab], ecx
	mov dword ptr [unasarg+0], ebx
	mov word ptr [unasarg+4], ax
	xchg edx, dword ptr [dumparg+4]
	cmp edx, dword ptr [dumparg+4]
	jz @F
	xor edx, edx
	mov dword ptr [dumparg+0], edx
@@:
	@tprintf <"unass & dump defaults set",lf>

	test [fMode], FMODE_NODISP
	jnz nodisp
	cmp [cLoad], 0
	jz nodisp

	call singlprot			; display instruction at CS:EIP
nodisp:
	ret
setcmddefaults endp

;**** programmeinstieg (u.a nach hot key) ***

debug_entry_16:
	@switch32bit

debug_entry proc c public

	cli
	cld
	push ds
	mov ds, cs:[__csalias]
	test [fMode], FMODE_INDEBUG
	jnz mains
	pop [r1.rDS]
	mov [r1.rSS], ss
	mov [r1.rEsp], esp
	push ds
	pop ss
	mov esp, [myesp]


	mov [dwCurPsp], 0
	and [fStat], not FSTAT_DTAREAD

	call DeactAllHWBreaks ; hardware breaks reset

	@tprintf <"debug_entry: calling register save",lf>
	call savereg 		; save all registers

	push ds
	pop es
	@loadflat
	push 0
	pop fs

	call setmyenvironment

	xor ebp,ebp
	@tprintf <"calling reset breaks",lf>
	call ResetBreaks	 ;gesetzte breaks reset, bpcount setzen

	@tprintf <"calling init first entry",lf>
	call firstentry		 ;ist normalerweise bereits geschehen

	test [fStat], FSTAT_DOSOUT
	jz @F
	call MarkDOSused
@@:
if _TRACE_
	movzx eax, fEntry
	@tprintf <"debug_entry: fEntry=%X",lf>, eax
endif
	test [fEntry], FENTRY_SYSREQ
	jnz entry2
	test [fEntry], FENTRY_INT01 or FENTRY_INT03
	jz entry21 				; falls weder int 1 oder 3 -> anhalten
	cmp byte ptr [bpCount],0; eigener Breakpoint getroffen?
	jnz entry2				; dann in jedem fall anhalten
							; kontrolle auf externe breaks
							; nur noch bei int 3 einstieg
							; da ansonsten problematisch
	@tprintf <"debug_entry: bpcount == 0",lf>
	test [fEntry], FENTRY_INT03
	jz entry21
if 0
	test [fEntry], FENTRY_HWBREAK
	jnz entry21
endif
	call getactcseip
	mov ecx,eax
	@tprintf <"calling checkifbpreached",lf>
	call checkifbpreached		;schauen ob hart kodierter break
	jnc _reset					;C wenn ja, ansonsten weiter
	invoke printf, CStr("hard coded break reached",lf)
entry2:
	test [fEntry], FENTRY_SILENTBRK
	jnz entry21
	and [fMode], not (FMODE_EXEACTIVE or FMODE_NODISP);hard break
entry21:
debug_entryx::
	call setcmddefaults
	jmp mains
debug_entry endp

getdosparms proc stdcall uses edi

local	trmcs:RMCS

	call getindosflag
	and al, al
	jnz exit
	lea edi,trmcs
	xor eax,eax
	mov dword ptr [edi.RMCS.rSP],eax
	mov dword ptr [edi.RMCS.rFlags], 202h
	mov word ptr [edi.RMCS.rEAX+0],5d06h	;get SDA
	xor ecx, ecx
	mov bx, 21h
	mov ax, 300h
	int 31h		;sim real-mode int
	mov ax,[edi.RMCS.rDS]
	shl eax,16
	mov ax,word ptr [edi.RMCS.rESI]
	mov [sdaadr],eax
	mov eax,[edi.RMCS.rECX]
	mov word ptr [sdalen1],ax
	mov eax,[edi.RMCS.rEDX]
	mov word ptr [sdalen2],ax

	mov byte ptr [trmcs.rEAX+1],52h	;get LoL
;	mov cx, 0
;	mov bx, 21h
	mov ax, 300h
	int 31h
	movzx eax,[edi.RMCS.rES]
	movzx ecx,word ptr [edi.RMCS.rEBX]
	shl eax,4
	add eax,ecx
	mov [dwLoL],eax
exit:
	ret
getdosparms endp

if ?32BIT
if ?WIN32REBOOT
handlewin32reboot proc stdcall uses es
	push edx
	push 0
	pop es
	mov ax,1684h
	mov bx,0009h
	@callint2F
	pop edx
	mov eax,es
	and ax,ax
	jz exit
if 1
	push eax
	movzx edi,di
	push edi
	mov ebx,esp
	mov ecx,cs
	mov ax,0204h		  ; CX:EDX -> int 01 handler
	call fword ptr [ebx]
	pop eax
	pop eax
endif
exit:
	ret
handlewin32reboot endp
endif
endif

resetexception proc stdcall

	pushad
	movzx ebx,bl
	clc
	test byte ptr [ebx*8+offset oldexcvecs+6],FTRAP_ACTIVE
	jz resetexcvec_2
	mov edx,dword ptr [ebx*8+offset oldexcvecs]
	mov cx,word ptr [ebx*8+offset oldexcvecs+?SEGOFFS]
	@DpmiCall 203h
if ?WINDOWS
	jnc @F
	@DpmiCall 283h
@@:
endif
	mov word ptr [ebx*8+offset oldexcvecs+6],0
resetexcvec_2:
	popad
	ret
resetexception endp

;--- BL = exception no
;*** in al bits (FTRAP_ACTIVE, FTRAP_STOP) ***

setexception proc stdcall

	movzx ebx,bl
	clc
	test byte ptr [ebx*8+offset oldexcvecs+6],FTRAP_ACTIVE
	jnz setexcvec_2

	pushad
	@DpmiCall 202h
	jc setexcvec_1
	mov dword ptr [ebx*8+offset oldexcvecs],edx
	mov word ptr [ebx*8+offset oldexcvecs+?SEGOFFS],cx

	mov edx,ebx
	shl edx,2
	add edx,offset exc00h
ife ?32BIT
	mov ecx,[__cs16alias]
else
	mov ecx,cs
endif
	@DpmiCall 203h
if ?WINDOWS
	jnc @F
	@DpmiCall 283h
@@:
endif
if ?32BIT
if ?WIN32REBOOT
	cmp bl,1
	jnz @F
	pushfd
	call handlewin32reboot
	popfd
@@:
endif
endif
setexcvec_1:
	popad
setexcvec_2:
	jc @F
	mov word ptr [ebx*8+offset oldexcvecs+6],ax
@@:
	ret
setexception endp

ife ?WINDOWS
;--- in: bl=interrupt number
;--- oldpmvecs is room for ints 00-0F

setpmvec proc stdcall
	pushad
	mov ax,0204h	; get int vector 
	int 31h
	movzx ebx,bl
if ?32BIT
	movzx eax,bl
	add eax,eax
	mov dword ptr [ebx*4+eax+offset oldpmvecs+0], edx
	mov word ptr [ebx*4+eax+offset oldpmvecs+?SEGOFFS], cx
else
	mov word ptr [ebx*4+offset oldpmvecs+0], dx
	mov word ptr [ebx*4+offset oldpmvecs+?SEGOFFS], cx
endif
if ?CS16ALIAS
	mov ecx,[__cs16alias]
else
	mov ecx,cs
endif
	mov edx,[ebx*4+offset mypmvecs]
	mov ax, 205h	; set int vector
	int 31h
	popad
	ret
setpmvec endp

setpmvecs proc stdcall

	xor ebx, ebx
setpmvecs_1:
	bt pmtraps,ebx
	jnc @F
	call setpmvec
@@:
	inc ebx
	cmp bl, SIZEOLDPMVECS
	jnz setpmvecs_1
setpmvecs_2:
	ret
setpmvecs endp

resetpmvec proc stdcall

	pushad
	movzx ebx, bl
if ?32BIT
	lea eax, [ebx*2]
	mov cx, word ptr  [ebx*4+eax+offset oldpmvecs+?SEGOFFS]
	jcxz @F
	mov edx, dword ptr [ebx*4+eax+offset oldpmvecs+0]
else
	mov cx, word ptr  [ebx*4+offset oldpmvecs+?SEGOFFS]
	jcxz @F
	mov dx, word ptr [ebx*4+offset oldpmvecs+0]
endif
	mov ax, 205h
	int 31h
@@:
	popad
	ret
resetpmvec endp

;--- reset all pm vecs that are marked as "trapped"

resetpmvecs proc stdcall

	xor ebx,ebx
nextitem:
	bt pmtraps,ebx
	jnc @F
	call resetpmvec
@@:
	inc ebx
	cmp bl, SIZEOLDPMVECS
	jnz nextitem
	ret
resetpmvecs endp

endif

getmyrmvecaddr:
	mov cx, __RmSeg
	push ds
	push ebx
	mov ds, [__RmDS]
	assume ds:_TEXT16
	movzx ebx, bl
	shl ebx, 2
	movzx edx, ds:[DEBRMVAR.wIntr00r]
	add edx, ebx
	pop ebx
	pop ds
	assume ds:@data
	ret

setrmvec:
	pushad
	mov ax, 200h	; get rm int
	int 31h
	movzx ebx, bl
	push cx
	push dx
	pop dword ptr [ebx*4+offset oldrmvecs]
	call getmyrmvecaddr
	mov ax, 201h	; set rm int
	int 31h
	popad
	ret

setrmvecs proc stdcall

	@tprintf <"setrmvecs",lf>
	xor eax, eax
	call rmbreak
	xor ebx, ebx
nextitem:
	bt rmtraps, ebx
	jnc @F
	call setrmvec
@@:
	inc ebx
	cmp bl, 20h
	jnz nextitem
	mov eax, 1
	call rmbreak
	ret
setrmvecs endp

resetrmvec proc
	pushad
if ?WINDOWS eq 0
	mov ax, 200h	; get rm int
	int 31h
	mov esi, ecx
	mov edi, edx
	call getmyrmvecaddr
	cmp cx, si
	jnz @F
	cmp dx, di
	jz resetrmvec_1
@@:
	push ebx
	@errorout MSG_RMINTCHG
	pop ebx
resetrmvec_1:
endif
	movzx ebx, bl
	push dword ptr [ebx*4+offset oldrmvecs]
	pop dx
	pop cx
	mov ax, 201h	; set rm int
	int 31h
	popad
	ret
resetrmvec endp

;--- reset all real-mode vecs that have been trapped

resetrmvecs proc stdcall
	@tprintf <"resetrmvecs enter",lf>
	xor eax, eax
	call rmbreak
	xor ebx, ebx
nextitem:
	bt rmtraps, ebx
	jnc @F
	call resetrmvec
@@:
	inc ebx
	cmp bl, 20h
	jnz nextitem
	mov eax, 1
	call rmbreak
	ret
resetrmvecs endp

if ?TRAPRM214B
traprm21 proc stdcall public
	cmp byte ptr cInt21RMTraps, 0
	jnz @F
	mov bl, 21h
	mov ax, 200h	; get rm int
	int 31h
	mov fs, [__RmDS]
	assume fs:_TEXT16
	mov word ptr fs:[DEBRMVAR.oldint21r+0],dx
	mov word ptr fs:[DEBRMVAR.oldint21r+2],cx
	mov cx, [__RmSeg]
	mov dx, fs:[DEBRMVAR.wIntr21r]
	mov bl, 21h
	mov ax, 201h	; set rm int
	int 31h
@@:
	inc byte ptr cInt21RMTraps
	ret
	assume fs:nothing
traprm21 endp

untraprm21 proc stdcall public
	dec byte ptr cInt21RMTraps
	jnz exit
	mov ecx, [__RmDS]
	jecxz exit
	mov fs, ecx
	assume fs:_TEXT16
	xor ecx, ecx
	xchg ecx, fs:[DEBRMVAR.oldint21r]
	jecxz exit
	push ecx
	mov bl, 21h
	mov ax, 200h	; get rm int
	int 31h
	cmp cx,__RmSeg
	jnz @F
	cmp dx,fs:[DEBRMVAR.wIntr21r]
	jz untraprm21_1
@@:
	invoke printf,CStr("debuggee has changed real mode int 0x21!",lf)
untraprm21_1:
	pop dx
	pop cx
	mov bl, 21h
	mov ax, 201h	; set rm int
	int 31h
exit:
	ret
	assume fs:nothing
untraprm21 endp
endif

if ?TRAPRM2F
traprm2F proc stdcall
	mov bl, 2fh
	mov ax, 200h	; get rm int
	int 31h
	mov fs, [__RmDS]
	assume fs:_TEXT16
	mov word ptr fs:[DEBRMVAR.oldint2Fr+0], dx
	mov word ptr fs:[DEBRMVAR.oldint2Fr+2], cx
	mov cx, __RmSeg
	mov dx, fs:[DEBRMVAR.wIntr2Fr]
	mov bl, 2fh
	mov ax, 201h	; set rm int
	int 31h
	ret
	assume fs:nothing
traprm2F endp

untraprm2F proc stdcall
	mov fs, [__RmDS]
	assume fs:_TEXT16
	xor ecx, ecx
	xchg ecx, fs:[DEBRMVAR.oldint2Fr]
	jecxz exit
	push ecx
	mov bl, 2fh
	mov ax, 200h	; get rm int
	int 31h
	cmp cx, __RmSeg
	jnz @F
	cmp dx, fs:[DEBRMVAR.wIntr2Fr]
	jz untraprm2f_1
@@:
	invoke printf, CStr("debuggee has changed real mode int 0x2F!",lf)
untraprm2f_1:
	pop dx
	pop cx
	mov bl, 2fh
	mov ax, 201h	; set rm int
	int 31h
exit:
	ret
	assume fs:nothing
untraprm2F endp
endif

if ?TRAPRM2A
traprm2A proc stdcall
	mov bl, 2ah
	mov ax, 200h	; get rm int
	int 31h
	mov fs, [__RmDS]
	assume fs:_TEXT16
	mov word ptr fs:[DEBRMVAR.oldint2Ar+0], dx
	mov word ptr fs:[DEBRMVAR.oldint2Ar+2], cx
	mov cx, __RmSeg
	mov dx, fs:[DEBRMVAR.wintr2Ar]
	mov bl, 2ah
	mov ax, 201h	; set rm Int
	int 31h
	ret
	assume fs:nothing
traprm2A endp

untraprm2A proc stdcall
	mov fs, [__RmDS]
	assume fs:_TEXT16
	xor ecx, ecx
	xchg ecx,fs:[DEBRMVAR.oldint2Ar]
	jecxz exit
	push ecx
	mov bl, 2ah
	mov ax, 200h	; get rm int
	int 31h
	cmp cx, __RmSeg
	jnz @F
	cmp dx, fs:[DEBRMVAR.wintr2Ar]
	jz untraprm2A_1
@@:
	invoke printf, CStr("debuggee has changed real mode int 0x2A!",lf)
untraprm2A_1:
	pop dx
	pop cx
	mov bl, 2ah
	mov ax, 201h	; set rm int
	int 31h
exit:
	ret
	assume fs:nothing
untraprm2A endp
endif

if ?TRAPRM15
traprm15 proc stdcall
	mov bl, 15h
	mov ax, 200h	; get rm int
	int 31h
	mov fs, [__RmDS]
	assume fs:_TEXT16
	mov word ptr fs:[DEBRMVAR.oldint15r+0], dx
	mov word ptr fs:[DEBRMVAR.oldint15r+2], cx
	mov cx, __RmSeg
	mov dx, fs:[DEBRMVAR.wintr15r]
	mov bl, 15h
	mov ax, 201h	; set rm int
	int 31h
	ret
	assume fs:nothing
traprm15 endp

untraprm15 proc stdcall
	mov fs, [__RmDS]
	assume fs:_TEXT16
	xor ecx, ecx
	xchg ecx, fs:[DEBRMVAR.oldint15r]
	jecxz exit
	push ecx
	mov bl, 15h
	mov ax, 200h	; get rm int
	int 31h
	cmp cx, __RmSeg
	jnz @F
	cmp dx, fs:[DEBRMVAR.wintr15r]
	jz untraprm15_1
@@:
	invoke printf, CStr("debuggee has changed real mode int 0x15!",lf)
untraprm15_1:
	pop dx
	pop cx
	mov bl, 15h
	mov ax, 201h	; set rm int
	int 31h
exit:
	ret
	assume fs:nothing
untraprm15 endp
endif

;--- get dos memory in eax, returns selector in ax, segm in dx

GetDosMem proc

if ?WINDOWS
	@savewinsegregs
	push eax
	call _GlobalDosAlloc
	@restorewinsegregs
	movzx eax,ax
else
	mov cl, al
	shr eax, 4
	test cl, 0Fh
	jz @F
	inc eax
@@:
	mov ebx, eax
	mov ax, 100h
	@DpmiCall
	xchg eax, edx
	jnc @F
	xor eax, eax
@@:
endif
	ret
GetDosMem endp

;--- get size of file (handle in eax) in eax

GetFileSize proc
	mov ebx, eax
	xor edx, edx
	xor ecx, ecx
	mov ax, 4202h
	@DosCall
	push dx
	push ax
	pop eax
	ret
GetFileSize endp

InitRMDbgHlp proc stdcall

local	dwSize:dword
local	dwSizeSaveBuff
local	fhandle:dword
local	_InitRmDbgHlp:PF32
local	desc[8]:byte

	@tprintf <"InitRMDbgHlp enter",lf>
;----------------------- get task state save addresses

	@DpmiCall 305h
	mov word ptr dpmisrtsRM+0, cx
	mov word ptr dpmisrtsRM+2, bx
if ?32BIT
	mov dword ptr dpmisrtsPM+0, edi
	mov word ptr dpmisrtsPM+4, si
else
	mov word ptr dpmisrtsPM+0, di
	mov word ptr dpmisrtsPM+2, si
endif
	movzx eax,ax				 ;size of task state save buffer
	mov dwSizeSaveBuff, eax

	invoke _createfilename, CStr("rmdbghlp.bin")
	mov edx, eax
	mov ax,3d20h			;r/o, deny write
	@DosCall
	jnc @F
	movzx eax,ax
	push eax
	push edx
	@errorout ERR_FILE_OPEN
	add esp,2*4
	jmp error
@@:
	@tprintf <"rmdbghlp.bin opened, handle=%X",lf>, eax
	mov [fhandle], eax
	call GetFileSize
	mov dwSize, eax
	@tprintf <"size of rmdbghlp.bin = %X",lf>, eax

	add eax, dwSizeSaveBuff

	call GetDosMem
	and eax, eax
	jz error
	@tprintf <"GetDosMem succeeded, sel=%X, segm=%X",lf>, eax, edx

	mov [__RmDS], eax
	mov [__RmSeg], dx
	mov fs, eax
	assume fs:_TEXT16
	mov edx, eax
	xor ecx,ecx
	invoke _fileread, fhandle, edx::ecx, dwSize, 0
	@tprintf <"fileread(rmdbghlp.bin) returned %X",lf>, eax
	@close fhandle
	@tprintf <"close(%X) returned %X",lf>, fhandle, eax

	cmp word ptr fs:[0], sizeof DEBRMVAR
	jnz error

	mov cx, 1
	xor eax, eax
	@DpmiCall
	jc error
	@tprintf <"alloc selector for realmode CS ok, is %X",lf>, eax
	mov __RmCS, eax
	mov ebx, [__RmDS]
	lea edi, desc
	mov ax, 000bh
	@DpmiCall
if _TRACE_
	lar eax, ebx
	lsl ecx, ebx
endif
	@tprintf <"get descriptor %X (acc=%X, lim=%X)",lf>, ebx, eax, ecx
	mov ebx, __RmCS
	or byte ptr desc+5, 8
if ?32BIT
	and byte ptr desc+6,not 40h		;required by DOSEMU
endif
	mov ax, 000ch
	@DpmiCall
if _TRACE_
	lar eax, ebx
	lsl ecx, ebx
endif
	@tprintf <"set descriptor %X (acc=%X, lim=%X)",lf>, ebx, eax, ecx

	mov word ptr _InitRmDbgHlp+4, bx
	mov dword ptr _InitRmDbgHlp, sizeof DEBRMVAR

	@tprintf <"calling _InitRmDbgHlp (%X)",lf>, dword ptr _InitRmDbgHlp
	mov eax, [__RmDS]
	push __RmSeg
	push ax
	call _InitRmDbgHlp
	@tprintf <"returned from _InitRmDbgHlp",lf>

if ?RMCALLBACK
	push ds
	mov es,[__csalias]
	push cs
	pop ds
	mov esi,offset intrxx
	mov edi,offset rmcsi0y
	mov ax, 303h
	@DpmiCall
	pop DS
	jc alloccbs_er
	mov word ptr fs:[DEBRMVAR.intxxcb+0], dx
	mov word ptr fs:[DEBRMVAR.intxxcb+2], cx
else
	mov word ptr fs:[DEBRMVAR.pmcs], cs
	mov word ptr fs:[DEBRMVAR.pmds], ds
	mov eax,[myesp]
	mov fs:[DEBRMVAR.pmesp], eax
	mov fs:[DEBRMVAR.pmeip], offset rm2pmentry

	mov eax, dpmisrtsRM
	mov fs:[DEBRMVAR.rmsavestate], EAX

	mov ax,__RmSeg
	shl eax, 16
	mov ax,word ptr dwSize
	mov fs:[DEBRMVAR.savestatebuffRM], eax
	mov ecx, fs
	shl ecx, 16
	mov cx, ax
	mov fs:[DEBRMVAR.savestatebuffPM], ecx
@@:
	@DpmiCall 306h
	mov word ptr fs:[DEBRMVAR.rm2pmjump+0], cx
	mov word ptr fs:[DEBRMVAR.rm2pmjump+2], bx
ife ?32BIT
	movzx edi,di
endif
	mov dword ptr [pm2rmjump+0], edi
	mov word ptr  [pm2rmjump+4], si
endif
	call setrmvecs
	clc
	jmp exit
error:
	xor eax, eax
	mov rmtraps, eax
	stc
exit:
ife ?FLAT
	push ds
	pop es
endif
	@tprintf <"InitRMDbgHlp exit",lf>
	ret
	assume fs:nothing
InitRMDbgHlp endp

ExitRMDbgHlp proc stdcall

local	_Wep:PF32

	@tprintf <"ExitRMDbgHlp enter",lf>
	cmp [__RmDS], 0
	jz @F
	call resetrmvecs
@@:
if ?RMCALLBACK
else
	mov 	ecx,[__RmCS]
	jecxz	@F
	mov		fs, ecx
	assume	fs:_TEXT16
	mov		word ptr _Wep+4,cx
	movzx	ecx, fs:[DEBRMVAR.wExit]
	mov		dword ptr _Wep+0, ecx
	call	_Wep
	mov		ebx, fs
	push	0
	pop		fs
	mov		ax,1
	@DpmiCall
@@:
	mov 	edx, [__RmDS]
	and 	edx,edx
	jz		@F
if ?WINDOWS
	@savewinsegregs
	push	dx
	call	_GlobalDosFree
	@restorewinsegregs
else
	@DpmiCall 101h
endif
@@:
endif
if ?RMCALLBACK
	mov cx, word ptr fs:[intxxcb+2]
	mov dx, word ptr fs:[intxxcb+0]
	mov ax, 304h
	@DpmiCall
endif
	ret
	assume fs:nothing

ExitRMDbgHlp endp

;*** display all traps

_trapsout proc c

	invoke printf, CStr("Trapped vectors:",lf)
	mov ebx, offset trapvars
	mov eax, offset trapvarsend
	mov ch, _RDONLY_
	call symtout
if ?USETOOLHELP
	test fToolHelp, FTH_REGISTERED
	jz trapsout_4
	@errorout MSG_EXC_FROM_TOOLHELP
	mov ecx, [toolhlpvecs]
	and ecx, [toolhlpmask]
	mov dl, 0
trapsout_3:
	jecxz trapsout_2
	shr ecx, 1
	jnc @F
	mov al, dl
	invoke _hexout
	@putchr ' '
@@:
	inc dl
	jmp trapsout_3
trapsout_2:
	invoke _crout
endif
trapsout_4:
	ret
_trapsout endp

;--- R1 command

_myregsout proc c

	invoke myregsout,[dwFormStr], addr r1
	ret
_myregsout endp

;*** register anhand frmstr und frmval aufbereiten ***

myregsout proc stdcall lfrmstr:dword,lregs:dword

	mov 	esi,lregs
	movzx	eax,word ptr [esi.MYREGSTR.rGS]
	push	eax
	movzx	eax,word ptr [esi.MYREGSTR.rFS]
	push	eax
	movzx	eax,word ptr [esi.MYREGSTR.rES]
	push	eax
	movzx	eax,word ptr [esi.MYREGSTR.rDS]
	push	eax
	push	[esi.MYREGSTR.rEsp]
	movzx	eax,word ptr [esi.MYREGSTR.rSS]
	push	eax
	push	[esi.MYREGSTR.rEip]
	movzx	eax,word ptr [esi.MYREGSTR.rCS]
	push	eax
	push	[esi.MYREGSTR.rEfl]
	push	[esi.MYREGSTR.rEbp]
	push	[esi.MYREGSTR.rEdi]
	push	[esi.MYREGSTR.rEsi]
	push	[esi.MYREGSTR.rEdx]
	push	[esi.MYREGSTR.rEcx]
	push	[esi.MYREGSTR.rEbx]
	push	[esi.MYREGSTR.rEax]
	push	lfrmstr
	call	printf
	add 	esp,16*4+4
	ret
myregsout endp

if ?WINDOWS

;*** direkter aufruf aus DEB16FW.EXE ***
;--- check if we are on top of all exception vector chains

CheckUnregister proc far stdcall uses ds esi

	xor 	eax,eax
	mov 	ds,cs:[__csalias]
	push	ds
	pop 	es
	mov 	ecx,20h
	mov 	esi,offset exc00h
	mov 	ebx,eTraps
nextitem:
	test	bl,01
	jz		skipitem
	push	eax
	push	ebx
	push	ecx
	mov bl,al
	mov ax,0202h	;get exc vec
	int 31h
if ?32BIT eq 0
	movzx	edx,dx
endif
ife ?32BIT
	mov 	eax,[__cs16alias]
else
	mov 	eax,cs
endif
	cmp 	cx,ax
	jnz 	error
	cmp 	edx,esi
	jnz 	error
	pop 	ecx
	pop 	ebx
	pop 	eax
skipitem:
	shr 	ebx,1
	add 	esi,4
	inc 	al
	loop	nextitem
	@mov 	eax,1
	jmp 	exit
error:
	pop 	ecx
	pop 	ebx
	pop 	eax
	xor 	eax,eax
exit:
	ret
CheckUnregister endp

endif

if ?USETOOLHELP
toolhelpregister proc stdcall

ife ?USEINT41
	cmp fUseTH, 0
	jz @F
	test fToolHelp, FTH_REGISTERED
	jnz @F

	@savewinsegregs
	@tprintf <"calling InterruptRegister",lf>

if ?CS16ALIAS        
	mov eax, __cs16alias
else
	mov eax, cs
endif
	shl eax, 16
	mov ax, lowword offset intrcb
	push [hMyTask]
	push eax
	call _InterruptRegister
	@tprintf <"InterruptRegister returned ax=%X",lf>, eax

if ?CS16ALIAS
	mov eax,__cs16alias
else
	mov eax,cs
endif
	shl eax, 16
	mov ax, lowword offset notifycb
	push [hMyTask]
	push eax
	push word ptr NF_NORMAL
	call _NotifyRegister
	@tprintf <"NotifyRegister returned ax=%X",lf>, eax
	or fToolHelp, FTH_REGISTERED
	@restorewinsegregs
@@:
endif
	ret
toolhelpregister endp

toolhelpunregister proc stdcall

ife ?USEINT41
	test fToolHelp, FTH_REGISTERED
	jz @F
	@savewinsegregs

	push [hMyTask]
	call _NotifyUnregister
	@tprintf <"NotifyUnregister returned ax=%X",lf>, eax

	push [hMyTask]
	call _InterruptUnregister
	@tprintf <"InterruptUnregister returned ax=%X",lf>, eax

	and fToolHelp, not FTH_REGISTERED

	@restorewinsegregs

@@:
endif
	ret
toolhelpunregister endp
endif

getfatalexitaddr:
	push es
	push 0
	pop es
	mov ax,1684h
	mov bx,9		   ;REBOOT device ID
	@callint2F
	mov eax,es
	pop es
	and ax,ax
	jz @F
	mov dword ptr _FatalExit+0,edi
	mov word ptr _FatalExit+?SEGOFFS,ax
@@:
	ret

AllocRing0Switch proc

	test [fMode], FMODE_STRICT
	jnz exit
	mov bx,[rLDT]
	invoke getbaser,ebx			;get base of LDT
	jc forcestrict 			;not possible

	mov [excexit],offset forcestrict

	mov ebx,eax
	mov eax, [worksel]
	lea eax, [eax+8]
	mov [ring0sel],eax
	movzx ecx,ax
	mov dl,cl
	and cl,0F8h
	add ebx,ecx
	add cx,08h
	and dl,04
	add cl,dl					;1. selector ist call gate
	mov eax,offset ring0rou 	;basisadresse
if ?USEFLATR0CS
	add eax,MyBase
endif
;--- direct write into LDT. will cause a page fault
;--- if LDT is r/o. set mode to strict to avoid
;--- exceptions inside the exception routine.
	or [fMode], FMODE_STRICT
	mov @flat:[ebx+0],ax		;LOWORD(offset)
	mov @flat:[ebx+2],cx		;selector (in LDT)
	shr eax,16
	mov @flat:[ebx+6],ax		;HIWORD(offset)
	mov eax,cs
	lar eax,eax
	and ax,6000h
	or ah,8Ch						;386 call gate, present
	mov word ptr @flat:[ebx+4],ax

	add ebx,8

if ?USEFLATR0CS
	mov word ptr @flat:[ebx+0],0FFFFh
	xor eax,eax
	mov dword ptr @flat:[ebx+2],eax
	mov word ptr @flat:[ebx+5],0CF9Bh	  ;ring 0,code,present, 32-Bit
	mov byte ptr @flat:[ebx+7],al
else
										  ;2. selector ist code segment
	mov eax, MyBase
	mov ecx,cs
	lsl ecx,ecx
	mov word ptr @flat:[ebx+0],cx
	mov word ptr @flat:[ebx+2],ax
	shr eax,16
	mov byte ptr @flat:[ebx+4],al
	mov word ptr @flat:[ebx+5],409Bh	  ;ring 0,code,present, 32-Bit
	mov byte ptr @flat:[ebx+7],ah
endif
	and [fMode], not FMODE_STRICT
	jmp exit
forcestrict:
	@tprintf <"AllocRing0Switch: no write access to LDT",lf>
	or fMode, FMODE_STRICT
	mov ring0sel, 0
exit:
	mov excexit,0
	ret
        
AllocRing0Switch endp

;--- win9x hesitates to send exc 01 messages!!!

forceexc01supply proc

	push ds
	mov ds,cs:[__csalias]
	cmp wWinVersion, 0		;required for win9x only
	jz exit
	test byte ptr eTraps, 2
	jz exit
	push excexit
	mov excexit, offset isok
	pushad
	mov ecx,1000000h
	@loadesp esi
@@:
	pushfd
	or byte ptr ss:[esi-3],1
	popfd
	loop @B
	@stroutc "error: cannot receive single-step exceptions",lf
isok:
	popad
	pop excexit
exit:
	pop ds
	ret

forceexc01supply endp

if ?LOADVDD

	include ISVBOP.INC


if ?FLAT

InitBop proc
	mov cx, 1
	xor eax, eax
	int 31h
	jc error
	push ebx
	mov ebx, eax
	mov dwBopSel, eax

	mov ecx, offset runcxx
	push ecx
	pop dx
	pop cx
	mov ax, 7
	int 31h
	mov cx,0
	mov dx,-1
	mov ax,8
	int 31h
	mov eax, cs
	lar ecx, eax
	shr ecx, 8
	mov ax,9
	int 31h

	pop ebx
error:
	ret
InitBop endp

DeinitBop proc
	invoke RunBop, 1
	mov hVDD, -1
	push ebx
	mov ebx, dwBopSel
	mov ax,1
	int 31h
	pop ebx
	ret
DeinitBop endp

;--- NTVDM wont run BOP code if it is executed in a PE binary

RunBop proc stdcall public dwFunc:dword

	push dwBopSel
	push eax
	mov eax, dwFunc
	and eax, eax
	jnz @F
	mov es,[dwBopSel]
	push es
	pop ds
@@:
	lea eax, [eax+eax*4]
	xchg eax, [esp]
	call fword ptr [esp]
	lea esp,[esp+2*4]
	ret
runcxx label dword
	RegisterModule
	retf
	UnRegisterModule
	retf
	DispatchCall
	retf

RunBop endp

endif

svdd1 db "DEBXXVDD.DLL",0
svdd2 db "Dispatch",0
svdd3 db "Init",0

LoadDebugVdd proc

	pushad
if ?FLAT
	invoke InitBop
	mov esi, offset svdd1 - offset runcxx
	mov ebx, offset svdd2 - offset runcxx
	mov edi, offset svdd3 - offset runcxx
	push ds
	push es
	invoke RunBop, 0
	pop es
	pop ds
else
	mov esi, offset svdd1	;DS:SI
	mov ebx, offset svdd2	;DS:BX
	mov edi, offset svdd3	;ES:DI
	RegisterModule
endif
	jc error
	mov hVDD, eax
;--- it's too early here for real displays - value of variable swap hasn't
;--- been read yet.
	@tprintf <"VDD debxxvdd registered",10>
	popad
	ret
error:
	@tprintf <"RegisterModule debxxvdd failed",10>
	popad
	ret

LoadDebugVdd endp

endif

;*** first entry - register

firstentry proc stdcall

	pushad
	test [fStat], FSTAT_DEBUGINIT ;erster einsprung ?
	jnz fentryex

	mov bRC, -1

	mov ax, 204h
	mov bl, 21h							 ;int 21 pm nicht direkt aufr.
	int 31h
	mov dword ptr oldint21, edx
	mov word ptr oldint21+?SEGOFFS, cx
if ?HIDEINT21
	mov dword ptr oldi21p, edx
	mov word ptr oldi21p+?SEGOFFS, cx
endif
if ?WATCHI2F
	mov bl,2Fh
	int 31h
	mov dword ptr oldint2F, edx
	mov word ptr oldint2F+?SEGOFFS, cx
endif
if ?SAVEINT08
	mov bl,08h
	int 31h
	mov dword ptr oldint08, edx
	mov word ptr oldint08+?SEGOFFS, cx
	mov bl,1Ch
	int 31h
	mov dword ptr oldint1C, edx
	mov word ptr oldint1C+?SEGOFFS, cx
endif

	call kbd_init
	dec bRC

if ?WATCHI31
	mov ax, 205h					; set my int 31h
	mov bl, 31h
if ?CS16ALIAS        
	mov ecx, [__cs16alias]
else
	mov ecx, cs
endif        
	mov edx, offset myint31
	@DpmiCall						; use @DpmiCall here!
endif
if ?WATCHI21
	mov ax, 205h
	mov bl, 21h
if ?CS16ALIAS
	mov ecx, [__cs16alias]
else
	mov ecx, cs
endif        
	mov edx, offset myint21
	@DpmiCall						; use @DpmiCall here!
endif
if ?WATCHI2F
	mov ax, 205h
	mov bl, 2Fh
if ?CS16ALIAS
	mov ecx, [__cs16alias]
else
	mov ecx, cs
endif        
	mov edx, offset clientint2f
	@DpmiCall
endif

	@tprintf <"call getalternateadapter",lf>
	call getalternateadapter

	mov ax,3306h
	int 21h
	cmp bx,3205h					; nt VDM?
	jnz @F
	or [fMode], FMODE_STRICT
	or [fStat], FSTAT_ISNT
	or [fKeybrd], FKB_EXECINT01 or FKB_ALTSCROLL
	or [fTMode], FTMODE_NOTRACEFORINT	; avoid trace stops in INT 21/33/...
if ?LOADVDD
	call LoadDebugVdd
endif
@@:

	dec bRC
	or [fStat], FSTAT_DEBUGINIT
	mov eax, offset top_of_stack
	mov [myesp], eax

	test [f80x87], F80X87_IGNORE
	jnz @F
	int 11h
	shr al,1
	and al, F80X87_FPU_PRESENT
	mov [f80x87], al
@@:
	dec bRC
	mov ah,30h
	@DosCall
	mov [wDosVersion],ax
	cmp al,20						; os/2 dos-box?
	jb @F
	or [fMode], FMODE_STRICT
@@:
	@loadflat
	mov ecx, 0ffff5h
	cmp dword ptr @flat:[ecx+0],"2/20"	; DOSEMU "02/25/93"
	jnz @F
	cmp dword ptr @flat:[ecx+4],"39/5"
	jnz @F
	or fMode, FMODE_STRICT
	or fKeybrd, FKB_EXECINT01
	mov wRMStop, 0F800h
	mov wPMBreak, 0F4h
@@:
	mov ax, 1600h
	@callint2F
	and al, al
	jz @F
	mov [wWinVersion], ax
	or fKeybrd, FKB_EXECINT03
	mov wRMBreak, 63h				; ARPL is breakpoint
@@:
	dec bRC
	@tprintf <"get indos flag addr",lf>
if 1
	sub esp, sizeof RMCS+2
	@loadesp edi
	mov [edi].RMCS.rSSSP, 0
	mov [edi].RMCS.rFlags, 202h
	mov byte ptr [edi].RMCS.rAX+1, 34h
	xor ecx, ecx
	mov bx, 0021h
	mov ax, 300h
	int 31h
	movzx eax, [edi].RMCS.rES
	movzx ecx, [edi].RMCS.rBX
	shl eax, 4
	add eax, ecx
	mov [dwIndos], eax
	add esp, sizeof RMCS+2
else
	push es
	mov ah, 34h
	@DosCall
	mov word ptr [dwIndos+0], bx
	mov word ptr [dwIndos+2], es
	pop es
endif
	dec bRC

if ?WINDOWS
	@tprintf <"call GetCurrentTask",lf>
	@savewinsegregs
	call _GetCurrentTask
	@restorewinsegregs
	mov [hMyTask], ax
endif
	dec bRC

	mov ah, 51h
	int 21h
	movzx ebx,bx
	mov [_psp], ebx
	push es
	mov es, ebx
	movzx eax, word ptr es:[002Ch]
	pop es
	mov [_environ], eax
	mov ax,6
	@DpmiCall
	push cx
	push dx
	pop eax
	shr eax,4
	mov [_pspseg],eax

	dec bRC

if ?SETERRORMODE
	@tprintf <"call seterrormode",lf>
	@savewinsegregs
	push word ptr (SEM_NOOPENFILEERRORBOX or SEM_FAILCRITICALERRORS)
	call _SetErrorMode
	@restorewinsegregs
endif

	dec bRC
	sgdt [rGDTR]
	sidt [rIDTR]
	sldt [rLDT]

	dec bRC
	@tprintf <"get sda parms",lf>
	call getdosparms

	dec bRC
	@tprintf <"alloc flat heap",lf>
									; buffer for SDA alloc
	mov bx, 4						; 256kb flat heap
	xor ecx, ecx
	@DpmiCall 501h
	jc @F
	mov word ptr [flatheap+0], cx
	mov word ptr [flatheap+2], bx
	mov word ptr [flathdl+0], di
	mov word ptr [flathdl+2], si
	mov ecx, [sdalen1]
	invoke allocflat, ecx
	mov [pSdaSave], eax
@@:
	dec bRC
									; descriptor alloc
									; 1. work descriptor (diverses)
									; 2. 386 call gate
									; 3. CS for Ring 0
	mov 	cx,0003
	@DpmiCall 0000
	jc fentryex
	mov [worksel],eax
	mov cx, 0
	mov dx, -1
	mov ebx, eax
	mov ax, 7
	int 31h
	dec bRC

if ?WINDOWS
	test [fStat], FSTAT_ISNT
	jnz @F
	invoke LoadGraphHlp
	cmp byte ptr wWinVersion, 4
	jb deb16fwh_done
@@:
	invoke loaddll32, CCStr("deb16fwh.dll")
	mov hdeb16fwh, eax
	and eax, eax
	jz deb16fwh_done
	mov esi, offset szListModule
	cmp wWinVersion, 0
	jz @F
	mov dwWin9xProcsEnd, 0	; don't import the console functions ( wouldn't work in win9x )
@@:
deb16fwh_nextitem:
	lodsd
	and eax, eax
	jz deb16fwh_done
	invoke	getproc32, hdeb16fwh, eax, 0
	mov [esi], eax
	add esi, 4
	jmp deb16fwh_nextitem
deb16fwh_done:
endif

	dec bRC

	mov eax, offset _heap
	mov [pHeap], eax
	mov edi, eax
	mov ecx, ?HEAPSIZE
	add eax, ecx
	mov [pHeapMax], eax
	shr ecx, 2
	xor eax, eax
	rep stosd

	@tprintf <"read helpfile",lf>
	dec bRC
	mov [dwFormStr], offset deffrmstr
	cmp flatheap,0
	jz fentry_3
	invoke readhelpfile
	@tprintf <"read macro file",lf>
	call ReadMacros
fentry_3:
	dec bRC
if ?READINIPARMS
	@tprintf <"read profile file",lf>
	call _getinifileparms
endif
ife ?WINDOWS
	call LoadGraphHlp
	and eax, eax
	jz fentryex
endif        

	dec bRC
if ?USETOOLHELP
	call toolhelpregister
endif
	dec bRC


	@tprintf <"install gen trap handlers",lf>
	mov al, 00
if ?USEINT41
	or byte ptr eTraps, 2	;always single step exc
	or byte ptr eStops, 2	;always single step exc
endif        
	mov ebx,eTraps
	mov edx,eStops
fentry1x:
	pushad
	mov cl, dl
	and cl, 1			; stop-bit nach bit 1
	shl cl, 1
	mov ch, bl
	and ch, 1
	or	cl, ch			; trap-bit nach bit 0
	mov ch, 0
	test cl, 1
	jz	@F
	mov bl, al			; trapnr
	mov al, cl
	call setexception
@@:
	popad

	shr ebx,1
	shr edx,1
	inc al
	cmp al,20h
	jnz fentry1x

	dec bRC

	@tprintf <"install com irq handler",lf>
	call installirqcom

	dec bRC

	@tprintf <"install kbd irq handler",lf>
	call installirqkbd

	@tprintf <"install mou irq handler",lf>
	call installirqmou

	dec bRC

ife ?WINDOWS
	@tprintf <"install int handlers",lf>
	call setpmvecs
endif

	dec bRC
        
	call InitRMDbgHlp
	jnc @F
	@errorout ERR_RMINT_HANDLER
@@:
	@tprintf <"done init rm stuff",lf>
if ?TRAPRM2F
	call traprm2F
endif
if ?TRAPRM2A
	call traprm2A
endif
if ?TRAPRM15
	call traprm15
endif
	@tprintf <"call getdpmiparms",lf>
	call getdpmiparms

	call getfatalexitaddr

if ?WATCHINT23
	cmp bCtrlC, 0
	jz @F
	push ds
	mov ax, 2523h
	mov edx, offset intr23
 if ?CS16ALIAS
	mov ds,[__cs16alias]
 else
	push cs
	pop ds
 endif
	@DosCall
	pop ds
@@:
endif

ife ?WINDOWS		; for windows not possible
 if ?WATCHINT24
	push ds
	mov ax, 2524h
	mov edx, offset intr24
  if ?CS16ALIAS
	mov ds,[__cs16alias]
  else
	push cs
	pop ds
  endif
	@DosCall
	pop ds
 endif
endif

	@tprintf <"call install_i41",lf>
	call install_i41

	mov bRC, 0
	@tprintf <"call forceexc01supply",lf>
	call forceexc01supply
	@tprintf <"call AllocRing0Switch",lf>
	call AllocRing0Switch

	@tprintf <"firstentry exit",lf>
	mov bRC, 1

fentryex:
	popad
	mov al, 00
	xchg al, bRC
	mov ah, 00
	ret
firstentry endp

@idtentry macro x
int&x&idtentry:
	push x&h
	jmp setidtentry
endm

	@idtentry 00
	@idtentry 01
	@idtentry 02
	@idtentry 03
	@idtentry 04
	@idtentry 05
	@idtentry 06
	@idtentry 07
if _IDTVECS_ gt 8
	@idtentry 08
	@idtentry 09
	@idtentry 0A
	@idtentry 0B
	@idtentry 0C
	@idtentry 0D
	@idtentry 0E
	@idtentry 0F
endif
setidtentry:
;	@cli
	push ds
	mov ds, cs:[__csalias]
	pop [dstemp]
	pop [idtexc]
	cmp byte ptr idtexc, 1
	jnz @F
	or [fEntry], FENTRY_INT01
@@:
	cmp byte ptr idtexc,3
	jnz @F
	or [fEntry], FENTRY_INT03
@@:
	or [fEntry], FENTRY_IDT
	test [fEntry], FENTRY_INT01 or FENTRY_INT03
	jnz @F
	push gs
	push eax
	@loadflat
	@stroutc "exception "
	mov al, byte ptr [idtexc]
	invoke _hexout
	@stroutc " occured",lf
	pop eax
	pop gs
	cmp byte ptr idtexc,8
	jb @F
	pop [idterrc]		 ;errorcode vom stack entfernen
	push gs
	push eax
	@loadflat
	@stroutc "Errorcode="
	@dwordout [idterrc]
	invoke _crout
	pop eax
	pop gs
@@:
	mov ds,[dstemp]
	jmp debug_entry

;--- mindex:
;--- tvalue:

set_reset_idtvec proc stdcall mindex:dword, tvalue:dword

	pushad
	test [fMode], FMODE_STRICT
	jnz exit2
	pushfd
	cli
	@loadflat
	mov ecx, tvalue
	mov ebx, mindex
	mov edx, offset oldidtvecs
	mov esi, offset myidtvecs
	cmp ebx, _IDTVECS_
	jnb set_reset_idtvec_er
	jecxz reset_idtvec					; set/reset?

	mov eax, [ebx*8+edx] 				; already set?
	and eax,eax
	jnz exit							; then done
	@tprintf <"set idt vec %X",lf>, mindex
	mov ecx, dword ptr [rIDTR+2]
	mov eax, @flat:[ebx*8+ecx]			; save current vector
	mov [edx+ebx*8], eax
	mov eax, @flat:[ebx*8+ecx+4]
	mov [edx+4+ebx*8], eax
	mov eax,[esi+ebx*4]					; set new vector
	mov word ptr @flat:[ebx*8+ecx+0], ax
	shr eax,16
	mov word ptr @flat:[ebx*8+ecx+6], ax
	mov word ptr @flat:[ebx*8+ecx+4], 0EE00h; 32bit task gate
	mov word ptr @flat:[ebx*8+ecx+2], cs
	clc
	jmp exit
reset_idtvec:
	xor eax, eax
	xchg eax, [edx+ebx*8+0]
	and eax, eax
	jz exit
	@tprintf <"reset idt vec %X",lf>, mindex
	mov edx, [edx+ebx*8+4]
	mov ecx, dword ptr [rIDTR+2]
	mov @flat:[ebx*8+ecx+0], eax
	mov @flat:[ebx*8+ecx+4], edx
	clc
	jmp exit
set_idtvec_er:
reset_idtvec_er:
set_reset_idtvec_er:
	stc
exit:
	pop eax
	test ah, FL_INT
	jz @F
	sti
@@:
exit2:
	popad
	ret
set_reset_idtvec endp

installirqmou proc stdcall public

if ?WINDOWS
	test [__outmode], _VIOOUT
	jnz installirqmou_ex
	@savewinsegregs
	call _MouseGetIntVect
	@restorewinsegregs
	sub al,8
	cmp al,8
	jc @F
	sub al,68h
@@:
	mov cl,al
	mov ax,1
	shl ax,cl
	or [wPicOn],ax
installirqmou_ex:
else
;	or byte ptr [wPicOff+1], 10h	; disable IRQ 12 (PS/2)
endif
	ret
installirqmou endp

if ?READINIPARMS

CCONST segment		; use CCONST to ensure it is in first 64 kB

szParms db ?MYNAME,00

CCONST ends

persistab label dword
if ?USETOOLHELP
	dd CCStr("UseTH")
	dd offset fUseTH
	db 0

	dd CCStr("ToolhlpVecs")
	dd offset toolhlpvecs
	db 0
endif
	dd CCStr("TrappedExcVecs")
	dd offset eTraps
	db 0

	dd CCStr("StopExcVecs")
	dd offset eStops
	db 0

	dd CCStr("FirstExc")
	dd offset eFirst
	db 0

;	dd CCStr("VPage")
;	dd offset myvpage
;	db 0

	dd CCStr("WaitLines")
	dd offset xLines
	db 0

	dd CCStr("TrappedRMVecs")
	dd offset rmtraps
	db 0

ife ?WINDOWS
	dd CCStr("TrappedPMVecs")
	dd offset pmtraps
	db 0
endif
if ?WATCHINT23
	dd CCStr("Ctrl-C")
	dd offset bCtrlC
	db 0
endif
if ?HWBREAKS
	dd CCStr("HWBrk")
	dd offset bHWBrk
	db 0
endif
ife ?WINDOWS
	dd CCStr("ScrnSwap")
	dd offset fSwap
	db 0

	dd CCStr("GraphHlp")
	dd offset graphhlpstr
	db 64					;is a string (maxsize)
endif
	dd 0

keytab label dword
	dd CCStr("KEYSN")
	dd CCStr("KEYSS")
	dd CCStr("KEYSA")

	.code

if ?WINDOWS

GetPrivateProfileStringA proc stdcall uses ebx esi szSection:ptr BYTE, szEntry:ptr BYTE, szDefault:ptr BYTE, pReturn:ptr BYTE, sizeReturn:DWORD, szFilename:ptr BYTE

	@savewinsegregs
	mov ecx,ds
	mov eax, szSection
	push cx				;1. section
	push ax

	mov eax, szEntry
	push cx				;2. entry
	push ax

	mov eax, szDefault
	push cx				;3. default string
	push ax

	mov eax,pReturn
	push cx				;4. addr returnbuffer
	push ax

	mov eax,sizeReturn
	push ax				;5. size returnbuffer

	mov eax,szFilename
	push cx
	push ax				;6. filename

	call _GetPrivateProfileString
	@restorewinsegregs
	movzx eax,ax
	ret
GetPrivateProfileStringA endp

WritePrivateProfileStringA proc stdcall uses ebx esi szSection:ptr BYTE, szEntry:ptr BYTE, szString:ptr BYTE, szFilename:ptr BYTE

	@savewinsegregs
	mov ecx,ds
	mov eax, szSection
	push cx				;1. section
	push ax

	mov eax, szEntry
	push cx				;2. entry
	push ax

	mov eax, szString
	push cx				;3. string
	push ax

	mov eax, szFilename
	push cx				;4. file
	push ax

	call _WritePrivateProfileString
	@restorewinsegregs
	movzx eax, ax
	ret
WritePrivateProfileStringA endp

endif

_getinifileparms proc c

local	pszIniPath:dword
local	varaddr:dword

	pushad
	push pNearHeap

	invoke _createfilename, CStr("debxxf.ini")
	add [pNearHeap], edx
	mov pszIniPath, eax

	mov esi,offset persistab
nextitem:
	lodsd
	and eax,eax
	jz exit

	mov edx, eax
	lodsd
	mov varaddr, eax
	@tprintf <"getinifileparams: %s",lf>, edx
	invoke GetPrivateProfileStringA, addr szParms, edx,\
		addr szNull, [pNearHeap], 100h, pszIniPath
	and eax, eax
	push esi
	jz getinifileparms_1

	cmp byte ptr [esi], 0
	jz @F
	movzx ecx, byte ptr [esi]
	mov edi, varaddr
	mov esi, pNearHeap
	rep movsb
	jmp getinifileparms_1
@@:
	mov esi,[pNearHeap]
	call getnumber		;parser funktion aufrufen
	jc @F
	mov edx,varaddr
	mov [edx], eax
	@tprintf <"getinifileparams: value %u set",lf>, eax
@@:
getinifileparms_1:
	pop esi
	inc esi
	jmp nextitem
exit:
	mov bl, 0
	mov esi, offset keytab
@@:
	lodsd
	invoke GetPrivateProfileStringA, CCStr("keys"), eax,\
		addr szNull, [pNearHeap], 100h, pszIniPath
	mov cl, bl
	call setkeys
	inc bl
	cmp bl, 3
	jb @B

	pop pNearHeap
	popad
	ret
_getinifileparms endp

_writeinifileparms proc c

local pszIniPath:dword

	pushad
	push pNearHeap

	invoke _createfilename, CStr("debxxf.ini")
	add [pNearHeap], edx
	mov pszIniPath, eax

	mov esi, offset persistab
nextitem:
	lodsd				;get variable name
	and eax,eax
	jz exit

	mov ebx, eax
	lodsd				;address of variable

	cmp byte ptr [esi],0
	jnz skipitem
	invoke sprintf, pNearHeap, CStr("%x"), dword ptr [eax]

	invoke WritePrivateProfileStringA, addr szParms, ebx, pNearHeap, pszIniPath
	@tprintf <"writeinifileparams: %s, value %s",lf>, ebx, pNearHeap

skipitem:
	inc esi
	jmp nextitem
exit:
	pop pNearHeap
	popad
	ret
_writeinifileparms endp

endif

;*** unregister: do things in reversed order than register

_unregister proc stdcall public

	pushad
	test [fStat], FSTAT_DEBUGINIT	; anything initialized?
	jz unregex
									; breakpoints reset
	invoke	ResetBreaks
if 0	;no longer used, done by Free16bitLibs now        
									; free loaded DLLs
	mov ecx,[dwLoadedLibs]
	jecxz unregister_1
	mov esi,offset libhandles
@@:
	lodsd
	push esi
	push ecx
 if ?WINDOWS
	@savewinsegregs
	push ax
	call _FreeLibrary
	@restorewinsegregs
 else
	movzx edx, ax
	mov ax, 4b80h
	@DosCall
 endif
	pop ecx
	pop esi
	loop @B
unregister_1:
endif
	cmp byte ptr cInstalled_i41, 0
	jz @F
	mov byte ptr cInstalled_i41, 1
	call deinstall_i41				; deinstall int 41 handler
@@:

	call kbd_exit

if ?READINIPARMS
	call _writeinifileparms
endif
	mov ecx,_IDTVECS_		; IDT vectors restore
@@:
	jecxz @F
	dec ecx
	invoke set_reset_idtvec, ecx, 0
	jmp @B
@@:
if ?TRAPRM214B
	mov byte ptr cInt21RMTraps,1
	call untraprm21			; int 21 RM restore
endif
if ?TRAPRM2A
	call untraprm2a			; int 2A RM restore
endif
if ?TRAPRM15
	call untraprm15			; int 15 RM restore
endif
if ?TRAPRM2F
	call untraprm2F			; int 2F RM restore
endif
	@tprintf <"unregister: unload rmdbghlp.bin",lf>
	call ExitRMDbgHlp		; free RM CBs

ife ?WINDOWS
	call resetpmvecs 		; restore protected mode int vecs that were trapped
endif

	call deinstallirqcom 	; restore irq for comx
							; that must be done before interrupt unregister
							; (reversed order)
	@tprintf <"unregister: free exception vectors",lf>
	mov bl,0
@@:
	call resetexception
	inc bl
	cmp bl,20h
	jnz @B

	mov ax, 205h
if ?WATCHI31
	mov cx, word ptr oldint31+?SEGOFFS
	mov edx, dword ptr oldint31+0
	mov bl, 31h
	@DpmiCall
endif
if ?WATCHI21
	mov cx, word ptr oldint21+?SEGOFFS
	mov edx, dword ptr oldint21+0
	mov bl, 21h
	@DpmiCall
endif
if ?WATCHI2F
	mov cx, word ptr oldint2F+?SEGOFFS
	mov edx, dword ptr oldint2F+0
	mov bl, 2Fh
	@DpmiCall
endif

;--- ?SAVEINT08 ensures that while the debugger
;--- is active, the debuggee cannot gain control by
;--- chaining into int 08/1Ch.
;--- todo: check if it is really ok NOT to restore the vectors
if 0;?SAVEINT08	; it's deactive at least since v2.10
	mov edx, dword ptr oldint1C+0
	mov cx, word ptr oldint1C+?SEGOFFS
	mov bl, 1Ch
	@DpmiCall
	mov edx, dword ptr oldint08+0
	mov cx, word ptr oldint08+?SEGOFFS
	mov bl, 08h
	@DpmiCall
endif

if ?USETOOLHELP
	call toolhelpunregister
endif

if ?LOADVDD	;ife ?FLAT
	mov eax, -1
	xchg eax, [hVDD]
	cmp eax, -1
	jz @F
 if ?FLAT        
	invoke DeinitBop
 else
	UnRegisterModule
 endif
@@:
endif

	@tprintf <"about to free heap/selectors",lf>

;--- switch to debuggee screen

	and [fExit], not FEXIT_NOSWITCH
	call SwitchToDebuggeeScreen

;--- from here no displays possible!!

	call vdestructor

if ?WINDOWS
	mov ecx,hdeb16fwh			; unload deb16fwh.dll
	jecxz @F					; (must be done AFTER restore exception vecs)
	invoke freedll32,ecx
@@:
endif

;	@tprintf <"free memory",lf>
	mov ax, 502h
	mov si, word ptr [flathdl+2]
	mov di, word ptr [flathdl+0]
	@DpmiCall

	mov ax, 1
ife ?FLAT
	xor ebx, ebx
	mov gs, ebx
	mov fs, ebx
	xchg ebx,[__flatsel]
	@DpmiCall
endif
	mov ebx,[worksel]
	@DpmiCall
	add bx, 8
	@DpmiCall
	add bx, 8
	@DpmiCall

;	@tprintf <"exit unregister",lf>
	and [fStat], not FSTAT_DEBUGINIT
unregex:
	popad
	ret
_unregister endp

;*** installation code

;*** here SS might be <> DS
;*** so take care if push/pop segment registers
;--- (es:)ebx = cmdline
;--- out: eax=0|1
;---     0=error?
;---     1=ok?

parsecmdline proc stdcall uses edi

;	@tprintf <"enter parsecmdline",lf>
	mov edi,[pKbdBuff]
nextchar:
	mov al, es:[ebx]
	inc ebx
	cmp al, 00
	jz noparm
	cmp al, cr
	jz noparm
	cmp al, '/'
	jz parsecmdline1
	cmp al, '-'
	jz parsecmdline1
	cmp al, '@'
	jz parsecmdline_0
	cmp al,' '
	jz nextchar

	or [fStat], FSTAT_BUFNOTEMPTY
	mov word ptr [edi], " L"
	add edi, 2
@@:
	mov [edi], al
	inc edi
	mov al, es:[ebx]
	inc ebx
	and al, al
	jz @F
	cmp al, cr
	jnz @B
	mov al, 0
@@:
	mov [edi], al
	jmp noparm

;--- input file in cmdline

parsecmdline_0:
	mov dword ptr [edi+0], "MPNI"
	mov dword ptr [edi+4], "=NF"
	add edi, 7
@@:
	mov al, es:[ebx]
	inc ebx
	cmp al, ' '
	jbe @F
	or [fStat], FSTAT_BUFNOTEMPTY
	mov [edi], al
	inc edi
	jmp @B
@@:
	mov dword ptr [edi+0], "PNI;"
	mov dword ptr [edi+4], ";4=M"
	add edi, 8
	dec ebx
	jmp nextchar

processed_option:
	inc ebx
	jmp nextchar

;--- option, '-' or '/' in cmdline

parsecmdline1:
	push processed_option
	mov al, es:[ebx]
	or al, 20h
	cmp al, 'e'			;/E"command"
	jz option_e
	cmp al, 'd'			;/D=Output using Int 41
	jz option_d
	cmp al, 'q'
	jz option_q
	cmp al, 'c'
	jz option_c			;/C=output COMx
	cmp al, '2'
	jz option_2
	cmp al, 'n'
	jz option_n
	cmp al, 's'
	jz option_s
	cmp al, 'f'
	jz option_f
usageout1:
	pop eax
usageout:
	mov [__outmode], _DOSOUT
	@stroutc "Usage: ",?MYNAME," [/2 | /Cn (1",3Ch,"=n",3Ch,"=4) | /D] [/Ecommand][/Q] [/S] [/F]",lf
	xor eax, eax
	jmp exit
option_e:
	or [fStat], FSTAT_BUFNOTEMPTY
	retn
option_d:
	mov ax, _DBGOUT + _DBGINP*100h
	call _SetIOMode
	retn
option_q:
	or [fMode], FMODE_QUIET	 ;/Q=quiet
	retn
option_c:
	mov al, es:[ebx+1]
	cmp al, '1'
	jb usageout1
	cmp al, '4'
	ja usageout1
	inc ebx
	sub al, '0'
	movzx eax, al
	mov _comno, eax
	mov ax, _SEROUT + _SERINP*100h
	call _SetIOMode
	retn
option_2:
	mov ax, _ALTOUT + _ALTINP*100h
	call _SetIOMode
	retn
option_n:
	mov [__outmode], 0
	retn
option_s:
	or [fMode], FMODE_STRICT
	retn
option_f:
	or [f80x87], F80X87_IGNORE
	retn
noparm:
	@mov eax, 1
exit:
	@tprintf <"exit  parsecmdline",lf>
	ret
parsecmdline endp

;-------------------------------------------------

;*** set output mode
;*** AL=output flags

_SetIOMode proc stdcall

	mov [__outmode], al
	mov [__inpmode], ah
	ret
_SetIOMode endp

if ?FLAT
_Enable proc stdcall initstruct:ptr
else
_Enable proc far stdcall initstruct:far ptr
endif
local	rc:dword

	@tprintf <"_Enable: enter",lf>
	pushad
	pushfd
	push ds
	push es
	push fs
	push gs

	mov ds, cs:[__csalias]
	@loadflat

if ?FLAT
	mov ebx, initstruct
else
	les ebx, initstruct
endif
	mov ecx, ss
	mov edx, esp
	push ds
	pop ss
	mov esp,[myesp]
	push ecx
	push edx
ife ?FLAT        
	mov eax, es
	and ax, ax
	jnz @F
	or [fMode], FMODE_QUIET
	jmp enable_1
@@:
endif
	@tprintf <"_Enable: MS 2",lf>
if ?USEHOSTSTACK
	mov eax, dword ptr es:[ebx.DEBUGPARM.pHostStack]
	mov dword ptr hoststack+0, eax
ife ?FLAT        
	mov ax,word ptr es:[ebx.DEBUGPARM.pHostStack+4]
else
	mov eax, ss
endif
	mov word ptr hoststack+4,ax
endif
	@tprintf <"_Enable: parse cmdline",lf>
	@loadflat
if ?FLAT
	mov ebx, es:[ebx.DEBUGPARM.pCmdLine]
else
	les ebx, es:[ebx.DEBUGPARM.pCmdLine]
endif
	invoke parsecmdline
	and eax, eax
	jz @F
enable_1:
ife ?FLAT
	push ds
	pop es
endif

	call Load16bitProcs
	and eax,eax
	jz load16error

	call setmyenvironment
	@tprintf <"_Enable: call firstentry",lf>
	call firstentry
	and eax, eax
	jz load16error

if ?WINDOWS
	jmp @F
else        
	test [fMode], FMODE_QUIET
	jnz @F
endif        
	push eax
	@tprintf <"_Enable: output hello",lf>
	invoke printf, CStr(lf,"%s",lf), addr szDbgDll
	pop eax
	test [fVideo], FVIDEO_ERROR
	jz @F
	xor eax, eax
@@:
	call SwitchToDebuggeeScreen
	call restoreenvironment
load16error:
	pop edx
	pop ss
	mov esp, edx
	mov rc, eax
	pop gs
	pop fs
	pop es
	pop ds
	popfd
	popad
	mov eax, rc
	ret

_Enable endp


;--- assumes ds=flat, but ds=cs is ok

_disable proc stdcall public

	test cs:[fStat], FSTAT_DEBUGINIT
	jz exit
	push ds
	mov ds, cs:[__csalias]
	mov ecx, ss
	mov edx, esp
	cmp ecx, [__csalias]
	jz @F
	push ds
	pop ss
	mov esp, [myesp]
@@:        
	push ecx
	push edx
	push ds
	pop es
	@loadflat
	@tprintf <?MYNAME," will be disabled now",lf>
ife ?WINDOWS
 ife ?LDRDISABLE
	call enableloader
 endif
endif        
	call _unregister

;--- no displays after _unregister
;	@tprintf <?MYNAME," is disabled",lf>

	call SwitchToDebuggeeScreen
	call Free16bitMods
	test [fMode], FMODE_INDEBUG
	jz @F
	call restoreenvironment
@@:
ife ?FLAT
	push 0
	pop es
endif
if ?CS16ALIAS
	mov ebx, [__cs16alias]
	mov ax,1
	int 31h
endif
	pop edx
	pop ecx
	mov ss, ecx
	mov esp,edx
	mov ebx, ds
	pop ds
ife ?FLAT
	cmp ecx, ebx
	jz @F
	mov ax, 1			;free __csalias for non-FLAT
	int 31h
@@:
endif
exit:
	mov eax, cs:[dwLastRC]
	ret
_disable endp

if ?FLAT
Disable proc uses fs gs
else
Disable proc far
endif

	call _disable
	ret
Disable endp

if 0;?WINDOWS

Interpret proc far stdcall public lpCmd:fword

	push ds
	pushad
	mov es, cs:[__csalias]
	mov edi, es:[pKbdBuff]
	lds esi, lpCmd
@@:
	lodsb
	stosb
	and al, al
	jnz @B
	or es:[fStat], FSTAT_BUFNOTEMPTY
	popad
	pop ds
	ret
Interpret endp

endif

;------------------------------------------------------------

	.data?

_heap db ?HEAPSIZE dup (?)

	.code

;--- program entry
;--- actually, since deb16f(w).ovl is loaded "manually" (as overlays),
;--- this isn't the "real" entry, but called.

ife ?FLAT

LibEntry proc uses ds es ebx esi edi

;if 1
ife ?WINDOWS
	mov dx, 3CEh 			; see if in graphics mode
	in al, dx
	mov bl, al
	mov al, 6
	out dx, al
	inc dx
	in al, dx
	xchg bl, al
	dec dx
	out dx, al
	test bl, 1
	jz @F
	mov ax, 0003h
	int 10h
@@:
endif
	mov ebx, cs
	mov ax, 6 				; get base CS
	int 31h
	push cx
	push dx
	pop esi

	mov ax, 000Ah
	int 31h 				; alloc csalias
	jc LibEntry_err
	mov ebx, eax
ife ?32BIT
	lar ecx, eax
	shr ecx, 8
	and ch, not 40h			; reset D bit
	mov ax, 9
	int 31h
endif
	mov ds, ebx
	mov es, ebx
	mov ds:[__csalias], ebx
	mov ds:[MyBase], esi

if ?CS16ALIAS
	mov cx, 1
	xor eax, eax
	int 31h
	jc LibEntry_err
	mov ebx, eax
	mov ds:[__cs16alias], eax
	push esi
	pop dx
	pop cx
	mov ax, 7
	int 31h					; set base for 16-bit csalias
	mov esi, cs
	lsl eax, esi
	push eax
	pop dx
	pop cx
	mov ax, 8
	int 31h					; set limit for 16-bit csalias
	jc LibEntry_err
	lar ecx, esi
	shr ecx, 8
	and ch, 0BFh 			; reset D bit of 16-bit csalias
	mov ax, 9
	int 31h
	jc LibEntry_err
endif
	mov cx, 1
	xor eax, eax
	int 31h					; get flat selector
	jc LibEntry_err
	mov ebx, eax
	mov ds:[__flatsel],eax
	xor ecx, ecx
	xor edx, edx
	mov ax, 6
	int 31h					; set base to 0
	dec ecx
	dec edx
	mov ax, 8
	int 31h					; and limit to -1

	mov ax, 204h			; don't call int 31 pm direct
	mov bl, 31h
	int 31h
	mov dword ptr oldint31, edx
	mov word ptr oldint31+?SEGOFFS, cx
	@mov eax, 1
	jmp exit
LibEntry_err:
	xor eax, eax
exit:
	ret
LibEntry endp

else

	include winnt.inc

LibEntry proc stdcall public uses ebx hInstance:DWORD, reason:DWORD, reserved:DWORD

	.if (reason == DLL_PROCESS_ATTACH)
		mov [__csalias], ds
		mov [__flatsel], ds
if 0
		mov eax, ds
		lar eax, eax
		test ah, 4				;expand down?
		jz @F
		mov ax, 0
		mov cx, 1
		int 31h
		jc @F
		mov __flatsel, eax
		mov ebx, eax
		or ecx, -1
		mov edx, ecx
		mov ax, 8
		int 31h
@@:
endif
		mov ax, 204h			; int 31 pm nicht direkt aufr.
		mov bl, 31h
		int 31h
		mov dword ptr oldint31, edx
		mov word ptr oldint31+?SEGOFFS, cx
		mov ds:[MyBase], 0
	.elseif (reason == DLL_PROCESS_DETACH)
	.endif
	@mov eax, 1
	ret
LibEntry endp

endif

	END LibEntry

