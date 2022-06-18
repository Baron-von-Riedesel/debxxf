
;*** KONSTANTEN ***

ifdef _DEBUG
_TRACE_ 		= 1			; trace on
else
_TRACE_ 		= 0			; trace off
endif
_IDTVECS_		equ 0Fh		; no of supported IDT vectors (Trap)

?HEAPSIZE		equ 4000h	; debugger heap ( beyond 64 kB boundary! )
?RESTRACEFLG	equ 1		; clear TF on entry
?DBSTACK		equ 400h	; size stack
?DBHEAP 		equ 400h	; size 16-bit heap
?KBDBUFSIZ		equ 80		; max line length for getstring()
?SWITCHSTACK	equ 1		; if exc 0C, switch to own stack
if ?32BIT
?CS16ALIAS		equ 0
else
?CS16ALIAS		equ 1		; 16-Bit CS-Alias needed ( 16-bit clients )
endif
?WATCHINT24		equ 0		; catch int 24 (critical error)
?CHECKINDOS		equ 1		; check InDOS flag
?USESYMBOLS		equ 1		; search symbols in unassemble()
;?CHECKINDOSVALID equ 1		; ??? (windows problems?)
?FLOATREGS		equ 1		; save/restore floating point registers
?MMXREGS		equ 1		; support MMX registers
?XMMREGS		equ 1		; support XMM registers
?SYMTRACE		equ 1		; activate STRAC (???)
?CHECKSEGREGS	equ 1		; ??? most likely unnecessary
ife ?WINDOWS
?WATCHI2F		equ 1		; remember+use int 2F on entry
else
?WATCHI2F		equ 0
endif
?VMMINT20		equ 1		; make disassembler know int 20h protected-mode (VMM API)
?MAXLIBS		equ 8		; max. dlls that can be dynamically loaded
?USELOADMODULE	equ 0		; Windows: 1=use LoadModule() instead of int 21,4b00h
?RMCALLBACK		equ 0		; 1=use rm callback instead of raw mode switch for mode change
?EXPLENV		equ 0		; environment when loading program
?USEFLATR0CS	equ 1		; in ring 0 set base of CS to 0
?USEHOSTSTACK	equ 1		; don't use debugger stack when debuggee is loaded
?HSTACKCORR		equ 50h		; size of stack correction if ?USEHOSTSTACK is active
?RESETVIDEOONSAVESTATE = 1	; must be 1
?ALLOWSTI		equ 0		; 0=@cli und @sti are dummy
?ENTRYFROMRMPROT equ 1		; display if entry from real mode
if ?32BIT
?SUPI4112		equ 0
else
?SUPI4112		equ 1		; display int 41h, ax=0012h, ds:si in debugger
endif
?WIN32REBOOT	equ 0		; ??? is a Win9X thing (reboot.vxd) for Int 01
?SUPPBRL		equ 0		; support BRL command (list HW breaks)
?USERMESP		equ 0		; save real-mode ESP, not SP
?HWBREAKS		equ 1		; make usage of hwbreaks optional
?LOADVDD		equ 1 - ?WINDOWS; 1=load helper vdd in NTVDM
ife ?WINDOWS
?LDRDISABLE		equ 1		; 1=.LDR cmd disables loader while in debugger
endif

;--- ?SETEXCVECS now deactivated for Windows. It works, but seems unnecessary
;--- since there aren't that many Windows apps that modify exception vectors 0D/0E
;--- and/or interrupt vectors 08/1C.
;?SETEXCVECS	equ 1		; modify exc 0d/0e (+int 08/1c) while in debugger
?SETEXCVECS	equ 1 - ?WINDOWS; modify exc 0d/0e (+int 08/1c) while in debugger

;--- todo: check if ?SAVEINT08 works flawless
;--- particularly check if it's ok NOT to restore the vectors (see dpmixxfd.asm)
?SAVEINT08	equ 1			; modifies int 08+1C; active only if ?SETEXCVECS is also 1

?STOPONEXIT	equ 0			;windows: activate debugger if debuggee has terminated

ife ?WINDOWS
?SAVECURDIR	equ 1	;save/restore current directory
else
?SAVECURDIR	equ 0	;save/restore current directory
endif

if ?32BIT
?SUPPDOS4GW	equ 1	;support DOS4/GW program execution (int 21h, ax=FF01)
else
?SUPPDOS4GW	equ 0
endif

;--- ?HIDEINTXX: the idea was/is to prevent the debuggee from
;--- changing certain interrupt vectors. For this, int 21h ah=25h/35h
;--- and int 31h ax=204h/205h are trapped and the vectors to read/write
;--- are just read from/saved to debugger variables. This implies that
;--- the int vectors themselves have to be trapped and the vectors set
;--- by the debuggee be called while the debuggee is active.

if ?WINDOWS
?HIDEINT01	equ 0
?HIDEINT09	equ 0
?HIDEINT21	equ 0
else
?HIDEINT01	equ 1
?HIDEINT09	equ 1	;ensures that debxxf can detect sysreq while the debuggee is running
?HIDEINT21	equ 0	;todo: explain what 1 is supposed to achieve!
endif

if ?HIDEINT01 or ?HIDEINT09 or ?HIDEINT21
?WATCHI21	equ 1	;watch int 21h, ax=25xx/35xx
?WATCHI31	equ 1	;watch int 31h, ax=0202/0203 and ax=0204/0205
else
?WATCHI21	equ 0
?WATCHI31	equ 0
endif

if ?WINDOWS
?TRAPRM214B		equ 0
?TRAPRM2F		equ 0
?TRAPRM2A		equ 0
?TRAPRM15		equ 0
?READINIPARMS	equ 1
?USETOOLHELP	equ 1
?SETERRORMODE	equ 1
?WATCHINT23		equ 0
 if ?USETOOLHELP
?ETRAPS 		equ 00000000h	; which exceptions are handled by the debugger
?ESTOPS 		equ 00000000h	; on which exceptions will the debugger stop
?EFIRST 		equ 00000000h	; which exceptions are handled first
 else
?ETRAPS 		equ 0000744bh
?ESTOPS 		equ 0000744bh
?EFIRST 		equ 00000000h
 endif
;WM_KEYDOWN		equ 100h
WM_CLOSE		equ 10h
MYMAC			equ '+'
IGNMAC			equ '-'
?USEINT41		equ 0		; setting for Windows, std=0

else

?TRAPRM214B		equ 1		; requires generation of RMDBGHLP
?TRAPRM214C		equ 0		; dito
?TRAPRM2F		equ 0		; dito
?TRAPRM2A		equ 0		; dito
?TRAPRM15		equ 0		; dito
?READINIPARMS	equ 1
?USETOOLHELP	equ 0
?SETERRORMODE	equ 0
?WATCHINT23 	equ 1		; catch int 23 (ctrl-c)
;---                EDC-A---6--3-10
?ETRAPS 		equ 111010001001011b	; default for DOS versions: 0, 1, 3, 6, 0A, 0C, 0D, 0E
?ESTOPS 		equ 111010001001011b
?EFIRST 		equ 000000000001010b	; exceptions 1 + 3
MYMAC			equ '-'
IGNMAC			equ '+'
?USEINT41		equ 1			; setting for non-windows, std=1

endif

bPromptPM		equ '#'		; prompt PM
bPromptRM		equ '>'		; prompt RM
bPromptV86		equ '-'		; prompt V86

XON 	equ 11h			; Ctrl-Q
XOFF	equ 13h			; Ctrl-S

