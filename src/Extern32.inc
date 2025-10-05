
externdef __crtadr:dword
externdef __csalias:dword
externdef __cs16alias:dword
externdef __flatsel:dword
externdef __inpmode:byte
externdef __inpstream:dword
externdef __outmode:byte
externdef stdcall __stream:dword
externdef __vidaddr:dword               ;video adresse falls gewuenscht
externdef _comno:dword
externdef bComFlags:byte
externdef _psp:dword					;debugger psp
externdef _environ:dword				;debugger environment
externdef a1:ARGDESC
externdef a2:ARGDESC
externdef a3:ARGDESC
externdef a4:ARGDESC
externdef a5:ARGDESC
externdef a6:ARGDESC
externdef dwCurPsp:dword
externdef bpCount:byte
externdef cLoad:byte
externdef dosvartab:byte
externdef dosvartabend:byte
externdef dpmivartab:byte
externdef dpmivartabend:byte
externdef viovartab:byte
externdef viovartabend:byte
externdef dumparg:qword
externdef fDump:byte
;externdef dwLoadedLibs:dword
externdef dwSysRequest:dword
externdef dwUserSymbols:dword
ife ?WINDOWS
externdef pDbgeeSaveMem:dword
externdef g_pDbgeeSaveGfxBuff:dword
endif
externdef pDbgerSaveMem:dword
externdef wVideoMode:word
externdef dwLoL:dword
externdef wDosVersion:word
externdef dwTmp:dword
externdef exitproc:dword
externdef f80x87:byte
externdef fCPUMode:byte
externdef fEcho:byte
externdef fTrace:byte
externdef fEscapemode:byte
externdef fIrq:byte
externdef fKeybrd:byte
externdef fMode:byte
externdef fVideo:byte
externdef fSkipLF:byte
externdef fStat:byte
externdef fEntry:byte
externdef cLines:byte
externdef fUnass:byte
externdef frsave:byte
externdef hdlNRes:NRNT
externdef hdeb16fwh:dword               ;handle deb16fwh.dll
ife ?WINDOWS
externdef bLdr:BYTE
externdef wLdrFlgs:WORD
externdef wLdrMask:WORD
setloaderstate proto stdcall
 ife ?LDRDISABLE
enableloader proto stdcall
 else
chkloader proto
 endif
endif
if ?LOADVDD
externdef hVDD:dword
endif
externdef pListProcess32:dword
externdef pListModule32:dword
externdef pProcessInfo32:dword
;externdef dwWriteMemory:dword
externdef pWriteDbgConsole:dword
externdef pPeekDbgConsole:dword
externdef pReadDbgConsole:dword
externdef pCloseDbgConsole:dword

externdef inpmodefn:dword
externdef intvartab:byte
externdef intvartabend:byte
;externdef libhandles:dword
externdef meminfstr:byte
ife ?WINDOWS
externdef clVPage:byte
endif
externdef wDCC:word
externdef szNull:byte
if ?32BIT
externdef _FatalExit:PF32
externdef oldint31:PF32
externdef oldint2f:PF32 
externdef oldi09p:PF32
else
externdef _FatalExit:PF16
externdef oldint31:PF16
externdef oldint16:PF16
externdef oldint2f:PF16 
externdef oldi09p:PF16
endif
externdef outmodefn:dword
externdef pKbdBuff:dword
externdef pNearHeap:dword
externdef pSymtab:dword
externdef params:dword
externdef r1r:RMSPREGS
externdef rmint09:dword
externdef savefileexit:fword
externdef tQuestions:byte
externdef tregister:byte
externdef tregs:MYREGSTR
externdef ttycurpos:dword
externdef worksel:dword
externdef rGDTR:fword
externdef rIDTR:fword
externdef rTR:word
externdef rLDT:word
externdef wPicOn:word
externdef wPicValue:word
externdef excexit:dword
externdef gdtitem:byte
externdef idtitem:byte
externdef tritem:byte
externdef ldtitem:byte
externdef MyBase:dword
externdef MySize:dword
externdef heap:byte
externdef fSwap:byte
externdef __RmCS:dword
externdef __RmDS:dword
externdef wWinVersion:word
externdef stdcrt:CRTPARMS
externdef altcrt:CRTPARMS
externdef r1:MYREGSTR
externdef fExit:byte
externdef hMyTask:word
externdef bTranslateSyms:byte
externdef fcb:byte
;externdef graphhlp:DWORD
externdef graphhlpstr:BYTE
if ?HWBREAKS
externdef bHWBrk:BYTE
endif

ife ?WINDOWS
externdef _SaveGraphicScreen		:PF32
externdef _RestoreGraphicScreen		:PF32
externdef _SetDebuggerVideoMode		:PF32
;externdef _SaveDebuggerVideoRegister:PF32
externdef _VideoInit				:PF32
externdef _VideoDone				:PF32
endif

	.code

strcmp   proto stdcall :ptr byte, :ptr byte
strcpy   proto stdcall :ptr byte, :ptr byte
strlen   proto stdcall :ptr byte
vsprintf proto c :ptr byte, :ptr byte, :ptr DWORD
sprintf  proto c :ptr byte, :ptr byte, :VARARG
printf   proto c :ptr byte, :VARARG

;--- parser

GetStringToken      proto near stdcall :ptr byte, :ptr ARGDESC
GetParameter        proto near stdcall :ptr byte, :ptr ARGDESC

;--- unclear

RunBop              proto near stdcall :dword
CopyString2TmpHeap  proto near stdcall :dword
CopyStringInKbdBuff proto near stdcall :dword
GetComChar          proto near stdcall
GetI14Char          proto near stdcall
GetComStatus        proto near stdcall
GetExpression       proto near stdcall :dword
GetString           proto near stdcall :dword,:dword,:dword
GetNResNames        proto near stdcall :dword
;ImportProc          proto near stdcall libname:dword,procnr:dword
MarkDOSused         proto near stdcall
PutBytes2Heap       proto near stdcall :dword,:dword      ;grosser heap ( > 64k )
PutStringInHeap     proto near stdcall :dword
ResetBreaks         proto near stdcall
SetMyVideoEnviron   proto near stdcall
SetTheBreaks        proto near stdcall
VioPutCharDir       proto near stdcall :ptr CRTPARMS, :dword
VioSetCurPosDir     proto near stdcall :ptr CRTPARMS, :dword
VioGetCurPosDir     proto near stdcall :ptr CRTPARMS
_AUXPutChar         proto near stdcall :dword
_AllocDescriptor    proto near stdcall :dword,:dword
_DOSGetChar         proto near stdcall
__strout32          proto near

_doscall            proto near stdcall
_dpmicall           proto near stdcall
_errorout           proto near stdcall error:dword
_errno              proto near c
_reset              proto near stdcall
_spout              proto near stdcall :dword
ActAllHWBreaks      proto near stdcall
ClearAllAutoBreaks	proto near stdcall
allocflat           proto near stdcall :dword
malloc              proto near stdcall nBytes:dword
callproc32          proto near stdcall procaddr:dword,param:dword
callproc32_2        proto near stdcall procaddr:dword,param1:dword,param2:dword
checkclientmemory   proto near stdcall :dword,:dword,:dword
SwitchToDebuggeeScreen proto near stdcall
croutx              proto near stdcall
DeactAllHWBreaks    proto near stdcall
HWBreakHit          proto near stdcall
debug_entry         proto near c
deletedebughandle   proto near stdcall
eaxout              proto near stdcall
freeflat            proto near stdcall :dword
GetCurrentCS        proto near stdcall
getaccr             proto near stdcall
getalternateadapter proto near stdcall
getbaser            proto near stdcall :dword
_getchar            proto near stdcall
getcharex           proto near stdcall
getcr0              proto near c :dword
getcr3              proto near c :dword
getcurdir           proto near stdcall
getdefseg           proto near stdcall :dword
GetInpStatus        proto near stdcall
getlimitr           proto near stdcall
getline             proto near stdcall
getlinearaddr       proto near stdcall
getlinearcseip      proto near stdcall
GetModuleName       proto near pascal dwModule:dword,pModname:dword
getmysymaddr        proto near stdcall
getproc32           proto near stdcall hMod32:dword,procname:dword, :dword
getpsp              proto near stdcall
getptentry          proto near stdcall :dword
hindexout           proto near stdcall bereich:dword
inpmode             proto near c tvalue:dword
insertbrkpnt        proto near stdcall
installirqcom       proto near stdcall
installirqkbd       proto near stdcall
installirqmou       proto near stdcall
kbd_init            proto near
kbd_exit            proto near
kbd_checkpmints     proto near
kbd_setdebuggervecs proto near
kbd_setdebuggeevecs proto near
;myint09             proto far
myint2F             proto near stdcall
kl2gr               proto near stdcall
listhwbreaks        proto near stdcall
LoadState           proto near stdcall
myregsout           proto near stdcall :dword,:dword
mystrcat            proto near c parm1:dword,parm2:dword
outmode             proto near c tvalue:dword
ownerout            proto near stdcall hBlock:dword
preparecmdline      proto near stdcall
preparestring       proto near stdcall :dword
print_descriptor    proto near stdcall sel:dword
;print_gate          proto near stdcall gate:dword,bereich:dword,maxlen:dword
regsout_tss         proto near c               ;aktuelles tss ausgeben
;;resetkbd            proto near stdcall
restoremyscreenbuffer proto near stdcall pBuffer:ptr byte
_ring0              proto near stdcall routine:dword
;savedebughandle     proto near stdcall
SaveState           proto near stdcall
saveviostate        proto near stdcall
ife ?WINDOWS
getswap             proto near c :dword
endif
searchkap           proto near stdcall bereich:dword,searchstr:near ptr byte
SearchUserSymbol    proto near stdcall :dword,:ptr byte
set_reset_idtvec    proto near stdcall index:dword,value:dword
setdebuggeevidstate proto near stdcall
SwitchToDebuggerScreen	proto near stdcall
setptentry          proto near stdcall adresse:dword,newflags:dword,maske:dword
settracevars        proto near stdcall
setworkselbase      proto near stdcall base:dword
setworksellimit     proto near stdcall limit:dword
;;sprintf             proto near c
;;sscanf              proto near c
switch2debuggerstack proto near
switch2hoststack    proto near
symout              proto near stdcall
externdef regsout_2:near
SearchStdSymbol     proto near stdcall :dword,:dword
symtout             proto near
DisplaySymbolLine	proto near stdcall
transform           proto near stdcall :dword
tstkl               proto near stdcall
wrinp               proto near stdcall
allocdpmimem        proto near stdcall
setctrlctrap        proto near stdcall
resetctrlctrap      proto near stdcall
deinstallirqcom     proto near stdcall
ctrlccheck          proto near stdcall
checkifshouldwait   proto near stdcall
mains               proto near stdcall
checkifenhmode      proto near stdcall
getnumber           proto near stdcall
setbiosvars         proto near stdcall :ptr CRTPARMS
GetLastError        proto near stdcall
_fileread           proto near stdcall :dword,:qword,:dword,:dword
_filewrite          proto near stdcall :dword,:qword,:dword,:dword
lockdrive			proto near stdcall drive:dword
unlockdrive			proto near stdcall drive:dword
Load16bitProcs		proto near stdcall
LoadGraphHlp		proto near stdcall
externdef	Load16bitProcsEx:near
Free16bitMods		proto near stdcall
putchar             proto c :DWORD
malloc              proto near stdcall :DWORD
freedll32			proto near stdcall :DWORD
GetNextMcb			proto near stdcall :DWORD
;restoreenvironment  proto near
_disable			proto near stdcall
checkpmints			proto near stdcall
checkrmints			proto near stdcall
GetComSpeed         proto near stdcall :dword
SetComSpeed         proto near stdcall :dword, :dword
getkrnlhandle		proto near stdcall :dword
vdestructor			proto near stdcall
printchars			proto near stdcall :dword, :dword, :dword
_unregister			proto near stdcall
SetScreenSwap		proto near stdcall :dword
if ?WINDOWS
_Movsb				proto near
_Stosb				proto near
endif
if 1
traprm21	proto stdcall
untraprm21	proto stdcall
endif
print_descattr		proto near stdcall
setkeys				proto near stdcall
GetVIF				proto near stdcall
rmbreak				proto near stdcall
_getvesamode		proto near stdcall
VerifyAddress		proto near stdcall :QWORD, :DWORD


;--- command procs, called from main loop only

_allocdos           proto near c pb:PARMBLK
_allocmem           proto near c pb:PARMBLK
_allocmemx          proto near c pb:PARMBLK
_allocsel           proto near c pb:PARMBLK
_allocssel          proto near c pb:PARMBLK
_baudrate           proto near c pb:PARMBLK		;.B
_changedir          proto near c pb:PARMBLK		;.CD
;_changedrive        proto near c pb:PARMBLK	;
_clearbreakpnt      proto near c pb:PARMBLK		;BC
_cls                proto near c pb:PARMBLK		;CLS
_disablebreakpnt    proto near c pb:PARMBLK		;BD
_discardpages       proto near c pb:PARMBLK
_display            proto near c pb:PARMBLK		;??
_dosshell           proto near c pb:PARMBLK		;.SHE
_dpmiinfo           proto near c pb:PARMBLK		;DPMI
_dump               proto near c pb:PARMBLK		;Dump
_dumpdb             proto near c pb:PARMBLK		;DB
_dumpdd             proto near c pb:PARMBLK		;DD
_dumpdt             proto near c pb:PARMBLK		;DA
_dumpdw             proto near c pb:PARMBLK		;DW
_edit               proto near c pb:PARMBLK		;Edit
_editb              proto near c pb:PARMBLK		;EB
_editd              proto near c pb:PARMBLK		;ED
_editw              proto near c pb:PARMBLK		;EW
_enablebreakpnt     proto near c pb:PARMBLK		;BE
_excpm              proto near c pb:PARMBLK		;EPM
_farcall            proto near c pb:PARMBLK
_farcallrm          proto near c pb:PARMBLK
_fexit              proto near c pb:PARMBLK
_fill               proto near c pb:PARMBLK		;Fill
;_floatregs          proto near c pb:PARMBLK
_freedos            proto near c pb:PARMBLK		;FREEDOS
_freelib            proto near c pb:PARMBLK		;.FREE
_freemcb            proto near c pb:PARMBLK		;.MCBFree
_freemem            proto near c pb:PARMBLK		;FREEMem
_freenres           proto near c pb:PARMBLK		;.FS
_freesel            proto near c pb:PARMBLK		;FREESel
_gdi                proto near c pb:PARMBLK		;.GDI
_gdtout             proto near c pb:PARMBLK		;DG, GDT
_getcds             proto near c pb:PARMBLK		;.CDS
_getfpustatus		proto near c pb:PARMBLK		;FPS
_getdosparms        proto near c pb:PARMBLK		;.DOS
_getenvironment     proto near c pb:PARMBLK		;.ENV
_getfiles           proto near c pb:PARMBLK		;.DIR
_getmcb             proto near c pb:PARMBLK		;.MCB
_getowner           proto near c pb:PARMBLK		;.DO
_getpageattr        proto near c pb:PARMBLK
_getpsps            proto near c pb:PARMBLK		;.PSP
_getptentry         proto near c pb:PARMBLK		;PFlags
_getsft             proto near c pb:PARMBLK		;.SFT
_globalheap         proto near c pb:PARMBLK		;.DG
_help               proto near c pb:PARMBLK		;?
_idtout             proto near c pb:PARMBLK		;IDT
_input              proto near c pb:PARMBLK
_inpw               proto near c pb:PARMBLK
_inpd               proto near c pb:PARMBLK
_intcall            proto near c pb:PARMBLK
_intcallrm          proto near c pb:PARMBLK
_interpret          proto near c pb:PARMBLK
_intpm              proto near c pb:PARMBLK		;IPM
_intrm              proto near c pb:PARMBLK		;IRM
_iop                proto near c pb:PARMBLK
_iorestr            proto near c pb:PARMBLK
_kbdstate           proto near c pb:PARMBLK
_killpsp            proto near c pb:PARMBLK		;.Kill
_ldtout             proto near c pb:PARMBLK		;DL, LDT
_listhwbreaks       proto near c pb:PARMBLK		;BRList
_loader             proto near c pb:PARMBLK		;.LDR
_loadnres           proto near c pb:PARMBLK		;.LS
_localheap          proto near c pb:PARMBLK		;.DL
_lockdrive          proto near c pb:PARMBLK
_lockmem            proto near c pb:PARMBLK
_lockmemrm          proto near c pb:PARMBLK
_lockunlockmem      proto near c pb:PARMBLK
_markpage           proto near c pb:PARMBLK
_meminfo            proto near c pb:PARMBLK		;MEM
_moduleout          proto near c pb:PARMBLK		;.DM
_moduleout32        proto near c pb:PARMBLK		;.DM32
_move               proto near c pb:PARMBLK		;Move
_myprintf           proto near c pb:PARMBLK		;PRintf
_namesout           proto near c pb:PARMBLK		;.DN
_outp               proto near c pb:PARMBLK
_outpw              proto near c pb:PARMBLK
_outpd              proto near c pb:PARMBLK
_pagedir            proto near c pb:PARMBLK
_pagetab            proto near c pb:PARMBLK
_physmap            proto near c pb:PARMBLK
_physunmap          proto near c pb:PARMBLK
_processinfo32      proto near c pb:PARMBLK
_processout32       proto near c pb:PARMBLK
_quit               proto near c pb:PARMBLK		;Quit
_quitdef            proto near c pb:PARMBLK		;.Quit
_readsecs           proto near c pb:PARMBLK
_reallocdos         proto near c pb:PARMBLK
_reallocmem         proto near c pb:PARMBLK
_reallocmemx        proto near c pb:PARMBLK
_reboot             proto near c pb:PARMBLK
_rmsel              proto near c pb:PARMBLK
_search             proto near c pb:PARMBLK
_searchb            proto near c pb:PARMBLK
_searchd            proto near c pb:PARMBLK
_searchw            proto near c pb:PARMBLK
_segsmod            proto near c pb:PARMBLK
_setbreakpnt        proto near c pb:PARMBLK
_sethwbreak0        proto near c pb:PARMBLK
_sethwbreak1        proto near c pb:PARMBLK
_sethwbreak2        proto near c pb:PARMBLK
_sethwbreak3        proto near c pb:PARMBLK
_setintpm           proto near c pb:PARMBLK
_setintrm           proto near c pb:PARMBLK
_setpageattr        proto near c pb:PARMBLK
_show               proto near c pb:PARMBLK
_stacktrace         proto near c pb:PARMBLK
_setsymhandler      proto near c pb:PARMBLK		;.SYM
_symbolout          proto near c pb:PARMBLK		;.DS
_tasklist           proto near c pb:PARMBLK		;.DQ
_testproc           proto near c pb:PARMBLK
_tregsout           proto near c pb:PARMBLK
_tssout             proto near c pb:PARMBLK		;TSS
_type               proto near c pb:PARMBLK
_uncommited         proto near c pb:PARMBLK
_unlockdrive        proto near c pb:PARMBLK
_unlockmem          proto near c pb:PARMBLK
_unlockmemrm        proto near c pb:PARMBLK
_ushow              proto near c pb:PARMBLK
_vcd                proto near c pb:PARMBLK		;.VCD
_vcpi               proto near c pb:PARMBLK		;.VCPI
_viostate           proto near c pb:PARMBLK
_view               proto near c pb:PARMBLK		;View
_vpicd              proto near c pb:PARMBLK		;.VPICD
_writesecs          proto near c pb:PARMBLK
_xms                proto near c pb:PARMBLK		;.XMS

