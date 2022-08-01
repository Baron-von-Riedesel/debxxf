
;--- there are a few procs called in 16-bit dlls.
;--- if the stack is 32-bit ( hiword esp != 0 ), then
;--- the dpmildr api ax=4B87h is used for the call.

?CHECKPAGE equ 1

	.386
	.387
if ?FLAT
	.MODEL FLAT
else
	.MODEL TINY
endif
	option casemap:none
	option proc:private

	include const.inc
	include ascii.inc
	include function.inc
	include debxxfd.inc
	include dos.inc
	include dpmi.inc
	include putf.inc
	include errors.inc

MAX_PATH    = 260			;is 260 for win32

if ?32BIT or ?WINDOWS
?INKERNEL equ 1        
	include tlhelp32.inc
endif
ife ?FLAT
	include toolhelp.inc
endif
	include fcntl.inc
if ?FLAT or ?WINDOWS
	include winnt.inc
endif
;------------------------------------------------------------------------

	include extern32.inc
	include extern16.inc

	.data

hdlnresptr dd 0

savefileexit   df 0

dumptyp   db __BYTE__	  ;aktueller Typ fuer 'Display' (Text,Byte,Dword)
dumplines dd 10h		  ;anzahl zeilen fuer 'Display'
dumpbytes dd 10h		  ;anzahl bytes/zeile

if ?WINDOWS
hTaskView dd 0
endif

if ?USESYMBOLS
bTranslateSyms	db 0
endif

	.const

tCode		 db "code ",00
tData		 db "data ",00
tLoadOnCall  db "loadoncall ",00
tPreLoad	 db "preload ",00
tFixed		 db "fixed ",00
tMoveable	 db "moveable ",00
tDiscardable db "discardable ",00
tMixed1632   db "mixed1632 ",00
tQuestions	 db "???",0

segflgtab label word
	dw 0001h
	dd tData
	dd tCode
	dw 0040h
	dd tPreLoad
	dd tLoadOnCall
	dw 0010h
	dd tMoveable
	dd tFixed
	dw 1000h
	dd tDiscardable
	dd 0
	dw 2000h
	dd tMixed1632
	dd 0
	dw 0
        
	.code

if 0

;--- load a procedure from a dll

ImportProc proc stdcall libname:dword,procnr:dword

local	libhandle:dword
local	procaddr:dword

if ?WINDOWS
	@savewinsegregs
	mov eax,ds
	push ax
	mov eax,libname
	push ax
	call _LoadLibrary
	@restorewinsegregs
else
	xor ebx, ebx
	mov edx, libname
	mov ax, 4b00h
	@DosCall
endif
	movzx eax,ax
	cmp eax,20h
	jb ImportProc_er1
	mov libhandle,eax
if ?WINDOWS
	@savewinsegregs
	push ax
	push procnr
	call _GetProcAddress
	@restorewinsegregs
else
	mov ebx, eax
	mov edx, procnr
	mov cl, 1
	mov ax, 4B85h
	@DosCall
endif
	mov ecx, edx
	or cx, ax
	jz ImportProc_er2
	mov word ptr procaddr+0, ax
	mov word ptr procaddr+2, dx
	mov ebx, offset libhandles
	mov eax, libhandle
	mov ecx, [dwLoadedLibs]
	jecxz importproc_2
@@:
	cmp eax, [ebx]
	jz importproc_1
	add ebx, 4
	loop @B
importproc_2:
	mov [ebx], eax
	inc dword ptr [dwLoadedLibs]
	mov eax, procaddr
	jmp ImportProc_ex
importproc_1:
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
	mov eax,procaddr
	jmp ImportProc_ex
ImportProc_er2:
	@errorout ERR_CANNOT_FIND_EXPORT
if ?WINDOWS
	@savewinsegregs
	mov eax,libhandle
	push ax
	call _FreeLibrary
	@restorewinsegregs
else
	mov edx, libhandle
	movzx edx, dx
	mov ax, 4b80h
	@DosCall
endif
	xor eax, eax
	jmp ImportProc_ex
ImportProc_er1:
	@errorout ERR_CANNOT_FIND_LIBRARY
	xor eax,eax
ImportProc_ex:
	ret
ImportProc endp

endif

if ?FLAT
    .data
endif

if ?WINDOWS

;--- module KERNEL

kernelprocs label byte
	db 2
_ExitKernel PF16 0
	db 36
_GetCurrentTask PF16 0
	db 37
_GetCurrentPDB PF16 0
	db 47
_GetModuleHandle PF16 0
	db 131
_GetDOSEnvironment PF16 0
	db 184
_GlobalDosAlloc PF16 0
	db 185
_GlobalDosFree PF16 0
	db 65h
_NoHookDosCall PF16 0
;	db 30
;_WaitEvent PF16 0
;	db 31
;_PostEvent PF16 0
	db 107
_SetErrorMode PF16 0
	db 9Bh
_GetTaskDS PF16 0
	db 128
_GetPrivateProfileString PF16 0
	db 129
_WritePrivateProfileString PF16 0
	db 133	; GETEXEPTR(WORD)
_GetExePtr PF16 0
	db 0

userprocs label byte
;	db 1h
;_MessageBox PF16 0
	db 4h
_DisableOemLayer PF16 0
	db 74h
_PostAppMessage PF16 0
	db 0

toolhelpprocs label byte
	db 52h
_TaskGetCSIP PF16 0
	db 36h
_GlobalEntryHandle PF16 0
	db 33h
_GlobalFirst PF16 0
	db 34h
_GlobalNext PF16 0
	db 39h
_LocalFirst PF16 0
	db 3Ah
_LocalNext PF16 0
	db 49h
_NotifyRegister PF16 0
	db 4ah
_NotifyUnregister PF16 0
	db 4bh
_InterruptRegister PF16 0
	db 4ch
_InterruptUnregister PF16 0
	db 53h
_TaskSwitch PF16 0
	db 0

mouseprocs label byte
	db 4
_MouseGetIntVect PF16 0
	db 0

deb16fwvprocs label byte
	db 1
_SwitchToDebuggerScreen	PF16 0
	db 2
_SwitchToWindowsScreen	PF16 0
	db 3
_VideoInit				PF16 0
	db 4
_VideoDone				PF16 0
	db 0

_LoadLibrary       PF16 0
_FreeLibrary       PF16 0
_GetProcAddress    PF16 0
_OutputDebugString PF16 0
_ToolhelpHook      PF16 0
_ReadAccProc       PF16 0
_PeekMessageProc   PF16 0

else ;?WINDOWS

graphhlpprocs label byte
	db 1
_VideoInit label PF32
	dw -1, -1, 0
	db 2
_VideoDone label PF32
	dw -1, -1, 0
	db 3
_SaveGraphicScreen label PF32
	dw -1, -1, 0
	db 4
_RestoreGraphicScreen label PF32
	dw -1, -1, 0
	db 5
_SetDebuggerVideoMode label PF32
	dw -1, -1, 0
;	db 6
;_SaveDebuggerVideoRegister label PF32
;	dw -1, -1, 0
	db 0

endif    

;--- module SYMBHDLD/SYMBHDLW

symbhdlrprocs label byte
	db	2	; GETSYMBOLNAME proto far pascal a1:dword,a2:dword, a3:word
_GetSymbolName	PF16 0
	db	3	; GETSYMBOLADDR proto far pascal x1:FAR16,x2:word,x3:word
_GetSymbolAddr	PF16 0
	db	0

showfileprocs label byte
	db 2
_ShowTextFile	PF16 0
	db 3
_ShowMemory		PF16 0
	db 4
_SetIoParms		PF16 0
	db 0


CCONST segment

if ?WINDOWS
krnlstr		db "KERNEL",0
symbhdlrstr db "SYMBHDLW.DLL",0
toolhelpstr db "TOOLHELP.DLL",0
userstr		db "USER",0
;;keyboardstr	db "KEYBOARD",0
mousestr	db "MOUSE",0
deb16fwvstr	db "DEB16FWV.DLL",0
else
symbhdlrstr db "SYMBHDLD.DLL",0
endif
showfilestr	db "SHOWFILE.DLL",0


CCONST ends

	.data

ife ?WINDOWS
graphhlpstr  label byte
	db "DEBFVESA.DLL",0,0
	db 64 - ($ - graphhlpstr) dup (?)   ; ??? what is this good for?
endif

;--- dynamically loaded 16-bit modules

MODDESC struct
pProcs dd ?
pName  dd ?
dwHdl  dd ?
MODDESC ends

mods label dword
if ?WINDOWS
	MODDESC <offset kernelprocs,   offset krnlstr,     0>
	MODDESC <offset toolhelpprocs, offset toolhelpstr, 0>
	MODDESC <offset userprocs,     offset userstr,     0>
	MODDESC <offset mouseprocs,    offset mousestr,    0>
endif
NUM16BITDLLS equ ($ - offset mods) / sizeof MODDESC

sfmod    MODDESC <offset showfileprocs, offset showfilestr, 0>
shmod    MODDESC <offset symbhdlrprocs, offset symbhdlrstr, 0>
ife ?WINDOWS
graphhlp MODDESC <offset graphhlpprocs, offset graphhlpstr, 0>
else
graphhlp MODDESC <offset deb16fwvprocs, offset deb16fwvstr, 0>
endif

TOTAL16BITDLLS equ ($ - offset mods) / sizeof MODDESC

	.code

LoadGraphHlp proc stdcall public
	@mov ecx,1
	mov esi, offset graphhlp
	jmp Load16bitProcsEx
LoadGraphHlp endp

if 1;?WINDOWS

Load16bitProcs proc stdcall public

	@tprintf <"Load16bitProcs start",10>
	mov ecx, NUM16BITDLLS
	mov esi, offset mods
Load16bitProcsEx::
	@mov edi, 1
	.while ecx
		push ecx
		mov edx, [esi].MODDESC.pProcs	; procs array
		mov eax, [esi].MODDESC.pName	; module name
		push esi
		push edx
		push eax
if ?WINDOWS
		@savewinsegregs
		mov edx,ds
		push dx
		push ax
		call _LoadLibrary
		@restorewinsegregs
else
		xor ebx, ebx
		mov edx, eax
		mov ax, 4b00h
		int 21h			;use int 21h here
endif
		pop ecx
		pop esi
		movzx eax, ax
		cmp eax, 20h
		jnc @F
ife ?WINDOWS
		sub esp, 64
		@loadesp  edx
		invoke sprintf, edx, CStr("fatal error: cannot load %s",13,10,"$"), ecx
		@loadesp edx
		mov ah, 9
		int 21h			;use int 21h here
		add esp, 64
else
		@tprintf <"Load16bitProcs: cannot load %s",10>, ecx
endif
		xor edi, edi
		xor ebx, ebx
		jmp error1
@@:
		mov ebx, eax
		.while (byte ptr [esi])
			lodsb
			movzx eax, al
if ?WINDOWS
			push ebx
			@savewinsegregs
			push bx
			push eax
			call _GetProcAddress
			@restorewinsegregs
			pop ebx
else
			mov edx, eax
			mov cl, 1
			mov ax, 4B85h	;this API exists for 16 bit as well!
			int 21h			;use int 21h here
endif
			cmp byte ptr [esi], 0
			jz @F
			movzx eax, ax
			mov [esi], eax
			mov [esi+4], dx
			add esi, 6
			.continue
@@: 		   
			push dx
			push ax
			pop eax
			and eax, eax
			jnz @F
			movzx eax, byte ptr [esi-1]
			invoke printf, CStr("Load16bitProcs: cannot load proc# %X",10), eax
			xor edi, edi
			mov eax, edi
@@:
			mov [esi], eax
			add esi, 4
		.endw
error1:
		pop esi
		mov [esi].MODDESC.dwHdl, ebx
		add esi, sizeof MODDESC
		pop ecx
		dec ecx
	.endw
	@tprintf <"Load16bitProcs exit",10>
	mov eax, edi
	ret
Load16bitProcs endp

endif

Free16bitMods proc stdcall public
	mov esi, offset mods
	mov ecx, TOTAL16BITDLLS
	.while ecx
		push ecx
		mov eax, [esi].MODDESC.dwHdl
		and eax, eax
		jz @F
if ?WINDOWS
		@savewinsegregs
		push ax
		call _FreeLibrary
		@restorewinsegregs
else
		mov edx, eax
		mov ax,4b80h
		@DosCall
endif
@@:
		pop ecx
		add esi,sizeof MODDESC
		dec ecx
	.endw
	ret
Free16bitMods endp

_type proc c public pb:PARMBLK

	call MarkDOSused

	cmp _ShowTextFile, 0
	jnz @F
	@mov ecx, 1
	mov esi, offset sfmod
	call Load16bitProcsEx
	and eax, eax
	jz type_ex
@@:
if ?WINDOWS
	test [fStat], FSTAT_ISNT
	jz @F
	test [__outmode], _VIOOUT
	jnz type_ex
@@:
endif
	test [__outmode], _VIOOUT or _ALTOUT
	jnz @F
	@errorout ERR_VIODIROUT_NOT_ACTIVE
	jmp type_ex
@@:
	call _crout			; ensure that debugger output is active!
	call GetInpStatus	; ensure that debugger input is active

	mov esi, offset stdcrt
	test [__outmode], _ALTOUT
	jz @F
	mov esi, offset altcrt
@@:
	push word ptr [esi].CRTPARMS.crtadr
	mov eax,  [esi].CRTPARMS.pBuffer
	and ax, 7FFFh
	push ax
	movzx eax, [esi].CRTPARMS.cols
	push ax
	movzx eax, [esi].CRTPARMS.rows
	push ax

;--- modify the showfile.dll input routine
	mov edx, cs
	mov eax, offset getcharexx
	push dx
	push eax

if ?32BIT
	mov cx, 7
	mov ebx, esp
	mov edx, _SetIoParms
	mov ax, 4b87h	; call SetIoParms
	@DosCall
	add esp, 7*2
else
	call _SetIoParms
endif
	mov cl, pb.p1.bType
	cmp cl, __STRING__				   ;filename angegeben?
	jz type_3
	mov eax, pb.wSeg1
	and eax, eax
	jnz @F
	mov eax, [r1.rDS]				   ;memory view
@@:
	mov ecx, pb.dwOffs2
	cmp pb.p2.bType, __CONST__
	jz @F
	lsl ecx, eax
	jnz type_2						;kein gltiger selektor
	sub ecx, pb.dwOffs1
	cmp ecx, -1
	jz @F
	inc ecx
	lar edx, eax
	test dh, 4
	jz @F
	lsl ecx, eax
	inc ecx
	mov pb.dwOffs1, ecx
	neg ecx
@@:
if 1
	push ax							;selektor
	push ecx 						;laenge
	push pb.dwOffs1					;start offset
	push word ptr  [savefileexit+4]
	push dword ptr [savefileexit+0]
 if ?32BIT
	mov cx, 8
	mov ebx, esp
	mov edx, _ShowMemory
	mov ax, 4b87h	; call ShowMemory
	@DosCall
	add esp,8*2
 else
	call _ShowMemory
 endif
else
	invoke printf, CStr("eax=%x,ecx=%x,parm1=%x",lf),eax,ecx,pb.dwOffs1
endif
	jmp type_2
type_3: 								;modus "file view"
	mov eax,ds
	push ax
	push pb.dwOffs1
	xor eax,eax
	push eax
if ?32BIT
	mov cx, 5
	mov ebx, esp
	mov edx, _ShowTextFile
	mov ax, 4b87h	; call ShowTextFile
	@DosCall
	add esp,5*2
else
	call _ShowTextFile
endif
	and ax, ax
	jz type_2
	push [a1.dwPx]
	movzx eax,ax
	add eax, ERR_FILE_NOT_FOUND - 1
	@errorout eax
	add esp, 4
type_2:
type_ex:
	ret

getcharexx:
	push ds
	mov ds, cs:[__csalias]
	call getcharex
	pop ds
	retf

_type endp

;*** SH cmd: display internal variables

	@cmdproc

_show proc c public pb:PARMBLK
	cmp cl,__VOID__
	jz @F
	mov ch,0
	mov esi,[a1.dwPtr]
	call DisplaySymbolLine
	ret
@@:
	mov ebx, offset intvartab
	mov eax, offset intvartabend
	mov ch, 0
	call symtout
	call _kbdstate
	ret
_show endp

;--- display tmp registers

_tregsout proc c public pb:PARMBLK

	mov ebx, offset tregister
	mov eax, -1
	mov ch, 0
	call symtout
	ret
_tregsout endp

;*** DYN cmd

_ushow proc c public pb:PARMBLK

	mov esi,dwUserSymbols
	xor ecx,ecx
uymto2:
	and esi,esi
	jz uymtoex
	push ecx
	add esi,4
	mov ebx,[esi.SYMBOL.pText]
	invoke __stroutebx		; modifies ebx, eax
	test byte ptr [esi.SYMBOL.bType],_RDONLY_
	jz @F
	@stroutc " (r/o)"
@@:
	@stroutc ": "
	mov al,[esi.SYMBOL.bType]
	and al,1fh			   ;_RDONLY_ l”schen
	mov ebx,[esi.SYMBOL.dwProc]  ;proc holen
	mov cl,al
	call symout
	invoke _crout
uymto4:
	mov esi,[esi-4]
	pop ecx
	inc ecx
	jmp uymto2
uymtoex:
	and ecx,ecx
	jnz @F
	@errorout MSG_NO_USERVARS_DEFINED
@@:
	ret
_ushow endp

	@cmdprocend

if ?WINDOWS

;--- .GDI command

	@cmdproc
        
_gdi proc c public pb:PARMBLK

	test [fVideo], FVIDEO_ISDBGER
	jnz  @F
	call SwitchToDebuggerScreen		; .GDI cmd
	mov eax, INF_GDI_DISABLED
	jmp exit
@@:
	call SwitchToDebuggeeScreen
	mov eax, INF_GDI_ENABLED
exit:
	@errorout eax 
	ret
_gdi endp

	@cmdprocend

endif

;--- View command

_view proc c public pb:PARMBLK

local mykey:dword
local mymsg[32]:byte

	test [fVideo], FVIDEO_ISDBGER
	jz exit
	call SwitchToDebuggeeScreen
if ?WINDOWS
@@:
	call _PeekMessageProc
	and ax, ax
	jz @B
else
;	lea eax, mykey
;	invoke	GetTextMessage, 0, eax
	call _getchar
endif
	call SwitchToDebuggerScreen		; VIEW cmd
exit:
	ret
_view endp

if ?SUPPLHEAP

getlflags proc stdcall uses edi

	mov  byte ptr [edi],0
	shr  esi,1
	jnc  @F
	invoke strcpy, edi, CStr("fixed ")
	add edi, 6
@@:
	shr  esi,1
	jnc  @F
	invoke strcpy, edi, CStr("free ")
	add edi, 5
@@:
	shr  esi,1
	jnc  @F
	invoke strcpy, edi, CStr("moveable ")
	add edi, 9
@@:
	ret
getlflags endp

	.data
lhtypes label dword
	dd CStr("normal")
	dd CStr("pen")
	dd CStr("brush")
	dd CStr("font")
	dd CStr("palette")
	dd CStr("bitmap")
	dd CStr("rgn")
	dd CStr("dc")
	dd CStr("disabled dc")
	dd CStr("metadc")
	dd CStr("metafile")
        
	.code

getltype proc stdcall

	cmp  esi, 10
	ja @F
	mov  esi,[esi*4+offset lhtypes]
	invoke strcpy, ebx, esi
	ret
@@:
	invoke sprintf, ebx, CStr("%u"), esi
	ret
getltype endp

ife ?WINDOWS

LocalNext proc stdcall uses fs esi ebx plh:ptr LOCALENTRY

local	rc:dword

	mov rc, 0
	mov esi, plh
	mov bx, [esi.LOCALENTRY.le_wNext]
	and bx, bx
	jz  sm1
	mov ax, [esi.LOCALENTRY.le_hHeap]
	lsl ecx, eax
	jnz sm1
	cmp bx, cx
	jnc sm1
	mov rc, 1
	mov fs, ax
	mov [esi.LOCALENTRY.le_hHandle], bx
	mov [esi.LOCALENTRY.le_wAddress], bx
	mov ax, fs:[bx-2]
	and ax, 1
	shl ax, 1
	or  al, 1 				; immer fixed
	mov [esi.LOCALENTRY.le_wFlags], ax
	test al, 2
	jz  @F
	mov [esi.LOCALENTRY.le_hHandle], 0
@@:
	mov ax, fs:[bx-2]
	and al, 0FEh
	sub ax, bx
	mov [esi.LOCALENTRY.le_wSize], ax
	mov [esi.LOCALENTRY.le_wNext], 0
	mov ax, fs:[bx-2]
	and al, 0FEh
	cmp ax, bx
	jna sm1
	add ax, 2
	mov [esi.LOCALENTRY.le_wNext], ax
sm1:
	mov eax, rc
	ret
LocalNext endp

LocalFirst proc stdcall uses ebx hHeap:dword, plh:ptr LOCALENTRY

	mov eax, hHeap
	lar ebx, eax
	jz @F
	xor eax, eax
	jmp exit
@@:
	mov ebx,plh
	mov [ebx.LOCALENTRY.le_hHeap], ax
	push ds
	mov ds, eax
	mov ax, ds:[0006]
	pop ds
	mov [ebx.LOCALENTRY.le_wNext], ax
	invoke LocalNext, ebx
exit:
	ret
LocalFirst endp

endif

;--- .DL command

_localheap proc c public pb:PARMBLK

local	lcle:LOCALENTRY
local	szFlags[64]:byte
local	szType[16]:byte
local	dwItems:dword

	mov dwItems, 0
	mov eax, pb.dwOffs1
	cmp cl, __VOID__
	jnz @F
	mov eax, [r1.rDS]
@@:
	verr ax
	jnz exit
	mov fs, eax
	mov bx, fs:[0]
	cmp bx, "EN"
	jnz @F
	mov bx, fs:[0008]
	mov ax, fs:[bx+8]
	verr ax
	jnz exit
@@:
	mov  lcle.le_dwSize, sizeof LOCALENTRY
if ?WINDOWS
	@savewinsegregs
	mov ecx, ss
	push cx
	lea ecx, lcle
	push cx
	push ax
	call _LocalFirst
	@restorewinsegregs
	movzx eax, ax
else
	lea ecx, lcle
	invoke LocalFirst, eax, ecx
endif        
nextitem:
	and eax, eax
	jz exit
	cmp dwItems, 0
	jnz @F
	invoke printf, CStr("Hndl Addr Size Lock Type        Flags",lf)
	add eax, 12
	invoke printchars, '-',eax, 1
@@:
	inc dwItems
	mov si, lcle.le_wFlags
	lea edi, szFlags
	call getlflags
	mov si, lcle.le_wType
	lea ebx, szType
	call getltype
	movzx eax, lcle.le_hHandle
	movzx ecx, lcle.le_wAddress
	movzx edx, lcle.le_wSize
	movzx esi, lcle.le_wcLock
	invoke printf, CStr("%4X %4X %4X %4X %-11s %s",lf), eax, ecx, edx, esi, ebx, edi
if ?WINDOWS
	@savewinsegregs
	mov eax, ss
	push ax
	lea eax, lcle
	push ax
	call _LocalNext
	@restorewinsegregs
	movzx eax, ax
else
	invoke LocalNext, addr lcle
endif        
	jmp nextitem
exit:
	mov eax, dwItems
	and eax, eax
	jnz @F
	@errorout ERR_NO_LOCAL_HEAP
@@:
	ret
_localheap endp

endif

printchars proc stdcall public char:dword, dwSize:dword, bWithLF:dword

	pushad
	mov ecx, dwSize
@@:
	push ecx
	mov al,byte ptr char
	@putchr al
	pop ecx
	loop @B
	test byte ptr bWithLF,1
	jz @F
	invoke _crout
@@:
	popad
	ret
printchars endp

if ?USETOOLHELP

;--- .dg cmd  argument1: restrict to one entry, argument2: restrict to one task/module

_globalheap proc c public pb:PARMBLK

local	glblentry:GLOBALENTRY
local	modname[9]:byte
local	total:dword

	xor   eax,eax
	mov   total,eax
	invoke printf, CStr("Hndl     Addr     Size Type Data Flgs Lock PgLk Owner",lf)
	invoke printchars, '-', eax, 1
	mov   ebx, ss
	push  bx
	lea   ebx, glblentry
	mov   eax, sizeof GLOBALENTRY
	mov   glblentry.ge_dwSize, eax
	push  bx
	cmp   pb.p1.bType,__VOID__
	jz @F
	mov   ebx,pb.dwOffs1
	push  bx
	call  _GlobalEntryHandle
	jmp   globalheap_1
@@:
	push  word ptr GLOBAL_ALL
	call  _GlobalFirst
globalheap_1:
	and   ax,ax
	jz globalheap_ex
	push  ds
	pop   es
	movzx eax, glblentry.ge_hBlock
	mov   ecx, glblentry.ge_dwAddress
	mov   edx, glblentry.ge_dwBlockSize
	movzx edi, glblentry.ge_wType
	movzx esi, glblentry.ge_wData
	movzx ebx, glblentry.ge_hOwner

	cmp pb.p2.bType, __VOID__
	jz @F
	cmp ebx, pb.dwOffs2
	jnz globalheap_2
@@:
	add total, edx
	invoke printf, CStr("%4X %8X %8X %4X %4X "), eax, ecx, edx, edi, esi
	movzx eax, glblentry.ge_wFlags
	movzx ecx, glblentry.ge_wcLock
	movzx edx, glblentry.ge_wcPageLock
	movzx edi, glblentry.ge_hOwner
	invoke printf, CStr("%4X %4X %4X %4X "), eax, ecx, edx, edi
	movzx ecx, glblentry.ge_hOwner
	invoke GetModuleName, ecx, addr modname
	lea ebx, modname
	invoke __stroutebx		; modifies ebx, eax
	invoke _crout
globalheap_2:
	cmp pb.p1.bType,__VOID__
	jnz globalheap_exx
	mov eax, ss
	push ax
	lea  eax, glblentry
	push ax
	push word ptr GLOBAL_ALL
	call _GlobalNext
	jmp  globalheap_1
globalheap_ex:
	push total
	@errorout MSG_TOTAL_SIZE
globalheap_exx:
	ret
_globalheap endp
endif

if ?USESYMBOLS

getnxthdlnres proc stdcall

	mov esi, offset hdlNRes
	push eax
	mov eax, hdlnresptr
	add esi, eax
	add eax, sizeof NRNT
	and eax, NUMNRITEMS * sizeof NRNT - 1
	mov hdlnresptr, eax
	pop eax
	ret
getnxthdlnres endp

;--- .LS command

_loadnres proc c public pb:PARMBLK

local	xmodname[12]:byte

	push esi
	cmp  cl, __VOID__
	jnz  loadnres_3
	mov  esi, offset hdlNRes 		 ;nur geladene tabellen ausgeben
	mov  ecx, NUMNRITEMS
nextitem:
	push ecx
	mov  edx, [esi.NRNT.NRNTMOD]
	and  edx, edx
	jz skipitem
	invoke GetModuleName, edx, addr xmodname
	mov  eax, [esi.NRNT.NRNTMOD]
	lea  ebx, xmodname
	invoke printf, CStr("%4X %s",lf), eax, ebx
skipitem:
	add  esi, sizeof NRNT
	pop  ecx
	loop nextitem
	jmp  exit

loadnres_3: 							 ;neue tabelle laden
	invoke GetNResNames,pb.dwOffs1	 ;das ist lokale funktion!
	and eax, eax
	jnz @F
	@errorout ERR_INVALID_NRES_LOAD
	jmp exit
@@:
	movzx eax, ax
	push eax
	@errorout MSG_NONRESIDENT_NAMES_LOADED
	pop eax
	call getnxthdlnres
	xchg eax, [esi.NRNT.NRNTHDL]	 ;handle abspeichern
	and eax, eax
	jz @F
	push es
	mov es, eax
	mov ah, 49h
	@DosCall
	pop es
@@:
	mov eax,pb.dwOffs1
	mov [esi.NRNT.NRNTMOD], eax
exit:
	pop esi
	ret
_loadnres endp

_freenres proc c public pb:PARMBLK 			  ;funktion .FS

	mov  ecx,NUMNRITEMS
	mov  ebx,offset hdlNRes
	mov  eax,pb.dwOffs1
freenres_1:
	cmp  eax,[ebx.NRNT.NRNTMOD]
	jnz  @F
	xor  eax,eax
	mov  [ebx.NRNT.NRNTMOD],eax
	xchg eax,[ebx.NRNT.NRNTHDL]
	push es
	mov  es, eax
	mov  ah, 49h
	@DosCall
	pop  es
	jmp  exit
@@:
	add  ebx, sizeof NRNT
	loop freenres_1
exit:
	push ds
	pop  es
	ret
_freenres endp

endif

;--- get kernel module handle

getkrnlhandle proc stdcall public bDebuggerView:DWORD
if ?WINDOWS
	@savewinsegregs
	mov eax, cs
;	movzx eax, ax		; ???
	push eax
	call _GetModuleHandle
	@restorewinsegregs
else
 if ?LDRDISABLE
	call chkloader
	jc error
 endif
	mov edx, cs
;	 movzx edx, dx
	mov cl, 1			; edx contains a selector
	mov ax, 4b88h
	.if (bDebuggerView)
		@DosCall
	.else
		int 21h      
	.endif
;--- returning with C set just means that the handle isn't a module -
;--- DX is still set by the loader ( if it is active ).
endif
	movzx eax, dx
	clc
error:
	ret

getkrnlhandle endp

GetModule16Filename proc stdcall uses fs hModule:dword, pDest:dword, nMax:dword        
	mov fs, hModule
	movzx esi, word ptr fs:[000Ah]
	lea esi, [esi+8]
	cmp byte ptr fs:[esi+1],':'	;???
	jz @F
	inc esi
@@:
	mov edi, pDest
	mov ecx, nMax
@@:
	lodsb fs:[esi]
	stosb
	and al,al
	loopnz @B
	ret
GetModule16Filename endp        

;--- .DM command

_moduleout proc c public pb:PARMBLK

local fname[68]:byte
local modname[12]:byte

	push esi
	push edi

	mov eax, pb.p1.dwOffs
	cmp pb.p1.bType, __VOID__
	jnz @F
	invoke  getkrnlhandle, pb.p3.dwOffs
	jc exit
@@:
	push eax
	invoke printf, CStr("Hdle  Cnt Flgs Segs Name     Filename",lf)
	invoke printchars, '-', 60, 1
	pop eax
	mov ecx, pb.p2.dwOffs
nextitem:
	call CheckModuleHandle	; sets FS to module handle
	jnz exit

	push ecx
	invoke GetModuleName, fs, addr modname
	invoke GetModule16Filename, fs, addr fname, sizeof fname
	movzx ebx, fs:[NEHDR.ne_count]
	movzx ecx, fs:[NEHDR.ne_pgmflgs]
	movzx edx, fs:[NEHDR.ne_cseg]
	lea eax, modname
	lea esi, fname
	mov edi, fs
	invoke printf, CStr("%4X %4X %4X %4X %-8s %s",lf), edi, ebx, ecx, edx, eax, esi

	mov ax, fs:[0006]	; next MD
	pop ecx
	loop nextitem
exit:
	pop edi
	pop esi
	ret
_moduleout endp

flatcpy proc stdcall pDest:dword, pSrc:dword            
	pushad
	mov ecx, pSrc
	mov edi, pDest
@@:
	mov al,@flat:[ecx]
	stosb
	inc ecx
	and al,al
	jnz @B
	popad
	ret
flatcpy endp

if ?WINDOWS

outflat proc stdcall
mo32_1:
	mov al, @flat:[ebx]
	inc ebx
	and al, al
	jz @F
	@putchr al
	jmp mo32_1
@@:
	invoke _crout
	mov al, @flat:[ebx]
	and al, al
	jnz mo32_1
	ret
outflat endp

;-- eax = proc32 to call, edx=param

callprochlp proc stdcall dwProc:DWORD, dwParm:DWORD

local	dwMem:dword

	invoke	allocflat, 8000h
	jc		error
	mov		dwMem, eax
	mov		edx, dwParm
	mov 	@flat:[eax],edx
	invoke	callproc32, dwProc, eax
	mov		eax, dwMem
	clc
error:
	ret
callprochlp endp

checklibandproc proc stdcall procaddr:dword

	mov eax,hdeb16fwh
	and eax,eax
	jz exit1
	mov eax,procaddr
	and eax,eax
	jz exit2
	ret
exit2:
	@errorout ERR_CANNOT_FIND_PROC32
	stc
	ret
exit1:
	@errorout ERR_CANNOT_FIND_LIBHLP32
	stc
	ret

checklibandproc endp

;--- command .DM32, windows version

_moduleout32 proc c public pb:PARMBLK

local	pME:ptr MODULEENTRY32
local	szPath[MAX_PATH]:byte

	invoke checklibandproc, pListModule32
	jc exit
	xor edx,edx 			  ;my Process-ID
	cmp pb.p1.bType, __VOID__
	jz @F
	mov edx, pb.dwOffs1
@@:
	invoke callprochlp, eax, edx
	jc exit
	mov pME, eax
	mov esi, eax
	invoke printf, CStr("Hdl=Base  Cnt     Size Path",lf)
	add eax,20
	invoke printchars, '-', eax, 1
	.while (dword ptr @flat:[esi])
		lea ecx, [esi].MODULEENTRY32.szExePath
		lea edi, szPath
		invoke flatcpy, edi, ecx
		invoke printf, CStr("%8X %4X %8X %s",lf),\
			@flat:[esi].MODULEENTRY32.modBaseAddr,\
			@flat:[esi].MODULEENTRY32.GlblcntUsage,\
			@flat:[esi].MODULEENTRY32.modBaseSize,\ 
			addr szPath
		add esi, sizeof MODULEENTRY32
	.endw
	invoke freeflat, pME
exit:
	ret
_moduleout32 endp

;--- DQ32 command, windows only

_processout32 proc c public pb:PARMBLK

local	pPE:ptr PROCESSENTRY32
local	szPath[MAX_PATH]:byte

	invoke checklibandproc, pListProcess32
	jc exit
	invoke callprochlp, eax, edx
	jc exit
	mov pPE, eax
	mov esi, eax
	invoke printf, CStr("      Id   Module Thds Path",lf)
	add eax,30
	invoke printchars, '-', eax,1
	.while (dword ptr @flat:[esi])
		lea ecx, [esi].PROCESSENTRY32.szExeFile
		lea edi, szPath
		invoke flatcpy, edi, ecx
		invoke printf, CStr("%8X %8X %4u %s",lf),\
			@flat:[esi].PROCESSENTRY32.th32ProcessID,\
			@flat:[esi].PROCESSENTRY32.th32ModuleID,\
			@flat:[esi].PROCESSENTRY32.cntThreads,\
			addr szPath
		add esi, sizeof PROCESSENTRY32
	.endw
	invoke freeflat, pPE
exit:
	ret
_processout32 endp


testmflags1 proc stdcall
	test eax,PAGE_NOACCESS
	jz @F
	@stroutc "noaccess"
	ret
@@:
	test eax,PAGE_READONLY
	jz @F
	@stroutc "r/o     "
	ret
@@:
	test eax,PAGE_READWRITE
	jz @F
	@stroutc "r/w     "
	ret
@@:
	@stroutc "?       "
	ret
testmflags1 endp

testmflags1x proc stdcall
	test ecx,MEM_RESERVE
	jnz @F
	invoke testmflags1
	ret
@@:
	@stroutc "        "
	ret
testmflags1x endp

testmflags2 proc stdcall
	test eax,MEM_COMMIT
	jz @F
	@stroutc "commit "
	ret
@@:
	test eax,MEM_RESERVE
	jz @F
	@stroutc "reserve"
	ret
@@:
	test eax,MEM_FREE
	jz @F
	@stroutc "free   "
	ret
@@:
	@stroutc "?      "
	ret
testmflags2 endp

testmflags3 proc stdcall
	test eax,MEM_MAPPED
	jz @F
	@stroutc "mapped "
	ret
@@:
	test eax,MEM_PRIVATE
	jz @F
	@stroutc "private"
	ret
@@:
	@stroutc "?      "
	ret
testmflags3 endp

;--- .DP32 command, windows only

_processinfo32 proc c public pb:PARMBLK

local	xbereich:dword
local	adresse:dword
local	szStr[80]:byte

	invoke checklibandproc, pProcessInfo32
	jc exit
	mov edi, eax
	mov eax, pb.dwOffs2
	mov adresse, eax
	invoke printf, CStr("    Addr     Base     Size State   initProt currProt Type",lf)
	add eax, 2
	invoke printchars, '-', eax, 1
	invoke allocflat, 1000h
	jc exit
	mov xbereich, eax
nextitem:
	mov ecx, xbereich
	mov eax, pb.dwOffs1
	mov @flat:[ecx+0], eax		; process-id parameter
	mov eax, adresse
	mov @flat:[ecx+4], eax		; memory address parameter
	lea eax, [ecx+4*4]
	mov @flat:[ecx+8], eax		; MEMORY_BASIC_INFORMATION output
								; buffer parameter
	mov dword ptr @flat:[ecx+12], 0	; var to store process handle
	invoke callproc32, edi, ecx
	and eax, eax
	jz pi32_1
	mov esi, xbereich
	mov esi, @flat:[esi+8]
	mov eax, @flat:[esi].MEMORY_BASIC_INFORMATION.RegionSize
	add adresse, eax

	invoke printf, CStr("%8X %8X %8X "),\
		@flat:[esi].MEMORY_BASIC_INFORMATION.BaseAddress,\
		@flat:[esi].MEMORY_BASIC_INFORMATION.AllocationBase, eax

	mov eax, @flat:[esi].MEMORY_BASIC_INFORMATION.State
	invoke testmflags2
	@putchr ' '
	test eax,MEM_FREE
	jnz pi32_2

	mov eax, @flat:[esi].MEMORY_BASIC_INFORMATION.AllocationProtect
	invoke testmflags1
	@putchr ' '

	mov eax, @flat:[esi].MEMORY_BASIC_INFORMATION.Protect
	mov ecx, @flat:[esi+4*4]
	invoke testmflags1x
	@putchr ' '

	mov eax, @flat:[esi].MEMORY_BASIC_INFORMATION.Type_
	invoke testmflags3
pi32_2:
	invoke _crout
	dec dword ptr pb.p3.dwOffs
	jnz nextitem
pi32_1:
	mov ecx,xbereich
	mov dword ptr @flat:[ecx+8],0
	invoke callproc32, edi, ecx
	invoke freeflat, xbereich
exit:
	ret
_processinfo32 endp

endif

if ?32BIT

getpesize proc stdcall handle:dword
	mov edx, handle
	add edx, @flat:[edx+3Ch]
	mov eax, @flat:[edx].IMAGE_NT_HEADERS.OptionalHeader.SizeOfImage
	ret
getpesize endp

;--- command .DM32 for 32BIT
;--- syntax: .DM32 [unused] [,debuggerview]

_moduleout32 proc c public pb:PARMBLK

local handle:DWORD
local dpmihdl:DWORD
local dwCount:DWORD
local dwSize:DWORD
local szStr[260]:byte

if 0
	xor edx,edx 			  ;my Process-ID
	cmp pb.p1.bType, __VOID__
	jz @F
	mov edx,pb.dwOffs1
@@:
endif
if ?LDRDISABLE
	call chkloader
	jc exit
endif
	invoke printf, CStr("  Handle  Cnt     Size DpmiHndl Name",lf)
	add eax, 20
	invoke printchars, '-', eax, 1

	xor edx, edx
	mov ax, 4b83h	;get next module 
	call calldos
	.while ((!CARRY?) && eax)
		mov handle, eax
		mov dwCount, ecx
		mov dpmihdl, edx
		mov edx, eax
		mov ax, 4b86h	;get module file name
		call calldos
		mov szStr, 0
		.if (eax)
if ?FLAT
			invoke strcpy, addr szStr, eax
else
			invoke flatcpy, addr szStr, eax
endif
		.endif
		invoke getpesize, handle
		mov dwSize, eax
		invoke printf, CStr("%8X %4X %8X %8X %s",lf), handle, dwCount, dwSize, dpmihdl, addr szStr
		mov edx, handle
		mov ax, 4b83h
		call calldos
	.endw
exit:
	ret
calldos:
	.if (pb.dwOffs2)
		@DosCall
	.else
		int 21h
	.endif
	retn
_moduleout32 endp

endif

;*** names ausgeben ***
;*** FS:ESI -> namenstabelle ***
;*** ECX = laenge ***

GetNextName proc stdcall parm1:dword, pOut:ptr DWORD

nextitem:
	mov al, fs:[esi]
	inc esi
	movzx eax, al
	and eax, eax
	jz exit
	sub ecx, eax
	sub ecx, 3
	push ecx
	mov ecx, eax
	mov edi, [pNearHeap]
@@:
	mov al, fs:[esi]
	inc esi
	stosb
	loop @B
	mov al, 0
	stosb
	mov ax, fs:[esi]
	inc esi
	inc esi
	movzx edx, ax
	push edx 		;exportnr
	xor eax, eax
	and edx, edx
	jz @F

	mov eax, parm1
	movzx edx, dx
	movzx eax, ax
if ?WINDOWS
	@savewinsegregs
	push ax
	push edx
	call _GetProcAddress
	@restorewinsegregs
else
	mov ebx, eax
	mov cl, 1
	mov ax, 4B85h
	@DosCall
endif
	movzx edx, dx
	movzx eax, ax
@@:
	pop ecx
	mov edi, pOut
	mov [edi+0], ecx
	mov [edi+4], edx
	mov [edi+8], eax
	mov eax, pNearHeap
	mov [edi+12], eax
	pop ecx
exit:
	ret
GetNextName endp

CheckModuleHandle proc
	verr ax
	jnz @F
	mov fs, eax
	cmp word ptr fs:[0],'EN'
@@:
	ret
CheckModuleHandle endp

if ?32BIT or ?WINDOWS
IsValidModule32 proc stdcall bDebuggerContext:dword
if ?32BIT
	mov esi, eax
	xor edx, edx
	mov ax,4b83h
	.if bDebuggerContext
		@DosCall
	.else
		int 21h
	.endif
	.while (eax)
		.if (esi == eax)
			jmp found
		.endif
		mov edx, eax
		mov ax, 4b83h
		.if bDebuggerContext
			@DosCall
		.else
			int 21h
		.endif
	.endw
	stc
	ret
found:        
	clc
	ret
else
local	dwTemp:DWORD
local	dwEsp:DWORD
	push [excexit]
	mov [excexit], offset notfound
	push ds
	mov dwEsp, esp
	@mov ecx,2
	push @flat
	pop ds
	mov esi, eax
	lea edi, dwTemp
	call _Movsb
	cmp word ptr dwTemp, "ZM"
	jnz notfound
	lea esi, [eax+3Ch]
	mov cl, 4
	lea edi, dwTemp
	call _Movsb
	mov esi, dwTemp
	add esi, eax
	mov cl, 2
	lea edi, dwTemp
	call _Movsb
	cmp word ptr dwTemp,"EP"
	jz found
notfound:
	stc
found:
	mov esp,dwEsp
	pop ds
	pop [excexit]
	ret
endif
IsValidModule32 endp
endif

;--- .DN command (for both 16 and 32 bit)
;--- syntax: .DN hModule, [address|export], [debuggercontext]

_namesout proc c public uses esi edi pb:PARMBLK

local	wLaenge:dword
local	dwBestGuess:DWORD
local	dwP1:dword
local	dwP2:dword
local	dwP3:dword
local	dwP4:dword
local	dwAddress:DWORD
local	pEat:dword
local	pNames:dword
local	pOrdinals:dword
local	dwValues[4]:DWORD
local	szName[64]:byte

	mov eax,pb.dwOffs1
if ?32BIT or ?WINDOWS
	test eax, 0ffff0000h
	jz is16bit
	invoke IsValidModule32, pb.p3.dwOffs
	jc error
	mov edi, eax
	add edi, @flat:[edi+3Ch]
	mov ecx, @flat:[edi].IMAGE_NT_HEADERS.OptionalHeader.AddressOfEntryPoint
	.if (ecx)
		lea edx, [ecx+eax]
		invoke printf, CStr("Entry point: %X (%X)",lf), ecx, edx
	.endif
	mov ecx, @flat:[edi].IMAGE_NT_HEADERS.OptionalHeader.DataDirectory[IMAGE_DIRECTORY_ENTRY_EXPORT*sizeof IMAGE_DATA_DIRECTORY].Size_
;	mov ecx, [edi].IMAGE_NT_HEADERS.OptionalHeader.DataDirectory[IMAGE_DIRECTORY_ENTRY_EXPORT*sizeof IMAGE_DATA_DIRECTORY].Size_
	and ecx,ecx
	jz exit
	mov edi, @flat:[edi].IMAGE_NT_HEADERS.OptionalHeader.DataDirectory[IMAGE_DIRECTORY_ENTRY_EXPORT*sizeof IMAGE_DATA_DIRECTORY].VirtualAddress
	add edi, pb.dwOffs1
	invoke printf, CStr("Export directory=%X, Functions/Names/Ordinals RVA=%X/%X/%X",lf), edi,\
		@flat:[edi].IMAGE_EXPORT_DIRECTORY.AddressOfFunctions,\
		@flat:[edi].IMAGE_EXPORT_DIRECTORY.AddressOfNames,\
		@flat:[edi].IMAGE_EXPORT_DIRECTORY.AddressOfNameOrdinals
	invoke printf, CStr("Ordinal      RVA  Address Name",lf)
	add eax, 20
	invoke printchars, '-', eax, 1
	mov ecx, @flat:[edi].IMAGE_EXPORT_DIRECTORY.NumberOfFunctions
	and ecx,ecx
	jz exit
	or dwAddress,-1
	or dwBestGuess,-1
	cmp pb.p2.bType, __VOID__
	jz @F
	mov eax, pb.dwOffs2
	mov dwAddress, eax
@@:        
	mov ebx, @flat:[edi].IMAGE_EXPORT_DIRECTORY.Base
	mov edx, @flat:[edi].IMAGE_EXPORT_DIRECTORY.AddressOfNames
	add edx, pb.dwOffs1
	mov pNames, edx
	mov eax, @flat:[edi].IMAGE_EXPORT_DIRECTORY.AddressOfNameOrdinals
	add eax, pb.dwOffs1
	mov pOrdinals, eax
	mov esi, @flat:[edi].IMAGE_EXPORT_DIRECTORY.AddressOfFunctions
	add esi, pb.dwOffs1
	mov pEat, esi
	.while (ecx)

		push ecx
		mov ecx,@flat:[esi]
		and ecx, ecx
		jz isNull

		mov eax, ebx
		sub eax, @flat:[edi].IMAGE_EXPORT_DIRECTORY.Base
		push edi
		mov ecx, @flat:[edi].IMAGE_EXPORT_DIRECTORY.NumberOfNames
		mov edi, pOrdinals
		mov edx, offset szNull
		.while	(ecx)
			cmp  ax, @flat:[edi]
			jnz  @F
			sub  edi, pOrdinals
			shl  edi, 1
			add  edi, pNames
			mov  eax, @flat:[edi]
			add  eax, pb.dwOffs1
ife ?FLAT
			invoke flatcpy, addr szName, eax
			lea  edx, szName
else
			mov edx, eax
endif
			.break
@@:
			add  edi,2
			dec  ecx
		.endw
		pop edi

		mov ecx, @flat:[esi]		;RVA of function 
		mov eax, ecx
		add ecx, pb.dwOffs1
		.if (dwAddress != -1)
			.if (ecx <= dwAddress)
				push esi
				mov esi, dwAddress
				sub esi, ecx
				.if (esi < dwBestGuess)
					mov dwBestGuess, esi
					mov dwP1, ebx
					mov dwP2, eax
					mov dwP3, ecx
					mov dwP4, edx
				.endif
				pop esi
			.endif
			.if ((ecx != dwAddress) && (ebx != dwAddress))
				jmp isNull
			.endif
		.endif

		invoke printf, CStr("%7u %8X %8X %s",lf), ebx, eax, ecx, edx
isNull:            
		pop ecx
		add esi, 4
		inc ebx
		dec ecx
	.endw
	.if (dwBestGuess && (dwBestGuess != -1))
		invoke printf, CStr("%7u %8X %8X %s is nearest",lf), dwP1, dwP2, dwP3, dwP4
	.endif
	jmp exit
is16bit:        
endif        
	mov eax,pb.dwOffs1
	call CheckModuleHandle			  ;sets FS
	jnz error
	invoke printf,CStr("   #     CS:IP Name",lf)
	add eax, 5
	invoke printchars, '-', eax, 1
	movzx eax,word ptr fs:[0020h]    ;laenge nres names
	and eax,eax
	jz displayresidents
	mov wLaenge,eax
	call MarkDOSused
;------------------------- load nonresident names
	mov eax, fs
	invoke GetNResNames, eax
	and eax,eax
	jz displayresidents
;------------------------- display nonresident names
	mov ecx,wLaenge
	mov fs,eax
	xor esi, esi
	.while (1)
		invoke	GetNextName, pb.dwOffs1, addr dwValues
		.break .if (!eax)
		cmp byte ptr pb.wArgc,2
		jb @F
		mov eax, pb.dwOffs2
		movzx ecx, word ptr pb.wSeg2
		.if ((eax == dwValues+0) || ((ecx == dwValues+4) && (eax == dwValues+8)))
			;
		.else
			.continue
		.endif
@@:
		invoke printf, CStr("%4X %4X:%04X %s",lf),\
			[dwValues+0], dwValues+4, dwValues+8, dwValues+12
	.endw
;------------------------- free table of nonresident names
	push es
	push fs
	pop es
	push 0
	pop fs
	mov ah, 49h
	@DosCall
	pop es
displayresidents:        
;------------------------- display resident names
	mov fs,pb.dwOffs1
	movzx esi,word ptr fs:[0026h] ;residente namen
	.while (1)
		invoke	GetNextName, pb.dwOffs1, addr dwValues
		.break .if (!eax)
		cmp byte ptr pb.wArgc,2
		jb @F
		mov eax, pb.dwOffs2
		.continue .if (eax != dwValues+0)
@@:
		invoke printf, CStr("%4X %4X:%04X %s",lf),\
			[dwValues+0], dwValues+4, dwValues+8, dwValues+12
	.endw
	jmp exit
error:
	@errorout ERR_INVALID_MODULE_HANDLE
exit:
	ret
_namesout endp


if ?USESYMBOLS

if 0;ife ?FLAT
GetTranslateSyms proc
	@mov eax,1
	mov ecx, dword ptr oldsymhdl+0
	cmp ecx, offset mygetsymbolname
	jnz @F
	mov cx, word ptr oldsymhdl+4
	mov edx, cs
	cmp dx, cx
	jnz @F
	dec al
@@:
	ret
GetTranslateSyms endp
endif

;--- .SYM command

_setsymhandler proc c public pb:PARMBLK

	cmp byte ptr pb.wArgc,0
	jz disptranslation
if 0;ife ?FLAT
	xor ecx,ecx
	cmp ebx,1
	cmc
	adc ecx,ecx
	call GetTranslateSyms
	cmp ebx, eax
	jz disptranslation
	movzx eax,word ptr oldsymhdl+0
	mov dx,word ptr oldsymhdl+4
	push dx
	push eax
	call _SetSymbolicNameHandler
	mov dword ptr oldsymhdl+0,eax
	mov word ptr oldsymhdl+4,dx
else
	xor ecx,ecx
	cmp ebx,1
	cmc
	adc ecx,ecx
	mov bTranslateSyms, cl
endif
	.if (bTranslateSyms && (!_GetSymbolName))
		mov esi, offset shmod
		@mov ecx, 1
		call Load16bitProcsEx
		and eax,eax
		jnz @F
		mov bTranslateSyms, 0
@@: 	   
	.endif
disptranslation:        
if 0;ife ?FLAT
	call GetTranslateSyms
else
	movzx eax, bTranslateSyms
endif
	.if (eax)
		mov ecx, CStr("on")
	.else
		mov ecx, CStr("off")
	.endif
	invoke printf, CStr("symbol translation %s",lf), ecx
	ret
_setsymhandler endp

endif

;--- .DS command: display name of a 16bit export

_symbolout proc c public pb:PARMBLK

local	xhandle:dword
local	nreshdl:dword
local	szModName[9]:byte
local	procname[40]:byte

	pushad
	mov eax,pb.wSeg1
if ?WINDOWS
	@savewinsegregs
	push ax
	call _GetExePtr
	@restorewinsegregs
else
	mov cl, 1
	mov edx, eax
	mov ax, 4b88h
	@DosCall
endif
	and ax,ax
	jz error1
	mov xhandle,eax

	invoke GetModuleName, xhandle, addr szModName
if ?32BIT
  if ?FLAT
	lea eax,procname
	invoke setworkselbase, eax
	mov ecx, worksel
	shl ecx, 16
	xor cx,cx
  else
	mov ecx, ss
	shl ecx, 16
	lea eax,procname
	mov cx, ax
  endif
	mov eax,pb.wSeg1
	push ax
	mov ax,word ptr pb.dwOffs1
	push ax
	push ecx
	push word ptr sizeof procname
	mov ebx, esp
	mov cx, 5
	mov edx, _GetSymbolName
	mov ax, 4b87h	; call GetSymbolName
	@DosCall
	add esp,5*2
else
	@savewinsegregs
	mov eax,pb.wSeg1
	push ax
	mov ax,word ptr pb.dwOffs1
	push ax
	mov eax,ss
	push ax
	lea eax,procname
	push ax
	push word ptr sizeof procname
	call _GetSymbolName
	@restorewinsegregs
endif
	and ax,ax
	jz error2
	invoke printf, CStr("%s.%s",lf), addr szModName, addr procname
exit:
	popad
	ret
error1:
	@errorout ERR_NOMODULE_FOR_SEGMENT
	jmp exit
error2:
	@errorout ERR_UNKNOWN_SYMBOL
	jmp exit
        
_symbolout endp

;*** display owner of a selector
;--- used by disassembler symbol translation, so save all registers except eax

ownerout proc stdcall public hBlock:dword

local	modname[12]:byte
local	owner:dword

	pushad
	push fs
if ?WINDOWS
	@savewinsegregs
	push word ptr hBlock
	call _GetExePtr
	@restorewinsegregs
else
	mov cl, 1
	mov edx, hBlock
	mov ax, 4b88h
	@DosCall
endif
	movzx eax, ax
	mov owner, eax
	and eax, eax
	jz exit
	lea ebx, modname
	invoke GetModuleName, eax, ebx	; modifies fs
	invoke __stroutebx		; modifies ebx, eax
exit:
	pop fs
	popad
	mov eax,owner
	ret
ownerout endp

;*** eigner eines blocks ausgeben ***
;--- .DO command: display owner of a 16bit code selector

_getowner proc c public pb:PARMBLK

	mov eax,pb.dwOffs1
	cmp cl,__VOID__
	jnz @F
	call GetCurrentCS
@@:
	invoke ownerout, eax
	invoke printf, CStr("(%X)",lf), eax
	ret
_getowner endp

;--- .FEXIT command

_fexit proc c public pb:PARMBLK

	mov ax, word ptr _FatalExit+?SEGOFFS
	and ax, ax
	jz @F
	xor ecx,ecx
	mov eax,0204h
	call _FatalExit			; entry REBOOT device
@@:
;	@errorout MSG_UNSUPPORTED_FUNC
if ?WINDOWS
	test  [fStat], FSTAT_ISNT
	jnz   @F
	push  cs
	pop   ds
	call  _disable
	call  _DisableOemLayer
	push  0
	call  _ExitKernel
else
	push cs
	pop ds
	call _disable
	mov ax,4CFFh
	int 21h
endif        
@@:
	ret
_fexit endp

;--- .FREE command

_freelib proc c public pb:PARMBLK

	mov eax, pb.dwOffs1
if ?32BIT or ?WINDOWS
	test  eax,0FFFF0000h
	jz @F
	invoke freedll32, eax
	invoke printf, CStr("rc: %X",lf), eax
	jmp exit
@@:        
endif
	call CheckModuleHandle
	jnz freelib_err
	call SaveState
	jc freelib_err
if ?WINDOWS
	@savewinsegregs
	mov eax,pb.dwOffs1
	push ax
	call _FreeLibrary
	@restorewinsegregs
else
	mov edx, pb.dwOffs1
	movzx edx, dx
	mov ax, 4b80h
	@DosCall
endif
	movzx eax,ax
	push eax
	@errorout MSG_LIBRARY_FREED
	pop eax

	call LoadState
	jmp exit
freelib_err:
	@strout offset tQuestions
exit:
	ret
_freelib endp

if ?WINDOWS

checktaskhdl proc stdcall
	and ax,ax
	jz checktaskhdl_er
	verr ax
	jnz checktaskhdl_er
	lsl ecx,eax
	jnz checktaskhdl_er
	mov ebx,0FAh
	cmp ecx,ebx
	jbe checktaskhdl_er
	mov fs,eax
	cmp word ptr fs:[ebx],"DT"
	jnz checktaskhdl_er
	clc
	ret
checktaskhdl_er:
	stc
	ret
checktaskhdl endp

;--- .DQ command

_tasklist proc c public pb:PARMBLK

local	wTask:dword
local	dwCSIP:dword
local	tname[8]:byte

	invoke printf, CStr("   TD   SS:SP     CS:IP   Inst  PSP Name",lf)
	add eax, 4
	invoke printchars, '-', eax, 1
if ?WINDOWS
	@savewinsegregs
	call _GetCurrentTask
	@restorewinsegregs
else
	mov ah, 51h
	@DosCall
	movzx eax, bx
endif
	mov wTask,eax
	mov eax,edx
currenttask_1:					;<---
	call checktaskhdl		;fs auf TD setzen
	jc	currenttask_ex
	cmp ax,word ptr wTask
	jnz currenttask_3
	@putchr '*'
	xor ecx,ecx
	mov dwCSIP,ecx
	jmp currenttask_2
currenttask_3:
if ?WINDOWS
	push eax
	@savewinsegregs
	push ax
	call _TaskGetCSIP
	@restorewinsegregs
	push dx
	push ax
	pop eax
	mov dwCSIP,eax
	pop eax
else
	xor ecx,ecx
	mov dwCSIP,ecx
endif
	@putchr ' '
currenttask_2:
	pushad
	push ds
	mov ds,eax
	mov esi,0F2h
	lea edi,tname
	mov ebx,edi
	movsd
	movsd
	pop ds


	movzx ecx,word ptr fs:[0004h]
	movzx edx,word ptr fs:[0002h]
	movzx eax,ax
	movzx esi,word ptr dwCSIP+2
	movzx edi,word ptr dwCSIP+0
	invoke printf, CStr("%4X %4X:%04X %4X:%04X"), eax, ecx, edx, esi, edi

	movzx esi,word ptr fs:[001Ch]
	movzx edi,word ptr fs:[0060h]
	invoke printf, CStr(" %4X %4X %-.8s",lf), esi, edi, ebx

	popad
	mov ax,fs:[0]
	jmp currenttask_1
currenttask_ex:
	ret
_tasklist endp

endif

if ?SUPPSFT

SftNext proc stdcall public uses ebx esi edi pFileEntry:ptr FILEENTRY

	mov ebx, pFileEntry
nextitem:
	movzx esi, word ptr [ebx.FILEENTRY.feAddr+0]
	movzx eax, word ptr [ebx.FILEENTRY.feAddr+2]	;segment holen
	shl eax, 4
	add esi, eax
	mov ax, [ebx.FILEENTRY.feAnzahl] 
	cmp ax, @flat:[esi+4]			;anzahl files in struktur
	jb @F
	mov eax, @flat:[esi+0]
	mov [ebx.FILEENTRY.feAddr], eax
	mov [ebx.FILEENTRY.feAnzahl], 0
	cmp ax, -1
	jnz nextitem
	xor eax, eax
	jmp exit
@@:
	inc word ptr [ebx.FILEENTRY.feAnzahl]
	inc word ptr [ebx.FILEENTRY.feIndex]
	mov cx, [ebx.FILEENTRY.feLength]
	mul cl
	movzx eax, ax
	add esi, eax
	add esi, 6
	mov ax, @flat:[esi]
	and ax, ax			;Handles = 0? > dann eintrag frei
	jz nextitem

	mov eax, esi
	movzx ecx, word ptr [ebx.FILEENTRY.feAddr+2]
	shl ecx, 4
	sub eax, ecx
	mov es:[ebx.FILEENTRY.feOffset], ax

	lea edi, [ebx.FILEENTRY.feHandles]
	movzx ecx, [ebx.FILEENTRY.feLength]
ife ?FLAT
	push ds
	push @flat
	pop ds
	rep movsb
	pop ds
else
	rep movsb
endif
	@mov eax, 1
exit:
	ret

SftNext endp

SftFirst proc stdcall public uses ebx esi pFileEntry:ptr FILEENTRY

	mov esi, pFileEntry
if 1
	mov eax, dwLoL
	mov eax, @flat:[eax+4]
else
	push es
	@DosCall DOS_listoflists
	movzx ebx, bx
	mov eax, es:[ebx+4]			;startwert der SFT holen
	pop es
endif
	mov [esi.FILEENTRY.feAddr], eax
	inc eax						;if FFFF:FFFF then not supported
	jz sm9
	mov [esi.FILEENTRY.feAnzahl], 0
	mov [esi.FILEENTRY.feIndex], -1
	mov bx, wDosVersion
	xor eax, eax
	cmp bl, 20
	jnb sm9
	mov cx, 3bh					;DOS 4+
	cmp bl, 4
	jnb @F
	mov cx, 35h					;DOS 3
@@:
	mov [esi.FILEENTRY.feLength], cx 	;offset zu naechstem file
	invoke SftNext, esi
sm9:
	ret
SftFirst endp

;--- .SFT command

_getsft proc c public pb:PARMBLK

local	fe:FILEENTRY

if 0;?WINDOWS
	cmp fIrq, 0
	jnz @F
	@errorout ERR_DONTCALL_IFIRQOFF
	jmp exit
@@:
endif
;	call MarkDOSused
	invoke printf, CStr(" Idx       DPB Name        Mode  PSP  Cnt",lf)
	invoke printchars, '-', eax, 1
	invoke SftFirst, addr fe
	jmp sm1
getsft_1:
	pushad
	movzx eax, [fe.feIndex]
	movzx ebx, word ptr [fe.feAddr+2]
	movzx ecx, [fe.feOffset]
	lea edx, [fe.FILEENTRY.feName]
	movzx esi, [fe.feOpen_mode]
	movzx edi, [fe.fePSP]
	movzx ebp, [fe.feHandles]
	invoke printf, CStr("%4X %4X:%04X %-11.11s %4X %4X %4X",lf), eax, ebx, ecx, edx, esi, edi, ebp
	popad
	invoke SftNext, addr fe
sm1:
	and eax, eax
	jnz getsft_1
exit:
	ret
_getsft endp

endif

DOSFIND struct              ;11+13+4+3*1+4*2+1*4=43 Bytes
drive     db ?
searchpat db 11 dup (?)
searchatt db ?              ;0=r/o,1=hidden,2=sys,3=vol,4=dir
entrycnr  dw ?
cluster   dw ?
res       db 4 dup (?)
attr      db ?
filetime  dw ?
filedate  dw ?
filesize  dd ?
filename  db 13 dup (?)
DOSFIND ends

FINDATTR equ 1 + 2 + 4 + 10h	;r/o,hidden,system,dir

;--- .DIR command

_getfiles proc c public pb:PARMBLK

local	finddata:DOSFIND
local	hFind:dword

	cmp fIrq,0
	jnz @F
	@errorout ERR_DONTCALL_IFIRQOFF
	jmp exit
@@:
	call MarkDOSused

	lea edx, finddata
	mov ah, 1Ah					   ;set DTA
	@DosCall

	mov edx, pb.dwOffs1
	cmp pb.p1.bType, __STRING__
	jz @F
	mov edx, CCStr("*.*")
@@:
	mov cx, FINDATTR
	mov ax, 4E00h
	@DosCall
getfiles_1:
	jc exit
	test byte ptr [finddata.DOSFIND.attr],_A_SUBDIR
	jz @F
	@stroutc "   ",3Ch,"DIR",3Eh,"  "
	jmp getfiles_3
@@:
;	@dwordout [finddata._finddata.fsize]
	invoke printf, CStr("%10u"),[finddata.DOSFIND.filesize]
getfiles_3:
	@putchr ' '

	mov al,' '
	test byte ptr [finddata.DOSFIND.attr],_A_RDONLY
	jz @F
	mov al,'R'
@@:
	@putchr al

	mov al,' '
	test byte ptr [finddata.DOSFIND.attr],_A_HIDDEN
	jz @F
	mov al,'H'
@@:
	@putchr al

	mov al,' '
	test byte ptr [finddata.DOSFIND.attr],_A_SYSTEM
	jz @F
	mov al,'S'
@@:
	@putchr al
	@putchr ' '

	lea ebx,[finddata.DOSFIND.filename]
	invoke __stroutebx		; modifies ebx, eax
	invoke _crout
	mov ax, 4F00h
	@DosCall
	jmp getfiles_1
getfiles_2:
exit:
	ret
_getfiles endp

SetCurrentDirectory proc stdcall newdir:dword

	mov edx, newdir
	cmp byte ptr [edx+1], ':'
	jnz @F
	mov dl, [edx]
	or dl, 20h
	sub dl, 'a'
	mov ah, 0Eh
	@DosCall
	mov edx, newdir
@@:
	mov ax, 713Bh
	stc
	@DosCall
	jc chdir_1
	cmp ax, 7100h
	jnz chdir_ok
chdir_1:
	mov ah, 3Bh
	@DosCall
	jnc chdir_ok
	mov eax, -1
	jmp chdir_ex
chdir_ok:
	mov eax, 0
chdir_ex:
	ret
SetCurrentDirectory endp

;--- .CD command

_changedir proc c public pb:PARMBLK

local	str1[MAXPATH]:byte

	call MarkDOSused
	cmp pb.p1.bType, __VOID__
	jnz @F
	call getcurdir
	mov ebx,eax
	invoke __stroutebx		; modifies ebx, eax
	invoke _crout
	jmp changedir_ex
@@:
	invoke SetCurrentDirectory,pb.dwOffs1

	and eax,eax 			;0 ist ok, -1 ist Fehler
	jz changedir_ex
	@errorout ERR_INVALID_DIR
changedir_ex:
	ret
_changedir endp

if 0

;--- .CDR command, no longer required, .CD will do the job

_changedrive proc c pb:PARMBLK

	mov ebx,[a1.dwPx]
	mov dl,[ebx]
	or dl,20h
	sub dl,'a'
	mov ah,0Eh
	@doscallx
	ret
_changedrive endp
endif

if ?SUPPCDS

?USELASTDRIVE equ 0

CdsNext proc stdcall uses esi edi pCB:ptr CDSENTRY

	mov edi,pCB

	assume edi:ptr CDSENTRY

	mov ax,[edi.CDSENTRY.ceDistance]
	add word ptr [edi.CDSENTRY.ceEntry],ax	  ;distanz addieren

	xor eax,eax
	mov cx,word ptr [edi.CDSENTRY.ceLastdrive]
	inc ch
	cmp cl,ch
	jc sm1 						   ;fertig!
	mov [edi.CDSENTRY.ceDrive],ch
	movzx eax, word ptr [edi.CDSENTRY.ceEntry+2]
	movzx esi, word ptr [edi.CDSENTRY.ceEntry+0]
	shl eax, 4
	add esi, eax
	mov ax,@flat:[esi+67]				;flags
	mov [edi.CDSENTRY.ceFlags],ax
	mov eax,@flat:[esi+69]				;lpDPB
	mov [edi.CDSENTRY.ceDPB],eax
	mov al,@flat:[esi+79]
	mov [edi.CDSENTRY.ceRoot],al

	lea edi,[edi.CDSENTRY.cePath]
	@mov ecx,67			 ;maximal 67 stellen fuer den path
ife ?FLAT
	push ds
	push @flat
	pop ds
	rep movsb
	pop ds
else
	rep movsb
endif
	mov [edi],cl
	@mov eax,1
sm1:
	ret
	assume edi:nothing

CdsNext endp

DOS_version equ 30H ; get version number

CdsFirst proc stdcall uses ebx esi pCB:ptr CDSENTRY

	@DosCall DOS_version
	push es
	push eax
	@DosCall 52h				;get LoL in ES:BX
	movzx ebx,bx
	pop eax
	mov esi,pCB

	assume esi:ptr CDSENTRY

	mov ecx,81
	cmp al,4
	jb @F
	push ebx
	mov ax,3306h
	@DosCall
	mov edx,ebx
	pop ebx
	mov cl,47h
	cmp dx,3205h				;win NT?
	jz @F
	mov cl,88					;offset auf naechsten eintrag (gilt ab DOS 4.X)
@@:
	mov [esi.CDSENTRY.ceDistance],cx
if ?USELASTDRIVE
	mov al,es:[ebx+21h] 		;lastdrive
else
	mov ah,19h
	@DosCall
	mov dl,al
	mov ah,0Eh
	@DosCall
endif
	mov ah,00
	mov word ptr [esi.CDSENTRY.ceLastdrive],ax
	mov eax,es:[ebx+16h]		;offset naechster eintrag
	pop es
	inc eax
	jz exit					;falls adresse = FFFF:FFFF -> exit
	dec eax
	sub ax,[esi.CDSENTRY.ceDistance]
	mov [esi.CDSENTRY.ceEntry],eax
	invoke CdsNext, esi
exit:
	ret
	assume esi:nothing

CdsFirst endp

;--- .CDS command

_getcds proc c public pb:PARMBLK

local	ce:CDSENTRY
local	aktdrv:dword

if ?WINDOWS
	cmp fIrq,0
	jnz @F
	@errorout ERR_DONTCALL_IFIRQOFF
	jmp exit
@@:
endif
	call MarkDOSused
	mov ah, 19h
	@DosCall
	inc al
	mov byte ptr aktdrv,al		;A=1,B=2,...
	lea esi, ce
	invoke CdsFirst, esi
	jmp sm1
getcds_1:
	mov al,[esi.CDSENTRY.ceDrive]
	add al,'A'-1
	@putchr al
	mov al,[esi.CDSENTRY.ceDrive]
	cmp al,byte ptr aktdrv
	mov al,' '
	jnz @F
	mov al,'*'
@@:
	@putchr al
	@wordout [esi.CDSENTRY.ceFlags]
	@putchr ' '
	lea ebx,[esi.CDSENTRY.cePath]
	invoke __stroutebx		; modifies ebx, eax
	invoke _crout
	invoke CdsNext, esi
sm1:
	and eax,eax
	jnz getcds_1
exit:
	ret
_getcds endp
endif

;--- ECX=PSP segment (PSP value at [edi+1])
;--- EDX=

getpspname proc stdcall

	mov eax, @flat:[edi+8]
	mov [edx+0], eax
	lea eax, [edi+10h]
	shr eax, 4
	cmp eax, ecx 			;is mcb a psp?
	jz norm
	cmp ecx, 8
	jz system
	push ecx
	shl ecx, 4
	mov eax,@flat:[ecx-8]
	mov dword ptr [edx+0], eax
	mov eax,@flat:[ecx-4]
	mov dword ptr [edx+4], eax
	pop ecx
	ret
norm:
	mov eax,@flat:[edi+12]
	mov dword ptr [edx+4], eax
exit:
	ret
system:
	mov byte ptr [edx+2], 0
	ret
getpspname endp

;--- get next mcb in eax, size in edx

GetNextMcb proc stdcall public dwCurMcb:dword
	mov edx, dwCurMcb
	.if (!edx)
		mov edx,[dwLoL]				;list of lists	
		movzx eax,word ptr @flat:[edx-2]
		shl eax,4
	.else
		mov cl, @flat:[edx]
		movzx eax,word ptr @flat:[edx+3]
		inc eax
		shl eax,4
		add eax,edx
		cmp cl,'Z'
		jnz @F
		cmp eax,0A0000h		;dont touch A000 segment
		stc
		jz done
@@:
	.endif
	cmp byte ptr @flat:[eax],'Z'
	jz @F
	cmp byte ptr @flat:[eax],'M'
	stc
	jnz done
@@:
	movzx edx, word ptr @flat:[eax+3]
	shl edx, 4
done:
	ret
GetNextMcb endp

;--- .MCB command: list all mcbs in dos

_getmcb proc c public uses esi pb:PARMBLK

local	pspname[12]:byte

	mov byte ptr pspname+8,0
	invoke printf, CStr(" Addr T  Size  PSP Name     pPSP",lf)
	invoke printchars, '-', eax, 1
	xor edi,edi
nextitem:
	invoke GetNextMcb, edi
	jc exit
	mov edi, eax
	movzx ebx,word ptr @flat:[edi+3] ;size
	movzx ecx,word ptr @flat:[edi+1] ;psp
	lea edx,pspname
	mov byte ptr [edx], 0
	jecxz @F
	call getpspname
@@:
	mov eax, edi
	shl ebx, 4
	xor esi, esi
	push eax
	shr eax, 4
	inc eax
	cmp eax, ecx
	jnz @F
	shl eax, 4
	movzx esi,word ptr @flat:[eax+16h]
@@:
	pop eax
	lea eax, [eax+10h]
	push ebp
	movzx ebp, byte ptr @flat:[edi+0]
	invoke printf, CStr("%5X %c %5X %4X %-8s %4.0X",lf), eax, ebp, ebx, ecx, edx, esi
	pop ebp
	jmp nextitem
exit:
	ret
_getmcb endp

;--- .MCBFree command

_freemcb proc c public uses esi pb:PARMBLK

local	rmcs:RMCS

	xor ecx, ecx
	mov rmcs.rES, bx
	mov rmcs.rFlags, 202h
	mov byte ptr rmcs.rEAX+1, 49h
	mov dword ptr rmcs.rSP, ecx
	lea edi, rmcs
	mov bl, 21h
	mov ax, 300h
	@DpmiCall
	jc error1
	test byte ptr rmcs.rFlags,1
	jnz error2
	invoke printf, CStr("DOS MCB released",lf)
	jmp exit
error2:
	movzx eax, word ptr rmcs.rEAX
	invoke printf, CStr("DOS returned error %X",lf),eax
	jmp exit
error1:
	@errorout ERR_FROM_DPMI
exit:
	ret
_freemcb endp

;--- ebx = linear address psp
;--- edx = PSP name

pspout proc
	movzx eax,word ptr @flat:[ebx+16h] ;pPSP
	movzx edi,word ptr @flat:[ebx+2Ch] ;env
	movzx ecx,word ptr @flat:[ebx+30h] ;ss
	movzx esi,word ptr @flat:[ebx+2Eh] ;sp
	invoke printf, CStr("%5X %4X %4X %-8.8s %4X:%04X "), ebx, eax, edi, edx, ecx, esi
	movzx eax,word ptr @flat:[ebx+0Ch] ;termaddr.2
	movzx edx,word ptr @flat:[ebx+0Ah] ;termaddr.0
	movzx ecx,word ptr @flat:[ebx+36h] ;hdltab
	movzx esi,word ptr @flat:[ebx+34h]
	movzx edi,word ptr @flat:[ebx+32h] ;size
	push ecx
	invoke printf, CStr("%4X:%04X %4X:%04X %2X"), eax, edx, ecx, esi, edi
	pop ecx
	shl ecx,4
	add esi,ecx
	mov ecx,edi
	mov dh,' '
getpsps_3:
	mov dl,@flat:[esi]
	cmp dl,0FFh
	jz @F
	@putchr dh
	mov al,dl
	invoke _hexout
@@:
	inc esi
	mov dh,','
	loop getpsps_3
	invoke _crout
	ret
pspout endp

GetNextPsp proc stdcall uses ebx psp:dword 

	mov ebx, psp
	and ebx, ebx
	jz @F
	lea ebx, [ebx-10h]
@@:
nextitem:
	invoke GetNextMcb, ebx
	jc exit
	mov ebx, eax
	movzx eax, word ptr @flat:[ebx+1]
	shl eax, 4
	lea ecx, [ebx+10h]
	cmp eax, ecx			;is MCB a PSP?
	jnz nextitem
	cmp word ptr @flat:[eax], 20CDh
	jnz nextitem
exit:
	ret
GetNextPsp	endp        

;--- .PSP command

_getpsps proc c public pb:PARMBLK

local	szStr[8]:byte
local	me[sizeof MCBENTRY]:byte

	push esi
	push edi
	invoke printf, CStr(" Addr pPSP  Env Name         SS:SP     Int22    Hdltab Sz Handles",lf)
	invoke printchars, '-', eax, 1

	xor ebx, ebx
	.while (1)
		invoke GetNextPsp, ebx
		.break .if (CARRY?)
		mov ebx, eax
if ?FLAT
		lea edx,[ebx-8]
else
		lea edx, szStr
		mov eax,@flat:[ebx-8]
		mov ecx,@flat:[ebx-4]
		mov [edx+0],eax
		mov [edx+4],ecx
endif        
		call pspout
	.endw        
        
        
if ?WINDOWS
	@savewinsegregs
	call _GetCurrentTask
	@restorewinsegregs
	mov ax, dx
getpsps_5:
	and ax, ax
	jz	getpsps_4
	verr ax
	jnz getpsps_4
	mov fs, eax
	mov ax, fs:[0060h]
	verr ax
	jnz getpsps_4
	push dword ptr fs:[0000]
	mov bx, ax
	mov ax, 0006
	@DpmiCall
	push cx
	push dx
	pop ebx
	mov esi, 0F2h
	lea edi, [me.MCBENTRY.meName]
	mov edx, edi
	mov eax, fs:[esi+0]
	stosd
	mov eax, fs:[esi+4]
	stosd
	call pspout
	pop eax
	jmp getpsps_5
getpsps_4:
endif
	pop edi
	pop esi
	ret
_getpsps endp

;--- .Kill command

_killpsp proc c public uses esi pb:PARMBLK

local rmcs:RMCS

	xor eax, eax
	.while (1)
		invoke GetNextPsp, eax
		.break .if (CARRY?)
		mov ecx, eax
		shr ecx, 4
		.if (ecx == ebx)
			jmp found
		.endif
	.endw
	invoke printf, CStr("PSP not found",lf)
	jmp exit
found:
	invoke MarkDOSused
	xor ecx, ecx
	mov word ptr rmcs.rEBX, bx
	mov rmcs.rFlags, 202h
	mov byte ptr rmcs.rEAX+1, 50h
	mov dword ptr rmcs.rSP, ecx
	lea edi, rmcs
	mov bl, 21h
	mov ax, 300h
	@DpmiCall
	jc error1
	test byte ptr rmcs.rFlags, 1
	jnz error2
	mov  ah, 0
	@DosCall
	jc error3
	invoke printf, CStr("PSP killed",lf)
	mov ebx, [_psp]
	mov ah, 50h
	@DosCall
	jmp exit
error3:
	invoke printf, CStr("DOS function ah=00 failed",lf)
	jmp exit
error2:
	movzx eax, word ptr rmcs.rEAX
	invoke printf, CStr("DOS returned error %X",lf), eax
	jmp exit
error1:        
	@errorout ERR_FROM_DPMI
exit:        
	ret
_killpsp endp

;--- .Baud command

_baudrate proc c public pb:PARMBLK

local	xport:dword
local	pValue:dword

	mov fs, [__flatsel]
	mov ebx, [_comno]
	cmp ebx, 5
	jnb baudrate_er1
	mov dx, fs:[ebx*2+400h-2]
	and dx, dx
	jz baudrate_er1
	add dx, 3
	mov xport, edx		; LCR register port

	cmp byte ptr pb.wArgc, 0
	jz baudrate_1
	mov eax, pb.dwOffs1
	invoke SetComSpeed, _comno, eax
	mov edx, xport
	in al, dx
	mov pValue,eax
	cmp pb.p2.bType,__VOID__   ;parity
	jz @F
	mov ebx, [a2.dwPx]
	mov al, [ebx]
	or al, 20h
	mov ah, 08h
	cmp al, 'o'
	jz baudrate_11
	mov ah, 18h
	cmp al, 'e'
	jz baudrate_11
	mov ah, 00
baudrate_11:
	and byte ptr pValue, 0E7h
	or byte ptr pValue, ah
@@:
	cmp pb.p3.bType, __VOID__   ;databits
	jz @F
	mov al,byte ptr pb.p3.dwOffs
	sub al,5
	and al,3
	and byte ptr pValue,0FCh
	or byte ptr pValue,al
@@:
	cmp pb.p4.bType, __VOID__   ;stopbits
	jz @F
	mov al, byte ptr pb.p4.dwOffs
	and al, 3
	dec al
	shl al, 2
	and byte ptr pValue, 0FBh
	or byte ptr pValue, al
@@:
	mov al, byte ptr pValue
	mov edx, xport
	out dx,al
baudrate_1:
	mov eax, [_comno]
	invoke	GetComSpeed, eax
	push dx
	push ax
	pop eax
	push ds
	pop es
	push eax
	mov edx,xport
	in al, dx
	mov bh, al
	mov dl, al
	mov cl, al
	pop eax

	and cl, 04h
	shr cl, 2
	inc cl
	movzx ecx, cl		;stop bits

	mov bl, 'N'
	and bh, 18h		;parity
	test bh, 08h
	jz @F
	mov bl, 'E'
	test bh, 10h
	jnz @F
	mov bl, 'O'
@@:
	movzx ebx, bl

	and dl, 3
	add dl, 5
	movzx edx, dl

	mov esi, [_comno]
	invoke printf, CStr("COM%u is set to %u,%c,%u,%u",lf), esi, eax, ebx, edx, ecx
	jmp baudrate_ex
baudrate_er1:
	@errorout ERR_INVALID_COM
baudrate_ex:
	ret
_baudrate endp

;--- .REBOOT command

_reboot proc c public pb:PARMBLK

local	myprocaddr:fword

	cmp wWinVersion, 0	; windows enhanced mode or 9x?
	jz @F
	push 0
	pop es
	mov ax,1684h
	mov bx,9
	@callint2F
	mov eax, es
	and ax, ax
	jz @F
if ?32BIT eq 0
	movzx edi,di
endif
	mov dword ptr myprocaddr+0, edi
	mov word ptr myprocaddr+4, ax
	mov eax, 100h
	call myprocaddr
@@:
	mov word ptr @flat:[472h], 1234h
	in al, 92h
	or al, 1
	out 92h, al
	ret
_reboot endp

;--- cmd .SEG

_segsmod proc c public pb:PARMBLK

local	dwSegOfs:dword
local	dwNumSegs:dword
local	szName[9]:byte

	mov eax, pb.dwOffs1
if ?32BIT or ?WINDOWS
	test eax, 0ffff0000h
	jz is16bit
	invoke IsValidModule32, pb.p3.dwOffs
	jc error
	mov esi, eax
	mov edi, @flat:[esi+3Ch]
	add edi, esi
	movzx ebx, @flat:[edi].IMAGE_NT_HEADERS.FileHeader.NumberOfSections
	lea edi, [edi+ sizeof IMAGE_NT_HEADERS]
	invoke printf, CStr("Name      Address     Size    Flags",lf)
	invoke printchars, '-', eax, 1
	.while (ebx)

		mov eax, dword ptr @flat:[edi].IMAGE_SECTION_HEADER.Name_
		mov dword ptr szName+0, eax
		mov eax, dword ptr @flat:[edi].IMAGE_SECTION_HEADER.Name_+4
		mov dword ptr szName+4, eax
		mov szName[8], 0

		mov eax, @flat:[edi].IMAGE_SECTION_HEADER.VirtualAddress
		add eax, esi

		invoke printf, CStr("%-8.8s %8X %8X %8X",lf), addr szName, eax,\
			@flat:[edi].IMAGE_SECTION_HEADER.Misc.VirtualSize,\
			@flat:[edi].IMAGE_SECTION_HEADER.Characteristics
		add edi, sizeof IMAGE_SECTION_HEADER
		dec ebx
	.endw
	jmp exit
is16bit:        
endif        
	call CheckModuleHandle
	jnz error
	invoke printf, CStr("   # Hndl    Ofs Size Flgs Size Flags",lf)
	add eax, 15
	invoke printchars, '-', eax, 1
	movzx ecx, fs:[NEHDR.ne_cseg]	;number of segments
	and ecx,ecx
	jz exit
	mov dwNumSegs, 1
	movzx esi, fs:[NEHDR.ne_segtab]	;^ segment table
	movzx eax, fs:[NEHDR.ne_rsrctab];^ resource table
	sub eax, esi
	jc @F
	cdq
	div ecx
	mov dwSegOfs, eax
	mov dwNumSegs, ecx
@@:
	@mov eax, 1
	cmp byte ptr pb.wArgc, 2		; is a segment# given?
	jb @F
	mov dwNumSegs, eax				; then display only 1 segment
	mov eax, pb.dwOffs2				; get the segment #
	cmp eax, ecx
	ja error						; is too large
	push eax
	dec eax
	mul dwSegOfs
	add esi,eax
	pop eax
@@:        
nextitem:
	push eax
	pushad
	movzx ebx, word ptr fs:[esi+8]	; handle (selector)
	movzx edx, word ptr fs:[esi+0]	;
	movzx ecx, fs:[NEHDR.ne_segshift]	;segment shift
	and ecx, ecx
	jnz @F
	mov cl, 9
@@:        
	shl edx, cl
	movzx ecx, word ptr fs:[esi+2]
	movzx edi, word ptr fs:[esi+4]
	movzx esi, word ptr fs:[esi+6]
	invoke printf, CStr("%4X %4X %6X %4X %4X %4X "), eax, ebx, edx, ecx, edi, esi
	popad
	mov ebx, esi
	lea esi, segflgtab
	.while word ptr [esi]
		lodsw
		test ax, fs:[ebx+4]
		lodsd
		mov  edx, eax
		lodsd
		jz @F
		mov eax, edx
@@:
		and eax, eax
		jz @F
		@strout eax
@@:            
	.endw
	mov esi, ebx
        
	invoke _crout
	add esi, dwSegOfs
	pop eax
	inc eax
	dec dwNumSegs
	jnz nextitem
	jmp exit
error:
	@errorout ERR_INVALID_MODULE_HANDLE
exit:
	ret
_segsmod endp

ssout:
	test [fEntry], FENTRY_REAL
	jnz @F
	mov eax, es
	@wordout eax			 ;SS ausgeben
	ret
@@:
	mov ax,[r1r.rSS]
	@wordout eax
	ret

;*** display ebp
;*** in AX next CS if FAR

ebpout proc stdcall

	test [fEntry], FENTRY_REAL
	jnz ebpout_2
	test ch, 40h		; accrights of SS
	jnz @F
	@wordout ebx
	@stroutc "    "
	jmp ebpout_1
@@:
	@dwordout ebx
ebpout_1:
	test ah, 40h		;accrights of CS
	jnz @F
	mov ax, es:[ebx+4]
	mov si, es:[ebx+2]
	movzx edx, word ptr es:[ebx+0]
	ret
@@:
	mov eax, es:[ebx+8]
	mov esi, es:[ebx+4]
	mov edx, es:[ebx+0]
	ret
ebpout_2:
	@wordout ebx
	@stroutc "    "
	push ebx
	movzx eax, [r1r.rSS]
	shl eax, 4
	add ebx, eax
	mov ax, es:[ebx+4]			;new CS (evtl.)
	mov si, es:[ebx+2]			;new IP
	movzx edx, word ptr es:[ebx+0] ;new BP
	pop ebx
	ret
@@:
ebpout endp

eipout proc stdcall

	test [fEntry], FENTRY_REAL
	jnz eipout_1
	push eax
	lar eax, eax		; acc rights of CS
	shr eax, 8
	test ah, 40h
	pop eax
	jnz @F
eipout_1:
	@wordout esi
	@stroutc "    "
	ret
@@:
	@dwordout esi
	ret
eipout endp

;--- K command

_stacktrace proc c public pb:PARMBLK

local	actcs:dword
local	ssacc:dword

	call GetCurrentCS
	mov actcs,eax
	mov edx,[r1.rEbp]
	mov eax,[r1.rSS]
	test [fEntry], FENTRY_REAL
	jz @F
	movzx edx, dx				; HIWORD(ebp) ignorieren
	mov eax, [__flatsel]
	xor ecx, ecx
	jmp stacktrace_0
@@:
	lar ecx, eax
	jnz exit
	pushad
	invoke printf, CStr("SS:(E)BP      Type CS:(E)IP      Module",lf)
	add		eax,7
	invoke printchars, '-', eax, 1
	popad
	shr ecx,8
	test ch,40h			; HIWORD(esp) gueltig?
	jnz stacktrace_0
	movzx edx,dx
stacktrace_0:
	mov ssacc,ecx
	mov es,eax
	xor ebx,ebx
stacktrace_1:			;<----
	cmp ebx,edx 		;vergleich alter/neuer ebp
	jnb exit
	mov ebx,edx
	and bl,0FEh
	and ebx,ebx 		;ebp = 0?
	jz exit
if 0        
	mov eax,es
	lsl eax,eax
	cmp ebx,eax
	jnb exit		 ;ausserhalb segmentgrenzen?
endif        
	call ssout
	@putchr ':'
	mov eax,actcs
	lar eax,eax 		 ;accrights von CS
	shr eax,8
	mov ecx,ssacc
	call ebpout			   ;ebp ausgeben, ax:esi mit cs:eip laden
	test [fEntry], FENTRY_REAL
	jz @F			 ;edx ist mit neuem ebp geladen
	test dl,1
	jz stacktrace_23
	mov word ptr actcs,ax
	jmp stacktrace_22
@@:
	lar ecx,eax 		 ;CS ueberpruefen
	jnz stacktrace_23
	shr ecx,8
	test cl,08
	jz stacktrace_23
	lsl ecx,eax
	jb stacktrace_23
	mov actcs,eax
stacktrace_22:
	@stroutc " far  "
	jmp @F
stacktrace_23:
	@stroutc " near "
	mov eax, actcs
@@:
	@wordout actcs
	@putchr ':'
	call eipout			 ;eip ausgeben
	@putchr ' '
	test [fEntry], FENTRY_REAL
	jz @F
	invoke _crout
	jmp stacktrace_1
@@:
	invoke ownerout, actcs
	invoke _crout
	jmp stacktrace_1
exit:
	ret
_stacktrace endp

if 0

checkclientmemory proc stdcall adresse:dword,laenge:dword,flags:dword

	test [fmode], FMODE_STRICT
	jnz exit
	mov eax, adresse
	cmp eax, 100000h		;konventioneller Speicher immer ok!
	jb exit
	push eax
	invoke getptentry, eax
	pop edx
	jc error2
	and al,byte ptr flags
	cmp al,byte ptr flags
	jnz error2
	mov eax, laenge
	add eax, edx
	dec eax
	and dx, 0F000h
	and ax, 0F000h
	cmp eax, edx
	jz exit
	invoke getptentry,eax
	jc error2
	and al, byte ptr flags
	cmp al, byte ptr flags
	jnz error2
exit:
	clc
	ret
error2:
	stc
	ret
checkclientmemory endp

endif

;*** memory dump auf bildschirm ***

dumpd proc stdcall strptr:dword,laenge:dword

	mov ecx, laenge
	shr ecx, 2
	mov ebx, strptr
dumpd1:
	@dwordout [ebx]
	@putchr ' '
	@putchr ' '
	add ebx, 4
	loop dumpd1
	ret
dumpd endp

dumpw proc stdcall strptr:dword,laenge:dword

	mov ecx, laenge
	shr ecx, 1
	mov ebx, strptr
dumpw1:
	@wordout [ebx]
	@putchr ' '
	@putchr ' '
	add ebx, 2
	loop dumpw1
	ret
dumpw endp

dumpb proc stdcall strptr:dword,laenge:dword

	push esi
	mov ecx, laenge
	mov esi, strptr
	mov dl, 8
dumpb1:
	lodsb
	invoke _hexout
	mov al,' '
	dec dl
	jnz dumpb3
	cmp ecx, 1
	jz dumpb3
	mov dl, 8
	mov al, '-'
dumpb3:
	@putchr al
	and al, al
	loopnz dumpb1
	pop esi
	ret
dumpb endp

dumpt proc stdcall strptr:dword,laenge:dword

	@putchr ' '
	mov ecx, laenge
	mov ebx, strptr
dumpt5:
	mov al, [ebx]
	cmp al, ' '
	jnc dumpt6
	mov al, '.'
dumpt6:
	@putchr al
	inc ebx
	loop dumpt5
	ret
dumpt endp

dumpx proc stdcall anzahl:dword
	mov eax, [pNearHeap]
	mov cl, [dumptyp]
	cmp cl, __CHAR__
	jz dumptext
	cmp cl, __BYTE__
	jz dumpbyte
	cmp cl, __WORD__
	jz dumpword
	cmp cl, __DWORD__
	jz dumpdword
	jmp dump14
dumpdword:
	invoke dumpd, eax, anzahl
	jmp dumptext
dumpword:
	invoke dumpw, eax, anzahl
	jmp dumptext
dumpbyte:
	invoke dumpb, eax, anzahl
dumptext:
	invoke dumpt, [pNearHeap], anzahl
dump14:
	ret
dumpx endp

	@cmdproc

_dumpdt proc c public pb:PARMBLK
	mov [dumptyp], __CHAR__
	jmp _dump
_dumpdt endp

_dumpdb proc c public pb:PARMBLK
	mov [dumptyp], __BYTE__
	jmp _dump
_dumpdb endp

_dumpdw proc c public pb:PARMBLK
	mov [dumptyp], __WORD__
	jmp _dump
_dumpdw endp

_dumpdd proc c public pb:PARMBLK
	mov [dumptyp], __DWORD__
;	jmp _dump
_dumpdd endp

	@cmdprocend

_dump proc c public pb:PARMBLK

local	anzahl:dword
local	segbase:dword

	mov ebx, dword ptr [dumparg+0]
	mov al, pb.p1.bType
	cmp al, __VOID__
	jz @F
	mov ebx, pb.dwOffs1
@@:
	mov ecx, dword ptr [dumparg+4]
	cmp al, __FPTR__
	jnz @F
	mov ecx, pb.wSeg1
	and [fDump], not FDT_RMADDR
@@:
	cmp al, __RMLPTR__
	jnz @F
	mov ecx, [a1.dwSeg]
	or [fDump], FDT_RMADDR
@@:
	mov dword ptr [dumparg+4],ecx
	test [fDump], FDT_RMADDR
	jz @F
	movzx eax, cx
	shl eax, 4
	invoke setworkselbase, eax
	mov ecx,[worksel]
@@:
	lar eax, ecx
	jnz exit
	push ecx
	invoke getbaser, ecx
	mov segbase, eax
	pop fs
	mov ecx,[dumplines]
	cmp pb.p2.bType, __CONST__
	jnz @F
	mov ecx,pb.p2.dwOffs	;anzahl zeilen
	mov [dumplines], ecx
@@:
	cmp pb.p3.bType, __CONST__
	jnz @F
	mov eax,pb.p3.dwOffs	;anzahl bytes/zeile
	mov [dumpbytes], eax
@@:
	mov al, [dumptyp]
	mov edx, [dumpbytes]
	cmp al,__CHAR__
	jnz @F
	shl edx,2
@@:
	mov anzahl,edx		;anzahl bytes/zeile
dump2:					;<----
	push ecx
	push ebx

	push segbase
	push anzahl
	push fs
	push ebx
	push [pNearHeap]
	call ReadMemory		 ;ReadMemory(pNearHeap,fs:ebx,anzahl)
	jc error1
	mov anzahl,eax

	call dumpprefixout

	pop ebx
	push ebx

	call outdmpprefix
	invoke dumpx,anzahl
	invoke _crout

	pop ebx
	pop ecx
	add ebx,anzahl
	loop dump2

	mov dword ptr [dumparg+0],ebx
	jmp exit
error1:
	@errorout eax
	pop ebx
	pop ecx
exit:
	ret

dumpprefixout:
	mov eax, dword ptr [dumparg+4]
	test [fDump], FDT_RMADDR
	jz @F
	@putchr '&'
	jmp dpo_1
@@:
	cmp ax,word ptr [__flatsel]
	jz @F
dpo_1:
	@wordout eax
	@putchr ':'
	retn
@@:
	@putchr '%'
	retn

_dump endp

;*** read the bytes
;*** out: NC + eax= bytes read
;*** C if error

ReadMemory proc stdcall strptr:dword, quelle:qword, laenge:dword, segbase:dword

local	anzahl:dword
local	espreg:dword
local	dwEsp:dword

	test [fDump], FDT_SWITCHSCREEN
	jz @F
	call SwitchToDebuggeeScreen
@@:
	mov edi, strptr
	mov esi, dword ptr quelle
	mov ebx, dword ptr quelle+4
	lar edx, ebx
	shr edx, 8 			; attribute nach DX
	and dl, 1Ch			; memory/gate,CODE/DATA,conf/expand maskieren
	lsl eax, ebx
	cmp dl, 14h			; bit4=1->Memory bit3=0->DATA bit2=1->expdn?
	jnz noed
	test dh, 80h			; page granularity?
	jz @F
	shl eax, 12
	or eax, 0FFFh
@@:
	cmp eax, esi			; then eax is low lim. and esi must be > eax
	jnb error
	mov eax, -1
noed:
	sub eax, esi			; restliche bytes des segments errechnen
	jb error
	movzx ecx, word ptr laenge
	cmp eax, ecx			; kleinerer Wert -> cx
	ja @F
	mov ecx, eax
	inc ecx
@@:
if ?LOADVDD
	cmp [hVDD], -1
	jz @F
	invoke VerifyAddress, quelle, 1
	mov eax, MSG_PAGE_NOT_READABLE
	jc exit
@@:        
endif
	push [excexit]
	mov [excexit], offset rm_err
	mov eax, ecx
	push ds
	mov ds, dword ptr quelle+4
if ?WINDOWS
	mov dwEsp, esp
	call _Movsb
else
	rep movsb
endif
	clc
	jmp done
rm_err:
if ?WINDOWS
	mov esp, dwEsp
endif
	mov eax, MSG_PAGE_NOT_READABLE
	stc
done:
	pop ds
	pop [excexit]
	jmp exit
error:
	mov eax, MSG_END_OF_SEGMENT
	stc
exit:
	pushfd
	test [fDump], FDT_SWITCHSCREEN
	jz @F
	call SwitchToDebuggerScreen	; ReadMemory
@@:        
	popfd
	ret
ReadMemory endp

;--- eax = selector

outdmpprefix proc
	test [fDump], FDT_RMADDR
	jnz outdmp_2
	lar edx,eax
	shr edx,8
	test dh,0C0h 		; granularity page oder 32-Bit?
	jnz outdmp_0
	lsl edx,eax
	test edx,0FFFF0000h
	jnz outdmp_1
	jmp outdmp_2
outdmp_0:
	@dwordout ebx
	jmp exit
outdmp_1:
	mov eax,ebx
	shr eax,16
	call __nibout
outdmp_2:
	@wordout ebx
exit:
	@putchr ' '
	ret
outdmpprefix endp

;*** tabelle nichtresidenter namen eines 16-bit moduls laden
;--- out: handle in AX

GetNResNames proc stdcall uses fs gs edi esi nnModule:dword

local	hFile:dword
local	dwSize:dword
local	szPath[260]:byte

	mov eax, nnModule
	call CheckModuleHandle
	jnz error
	mov ecx, eax
	invoke GetModule16Filename, ecx, addr szPath, sizeof szPath

	call MarkDOSused

	lea edx,szPath				;szPath
	mov ax,3d00h				;open r/o, deny write
	@DosCall
	jc error
	movzx eax,ax
	mov hFile,eax				;hFile

	mov fs, nnModule
	movzx eax,WORD PTR fs:[0020h] ;laenge nichtresidente namen holen
	mov dwSize,eax
	inc eax 					;1 byte mehr (EOF)
if 1
	shr eax, 4
	inc eax
	mov ebx, eax
	mov ah,48h
	@DosCall

	movzx esi,ax
	or esi,esi
	jz gnn_ex1
else
	invoke allocflat,eax
	jc gnn_ex1
	mov esi,[worksel]
	mov edi,eax
	invoke setworkselbase,eax
endif
	mov fs,nnModule
	mov edx,fs:[002Ch]		;position nres names
	xor ecx,ecx
	invoke _fileread, hFile, esi::ecx, dwSize, edx
	cmp eax,-1
	jz @F
	mov fs, esi
	mov BYTE PTR fs:[eax],0
@@:
gnn_ex1:
	@close hFile
	mov eax,esi
	jmp exit
error:
	xor eax,eax
exit:
	ret
GetNResNames endp

	END
