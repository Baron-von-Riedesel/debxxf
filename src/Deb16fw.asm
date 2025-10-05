
;--- windows program to load and communicate with the debugger

	.286
	.model small
	.386
	.dosseg
	option casemap:none

?WINDOWS equ 1
?CHECKLIMITS equ 0

lf equ 10

	.nolist
	.nocref
	include windows.inc
	include winnt.inc	; using the DOS header struct
	include debxxf.inc	; communication structure to debugger
	include deb16fw.inc	; resource ids
	.list
	.cref

BOOL typedef SWORD
;WM_SETICON equ 80h	;works for windows version >= 4.0
ICON_SMALL equ 0
ICON_BIG   equ 1

DStr macro x
local sym
	.data
ifidni <x>,<"">
sym	db 0
else
sym	db x,0
endif
	.code
	exitm <sym>
endm

;NULL_ segment word public 'BEGDATA'
;NULL_ ends
;DGROUP group NULL_

	.data

	dw 0,0,5,0,0,0,0,0

hWndMain	HWND 0     ; handle main window
hMenu		HMENU 0
hIcon		HICON 0
hInst		HINSTANCE 0
hTask		HTASK 0
hCursorWait	HCURSOR 0
hCursorNorm	HCURSOR 0
scratchsel  WORD 0
memhdl		DWORD 0
base		DWORD 0
bVersion	BYTE 0

szModuleName db 128 dup (0)
pExt		dw 0
xdi			DEBUGPARM  <>

	.const

szHlpStr db "usage: DEB16FW <options> <application>",lf
    db lf
    db "options",lf
    db lf
    db "/Q: suppress logo",lf
    db "/S: strict mode on (default for OS/2, NT, XP)",lf
    db "/A: activate debugger",lf
    db "/2: use secondary display adapter",lf
    db "/Cn: use com port n for communication",lf
    db "/D: use debug terminal for output",lf
    db "/N: no output",lf
    db "/Ecommand: execute 'command'",lf
    db 0
szError1	db "DEB16FW.OVL not found",13,10,0
szError2	db "DEB16FW.OVL is invalid",13,10,0
szError3	db "DPMI: out of memory",13,10,0
szError4	db "DPMI: out of selectors",13,10,0
szName		db "deb16fw",0

	.code

GetExternals proc uses di
	invoke GetModuleHandle, addr DStr("kernel")
	mov di,ax
	invoke GetProcAddress, di, addr DStr("LoadLibrary")
	mov word ptr xdi._LoadLibrary+0,ax
	mov word ptr xdi._LoadLibrary+2,dx
	invoke GetProcAddress, di, addr DStr("FreeLibrary")
	mov word ptr xdi._FreeLibrary+0,ax
	mov word ptr xdi._FreeLibrary+2,dx
	invoke GetProcAddress, di, addr DStr("GetProcAddress")
	mov word ptr xdi._GetProcAddress+0,ax
	mov word ptr xdi._GetProcAddress+2,dx
	invoke GetProcAddress, di, addr DStr("OutputDebugString")
	mov word ptr xdi._OutputDebugString+0,ax
	mov word ptr xdi._OutputDebugString+2,dx
	invoke GetProcAddress, di, addr DStr("ToolhelpHook")
	mov word ptr xdi._ToolhelpHook+0,ax
	mov word ptr xdi._ToolhelpHook+2,dx
	mov word ptr xdi._ReadAccProc+0, offset ReadAccess
	mov word ptr xdi._ReadAccProc+2, cs
	mov word ptr xdi._PeekMessageProc+0, offset PeekMsg
	mov word ptr xdi._PeekMessageProc+2, cs
;	mov word ptr xdi._WriteAccProc+0, offset WriteAccess
;	mov word ptr xdi._WriteAccProc+2, cs
	ret
GetExternals endp

PeekMsg proc far stdcall
local msg:MSGSTRUCT
	mov msg.message, 0
@@:
	invoke PeekMessage, addr msg, 0, WM_KEYFIRST, WM_KEYLAST, PM_REMOVE or PM_NOYIELD
	mov ax, msg.message
	cmp ax, WM_KEYDOWN
	jz @F
	cmp ax, WM_SYSKEYDOWN
	jnz @B
@@:
	ret
PeekMsg endp

;--- called by disassembler (32-bit code)
;--- in: address to read in ds:esi
;---     length to get in dwtype (1,2,4)
;--- out: value in AL/AX/EAX
;--- no registers except esi must be modified, flags must be preserved.

ReadAccess proc far stdcall dwtype:dword
	pushf
if ?CHECKLIMITS
	push eax
	push edx
	mov eax, ds
	mov edx, dwtype
	dec edx
	add edx, esi
	lsl eax, eax
	cmp eax, edx
	pop edx
	pop eax
	jnc @F
	int 3             ; an int 3 will be reported to the debugger, who then assumes an access error
@@:
endif
	cmp dwtype, 1
	jz lb
	cmp dwtype, 2
	jz lw
	lodsd [esi]
	popf
	pop bp            ; return "manually" to avoid LEAVE (just in case)
	retf 4
lw:
	lodsw [esi]
	popf
	pop bp
	retf 4
lb:
	lodsb [esi]
	popf
	pop bp
	retf 4
ReadAccess endp

if 0
;--- called by debugger (32-bit code)
;--- address to access in es:edi
;--- value in eax/ax/al
;--- length to write in dwtype (1,2,4)
;--- no registers except edi must be modified.

WriteAccess proc far stdcall dwtype:dword
	pushf
	cmp dwtype,1
	jz lb
	cmp dwtype,2
	jz lw
	stosd
	popf
	pop bp
	retf 4
lw:
	stosw
	popf
	pop bp
	retf 4
lb:
	stosb
	popf
	pop bp
	retf 4
WriteAccess endp
endif

;--- read debugger as overlay and call its init proc

ReadOverlay proc stdcall pszPath:ptr byte

local	filehdl:word
local	dwHdrSize:DWORD
local	dwFileSize:DWORD
local	dwMemSize:DWORD
local	dfInitProc:PF32
local	buffer[100h]:byte

	mov filehdl,-1

;--- build a flat selector

	mov ax, 0
	mov cx, 1
	int 31h
	jc error4
	mov scratchsel, ax
	mov bx, ax
	mov cx, -1
	mov dx, -1
	mov ax, 8
	int 31h

;--- read debugger overlay

	mov dx, pszPath
	mov ax, 3D00h
	int 21h
	jc error1
	mov bx, ax
	mov filehdl, ax

;--- read MZ header in a temp buffer

	lea dx, buffer
	mov cx, sizeof buffer
	mov ax, 3f00h
	int 21h
	jc error2
	cmp ax, cx
	jc error2
	cmp buffer.IMAGE_DOS_HEADER.e_magic, "ZM"
	jnz error2

;--- position fileptr behind header

	movzx edx, buffer.IMAGE_DOS_HEADER.e_cparhdr	;size exehdr
	shl edx, 4
	mov dwHdrSize, edx
	push edx
	pop dx
	pop cx
	mov ax, 4200h				;pos from file start
	int 21h
	jc error2

;--- calc size of executable
;--- 1. number of 200h pages
;--- 2. bytes in last page (if 0, use all pages)
;--- 3. minimum additional paragraphs

	movzx eax, buffer.IMAGE_DOS_HEADER.e_cp	;number of 200h pages
	movzx ecx, buffer.IMAGE_DOS_HEADER.e_cblp	;bytes last page
	jecxz @F
	dec eax									;last one is not full	
@@:
	shl eax, 9
	add eax, ecx
	sub eax, dwHdrSize
	mov dwFileSize, eax						;bytes to read 
	add eax, 0Fh
	and al, 0F0h							;align to paras

	movzx ecx, buffer.IMAGE_DOS_HEADER.e_minalloc	;get _BSS size
	shl ecx, 4
	add eax, ecx
	mov dwMemSize, eax
	push eax
	pop cx
	pop bx

	mov ax, 501h
	int 31h
	jc error3
	mov word ptr memhdl+0,di
	mov word ptr memhdl+2,si
	mov word ptr base+0,cx
	mov word ptr base+2,bx

	mov edi, dwFileSize
	mov esi, base

;--- read the overlay. in 16bit dpmi mode the file has to be read
;--- in chunks < 64 kB!

	.while (edi)
		mov bx, scratchsel
		push esi
		pop dx
		pop cx
		mov ax, 7
		int 31h
		.if (edi > 8000h)
			mov cx,8000h
		.else
			mov cx, di
		.endif
		xor dx, dx
		push ds
		mov ds, bx
		mov bx, filehdl
		mov ax, 3f00h
		int 21h
		pop ds
		jc error2
		cmp ax, cx
		jnz error2
		movzx eax, ax
		add esi, eax
		sub edi, eax
	.endw
	mov bx, filehdl
	mov ah, 3Eh
	int 21h
	mov filehdl, -1

;--- handle relocations

	mov bx, scratchsel			;make a flat selector
	xor dx, dx
	xor cx, cx
	mov ax, 7
	int 31h

	mov es, bx
	mov cx, buffer.IMAGE_DOS_HEADER.e_crlc		;no of relocs
	lea si, buffer
	add si, buffer.IMAGE_DOS_HEADER.e_lfarlc	;offset relocs
	mov edi, base
	.while (cx)
		movzx eax,word ptr [si+2]
		shl eax, 4
		movzx edx,word ptr [si+0]
		add eax, edx
		mov word ptr es:[edi+eax],bx
		add si,4
		dec cx
	.endw

;--- relocations done, now make scratchsel be a 32bit flat code selector

	mov dx, word ptr base+0
	mov cx, word ptr base+2
	mov ax, 7
	int 31h

	mov eax, dwMemSize
	dec eax
	push eax
	pop dx
	pop cx
	mov ax, 8
	int 31h

	lar ecx, ebx
	shr ecx, 8
	xor cl, 8			; make a code selector
	or ch, 40h			; and 32bit
	mov ax, 9
	int 31h

	xor eax, eax
	mov dword ptr dfInitProc+0, eax
	mov word ptr dfInitProc+4, bx
	push ss
	pop es
	lea ebx, xdi
	call dfInitProc

	mov ax, 1
	ret
error1:
	mov dx, offset szError1	; overlay not found
	jmp errout
error2:
	mov dx, offset szError2	; invalid overlay
	jmp errout
error3:
	mov dx, offset szError3	; no memory
	jmp errout
error4:
	mov dx, offset szError4	; no selectors
;	jmp errout
errout:
	invoke MessageBox, 0, ds::dx, addr szName, MB_OK
	mov bx, filehdl
	.if bx != -1
		mov ah,3eh
		int 21h
	.endif
	mov bx, scratchsel
	.if bx
		mov ax, 1
		int 31h
	.endif
	mov si, word ptr memhdl+2
	mov di, word ptr memhdl+0
	mov ax, si
	or ax, di
	jz @F
	mov ax, 0502h
	int 31h
@@:
	xor ax,ax
	ret

ReadOverlay endp


WriteState proc stdcall hDlg:HWND

if 0
local wpl:WINDOWPLACEMENT
local szStr[80]:byte

	mov wpl.length_, sizeof wpl
	invoke GetWindowPlacement, hDlg, addr wpl
	invoke wsprintf, addr szStr,\
		addr DStr("%d,%d,%d,%d,%d,%d,%d,%d,%d,%d"),\
		wpl.flags, wpl.showCmd,\
		wpl.ptMinPosition.ptX,\
		wpl.ptMinPosition.ptY,\
		wpl.ptMaxPosition.ptX,\
		wpl.ptMaxPosition.ptY,\
		wpl.rcNormalPosition.rcLeft,\
		wpl.rcNormalPosition.rcTop,\
		wpl.rcNormalPosition.rcRight,\
		wpl.rcNormalPosition.rcBottom

	invoke WritePrivateProfileString, addr szName, addr DStr("WndState"),\
		addr szStr, addr szModuleName
endif
	ret
WriteState endp

;--- ReadState

ReadState proc stdcall hDlg:HWND

if 0
local szStr[80]:byte
local wpl:WINDOWPLACEMENT

	mov wpl.length_, sizeof wpl
	invoke GetPrivateProfileString, addr szName, addr DStr("WndState"),\
		addr DStr(""), addr szStr, sizeof szStr, addr szModuleName
	.if (ax)
		invoke sscanf, addr szStr,\
			CStr("%d,%d,%d,%d,%d,%d,%d,%d,%d,%d"),\
			addr wpl.flags,\
			addr wpl.showCmd,\
			addr wpl.ptMinPosition.ptX,\
			addr wpl.ptMinPosition.ptY,\
			addr wpl.ptMaxPosition.ptX,\
			addr wpl.ptMaxPosition.ptY,\
			addr wpl.rcNormalPosition.rcLeft,\
			addr wpl.rcNormalPosition.rcTop,\
			addr wpl.rcNormalPosition.rcRight,\
			addr wpl.rcNormalPosition.rcBottom
		.if (ax == 10)
			invoke SetWindowPlacement, hDlg, addr wpl
		.endif
	.endif
endif
	ret
ReadState endp

;--- Dialog1 - main dialog

Dialog1 proc far pascal hDlg:HWND, message:UINT, wParam:WPARAM, lParam:LPARAM

local ps:PAINTSTRUCT
local i:SWORD
local rc:BOOL

	push ss
	pop ds

	mov rc, FALSE

	mov ax, message
	.if (ax == WM_INITDIALOG)

		invoke ReadState, hDlg

		mov ax, sp
		sub ax, 180h
		movzx eax, ax
		mov dword ptr [xdi.pHostStack+0], eax
		mov word ptr [xdi.pHostStack+4], ss

;		invoke MessageBox, hDlg, addr DStr("calling Enable"), addr szName, MB_OK
		push 0
		push ds
		push 0
		push offset xdi
		call xdi.Enable
		.if (!eax)
			invoke MessageBox, 0, addr DStr("Enable() failed"), addr szName, MB_OK
			invoke PostMessage, hDlg, WM_CLOSE, 0, 0
		.else
;			.if (bVersion >= 4)
;				invoke SendMessage, hDlg, WM_SETICON, ICON_SMALL, hIcon
;				invoke SendMessage, hDlg, WM_SETICON, ICON_BIG, hIcon
;			.endif
			invoke GetSystemMenu, hDlg, FALSE
			mov hMenu, ax
			invoke AppendMenu, hMenu, MF_ENABLED, ID_HELP, addr DStr("Help")
			invoke AppendMenu, hMenu, MF_ENABLED, ID_DEBUGENTRY, addr DStr("Enter Debugger")
			invoke ShowWindow, hDlg, SW_SHOWMINNOACTIVE
		.endif

		mov rc,TRUE

	.elseif (ax == WM_QUERYOPEN)

		mov rc,1

	.elseif (ax == WM_QUERYDRAGICON)

		mov ax, hIcon
		mov rc, ax
if 0
	.elseif (ax == WM_PAINT)

		.if (bVersion < 4)
			invoke IsIconic, hDlg
			.if (ax)
				invoke BeginPaint, hDlg, addr ps
				invoke DrawIcon, ps.PShdc, 0, 0, hIcon
				invoke EndPaint, hDlg, addr ps
			.endif
		.endif

	.elseif (ax == WM_ERASEBKGND)

		.if (bVersion < 4)
			invoke IsIconic, hDlg
			.if (ax)
			   invoke DefWindowProc, hDlg,WM_ICONERASEBKGND, wParam, lParam
			   mov rc, ax
			.endif
		.endif
endif
	.elseif (ax == WM_CLOSE)

;		invoke MessageBox, hDlg, addr DStr("calling CheckUnregister"), addr szName, MB_OK
		call xdi.CheckUnregister
		.if (!ax)
			invoke MessageBox, hDlg, addr DStr("deb16fw cannot be deinstalled currently"), addr szName, MB_OK
		.else
			invoke DestroyWindow, hDlg
		.endif
		mov rc, TRUE

	.elseif (ax == WM_ENDSESSION)

		.if (wParam)
;			invoke MessageBox, hDlg, addr DStr("WM_ENDSESSION, disabling deb16fw"), addr szName, MB_OK
			call xdi.Disable
			invoke WriteState, hDlg
		.endif

	.elseif (ax == WM_DESTROY)

;		invoke WriteState, hDlg
;		invoke GetSystemMenu, hDlg, 1	; ???
		invoke PostQuitMessage, 0

	.elseif (ax == WM_SYSCOMMAND || ax ==  WM_COMMAND)

		.if (wParam == SC_RESTORE || wParam == ID_DEBUGENTRY)
;			invoke MessageBox, hDlg, addr DStr("calling DebugEntry"), addr szName, MB_OK
			 invoke SetCursor, hCursorWait
			 call xdi.DebugEntry
			 invoke SetCursor, hCursorNorm
		.elseif (wParam == ID_HELP)
			 invoke MessageBox, hDlg, addr szHlpStr, addr szName, 0
		.endif

	.endif
	mov ax, rc
	ret
Dialog1 endp

dlgclassproc proc far pascal hDlg:HWND, message:UINT, wParam:WPARAM, lParam:LPARAM
	invoke DefDlgProc, hDlg, message, wParam, lParam
	ret
dlgclassproc endp

;--- InitApp - Initialisierung Application

InitApplication proc

local wc:WNDCLASS

	mov wc.style, 0
	mov word ptr wc.lpfnWndProc+0, offset dlgclassproc
	mov word ptr wc.lpfnWndProc+2, cs
	mov wc.cbClsExtra, 0
	mov wc.cbWndExtra, DLGWINDOWEXTRA
	mov ax, hInst
	mov wc.hInstance, ax
	mov ax, hIcon
	mov wc.hIcon, ax
	mov ax, hCursorNorm
	mov wc.hCursor, ax
	mov wc.hbrBackground, COLOR_WINDOW + 1
	mov wc.lpszMenuName, 0
	mov word ptr wc.lpszClassName+0, offset szName
	mov word ptr wc.lpszClassName+2, ds
	invoke RegisterClass, addr wc
	mov ax, 1
	ret
InitApplication endp

;--- WINMAIN

WinMain proc pascal hInstance:HINSTANCE, hPrevInstance:HINSTANCE, lpszCmdLine:LPSTR , cmdShow:SWORD

local	pStr:LPSTR
local	msg:MSGSTRUCT
local	i:SWORD
local	fStart:SWORD
local	fOption:SWORD
local	fModeSet:SWORD
local	szStr[128]:byte
local	tDC:HDC
local	tMetrics:TEXTMETRIC

	mov fStart, FALSE
	mov fOption, FALSE

	mov ax, hInstance
	mov hInst, ax

	invoke LoadCursor, 0,IDC_WAIT
	mov hCursorWait, ax
	invoke LoadCursor, 0,IDC_ARROW
	mov hCursorNorm, ax
	invoke LoadIcon, hInstance, IDI_ICON1
	mov hIcon, ax

	mov eax, lpszCmdLine
	mov pStr, eax
	.while (1)
		push ds
		lds si, pStr
		lodsb
		mov word ptr pStr, si
		pop ds
		.break .if (!al)
		.if (al == '/') 	   
			mov fOption, TRUE
		.elseif (al == 'A' || al == 'a')
			.if (fOption)
				push ds
				lds si, pStr
				mov word ptr [si-2],'  '
				pop ds
				mov fStart, TRUE
			.endif
		.else
			mov fOption, FALSE
		.endif
	.endw

	movzx eax, word ptr lpszCmdLine+0
	mov dword ptr xdi.pCmdLine+0, eax
	mov ax, word ptr lpszCmdLine+2
	mov word ptr xdi.pCmdLine+4, ax

	.if (!hPrevInstance)
		invoke InitApplication
		.if (!ax)
			mov ax, 2
			ret
		.endif
	.else
		invoke GetInstanceData, hPrevInstance, addr hWndMain, sizeof hWndMain
		invoke PostMessage, hWndMain, WM_SYSCOMMAND, SC_RESTORE, 0
		mov ax, 3
		ret
	.endif

	invoke GetExternals

	invoke GetModuleFileName, hInst, addr szStr, sizeof szStr
	lea si, szStr
	add si, ax
	.while (ax)
		.break .if (byte ptr [si-1] == '\')
		dec si
		dec ax
	.endw
	mov byte ptr [si],0
	lea ax, szStr
	invoke lstrcpy, addr szModuleName, ds::ax
	invoke lstrcat, addr szModuleName, addr DStr("debxxf.ini")
	invoke lstrcpy, ds::si, addr DStr("deb16fw.ovl")

	lea ax, szStr
	mov word ptr xdi.pszDebuggerName+0, ax
	mov word ptr xdi.pszDebuggerName+2, ss

	invoke ReadOverlay, addr szStr
	.if (!ax)
		mov ax, 5
		ret
	.endif

	invoke GetVersion
	mov bVersion, al

	invoke CreateDialog, hInstance, IDD_DIALOG1, 0, Dialog1
	.if (!ax)
		invoke MessageBox, 0, addr DStr("CreateDialog failed"), addr szName, MB_OK
		jmp exit
	.endif
	mov hWndMain, ax
	.if (fStart)
		invoke PostMessage, hWndMain,WM_COMMAND,ID_DEBUGENTRY,0
	.endif

	.while (1)
		invoke GetMessage, addr msg, NULL, 0, 0
		.break .if (!ax)
		 .if (!msg.hwnd)
			.if (msg.message == 0C002h)	;debuggee has terminated!
				invoke SendMessage, hWndMain, WM_COMMAND, ID_DEBUGENTRY, 0
				.continue
			.elseif (msg.message == WM_CLOSE)
				mov ax, hWndMain
				mov msg.hwnd, ax
			.endif
		 .endif
		 invoke IsDialogMessage, hWndMain, addr msg
		 .if (!ax)
			 invoke DispatchMessage, addr msg
		 .endif
	.endw

;	invoke MessageBox, NULL, addr DStr("calling Disable"), addr szName, MB_OK
	call xdi.Disable
exit:
	mov bx, scratchsel
	mov ax, 1
	int 31h
	mov si, word ptr memhdl+2
	mov di, word ptr memhdl+0
	mov ax, 0502h
	int 31h

	mov ax, msg.wParam
	ret

WinMain endp

start:
	invoke InitTask
	push di
	push si
	push es
	push bx
	push dx
	invoke WaitEvent, 0
	invoke InitApp, di
	call WinMain
	mov ah, 4Ch
	int 21h

	end start
