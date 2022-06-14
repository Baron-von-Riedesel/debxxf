
;--- simple read/write console, used as terminal for deb16fw.

	.386
	.MODEL FLAT, stdcall
	option casemap:none
	option proc:private

	.nolist
	.nocref
	include winbase.inc
	include wincon.inc
	include vk.inc
	.cref
	.list

?ALLOCCONSOLE equ 1

CStr macro text:vararg
local sym
	.const
sym db text,0
	.code
	exitm <offset sym>
endm
        
	.DATA

hPipeInp	dword 0
hPipeOut	dword 0
hConInp		dword 0
hConOut		dword 0
dwNumParm	dword 0
dwNumParm2	dword 0
bEscapeMode db 0

	.CODE

ctrlproc proc stdcall dwCtrlCode:dword
	mov eax, dwCtrlCode
	.if (eax == CTRL_BREAK_EVENT)
		mov eax, 1
	.elseif (eax == CTRL_CLOSE_EVENT)
		mov eax, 1
	.else
		xor eax, eax
	.endif
	ret
ctrlproc endp

keysnottocopy label word
	dw VK_CONTROL
	dw VK_CAPITAL
	dw VK_SHIFT
	dw VK_NUMLOCK
	dw VK_MENU
	dw VK_APPS
	dw VK_PAUSE
NUMNCKEYS equ ($ - offset keysnottocopy) / 2        

checkkey proc uses edi pIR:ptr INPUT_RECORD

	mov edx, pIR
	mov ax, [edx].INPUT_RECORD.Event.KeyEvent.wVirtualKeyCode
	lea edi, keysnottocopy
	mov ecx, NUMNCKEYS
	repnz scasw
	ret

checkkey endp

readthread proc stdcall parm:dword


local	dwRead:dword
local	dwWritten:dword
local	dwBuff:dword
local	ie:INPUT_RECORD
local	szStr[64]:byte

	.while (1)
		invoke ReadConsoleInputA, hConInp, addr ie, 1, addr dwRead
		.break .if (!eax)
		.if (dwRead)
			.if (ie.EventType == KEY_EVENT && ie.Event.KeyEvent.bKeyDown)
				invoke checkkey, addr ie
				.continue .if (ZERO?)
				movzx eax, ie.Event.KeyEvent.wVirtualScanCode
				shl eax, 8
				mov al, byte ptr ie.Event.KeyEvent.AsciiChar
				mov dwBuff, eax
				invoke WriteFile, hPipeOut, addr dwBuff, 4, addr dwWritten, 0
				.break .if (!eax)
			.endif
		.endif
	.endw
if 0
	invoke GetLastError
	invoke wsprintf, addr szStr, CStr(<"WriteFile failed [%X]",13,10>), eax
	lea ecx, dwWritten
	invoke WriteConsole, hConOut, addr szStr, eax, ecx, 0
endif
	ret

readthread endp


;--- understands:
;--- ESC [<N>C 
;--- ESC [<N>D 
;--- ESC [2J 
;--- ESC [<C>;<R>H 

checkescapes proc                    

local	dwWritten:DWORD
local	csbi:CONSOLE_SCREEN_BUFFER_INFO

		mov ebx, edi
		.while (ecx)
			push ecx
			lodsb
			.if (al == 1Bh && bEscapeMode == 0)
				inc bEscapeMode
				mov dwNumParm,0
				mov dwNumParm2,0
			.elseif (bEscapeMode == 1 && al == '[')
				inc bEscapeMode
			.elseif (bEscapeMode > 1 && ((al >= '0') && (al <= '9')))
				sub al,'0'
				movzx eax,al
				.if (bEscapeMode == 2)
					lea edx,dwNumParm 
				.else
					lea edx,dwNumParm2
				.endif
				mov ecx,[edx]
				add ecx,ecx
				lea ecx,[ecx+ecx*4]
				add eax, ecx
				mov [edx], eax
			.elseif ((bEscapeMode == 2) && (al == ';'))
				inc bEscapeMode
			.elseif (bEscapeMode == 2 && ((al == 'C') || (al == 'D') || (al == 'J')))
				call clearbuffer
				.if (al == 'C')
;------------------------------------ /033[<n>C: move cursor right 
					invoke GetConsoleScreenBufferInfo, hConOut, addr csbi
					mov ecx, csbi.dwCursorPosition
					mov eax, dwNumParm
					cmp eax, 1
					adc eax, eax
					add cx, ax
					invoke SetConsoleCursorPosition, hConOut, ecx
				.elseif (al == 'D')
;------------------------------------ /033[<n>D: move cursor left
					invoke GetConsoleScreenBufferInfo, hConOut, addr csbi
					mov ecx, csbi.dwCursorPosition
					mov eax, dwNumParm
					cmp eax, 1
					adc eax, eax
					sub cx, ax
					invoke SetConsoleCursorPosition, hConOut, ecx
				.elseif (al == 'J' && dwNumParm == 2)
;------------------------------------ /033[2J: clear screen + set cursor to 0,0
					invoke GetConsoleScreenBufferInfo, hConOut, addr csbi
					mov ecx, csbi.dwSize
					invoke FillConsoleOutputCharacterA, hConOut, ' ', ecx, 0, addr dwWritten
					invoke SetConsoleCursorPosition, hConOut, 0
				.endif
				mov bEscapeMode, 0
			.elseif (bEscapeMode == 3 && (al == 'H'))
				call clearbuffer
				mov ecx, dwNumParm
				shl ecx, 16
				mov cx, word ptr dwNumParm2
				invoke SetConsoleCursorPosition, hConOut, ecx
				mov bEscapeMode, 0
			.else
				stosb
				mov bEscapeMode, 0
			.endif
			pop ecx
			dec ecx
		.endw
		mov ecx, edi
		sub ecx, ebx
		ret
clearbuffer:
		.if (edi != ebx)
			push eax
			mov ecx, edi
			sub ecx, ebx
			mov edi, ebx
			invoke WriteConsoleA, hConOut, edi, ecx, addr dwWritten, 0
			pop eax
		.endif
		retn

checkescapes endp

main proc c public

local	dwRead:dword
local	dwWritten:dword
local	dwThreadId:dword
local	dwChar:dword
local	szStr[80]:byte
local	szOut[80]:byte

	invoke	GetStdHandle, STD_INPUT_HANDLE
	mov hPipeInp,eax
	invoke	GetStdHandle, STD_OUTPUT_HANDLE
	mov hPipeOut,eax

if ?ALLOCCONSOLE
	invoke	FreeConsole
	invoke	AllocConsole
	invoke	GetStdHandle, STD_INPUT_HANDLE
	mov 	hConInp,eax
	invoke	GetStdHandle, STD_OUTPUT_HANDLE
	mov 	hConOut,eax
else
	invoke	CreateFile, CStr("CONIN$"),GENERIC_READ or GENERIC_WRITE,\
		0, 0, OPEN_EXISTING, 0, 0
	.if (eax != -1)
		mov 	hConInp,eax
;		invoke	SetStdHandle, STD_INPUT_HANDLE, eax
	.else
		jmp exit
	.endif
	invoke	CreateFile, CStr("CONOUT$"),GENERIC_READ or GENERIC_WRITE,\
		0, 0, OPEN_EXISTING, 0, 0
	.if (eax != -1)
		mov 	hConOut,eax
;		invoke	SetStdHandle, STD_OUTPUT_HANDLE, eax
	.else
		jmp exit
	.endif
endif
if 0
	lea		ebx, szStr
	lea		esi, dwWritten
	invoke	wsprintf, ebx, CStr(<"pipe in=%X, pipe out=%X",13,10>), hPipeInp, hPipeOut
	invoke	WriteConsole, hConOut, ebx, eax, esi, 0
	invoke	wsprintf, ebx, CStr(<"std in=%X, std out=%X",13,10>), hConInp, hConOut
	invoke	WriteConsole, hConOut, ebx, eax, esi, 0
endif
	invoke	SetConsoleMode, hConInp, ENABLE_WINDOW_INPUT

	invoke	SetConsoleCtrlHandler, ctrlproc, 1

	invoke	CreateThread, 0, 2000h, addr readthread, 0, 0, addr dwThreadId

;;	invoke	MessageBox, 0, CStr("xxxx"), 0, MB_OK

	.while (1)
		invoke ReadFile, hPipeInp, addr szStr, sizeof szStr, addr dwRead, 0
		.break .if (!eax)
		mov ecx, dwRead
		.if (ecx)
			lea edi, szOut
			lea esi, szStr
			call checkescapes
			.continue .if (ecx == 0)
			mov bEscapeMode, 0
			invoke WriteConsoleA, hConOut, addr szOut, ecx, addr dwWritten, 0
		.endif
	.endw

if 0
	invoke GetLastError
	invoke wsprintf, addr szStr, CStr(<"ReadFile failed [%X]",13,10>), eax
	lea ecx, dwWritten
	invoke WriteConsole, hConOut, addr szStr, eax, ecx, 0
endif
if 0
@@:
	invoke ReadConsole, hConInp, addr dwChar, 1, addr dwRead, 0
	cmp byte ptr dwChar, 0Dh
	jnz @B
endif
exit:
	xor eax,eax
	ret
main endp

mainCRTStartup proc c
	invoke main
	invoke ExitProcess, eax
mainCRTStartup endp

    END mainCRTStartup

