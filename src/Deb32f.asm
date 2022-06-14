
	.386
	.MODEL FLAT,stdcall
	option casemap:none

	include debxxf.inc

?DBGLIB textequ <"DEB32F">

	includelib dkrnl32s.lib

DebugOut macro x:vararg
endm

GetCommandLineA		proto
OutputDebugStringA	proto a1:ptr byte

GetCommandLine		textequ <GetCommandLineA>
OutputDebugString	textequ <OutputDebugStringA>

	.code

loadstring proc

	pop edx
	push esi
	mov esi, edx
@@:
	lodsb
	and al,al
	jnz @B
	mov eax,esi
	pop esi
	jmp eax
loadstring endp

main proc c

local	dbgparm:DEBUGPARM
local	hDbgLib:DWORD
local	dwRC:dword
local	parmblock[6]:DWORD

	DebugOut "Deb32f: enter"

	pushad				;save regs here so ESP is correct

	mov hDbgLib, 0
	call loadstring
szDebugLib  db ?DBGLIB,".DLL",0

	lea ebx,parmblock			;dummy for dlls
	mov ax,4b00h
	int 21h
	.if (eax < 32)
		call loadstring
		db "can't load ", ?DBGLIB,".DLL",13,10,'$',0
		mov ah,9
		int 21h
		jmp exit
	.endif
	mov hDbgLib, eax

	invoke GetCommandLine
	mov dword ptr dbgparm.DEBUGPARM.pCmdLine,eax
	mov dword ptr dbgparm.DEBUGPARM.pHostStack,esp

	lea ebx, dbgparm
	mov eax, hDbgLib
	add eax,1000h
	call eax

	DebugOut "Deb32f: calling Enable"
	lea ebx, dbgparm
	push ebx
	call dword ptr dbgparm.Enable
	and eax,eax
	jz exit

	DebugOut "Deb32f: calling DebugEntry"
	call dword ptr dbgparm.DebugEntry

	popad				;restore regs in case they were modified inside
	pushad				;the debugger (edited by user i.e.)

	DebugOut "Deb32f: calling Disable"
	call dword ptr dbgparm.Disable
	mov dwRC,eax

exit:
	popad
	mov edx, hDbgLib
	.if (edx)
		mov ax, 4B80h
		int 21h
	.endif
	mov al,byte ptr dwRC
	ret
main endp

astart proc c
	invoke main
	mov ah,4ch
	int 21h
astart endp

GetCommandLine proc uses ebx

	mov ah,51h
	int 21h
	mov ax,0006		;get base of bx in cx:dx
	int 31h
	push cx
	push dx
	pop eax
	add eax,81h
	ret
GetCommandLine endp

ifdef _DEBUG
OutputDebugString proc uses esi pStr:ptr Byte

	mov esi,pStr
	.while (1)
		lodsb
		.break .if (al == 0)
		mov dl,al
		mov ah,2
		int 21h
	.endw
	ret
OutputDebugString endp
endif

end astart

