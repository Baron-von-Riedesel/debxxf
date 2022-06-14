
;--- dll for deb16fw for switching screen to debugger and 
;--- back to windows on win3x/win9x systems

;--- if a file deb16fwv.ini is located in the same directory
;--- as the dll, some parameters are read from there. The entries
;--- have to be in section [parms], and currently known are:
;---   VideoMode: the text mode to switch when the debugger is activated
;---   Use8x8Charset: load the 8x8 charset after switch to text mode
;--- This private profile file is never written to, so it has to be
;--- created manually if needed. In most cases the default values will
;--- do nicely.


	.286
	.model small
	.dosseg
	.386

?USEDEATH		equ 1	;1=use Death/Resurrection, 0=use DisableOEMLayer/EnableOEMLayer
?SAVEGDISCREEN	equ 1	;1=save/restore screen content with BitBlt
?GETDESKTOPDC	equ 0	;1=wrap Death and Resurrection in a GetDC/ReleaseDC
?SELECTONINIT	equ 1	;1=select bitmap into compatible DC on init only
?FIXWIN31BUG	equ 1	;1=alloc bitmap 1 scan line larger than necessary
?CALLINT2F		equ 1	;1=call int 2f with ax=400x (required by svga256?)
?SETTEXTMODE	equ 1	;1=set text mode after GDI is off
?LOAD8X8CHARSET	equ 1	;1=load 8x8 charset after text mode is set
?SWITCHSTACK	equ 1	;1=switch stack (required for win31)
?DEFINENULLSEG	equ 1	;1=define a NULL segment at start of DGROUP

?TEXTMODE		equ 83h	;text mode to set (may be a vesa mode)

WF_STANDARD		equ 0010h	;flag returned by GetWinFlags

;--- windows prototypes, equates, structures

GetModuleHandle		proto far pascal :far ptr
GetProcAddress		proto far pascal :word, :far ptr
GetModuleFileName	proto far pascal handle:word, lpszName:far ptr, wMax:word
GetWinFlags			proto far pascal
GetVersion			proto far pascal
GetPrivateProfileInt	proto far pascal :far ptr, :far ptr, :word, :far ptr

if ?USEDEATH
Death				proto far pascal hDC:word
Resurrection		proto far pascal hDC:word, w1:word, w2:word, w3:word, w4:word, w5:word, w6:word
GetBiosKeyProc		proto far pascal
else
DisableOEMLayer		proto far pascal
EnableOEMLayer		proto far pascal
endif
BitBlt				proto far pascal hDC:word, x:word, y:word, _dx:word, dy:word, :word, :word, :word, :dword
CreateDC			proto far pascal :far ptr, :far ptr, :far ptr, :far ptr
CreateCompatibleBitmap	proto far pascal hDC:word, :word, :word
CreateCompatibleDC	proto far pascal hDC:word
DeleteDC			proto far pascal :word
DeleteObject    	proto far pascal hGDIObject:word
GetDeviceCaps		proto far pascal hDC:word, :word
SelectObject    	proto far pascal hDC:word,hGDIObject:word

ifdef _DEBUG
OutputDebugString	proto far pascal :far ptr
wsprintf			proto far C :far ptr, :far ptr, :vararg
endif

HORZRES		equ 8
VERTRES		equ 10
SRCCOPY		equ 0CC0020h

VideoInit				proto far pascal
VideoDone				proto far pascal
SwitchToDebuggerScreen	proto far pascal
SwitchToWindowsScreen	proto far pascal

@dprintf macro text:req, args:vararg
local sym
ifdef _DEBUG
	.const
sym db "%s", text, 0
	.code
	pusha
 ifb <args>
	invoke wsprintf, addr szBuffer, addr sym, addr szName, ds
 else
	invoke wsprintf, addr szBuffer, addr sym, addr szName, ds, args
 endif
	invoke OutputDebugString, addr szBuffer
	popa
endif
endm

;--------- macros to avoid too many if...endifs in the code

@SwitchStackTo macro
if ?SWITCHSTACK
	call SwitchStackTo
endif
endm
@SwitchStackBack macro
if ?SWITCHSTACK
	call SwitchStackBack
endif
endm

@SaveDC macro        
if ?GETDESKTOPDC
	invoke GetDC, 0
	push ax
endif
endm
@RestoreDC macro        
if ?GETDESKTOPDC
	pop ax
	invoke ReleaseDC, 0, ax
endif
endm

@SelectBitmap macro
ife ?SELECTONINIT
	invoke SelectObject, hCompDC, hBitmap
	push ax
endif
endm
@RestoreBitmap macro        
ife ?SELECTONINIT
	pop ax
	invoke SelectObject, hCompDC, ax
endif
endm    

@SaveScreen macro        
if ?SAVEGDISCREEN
	@SelectBitmap
	invoke BitBlt, hCompDC, 0, 0, _dx, _dy, hDesktopDC, 0, 0, SRCCOPY
	@RestoreBitmap
endif
endm  
@RestoreScreen macro        
if ?SAVEGDISCREEN
	@SelectBitmap
	invoke BitBlt, hDesktopDC, 0, 0, _dx, _dy, hCompDC, 0, 0, SRCCOPY
	@RestoreBitmap
endif        
endm

@CallInt2fIn macro        
if ?CALLINT2F
	mov ax, 4000h	;disable VDD video register trapping
	int 2Fh
	mov ax, 4001h	;switch into background
	int 2Fh
endif
endm  
@CallInt2fOut macro
if ?CALLINT2F
	mov ax,4002h	;switch into foreground
	int 2Fh
	mov ax,4007h	;(re)enable VDD video register trapping
	int 2Fh
endif
endm

@SetTextMode macro        
local label1
if ?SETTEXTMODE
	mov ax, wTextMode
	or ah, ah
	jz label1
	mov bx, ax
	mov ax, 4f02h
label1:
	int 10h
	cmp bUse8x8, 0
	jz label2
	mov ax, 1112h
	mov bl, 0
	int 10h
label2:        
endif
endm

;-------------- end of macros

if ?DEFINENULLSEG
NULL segment word public 'BEGDATA'
	dw 0,0,5,0,0,0,0,0
NULL ends
DGROUP group NULL
endif

	.data
        
hDesktopDC	dw 0		;desktop DC
hCompDC		dw 0		;memory DC
hBitmap		dw 0		;bitmap to save screen contents
hBitmapOld	dw 0		;original bitmap saved from memory DC
_dx			dw 0		;screen width
_dy			dw 0		;screen height
if ?USEDEATH
oldint09	dd 0
winint09	dd 0
endif
wTextMode	dw ?TEXTMODE
wFlags		dw 0
bUse8x8		db ?LOAD8X8CHARSET
bFlags		db 0

FL_INIT		equ 1		;VideoInit called

	.const
        
szDisplay	db "DISPLAY",0
szSection	db "parms",0
szVideoMode	db "VideoMode",0
sz8x8		db "Use8x8Charset",0
ifdef _DEBUG
szName		db "deb16fwv: ",0
endif


if ?SWITCHSTACK
	.data
	align 2
wStackTop dw top_of_stack
	.data?
	db 1000h dup (?)
top_of_stack label byte            
endif

ifdef _DEBUG
	.data?
szBuffer db 80 dup (?)
endif

	.code

if ?SWITCHSTACK
SwitchStackTo proc
	pop bx
	mov dx, ss
	mov cx, sp
	mov ss, ax            ; set SS:SP to AX:wStackTop
	mov sp, wStackTop
	sub wStackTop, 100h   ; just in case
	push dx
	push cx
	jmp bx
SwitchStackTo endp

SwitchStackBack proc
	pop dx
	pop cx
	pop ss
	mov sp,cx
	add wStackTop, 100h
	jmp dx
SwitchStackBack endp
endif

if ?USEDEATH 

setdebuggerkbdint proc

	pusha
	mov bl, 9
	mov ax, 204h
	int 31h
	mov word ptr [winint09+0], dx
	mov word ptr [winint09+2], cx

	mov dx, word ptr [oldint09+0]
	mov cx, word ptr [oldint09+2]
	mov ax, 205h
	int 31h
	popa
	ret
setdebuggerkbdint endp

setdebuggeekbdint proc

	pusha
	xor cx, cx
	mov dx, word ptr [winint09+0]
	xchg cx, word ptr [winint09+2]
	jcxz @F
	mov bl, 9
	mov ax, 205h
	int 31h
@@:
	popa
	ret
setdebuggeekbdint endp

endif

resethiwords proc
	movzx edi, di
	movzx esi, si
	movzx ebp, bp
	movzx ebx, bx
	movzx ecx, cx
	movzx edx, dx
	movzx eax, ax
	push 0
	pop fs
	push 0
	pop gs
	ret

resethiwords endp
;---------------------------------
;--- void SwitchToDebuggerScreen()
;---------------------------------

SwitchToDebuggerScreen proc far pascal uses ds es fs gs

	mov ax, DGROUP
	mov ds, ax
	mov es, ax
	@dprintf <"SwitchToDebuggerScreen",13,10>
	test bFlags, FL_INIT
	jnz @F
	push ax
	invoke VideoInit
	and ax, ax
	pop ax
	jz exit
@@:
	@SwitchStackTo     ; switch stack to AX:wStackTop
	@SaveScreen
	call resethiwords
ife ?USEDEATH
	invoke DisableOEMLayer
else
	test wFlags, WF_STANDARD
	jz @F
	@CallInt2fIn
@@:
	@SaveDC
	invoke Death, hDesktopDC
	@RestoreDC
	call setdebuggerkbdint
endif
	@SwitchStackBack
	@SetTextMode
exit:
	ret
SwitchToDebuggerScreen endp

;---------------------------------
;--- void SwitchToWindowsScreen()
;---------------------------------

SwitchToWindowsScreen proc far pascal uses ds es fs gs

	mov ax, DGROUP
	mov ds, ax
	mov es, ax
	@dprintf <"SwitchToWindowsScreen",13,10>
	@SwitchStackTo     ; switch stack to AX:wStackTop
	call resethiwords
ife ?USEDEATH
	invoke EnableOEMLayer
else
	call setdebuggeekbdint
	@SaveDC
	invoke Resurrection, hDesktopDC, 0, 0, 0, 0, 0, 0
	@RestoreDC
	test wFlags, WF_STANDARD
	jz @F
	@CallInt2fOut
@@:
endif
	@RestoreScreen
	@SwitchStackBack
	ret

SwitchToWindowsScreen endp

;---------------------------------
;--- BOOL VideoInit()
;--- returns 0 on error, else 1
;---------------------------------

VideoInit proc far pascal uses ds es gs

	mov ax, DGROUP
	mov ds, ax
	@dprintf <"VideoInit",13,10>
if 1;?USEDEATH
	cmp hDesktopDC,0
	jnz sm1
	invoke CreateDC, addr szDisplay, 0, 0, 0
	and ax, ax
	jz exit
	mov hDesktopDC, ax
	invoke GetDeviceCaps, hDesktopDC, HORZRES
	mov _dx, ax
	invoke GetDeviceCaps, hDesktopDC, VERTRES
	mov _dy, ax
sm1:
	cmp hCompDC,0
	jnz sm2
	invoke CreateCompatibleDC, 0
	and ax,ax
	jz exit
	mov hCompDC, ax
sm2:
	cmp hBitmap,0
	jnz sm3
	mov ax, _dy
  if ?FIXWIN31BUG
	push ax
	invoke GetVersion
	cmp ax, 0A03h
	pop ax
	jnz @F
	inc ax				; add one scan line (bug in win31)
@@:
  endif
	invoke CreateCompatibleBitmap, hDesktopDC, _dx, ax
	and ax, ax
	jz exit
	mov hBitmap, ax
  if ?SELECTONINIT
	invoke SelectObject, hCompDC, ax
	mov hBitmapOld, ax
  endif
endif
sm3:
	or bFlags, FL_INIT
	mov ax, 1
exit:
	ret
VideoInit endp

;---------------------------------
;--- void DestroyGDIObjects() 
;---------------------------------

DestroyGDIObjects proc        

if 1;?USEDEATH
	mov cx, hDesktopDC
	jcxz @F
	invoke DeleteDC, cx
	mov hDesktopDC, 0
@@:
	mov cx, hCompDC
	jcxz @F
  if ?SELECTONINIT
	invoke SelectObject, hCompDC, hBitmapOld
  endif
	invoke DeleteDC, hCompDC
	mov  hCompDC, 0
@@:
	mov cx, hBitmap
	jcxz @F
	invoke DeleteObject, cx
	mov hBitmap, 0
@@:
endif
	ret
DestroyGDIObjects endp        

;---------------------------------
;--- void VideoDone()
;---------------------------------

VideoDone proc far pascal uses ds es gs

	mov ax, DGROUP
	mov ds, ax
	@dprintf <"VideoDone",13,10>
	invoke DestroyGDIObjects
	ret
VideoDone endp

;---------------------------------
;--- int Wep(WORD wCode)
;---------------------------------

Wep proc far pascal uses ds wCode:word

	mov ax, DGROUP
	mov ds, ax
	@dprintf <"WEP",13,10>
	call DestroyGDIObjects
	mov ax, 1
	ret
Wep endp

;-------------------------------------
;--- int LibMain(). dll initialization
;-------------------------------------

LibMain proc far pascal uses ds

if ?USEDEATH
PF16 typedef ptr far16
local lpGetBiosKeyProc:PF16
endif
local szPath[128]:byte


	mov ax, DGROUP
	mov ds, ax

	@dprintf <"LibMain",13,10>

	invoke GetWinFlags
	mov wFlags, ax

if ?USEDEATH
 if 1
	.const
szKbd db "Keyboard",0
	.code
	invoke GetModuleHandle, addr szKbd
	and ax, ax
	jz exit
	invoke GetProcAddress, ax, 89h	; GetBiosKeyProc
	mov word ptr lpGetBiosKeyProc+0, ax
	mov word ptr lpGetBiosKeyProc+2, dx
	or ax, dx
	jz exit
	call lpGetBiosKeyProc
  else
	invoke GetBiosKeyProc
  endif
	mov word ptr [oldint09+0], ax
	mov word ptr [oldint09+2], dx
endif

;--------------- check if file deb16fwv.ini exists

	invoke GetModuleFileName, ds, addr szPath, sizeof szPath
	cmp ax, 3
	jbe @F
	sub ax, 3
	lea bx, szPath
	add bx, ax
	mov dword ptr ss:[bx],"ini"

;--------------- yes, it exists, now try to read VideoMode and Use8x8Charset

	invoke GetPrivateProfileInt, addr szSection, addr szVideoMode, ?TEXTMODE, addr szPath
	mov wTextMode, ax
	invoke GetPrivateProfileInt, addr szSection, addr sz8x8, ?LOAD8X8CHARSET, addr szPath
	mov bUse8x8, al
@@:
	mov ax,1
exit:
	ret
LibMain endp

	end LibMain
