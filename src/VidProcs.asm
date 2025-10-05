
;--- handles output to video devices
;--- handles debugger's/debuggee's text screen save/restore if required
;--- calls functions in debfvesa.asm if debuggee is in graphics mode

        .386
if ?FLAT
        .MODEL FLAT
else
        .MODEL TINY
endif
        option proc:private
        option dotname
        option casemap:none

        include const.inc
        include ascii.inc
        include function.inc
        include debxxfd.inc
        include dpmi.inc
        include putf.inc
        include errors.inc
        include toolhelp.inc
        include fcntl.inc

;------------------------------------------------------------------------

        include extern32.inc
        include extern16.inc

VioSetCurPosDir    proto stdcall crtport:ptr CRTPARMS, pos:dword
VioSetTextPage     proto stdcall pCrt:ptr CRTPARMS
VioShowCursor      proto stdcall
WriteConsoleString proto stdcall pStr:ptr byte

;------------------------------------------------------------------------

;--- Why is the video page set directly?
;--- Because this works more reliable, especially in emulated environments!
?LOWSETPAGE             equ 1   ; 1=program CRT to set text page, 0=use BIOS (DOS only)
?LOWSETCSR              equ 0   ; 1=set cursor shape directly, 0=use BIOS
?BUFFERED               equ 0   ; todo: save output to a buffer for scroll back
?SETDACDIR              equ 0   ; todo: explain what 1 is supposed to achieve

;--- functions DOS+swap:
;--- SwitchToDebuggerScreen (public)
;---   SaveDebuggeeScreen
;---     AllocDbgeeScreenBuffer
;---     graphic: SaveGraphicScreen
;---     !graphic: SaveTextScreen
;---   SetDebuggerScreen
;---     AllocDbgerScreenBuffer
;---     graphic: SetDebuggerVideoMode
;---     RestoreTextScreen
;--- SwitchToDebuggeeScreen (public)
;---   AllocDbgerScreenBuffer - alloc buffer for debugger text screen
;---   SaveTextScreen
;---   SetDebuggeeScreen
;---     AllocDbgeeScreenBuffer
;---     graphic: RestoreGraphicScreen
;---     !graphic: RestoreTextScreen

;--- functions DOS+!swap:
;--- SwitchToDebuggerScreen (public)
;---   SaveDebuggeeScreen
;---   SetDebuggerScreen
;---     VioGetCurPosDir
;---     settextpage
;---     getbiosvars
;---     !init: InitScreen
;--- SwitchToDebuggeeScreen (public)
;---   SetDebuggeeScreen
;---     settextpage

;--- functions Windows:
;--- SwitchToDebuggerScreen (public)
;---   AllocDbgerScreenBuffer
;---   _SwitchToDebuggerScreen
;---   RestoreTextScreen
;--- SwitchToDebuggeeScreen (public)
;---   AllocDbgerScreenBuffer
;---   SaveTextScreen
;---   SetDebuggeeScreen
;---     _SwitchToWindowsScreen

;--- Common:
;--- vdestructor (public)
;--- getalternateadapter (public) [used as constructor]
;--- _cls (public, CLS command)
;--- _videostate (public, .VIDeo command)
;--- SetScreenSwap (public, sets fSwap variable)
;--- _WriteTTY (public)
;--- VioSetCurPosDir (public) [used by keyboard.asm]
;--- VioGetCurPosDir (public) [used by keyboard.asm]
;--- VioSetCurType (public) [not used]

;--- BIOS variables
__vio_crt_adr equ 463h
__vio_cols    equ 44Ah
__vio_pagesiz equ 44Ch
__vio_pagesta equ 44Eh
__vio_curpos0 equ 450h
__vio_page    equ 462h
__vio_rows    equ 484h

?ALTISVALID   equ 0DEB1h

        .data

        public oldint10

if ?32BIT
oldint10 PF32 0
else
oldint10 PF16 0
endif

dwHdlDbgerSave  dd 0    ;dpmi handle of debuggers video save memory
pDbgerSaveMem   dd 0    ;pointer debugger video memory save

ife ?WINDOWS
dwHdlDbgeeSave  dd 0    ;dpmi handle of debuggee video save memory
pDbgeeSaveMem   dd 0    ;pointer debuggee video memory save
g_pDbgeeSaveGfxBuff dd 0
endif

ISVGA equ 1Ah

wDCC            dw 0    ; display combination code (VGA)
fVGA            db 0    ; if VGA this value is 1A, else 0
fVideo          db 0    ; FVIDEO_ flags

if ?WINDOWS
fSwap           db 1    ; screen swap on/off
else
clVPage         db 0    ; debuggee text mode video page
 if ?LOWSETCSR
clCrt0A0B       dw 0    ; client value CRT 0A/0B (cursor shape)
 else
wShape          dw 0    ; cursor shape returned by BIOS
 endif
fSwap           db 0    ; screen swap on/off
                        db 3 dup (0);value written to profile, must be dword
endif

if ?SETDACDIR
dbgattr db 0,7,0,0,0,2ah,2ah,2ah
dbeattr db 0,7,0,0,0,2ah,2ah,2ah
endif

stdcrt CRTPARMS <0,3d4h,0,1,50h,18h,0>
altcrt CRTPARMS <0B0000h,3b4h,0,0,50h,18h,0>

        .code

vdestructor proc stdcall public
        mov ax, 502h
        mov ecx, dwHdlDbgerSave
        jecxz @F
        push ecx
        pop di
        pop si
        @DpmiCall
@@:
ife ?WINDOWS
        mov ecx, dwHdlDbgeeSave
        jecxz @F
        push ecx
        pop di
        pop si
        @DpmiCall
        cmp dword ptr _VideoDone, -1
        jz @F
        call _VideoDone
@@:
else
        cmp pCloseDbgConsole, 0
        jz @F
        invoke callproc32, pCloseDbgConsole, 0
        jmp done
@@:
        call _VideoDone
done:
endif
        ret
vdestructor endp

ife ?WINDOWS

;--- set SCREENSwap variable

SetScreenSwap proc stdcall public bNewValue:dword

        mov al, byte ptr bNewValue
        cmp al, fSwap
        jz exit
        call SwitchToDebuggeeScreen
        mov al, byte ptr bNewValue
        mov fSwap, al
        and al,al
        jnz exit
        mov al, stdcrt.page_
        cmp al, clVPage
        jnz exit
        xor stdcrt.page_,1
exit:
        ret
SetScreenSwap endp

;--- function VESAMode

_getvesamode proc stdcall public uses ebx

        test fVideo, FVIDEO_GRAPH
        jz nograph
        call SwitchToDebuggeeScreen
        mov ax,4F03h
        int 10h
        cmp ax,004Fh
        mov eax,0
        jnz @F
        movzx eax,bx
@@:
        push eax
        call SwitchToDebuggerScreen
        pop eax
        ret
nograph:
        movzx eax, byte ptr @flat:[449h]
        ret

_getvesamode endp

endif

ife ?WINDOWS

;--- get number of character rows and cols directly from CRT
;--- (VGA only)
;--- better don't do that in NTVDM.

getcrtrowcols proc uses ecx edx
        mov dx,@flat:[__vio_crt_adr]
        mov al,1
        out dx,al
        inc edx
        in al,dx
        inc al
        push eax                ;save cols
        dec edx
        mov al,7
        out dx,al
        inc edx
        in al,dx                ;get "overflow" register
        mov ah,al               ;save it in AH
        dec edx
        mov al,12h
        out dx,al
        inc edx
        in al,dx                ;get bits 0-7 of vertical disp end
        mov ch,ah
        shr ch,5                ;40h -> pos 1
        shr ah,1                ;02h -> pos 0
        and ch,2
        and ah,1
        or ah,ch                ;now in ax bits 0-9 of vertical disp end-1
        movzx eax,ax
        inc eax
        mov ecx, eax
        dec edx
        mov al,9
        out dx,al
        inc edx
        in al,dx
        and al,1Fh
        inc al
        movzx eax,al    ;now in eax size of a char 
        xchg eax,ecx
        cdq
        div ecx         ;now in eax char rows
        pop ecx         ;restore cols
        mov ah,al
        mov al,cl
        ret
getcrtrowcols endp

endif

;--- will only be called for the standard video adapter
        
getbiosvars proc stdcall uses esi pSave:ptr CRTPARMS

        mov esi, pSave

        movzx edx,byte ptr @flat:[__vio_page]
        mov [esi.CRTPARMS.page_],dl

        movzx eax,word ptr @flat:[__vio_pagesta]
        add eax, 0B0000h
        cmp byte ptr @flat:[__vio_crt_adr],0B4h
        jz @F
        add ax, 8000h
@@:
        mov [esi.CRTPARMS.pBuffer],eax
ife ?WINDOWS
        cmp [fVGA], ISVGA
        jnz novga
        test [fStat], FSTAT_ISNT
        jnz novga
        call getcrtrowcols
        dec ah                  ;rows-1 expected
        jmp storeax
novga:
endif
        mov al, @flat:[__vio_cols]
        mov ah, @flat:[__vio_rows]
storeax:
        mov [esi.CRTPARMS.cols], al
        mov [esi.CRTPARMS.rows], ah
exit:
        ret
getbiosvars endp

;--- set screenbufferpos in ES:EDI (used for RestoreTextScreen)

setscreenbufpos proc stdcall
ife ?FLAT
        push @flat
        pop es
endif
        movzx edi,word ptr es:[044Eh]           ;page start
        cmp byte ptr es:[0463h],0D4h
        jz @F
        add edi,0B0000H
        ret
@@:
        add edi,0B8000H
        ret
setscreenbufpos endp

;--- get screenbufferpos in DS:ESI
;--- called if screen swap is active, used for SaveTextScreen

getscreenbufpos proc stdcall

ife ?FLAT
        push @flat
        pop ds
endif
        movzx esi,word ptr ds:[044Eh]
        cmp byte ptr ds:[0463h],0D4h
        jz @F
        add esi,0B0000H
        ret
@@:
        add esi,0B8000H
        ret
getscreenbufpos endp

;--- ensure that debugger video page is initialized
;--- will only be called if screen swap is NOT active

InitScreen proc stdcall pCrt:ptr CRTPARMS, bForceClear:dword

        or [fVideo], FVIDEO_INIT
        mov ecx, pCrt
        mov edi, [ecx].CRTPARMS.pBuffer
        mov al,[ecx].CRTPARMS.cols
        mov dl,[ecx].CRTPARMS.rows
        inc dl
        mul dl
        movzx ecx, ax
        test byte ptr bForceClear,1
        jz nextitem
        mov ax,0720h
nextitem2:
        mov @flat:[edi],ax
        add edi,2
        loop nextitem2
        jmp exit
nextitem:
        mov ax,@flat:[edi]
        cmp al,0
        jnz @F
        mov al,20h
@@:
        cmp ah,0
        jnz @F
        mov ah,7
@@:
        mov @flat:[edi],ax
skip:
        add edi,2
        loop nextitem
exit:
        ret
InitScreen endp

;*** restore physical screen from a screenbuffer
;--- will only be called if screen swap is active
;--- pCrt may be NULL, then it is the debuggees screen
;--- if not, pCrt.pBuffer should point to video memory
;--- after this call

;--- modifies EDI

RestoreTextScreen proc stdcall uses esi pCrt:ptr CRTPARMS, pBuffer:ptr byte

        mov ecx, pCrt
        jecxz @F
        invoke getbiosvars, ecx ;get page + buffer addr from bios
@@:
        mov ecx, pBuffer
        jecxz exit
        mov esi, ecx
ife ?FLAT
        push ds
        push es
        push @flat
        pop ds
endif
        call setscreenbufpos    ; in ES:EDI
        push edi
        cld
        lodsd
        push eax                                ; save cursor pos on stack
        lodsd
        .if (!eax)
                movzx ecx,word ptr ds:[__vio_pagesiz]
                shr ecx, 1
                mov eax, 0720h          ; spaces, white on black background
                rep stosw
        .else
                mov ecx, eax
                and ecx, 0FFFh          ; max text page size is 4000h-1
                rep movsd
        .endif
        pop eax
        pop edi
ife ?FLAT
        pop es
        pop ds
endif
        mov ecx, pCrt
        jecxz @F
        mov [ecx].CRTPARMS.pBuffer, edi
        mov ax, [ecx].CRTPARMS.curpos
@@:
        invoke VioSetCurPosDir, ecx, eax
exit:
        ret
RestoreTextScreen endp

ife ?WINDOWS

;*** textpage in AL

settextpage proc stdcall pCrt:ptr CRTPARMS
if ?LOWSETPAGE
        test [fStat],FSTAT_ISNT
        jz @F
        mov ah,05                                       ;set video page
        @CallInt oldint10
        ret
@@:
        invoke VioSetTextPage, pCrt     ;set page (in AL) in CRT directly
else
        mov ah,05                                       ;set video page
        @CallInt oldint10
endif
        ret
settextpage endp

;*** debuggee in grafics mode (DOS)
;*** video state of debuggee is to be saved
;--- alloc buffer for save state 

AllocDbgeeScreenBuffer proc        
        mov eax, [pDbgeeSaveMem]
        and eax, eax
        jnz exit
        mov bx, 0
        mov cx, 4000h   ;16 kB max text page size
        mov ax, 501h
        @DpmiCall
        jc @F
        mov word ptr dwHdlDbgeeSave+0,di
        mov word ptr dwHdlDbgeeSave+2,si
        push bx
        push cx
        pop eax
        mov pDbgeeSaveMem, eax
        mov dword ptr @flat:[eax+4],2
@@:
exit:
        ret
AllocDbgeeScreenBuffer endp

endif

;--- alloc screen buffer for debugger text screen if not there
;--- returns C if error

AllocDbgerScreenBuffer proc

        mov eax, pDbgerSaveMem
        and eax, eax
        jnz done
        mov bx, 0
        mov cx, 4000h
        mov ax, 501h
        @DpmiCall
        jc done
        mov word ptr dwHdlDbgerSave+0, di
        mov word ptr dwHdlDbgerSave+2, si
        mov word ptr pDbgerSaveMem+0, cx
        mov word ptr pDbgerSaveMem+2, bx
ife ?WINDOWS
        cmp dword ptr _VideoInit, -1
        jz @F
        mov ecx, @flat
        mov dl, ?32BIT
        call _VideoInit
@@:
endif
        mov eax, pDbgerSaveMem
        mov stdcrt.pBuffer, eax
        mov dword ptr @flat:[eax+0],0           ;clear cursor pos
        mov dword ptr @flat:[eax+4],0           ;clear save size
        clc
done:
        ret
AllocDbgerScreenBuffer endp

;*** save current textscreen content ***
;*** called if screen swap is active
;*** pCrt may be null, then the debuggee's screen is to be saved
;*** and the BIOS variables may be used.
;*** after this call pCrt.pBuffer should point to pBuffer+8

SaveTextScreen proc stdcall uses ds es pCrt:ptr CRTPARMS, pBuffer:ptr

        invoke VioGetCurPosDir, pCrt
        mov edi,pBuffer
        mov ecx, pCrt
        jecxz @F
        lea edx, [edi+8]
        mov [ecx].CRTPARMS.pBuffer, edx
@@:
ife ?FLAT
        push @flat
        pop es
endif
        cld
        stosd                                           ;save cursor pos
        mov al,es:[__vio_cols]
        mov ah,es:[__vio_rows]
ife ?WINDOWS
        cmp [fVGA], ISVGA
        jnz @F
        test [fStat], FSTAT_ISNT
        jnz @F
        call getcrtrowcols
        dec ah
@@:
endif

        inc ah
        mul ah
        movzx eax, ax
        add eax, eax                    ;groesse in bytes
        dec eax
        and ax,3FFFh
        inc eax
        shr eax,2                               ;groesse in dwords
        movzx ecx,ax
        call getscreenbufpos    ;get start in DS:ESI
        mov eax,ecx                             ;get size in dwords
        stosd
        rep movsd
        ret
SaveTextScreen endp

ife ?WINDOWS

if ?SETDACDIR

;--- set VGA DACs in text mode if swap is on - why?
;--- problem if running in NTVDM

setattrdac proc stdcall uses esi edi pNew:dword, pSave:dword

local temp[8]:byte

        cmp [fVGA], ISVGA
        jnz exit
        mov esi,pNew
        mov edi,pSave
        and edi,edi
        jnz @F
        lea edi,temp
@@:
        call getinpst
        in al,dx        ;reset attribute controller
        mov dl,0C0h
        mov al,0
        call setattr
        mov al,7
        call setattr
        mov al,20h
        out dx,al

        mov dl,0C8h
        mov al,0
        call setdac
        mov al,7
        call setdac
exit:
        ret
getinpst:
        mov dx,3cch     ;mono or color?
        in al,dx
        test al,1
        jnz @F
        mov dl,0bah
        retn
@@:
        mov dl,0dah
        retn
setattr:
        out dx,al
if 1
        inc edx
        in al,dx
        stosb
        dec edx
endif
        lodsb
        out dx,al
        retn
setdac:
if 1
        mov ah,al
        dec edx         ;use 3C7h for read mode
        out dx,al
        inc edx
        inc edx
        in al,dx
        stosb
        in al,dx
        stosb
        in al,dx
        stosb
        dec edx
        mov al,ah
endif
        out dx,al
        inc edx
        lodsb
        out dx,al       ;set r,g,b for dac entry 0
        lodsb
        out dx,al
        lodsb
        out dx,al
        dec edx
        retn
setattrdac endp

endif

;*** set screen to debugger in non-windows environment
;--- changes ESI

SetDebuggerScreen proc stdcall

        mov esi, offset stdcrt
        test fSwap, 1
        jnz swap
        invoke VioGetCurPosDir, esi
        mov al, [esi].CRTPARMS.page_
        invoke settextpage, esi ;set text page to page of debugger
        invoke getbiosvars, esi ;get new page + page size from bios
        test [fVideo], FVIDEO_INIT
        jnz exit
        invoke InitScreen, esi, 0
        jmp exit
swap:
        test [fVideo], FVIDEO_GRAPH or FVIDEO_WASINGRFX
        jz @F
        and [fVideo], not FVIDEO_WASINGRFX
        push esi
        call _SetDebuggerVideoMode
        pop esi
        jmp modeset
@@:
if ?SETDACDIR
        invoke setattrdac, addr dbgattr, addr dbeattr
endif
modeset:
        invoke RestoreTextScreen, esi, pDbgerSaveMem
exit:
        invoke VioShowCursor
        ret
SetDebuggerScreen endp

endif

;--- switches to screen of debuggee
;--- text screen of debugger is saved already
;--- may modify general purpose registers

SetDebuggeeScreen proc stdcall

if ?WINDOWS
		test [__outmode], _ALTOUT
		jz @F
		call _SetWindowsKbd
		ret
@@:
        call _SwitchToWindowsScreen
else
		test [__outmode], _VIOOUT
		jz done
        test fSwap, 1
        jz noswap
        invoke AllocDbgeeScreenBuffer   ;memory fuer textscreen allocate
        jc noswap
        test [fVideo], FVIDEO_GRAPH
        jz @F
        or [fVideo], FVIDEO_WASINGRFX
        call _RestoreGraphicScreen      ; restore debuggee's screen
        ret
@@:
        invoke RestoreTextScreen, 0, pDbgeeSaveMem
 if ?SETDACDIR
        invoke setattrdac, addr dbeattr, 0
 endif
        jmp exit
noswap:
        mov al, [clVPage]                       ; debuggee in text mode, so
        invoke settextpage, 0           ; just set text page
exit:
        mov dx, @flat:[__vio_crt_adr]
 if ?LOWSETCSR
        mov ah, byte ptr clCrt0A0B+0
        mov al, 0Ah
        out dx, ax
        mov ah, byte ptr clCrt0A0B+1
        inc al
        out dx, ax
  else
        mov ah, 1
        mov cx, [wShape]
        @CallInt oldint10
  endif
endif
done:
        ret
SetDebuggeeScreen endp

;*** search a second video adapter

getalternateadapter proc stdcall public

        pushad
        mov bl,10h
        mov ax, 204h
        @DpmiCall
        mov dword ptr oldint10+0,edx
        mov word ptr oldint10+?SEGOFFS,cx

        mov ax,1a00h
        int 10h
        mov [fVGA],al
        mov edx,3b4h
        xor ah,ah
        cmp al,ISVGA                    ; VGA?
        jnz setvideoaddr1               ; no, exit
        mov [wDCC],bx
        mov dl,0CCh
        in al, dx
        test al,1                               ; is monochrome active?
        jnz @F
        mov stdcrt.crtadr,3b4h
;       mov stdcrt.page_,0              ; then there's 1 page only?
@@:
        mov dx,stdcrt.crtadr
        cmp bh,00                               ; exists a second adapter?
        jnz @F
        cmp [__outmode], _ALTOUT
        jnz setvideoaddr1
        mov [__outmode], _VIOOUT
        jmp setvideoaddr1
@@:
        mov dl,0B4h
        cmp bh,01                               ; MDA?
        jz setvideoaddr1
        cmp bh,05                               ; ega with nonochrom?
        jz setvideoaddr1
        cmp bh,07                               ; vga with monochrom?
        jz setvideoaddr1
        cmp bh,0bh                              ; mcga with monochrom?
        jz setvideoaddr1
        mov dl,0D4h
setvideoaddr1:
        mov [altcrt.crtadr],dx
        cmp dx,3B4h
        jz @F
        mov [altcrt.pBuffer],0B8000h
@@:
        popad
        ret
getalternateadapter endp

;--------------------------------------------------

        @cmdproc

_cls proc c public pb:PARMBLK

        mov esi, offset stdcrt
        test [__outmode],_ALTOUT
        jz @F
        mov  esi, offset altcrt
@@:
        test [__outmode],_ALTOUT or _VIOOUT
        jz novio
if ?WINDOWS
        test [fVideo], FVIDEO_NOSWITCH
        jz @F
        test [__outmode],_VIOOUT
        jz @F
        invoke WriteConsoleString, CStr(1Bh,"[2J")
        jmp exit
@@:
endif
        invoke InitScreen, esi, 1
        invoke VioSetCurPosDir, esi, 0
        jmp exit
novio:
        mov ecx,50*2
@@:
        @putchr lf
        loop @B
exit:
        ret
_cls endp

;--- command .VIDeo

_viostate proc c public pb:PARMBLK

        mov ebx,offset viovartab
        mov eax,offset viovartabend
        mov ch,_RDONLY_
        call symtout
        ret
_viostate endp

        @cmdprocend

ife ?WINDOWS

;--- function SCREENSwap()
;--- get/set variable fSwap

getswap proc c public value:dword
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

;--- save debuggee screen in non-windows environments in swap mode.
;--- please note: debuggee may have just switched to text mode.
;--- but left it inconsistent (fonts not loaded in plane 2, for example)
;--- so if debuggee IS in text mode, check if it was in grafics the last
;--- time we left!

SaveDebuggeeScreen proc
        test fSwap, 1
        jnz swap

;--- in page-flip mode nothing is to be saved.
        mov ah, @flat:[462h]
        mov [clVPage],ah
        mov al, stdcrt.page_
        cmp al, ah                              ; if debugger has no video page of its own
        jnz exit

        mov fSwap, 1                    ; "auto-switch" to swap mode
swap:
        invoke AllocDbgeeScreenBuffer

        test [fStat], FSTAT_ISNT
        jz @F
        mov al, @flat:[465h]    ; in NTVDM, avoid CRT access, just rely on what BIOS tells
        and al, 11b                             ; bit 0 = 1 is text mode, bit 1 = 1 is gfx mode
        cmp al, 1                               ; if it's 1, assume text mode.
        jz debuggeeintextmode
@@:
        cmp [fVGA], ISVGA
        jnz debuggeeintextmode
        mov dx,3CEh                     ; see if in gfx mode (todo: move this function to dll)
        in al,dx
        mov bl,al
        mov al,6
        out dx,al
        inc edx
        nop
        in al,dx
        xchg bl,al
        dec edx
        out dx,al
        test bl,1
        jz debuggeeintextmode           ; in text mode, save screen

        or [fVideo], FVIDEO_GRAPH
        call _SaveGraphicScreen         ; call out debfvesa.dll
        mov g_pDbgeeSaveGfxBuff, eax; save just as info
        ret
debuggeeintextmode:
        and [fVideo], not FVIDEO_GRAPH
        invoke SaveTextScreen, 0, pDbgeeSaveMem
exit:

;--- save debuggee cursor shape

if ?LOWSETCSR
        mov dx,@flat:[__vio_crt_adr]
        mov al,0Bh
        out dx,al
        inc edx
        in al,dx
        mov ah,al
        dec edx
        mov al,0Ah
        out dx,al
        inc edx
        in al,dx
        mov clCrt0A0B,ax
else
        mov bh, [clVPage]
        mov ah, 3
        @CallInt oldint10
        mov [wShape], cx
endif
        ret
SaveDebuggeeScreen endp
endif

;*** if needed, switch video state to debugger
;--- inp: cs=ds=dgroup (gs=flat for MZ binaries/overlays)

SwitchToDebuggerScreen proc stdcall public

        test [fVideo], FVIDEO_ISDBGER   ;screen owned by debugger?
        jnz exit
if ?WINDOWS
        cmp [pWriteDbgConsole], 0
        jnz exit
endif
        test [__outmode], _ALTOUT
        jz @F
if ?WINDOWS
        call _SetDebuggerKbd
endif
        or [fVideo], FVIDEO_ISDBGER
        jmp exit
@@:
        pushad
        push fs
        @loadflat
ife ?WINDOWS
        test fSwap,1
        jz nobuffer
endif
        invoke AllocDbgerScreenBuffer
        test [fVideo],FVIDEO_NOSWITCH
        jnz nodisp
if ?WINDOWS
        call _SwitchToDebuggerScreen
        invoke RestoreTextScreen, addr stdcrt, pDbgerSaveMem
else
nobuffer:
        call SaveDebuggeeScreen
        call SetDebuggerScreen
endif
switch_done:
        or [fVideo], FVIDEO_ISDBGER
nodisp:
        pop fs
        popad
exit:
        ret
SwitchToDebuggerScreen endp

;--- activate debuggee screen
;*** save video state debugger
;*** restore video state debuggee
;*** doesn't modify registers

SwitchToDebuggeeScreen proc stdcall public

        test [fVideo], FVIDEO_ISDBGER           ; do nothing if debugger doesn't own the screen
        jz exit
        test [fExit], FEXIT_NOSWITCH            ; switch deactivated?
        jnz exit
        pushad
        push es
        push fs
        push gs
        cld
        @loadflat
        test fSwap, 1
        jz @F
        test [__outmode], _VIOOUT
        jz @F
        call AllocDbgerScreenBuffer             ; alloc memory for textscreen
        jc @F
        invoke SaveTextScreen, addr stdcrt, pDbgerSaveMem
@@:
        call SetDebuggeeScreen
        pop gs
        pop fs
        pop es
        popad
        and [fVideo], not FVIDEO_ISDBGER
exit:
        ret
SwitchToDebuggeeScreen endp

if ?WINDOWS

WriteConsole proc stdcall

        test [fVideo], FVIDEO_ERROR
        jnz exit
        pushad
        invoke callproc32, pWriteDbgConsole, eax
        and eax, eax
        popad
        jz @F
exit:
        ret
@@:
        or [fVideo], FVIDEO_ERROR
if 0
        @savewinsegregs
        push [hMyTask]
        push word ptr WM_CLOSE
        push word ptr 0
        push dword ptr 0
        call _PostAppMessage
        @restorewinsegregs
endif
        ret
WriteConsole endp

WriteConsoleString proc stdcall uses esi eax pStr:ptr byte
        mov esi, pStr
        .while (1)
                lodsb
                .break .if (al == 0)
                call WriteConsole
        .endw
        ret
WriteConsoleString endp
endif

;--------------------------- streams to video

        @defstream _VIOOUT,myvioout

myvioout proc stdcall zeichen:dword

        call SwitchToDebuggerScreen
        invoke VioPutCharDir, addr stdcrt, eax
done:
        ret

myvioout endp

        @defstream _ALTOUT,myaltout

myaltout proc stdcall zeichen:dword

        call SwitchToDebuggerScreen
        invoke VioPutCharDir, addr altcrt, eax
        ret
myaltout endp

if ?BUFFERED
        @defstream _MEMOUT, memout

        .data
pBufferCur      dd 0
pBufferBeg      dd 0
pBufferEnd      dd 0
pLastLF         dd 0
dwBufferHandle  dd 0
fWasCR          db 0

        .code

;--- copy full lines (terminated with LF) into buffer only

memout proc stdcall char:dword
        mov edi, pBufferCur
        and edi, edi
        jnz @F
        mov bx,1
        mov cx,0
        mov ax,501h
        @DpmiCall
        jc exit
        mov word ptr dwBufferHandle+0,di
        mov word ptr dwBufferHandle+2,si
        mov word ptr pBufferBeg+0,cx
        mov word ptr pBufferBeg+2,bx
        mov edi, pBufferBeg
        mov ecx,10000h/4
        push edi
ife ?FLAT
        push es
        push @flat
        pop es
endif
        xor eax,eax
        rep stosd
ife ?FLAT
        pop es
endif
        pop edi
        mov pLastLF,edi
        lea eax,[edi+10000h]
        mov pBufferEnd, eax
@@:
        mov al,char
        cmp al,13
        jnz @F
        mov bWasCR, 1
        jmp exit
@@:
        cmp al,10
        jz @F
        cmp bWasCR,0
        jz @F
        mov edi,pLastLF
@@:
        mov @flat:[edi],al
        inc edi
        cmp edi, pBufferEnd
        jb @F
        mov edi, pBufferBeg
@@:
        mov pBufferCur, edi
        cmp al,10
        jnz @F
        mov pLastLF, edi
@@:
        mov bWasCR, 0
exit:
        ret
memout endp

endif

externdef _streamtab:byte
externdef _streamend:byte

__WriteTTY proc stdcall public

        push ebx
        push eax
        mov ds, cs:[__csalias]
        push ds
        pop es
        @loadflat
        test fEcho, 1
        jz main1
        mov ebx, offset _streamtab
        mov ah,[__outmode]
nextitem:
        cmp ebx, offset _streamend
        jz main1
        test ah, [ebx]
        jz @F
        push eax
        push eax
        call dword ptr [ebx+1]
        pop eax
@@:
        add ebx, 5
        jmp nextitem
main1:
        pop eax
        pop ebx
        ret
__WriteTTY endp

bs equ 08

;*** display char direkt in videosegment
;*** with scroll, also for MDA/Hercules

VioPutCharDir proc stdcall pCrt:ptr CRTPARMS, zeichen:dword

local   spalten:dword
local   zeilen:dword

        pushad
        mov edi,pCrt
        invoke VioGetCurPosDir, edi
        mov ebx, eax
        mov ch,[edi].CRTPARMS.rows                      ; zeilen-1
        mov cl,[edi].CRTPARMS.cols                      ; spalten
        mov esi,[edi].CRTPARMS.pBuffer
        movzx eax,ch
        movzx ecx,cl                                            ; ecx=anzahl spalten
        mov zeilen, eax
        mov spalten, ecx
        movzx eax,bh                                            ; eax=aktuelle zeile
        movzx ebx,bl
        mul cl
        add eax,ebx
        shl eax,1
        mov ebx,eax                                             ; ebx=aktueller offset
        mov eax,zeichen

        cmp al,cr
        jnz ppch21
        mov eax,ebx
        shr eax,1
        div cl
        mov al,ah
        xor ah,ah
        add eax,eax
        sub ebx,eax
        mov al,cr
        test [fVideo], FVIDEO_NOSWITCH
        jnz ppch3
        call checkifshouldwait  ; schauen ob waitl erreicht
        call GetInpStatus
        jz ppch3                                ; kein zeichen da
        call getcharex                  ; get key (ohne translate)
        call ctrlccheck
        jmp ppch3
ppch21:
        cmp al,lf
        jnz ppch1
        add ebx,ecx
        add ebx,ecx
        jmp ppch3
ppch1:
        cmp al,bs
        jnz ppch11
        and ebx,ebx
        jz ppch4
        sub ebx,2
        jmp ppch4
ppch11:
        cmp al,TAB
        jnz ppch1x
        mov eax,ebx             ;screenpointer / bytes pro zeile
        shr eax,1
        div cl
                                                ;rest ist position in zeile
        mov al,' '
@@:
        call writechar
        inc ah
        test ah,07
        jnz @B
        jmp ppch3
ppch1x:
        call writechar
ppch3:
        mov eax,zeilen
        inc eax
        mul ecx
        add eax,eax             ; eax=maximaler offset
        cmp ebx,eax
        jc ppch4
        mov eax,zeilen
        call scrollsc           ; will destroy esi + edi
        mov ebx,eax
ppch4:
        mov eax, ebx
        mov ecx, spalten
        shr eax, 1
        div cl                            ;AX/CL
        xchg al,ah
        invoke VioSetCurPosDir, pCrt, eax
        popad
        ret
writechar:
if ?WINDOWS
        cmp [pWriteDbgConsole], 0
        jz @F
        cmp edi, offset stdcrt
        jnz @F
        call WriteConsole
        inc byte ptr [edi].CRTPARMS.curpos
        jmp label1
@@:
endif
        mov @flat:[ebx+esi],al
label1:
        inc ebx
        inc ebx
        retn
VioPutCharDir endp

;*** alles eine zeile nach oben kopieren
;*** ecx=bildschirmspalten
;*** eax=bildschirmzeilen-1
;*** esi=?
;*** out: eax =

scrollsc proc stdcall
if ?WINDOWS
        cmp [pWriteDbgConsole], 0
;       test [fStat], FSTAT_ISNT
        jz @F
        cmp edi, offset stdcrt
        jnz @F
        pushad
        mov al,lf
        call WriteConsole
        popad
        mul ecx
        shl eax,1
        ret
@@:
endif
if ?FLAT eq 0
        push ds
        push es
        mov edx,@flat
        mov ds,edx
        mov es,edx
endif

        mov ebx,esi                     ;start 1. zeile retten
        cld
        mov edi,esi                     ;edi -> start 1. zeile
        mov esi,ecx
        add esi,esi
        add esi,edi                     ;esi -> start 2. zeile

        mul ecx

        xchg ecx,eax
        shr ecx,1                       ;je 2 bytes/zelle
        rep movsd
        xchg ecx,eax

        mov esi,edi
        mov eax,07200720h
        shr ecx,1
        rep stosd
        mov eax,esi
        sub eax,ebx                     ;offset -> eax

if ?FLAT eq 0
        pop es
        pop ds
endif
        ret
scrollsc endp

if ?WINDOWS
setcurposnt proc uses ecx
if 1
        mov cx, [edi].CRTPARMS.curpos
        .while (ah > ch)
                push eax
                mov al, lf
                call WriteConsole
                pop eax
                inc ch
        .endw
        .if (al != cl)
                push eax
                .if (al == 0)
                        mov al, cr
                        call WriteConsole
                .elseif (al < cl)
                        sub cl, al
@@:
                        mov al, bs
                        call WriteConsole
                        dec cl
                        jnz @B
                .else
                        sub al, cl
                        mov cl, al
@@:
                        invoke WriteConsoleString, CStr(1bh,"[C")
                        dec cl
                        jnz @B
                .endif
                pop eax
        .endif
else
        .if (ax != [edi].CRTPARMS.curpos)
                pushad
                sub   esp,16
                movzx ebp, sp
                movzx ecx, ah
                movzx eax, al
                invoke sprintf, ebp, CStr(1bh,"[%u;%uH"), ecx, eax
                invoke WriteConsoleString, ebp
                add esp,16
                popad
        .endif
endif
        ret
setcurposnt endp
endif

VioSetCurPosDir proc stdcall public uses ebx esi edi pCrt:ptr CRTPARMS, dwNewPos:dword

        mov eax, dwNewPos
        mov edi, pCrt
        and edi, edi
        jz @F
        mov dx, [edi].CRTPARMS.crtadr
        mov bh, [edi].CRTPARMS.cols
        mov esi, [edi].CRTPARMS.pBuffer
        movzx ecx,[edi].CRTPARMS.page_
if ?WINDOWS
        cmp [pWriteDbgConsole], 0
;       test [fStat], FSTAT_ISNT
        jz label1
        cmp edi, offset altcrt
        jz label1
        call setcurposnt
label1:
endif
        mov [edi].CRTPARMS.curpos,ax
        and si,7FFFh
        cmp edi, offset altcrt
        jz sm1
        test fSwap,1
        jz sm1
        cmp word ptr [edi].CRTPARMS.pBuffer+2,000Bh     ;physical screen?
        jz sm2
        jmp exit
@@:
        mov dx, @flat:[__vio_crt_adr]
        mov bh, @flat:[__vio_cols]
        mov si, @flat:[__vio_pagesta]
        movzx ecx,byte ptr @flat:[__vio_page]
sm1:
        cmp edi, offset altcrt
        jnz @F
if ?LOWSETCSR
        jmp sm2
else
        mov bl,al                       ; col -> bl
        mov al,ah                       ; row -> al
        mul bh
        mov bh,00
        add ax,bx                       ; ergibt offset (einfach,ohne attribute)
        shr si,1
        add ax,si
        mov cl,al                       ; first high byte to index 0Eh
        mov al,0eh
        out dx,ax
        mov ah,cl                       ; then low byte to index 0Fh
        mov al,0fh
        out dx,ax
        jmp exit
endif
@@:
        mov @flat:[ecx*2+__vio_curpos0],ax
sm2:
if ?LOWSETCSR
        mov bl,al                       ; col -> bl
        mov al,ah                       ; row -> al
        mul bh
        mov bh,00
        add ax,bx                       ; ergibt offset (einfach,ohne attribute)
        shr si,1
        add ax,si
        mov cl,al                       ; first write high byte to index 0Eh
        mov al,0eh
        out dx,ax
        mov ah,cl                       ; then write low byte to index 0Fh
        mov al,0fh
        out dx,ax
else
        mov bh, cl
        mov dx, ax
        mov ah,2
        @CallInt oldint10
endif
exit:
        ret
VioSetCurPosDir endp

;--- will always get current cursor address from BIOS!

VioGetCurPosDir proc stdcall public uses edi pCrt:ptr CRTPARMS

        mov edi,pCrt
        and edi, edi
        jz getphyspos
        cmp edi, offset altcrt
        jnz getstdcrtpos
        mov dx, [edi].CRTPARMS.crtadr
        mov al, 0eh
        out dx, al
        inc dx
        in  al, dx
        mov ah, al
        dec dx
        mov al, 0fh
        out dx, al
        inc dx
        in al, dx
        mov cl, 80
        div cl
        xchg al, ah
        movzx eax, ax
        jmp sm2

getphyspos:
        movzx ecx, byte ptr @flat:[__vio_page]
        mov ax, @flat:[ecx*2+__vio_curpos0]
        jmp exit
getstdcrtpos:
        mov ax, [edi].CRTPARMS.curpos
        test fSwap,1
        jnz exit
        movzx ecx, [edi].CRTPARMS.page_
        movzx eax, word ptr @flat:[ecx*2+__vio_curpos0]
sm2:
        mov [edi].CRTPARMS.curpos,ax
exit:
        ret

VioGetCurPosDir endp

if 0
VioSetCurType proc stdcall public pCrt: ptr CRTPARMS, line2:dword,line1:dword

        mov edx,pCrt
        mov dx, [edx].CRTPARMS.crtadr
        mov al,0Ah
        mov ah,byte ptr line1
        out dx,ax
        inc al
        mov ah,byte ptr line2
        out dx,ax
        ret
VioSetCurType endp
endif

ife ?WINDOWS

VioShowCursor proc stdcall

if ?LOWSETCSR
        mov dx,@flat:[__vio_crt_adr]
        mov ah,@flat:[485h]
        sub ah,2
        mov al,0Ah
        out dx,ax
        inc al
        inc ah
        out dx,ax
else
        mov cx,@flat:[460h]
        mov ah, 1
        @CallInt oldint10
endif
        ret
VioShowCursor endp

if ?LOWSETPAGE

;*** video text page direkt setzen ***
;*** AL=new page
;--- sets __vio_page and __vio_pagesta

VioSetTextPage proc stdcall uses ebx pCrt:ptr CRTPARMS

        mov ebx, pCrt
        mov @flat:[__vio_page],al 
        mov cx,@flat:[__vio_pagesiz]
        mov ah,00
        mul cx
        mov @flat:[__vio_pagesta],ax
        mov cx,ax
        shr cx,1
        mov dx,@flat:[__vio_crt_adr]
        and ebx, ebx
        jz @F
        mov dx,[ebx].CRTPARMS.crtadr
@@:
;--- set page start

        mov al,0Ch
        mov ah,ch
        out dx,ax
        inc al
        mov ah,cl
        out dx,ax

;--- set cursor pos

        and ebx,ebx
        jz @F
        mov cx,[ebx].CRTPARMS.curpos
        jmp sm1
@@:
        movzx ecx,byte ptr @flat:[__vio_page]
        mov cx,@flat:[ecx*2+__vio_curpos0]
sm1:
        invoke VioSetCurPosDir, ebx, ecx
        ret
VioSetTextPage endp

endif

endif

        end

