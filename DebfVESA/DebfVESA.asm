
;--- handle debuggee in graphic mode with VESA BIOS help
;--- this code is called by both the 16 and 32bit versions
;--- of the debugger, so stack may be 16 or 32bit!


	.386
	.MODEL SMALL

	option proc:private
	option casemap:none

?BIOSMODESET	equ 0	;0=save/load debuggers text mode state
						;1=use BIOS to set debugger's text mode
?SCREENOFF		equ 1	;1=turn off screen during memory transfer
?LOADFONTS		equ 1	;load fonts by hand into map 2
?VDDCALL		equ 0	;call int 2F, ax=4000h/4007h

;----------------------------------------------------------------

if ?BIOSMODESET
?SAVEFULL		equ 1	;1=save all 4 planes (256 kB)
						;0=save 16kB of 0 + 1, 8kB of 2, nothing of 3
else
?SAVEFULL		equ 0
endif

if ?SAVEFULL eq 1
?PLANE01SIZE	equ 10000h
?PLANE2SIZE		equ 10000h
?PLANE3SIZE		equ 10000h
?TEXTPAGESIZE	equ 0h
else
?PLANE01SIZE	equ 0h
?PLANE2SIZE		equ 2000h	;1 font only
?PLANE3SIZE		equ 0
?TEXTPAGESIZE	equ 4000h	;just save content of text page
endif
?TOTALSIZE		equ ?PLANE01SIZE * 2 + ?PLANE2SIZE + ?PLANE3SIZE + ?TEXTPAGESIZE

;----------------------------------------------------------------

;*** DPMI realmode call structure

RMCS    struct   ; real mode call structure
rEDI    dd ?     ; 0
rESI    dd ?     ; 4
rEBP    dd ?     ; 8
RESERVE dd ?     ; 12
rEBX    dd ?     ; 16
rEDX    dd ?     ; 20
rECX    dd ?     ; 24
rEAX    dd ?     ; 28
rFlags  dw ?
rES     dw ?
rDS     dw ?
rFS     dw ?
rGS     dw ?
rIP     dw ?
rCS     dw ?
rSSSP   dd ?
RMCS    ends

;----------------------------------------------------------------

DGROUP group _TEXT, _DATA		;put everything in one segment

	.data

__csalias		dd 0	;code alias = DGROUP
__flatsel		dd 0	;comes from debugger, dont free it
pDbgeeSaveMem	dd 0 	;linear address save buffer
dwHdlDbgeeSave	dd 0	;DPMI handle save buffer
dwSaveBufSel	dd 0	;selector save state buffer
dwSaveBufSize	dd 0	;size in bytes of save state buffer
wSaveBufSeg		dw 0	;segment save state buffer
	align 4
oldint10		df 0
oldint31		df 0
if ?LOADFONTS
dwFont8x8		dd 0
dwFont8x14		dd 0
dwFont8x16		dd 0
savedregs		db 12 dup (?)
endif
ife ?BIOSMODESET
pDbgerSaveState	dd 0 	;linear address debugger's text mode save state
dwHdlDbgerSave	dd 0	;DPMI handle
else
wVideoMode		dw 83h	;text video mode to set for debugger screen
endif
bIs32			db 0    ;1=running as 32bit client

	.data?

rmcs RMCS <?>        

;----------------------------------------------------------------

	.code

_Int10:					;--- call saved int 10h vector
prefix1 label byte
	nop
	pushfd
prefix2 label byte
	nop
	call [oldint10]
	ret
_Int31:					;--- call saved int 31h vector
prefix3 label byte
	nop
	pushfd
prefix4 label byte
	nop
	call [oldint31]
	ret

;----------------------------------------------------------------
;--- call VESA function 4F04
;--- DL=subfunction
;---   00=get size of buffer
;---   01=save state (requires wSaveBugSeg)
;---   02=restore state (requires wSaveBugSeg)
;----------------------------------------------------------------

vesa04 proc uses edi
	push ds
	pop es
	xor ecx, ecx
	mov edi, offset rmcs
	mov [edi].RMCS.rSSSP, ecx
	mov ax, wSaveBufSeg
	mov [edi].RMCS.rES, ax
	mov [edi].RMCS.rFlags, 202h
	mov byte ptr [edi].RMCS.rEDX, dl
	mov word ptr [edi].RMCS.rECX, 000Fh	; bit 0-3 set (all state info)
	mov word ptr [edi].RMCS.rEBX, 0000h
	mov word ptr [edi].RMCS.rEAX, 4F04h
	mov bx, 0010h
	mov ax, 300h
	call _Int31
	jc error
	cmp byte ptr [edi].RMCS.rEAX,4Fh	;supported?
	jnz error
	mov ebx, [edi].RMCS.rEBX
	clc
	ret
error:
	stc
	ret
vesa04 endp

;--- set VGA planes: changes eax edx
;--- set register: 3CE: 4
;--- set register: 3C4: 2

setplane0 proc
	mov ah,0
setplane::
	mov dx,3ceh
	mov al,4		;"read map select"
	out dx,ax
	mov dl,0c4h
	mov cl,ah
	mov ah,1
	shl ah,cl
	mov al,2		;"map mask" register:select map to write to
	out dx,ax
	ret
setplane0 endp

setplane1:
	mov ah, 1
	jmp setplane
setplane2:
	mov ah, 2
	jmp setplane
if ?PLANE3SIZE
setplane3:
	mov ah, 3
	jmp setplane
endif
        
;--- setevenmode: changes edx, eax
;--- sets register: 3CE: 1, 5, 6, 8
;--- sets register: 3C4: 4

setevenmode proc
	mov dx,3ceh
	mov ax,1	; "enable set/reset" register
	out dx,ax
        
	mov al,5	; "graphics mode" register
	out dx,al
	inc edx
	in al,dx
	and al,084h ; reset "odd/even mode", set "read mode 0"
	out dx,al	; set "write mode" 0

	mov dl,0ceh
	mov al,6	; "miscellaneous" register
	out dx,al
	inc edx
	in al,dx
	and al,0F0h ; reset "odd/even"
	or al,5		; set addr=A0000h,64K, graphics mode
	out dx,al

	mov dl,0ceh
	mov al,8	; "bit mask" register: all bits to change 
	mov ah,0FFh
	out dx,ax

	mov dl,0c4h
	mov al,4
	out dx,al
	inc edx
	in al,dx
	and al,not 8; reset "chain 4"
	or al,4+2	; set odd/even=4, set extended memory=2	
	out dx,al
	ret
setevenmode endp

;--- save parts of debuggee's video memory
;--- inp: EDI = save buffer
;--- called by SaveGraphicScreen()

savevidmem proc stdcall uses ds es esi edi

	call setevenmode		; ensures video memory is addressed via A0000h
	mov es, [__flatsel]
	mov ds, [__flatsel]
	cld
if ?PLANE01SIZE        
	call setplane0			; character plane
	mov esi, 0A0000h
	mov ecx, ?PLANE01SIZE/4
	rep movsd
	call setplane1			; attribute plane
	mov esi, 0A0000h
	mov ecx, ?PLANE01SIZE/4
	rep movsd
endif        
	call setplane2			; charset plane
	mov esi, 0A0000h
	mov ecx, ?PLANE2SIZE/4
	rep movsd
if ?PLANE3SIZE
	call setplane3
	mov esi, 0A0000h
	mov ecx, ?PLANE3SIZE/4
	rep movsd
endif        
	ret
savevidmem endp

;--- (re)load parts of debuggee's video memory
;--- inp: ESI=pBuffer
;--- called by RestoreGraphicScreen()

loadvidmem proc stdcall uses ds es esi edi

	call setevenmode		; ensures video memory is addressed via A0000h
	mov es, [__flatsel]
	mov ds, [__flatsel]
	cld
if ?PLANE01SIZE        
	call setplane0
	mov edi, 0A0000h
	mov ecx, ?PLANE01SIZE/4
	rep movsd
	call setplane1
	mov edi, 0A0000h
	mov ecx, ?PLANE01SIZE/4
	rep movsd
endif        
	call setplane2
	mov edi, 0A0000h
	mov ecx, ?PLANE2SIZE/4
	rep movsd
if ?PLANE3SIZE
	call setplane3
	mov edi, 0A0000h
	mov ecx, ?PLANE3SIZE/4
	rep movsd
endif        
	ret
loadvidmem endp

;--- alloc debuggee's video memory save buffer
;--- it contains only the parts of the video memory which
;--- will be modified by the debugger!
;--- this consists mainly of some parts of the 4 64kB VGA planes
;--- called by VideoInit()

AllocDbgeeSaveMem proc

	mov bx, HIGHWORD ?TOTALSIZE
	mov cx, LOWWORD ?TOTALSIZE
	mov ax, 501h
	int 31h
	jc @F
	mov word ptr dwHdlDbgeeSave+0, di
	mov word ptr dwHdlDbgeeSave+2, si
	push bx
	push cx
	pop eax
	mov pDbgeeSaveMem, eax
@@:        
	ret
AllocDbgeeSaveMem endp

ife ?BIOSMODESET

;--- we have allocated 1 vesa save state buffer in conv. memory
;--- but we use it to save debuggee's and debugger's video state
;--- so we have to swap contents
;--- called by SetDebuggerVideoMode() & InitDebuggerVideoState()

SwapSaveBuf proc uses ds es esi edi

	mov edi, pDbgerSaveState
	mov ecx, dwSaveBufSize
	jecxz exit
	mov es, [__flatsel]
	mov ds, dwSaveBufSel
	xor esi, esi
	shr ecx, 2				;size is a multiple of 64!
@@:        
	mov eax, [esi]
	mov edx, es:[edi]
	mov es:[edi], eax
	mov [esi], edx
	add esi, 4
	add edi, 4
	dec ecx
	jnz @B
exit:
	ret
SwapSaveBuf endp

endif

if ?LOADFONTS

;--- when loading a font in plane 2, we have to save/restore
;--- some VGA registers

savetab label word
	dw 3c4h
	db 2, 4, -1
	dw 3ceh
	db 1, 4, 5, 6, 8, -1
	dw -1

saveregs proc
	lea esi, savetab
	jmp nextitem3
nextitem:
	mov al, cl		;restore index register
	out dx, al
nextitem3:
	lodsw
	mov edx, eax
	cmp ax, -1
	jz done
	in al, dx
	mov cl, al
nextitem2:
	lodsb
	cmp al, -1
	jz nextitem
	out dx, al
	inc edx
	in al, dx
	stosb
	dec edx
	jmp nextitem2
done:
	ret
saveregs endp

loadregs proc
	lea esi, savetab
	jmp nextitem3
nextitem:
	mov al,cl		;restore index register
	out dx,al
nextitem3:
	lodsw
	mov edx, eax
	cmp ax,-1
	jz done
	in al,dx
	mov cl,al
nextitem2:
	lodsb
	cmp al,-1
	jz nextitem
	mov ah, [edi]
	out dx,ax
	inc edi
	jmp nextitem2
done:
	ret
loadregs endp

;--- called by SetDebuggerVideoMode()
;--- inp: eax=flat ROM font ptr
;--- inp: cl=char size

LoadFontInPlane2 proc uses esi edi ebx

	cld
	push eax
	push ecx
	mov edi, offset savedregs
	call saveregs
	call setevenmode
	call setplane2
	pop ecx
	pop esi
	mov edi, 0A0000h
	push ds
	push es
	mov es, [__flatsel]
	mov ds, [__flatsel]
	mov dl, 00h
	mov dh, cl
nextchar:
	movzx ecx, dh
	lea ebx, [edi+20h]
	rep movsb
	mov edi, ebx
	inc dl
	jnz nextchar
	pop es
	pop ds
	mov edi, offset savedregs
	call loadregs
	ret
LoadFontInPlane2 endp

endif

if ?TEXTPAGESIZE

;--- save/load text page (instead of plane 0+1)
;--- called by RestoreGraphicsScreen()
;--- and SetDebuggerVideoMode()
;--- AL=0 save text page, AL=1 restore text page

CopyTextPage proc uses ds es
	mov cl,al
	mov edi, pDbgeeSaveMem
	add edi, ?PLANE2SIZE + ?PLANE3SIZE	;planes 0+1 can be ignored
	mov es, [__flatsel]
	mov ds, [__flatsel]
	mov esi, 0B0000h
	mov dx, 3ceh
	mov al, 6
	out dx, al
	inc edx
	in al, dx
	test al, 04h
	jz @F
	mov si, 8000h
@@:
	test cl, 1
	jz @F
	xchg esi, edi
@@:
	mov ecx, ?TEXTPAGESIZE/4
	rep movsd
	ret
CopyTextPage endp

endif

;--- set text mode for debugger
;--- this proc is usually called after SaveGraphicScreen() has been called

SetDebuggerVideoMode proc far stdcall public uses ds es esi edi

	mov ds, cs:[__csalias]
	push ds
	pop es
if ?BIOSMODESET
	mov bx, wVideoMode
	or bh, 80h			; dont clear video memory
	mov ax, 4f02h
	call _Int10
else
	call SwapSaveBuf
	mov dl, 2			; restore state
	call vesa04			; restore debugger's text mode
	call SwapSaveBuf
	push ds
	mov ds, [__flatsel]
	mov cl, ds:[485h]	; get character scan lines
	pop ds
  if ?LOADFONTS
	mov eax, dwFont8x14
	cmp cl, 14
	jz isok
	mov eax, dwFont8x8
	cmp cl, 8
	jz isok
	mov eax, dwFont8x16
isok:
	call LoadFontInPlane2
  else
	mov al, 1
	cmp cl, 14
	jz isok
	mov al, 2
	cmp cl, 8
	jz isok
	mov al, 4
isok:
	mov ah, 11h
	mov bl, 0
	call _Int10				; ax=1101h/1102h/1104h
  endif
endif 
if ?TEXTPAGESIZE
	mov al, 0				; save content of text page
	call CopyTextPage
endif
	ret

SetDebuggerVideoMode endp

;--- save debuggee graphic video state
;--- returns save buffer in EAX

SaveGraphicScreen proc far stdcall public uses ds es edi

	mov ds, cs:[__csalias]
	push ds
	pop es
if ?VDDCALL
	mov ax, 4000h
	int 2Fh
endif
	mov dl,01			; save VESA state
	call vesa04
if ?SCREENOFF
	mov dx, 3c4h
	mov al, 1
	out dx, al
	inc dx
	in al, dx
	push eax
	or al, 20h
	out dx, al
endif
	mov edi, pDbgeeSaveMem
	and edi, edi
	jz @F
	mov bx, 0
	mov dx, 0
	mov ax, 4f05h
	call _Int10
	invoke savevidmem	; calls setevenmode(), which sets buffer to 0A0000h!
@@:  
if ?SCREENOFF
	pop eax
	mov ah, al
	mov dx, 3c4h
	mov al, 1
	out dx, ax
endif
if ?VDDCALL
	mov ax, 4007h
	int 2Fh
endif
	mov eax,pDbgeeSaveMem
	mov edx,?TOTALSIZE
	ret

SaveGraphicScreen endp

;--- restore debuggee graphic video state
;--- changes edi

RestoreGraphicScreen proc far stdcall public uses ds es esi

	mov ds, cs:[__csalias]
	push ds
	pop es
if ?VDDCALL
	mov ax, 4000h
	int 2Fh
endif
if ?SCREENOFF
	mov dx, 3c4h
	mov al, 1
	out dx, al
	inc dx
	in al, dx
	or al, 20h
	out dx, al
endif
if ?TEXTPAGESIZE
	mov al, 1				; restore text page (must be called while still in text mode)
	call CopyTextPage
endif
	mov esi, pDbgeeSaveMem
	and esi, esi
	jz @F
	invoke loadvidmem		; calls setevenmode(), which sets buffer to 0A0000h!
@@:

	mov dl, 02				; restore VESA state
	call vesa04

if ?VDDCALL
	mov ax, 4007h
	int 2Fh
endif
	ret

RestoreGraphicScreen endp        

;--- save debugger's video state to avoid BIOS mode set call
;--- if debuggee is in graphics mode.
;--- called by VideoInit()

InitDebuggerVideoState proc stdcall

ife ?BIOSMODESET
	cmp pDbgerSaveState,0
	jnz @F
	mov cx, word ptr dwSaveBufSize+0
	mov bx, word ptr dwSaveBufSize+2
	mov ax, 501h
	int 31h
	jc @F
	mov word ptr dwHdlDbgerSave+0, di
	mov word ptr dwHdlDbgerSave+2, si
	mov word ptr pDbgerSaveState+0, cx
	mov word ptr pDbgerSaveState+2, bx
;	call SwapSaveBuf
	mov dl, 1		; save mode state
	call vesa04
	call SwapSaveBuf; copy state from conv to ext memory
@@:
endif
	ret
InitDebuggerVideoState endp

if ?LOADFONTS

;--- get linear addresses of fonts 8x8, 8x14, 8x16
;--- calls INT 10h, AX=1130h, BH=XX
;--- int 10h returns font in ES:BP

	.data
fontparms label byte
	db 2
	dd offset dwFont8x14
	db 3
	dd offset dwFont8x8
	db 6
	dd offset dwFont8x16
lfontsize equ ($ - offset fontparms) / 5
	.code

GetFonts proc uses esi edi

	mov ecx, lfontsize
	mov esi, offset fontparms
nextitem:
	push ecx
	mov al,[esi]
	xor ecx, ecx
	mov edi, offset rmcs
	mov [edi].RMCS.rSSSP, ecx
	mov [edi].RMCS.rFlags, 202h
	mov [edi].RMCS.rES, cx
	mov [edi].RMCS.rEBP, ecx
	mov byte ptr [edi].RMCS.rEBX+1, al		;8x14
	mov word ptr [edi].RMCS.rEAX, 1130h
	mov bx, 10h
	mov ax, 300h
	int 31h
	jc @F
	movzx edx, [edi].RMCS.rES
	shl edx, 4
	movzx ecx, word ptr [edi].RMCS.rEBP
	add edx, ecx
	mov eax, [esi+1]
	mov [eax], edx
@@:
	add esi, 4+1
	pop ecx
	loop nextitem
	clc
	ret
GetFonts endp

endif

;--- get a realmode buffer to save VESA state

GetSaveStateBuffer proc

	mov dl, 0				; get size of buffer
	call vesa04
	jc exit
	movzx eax, bx
	shl eax, 6				; 64 byte blocks -> bytes
	mov dwSaveBufSize, eax	; size in Bytes
	shl bx, 2				; convert 64 byte blocks to paragraphs
	mov ax, 100h
	int 31h
	jc exit
	mov dwSaveBufSel, edx
	mov wSaveBufSeg, ax
exit:
	ret
GetSaveStateBuffer endp

;-------------------------------------------------------------
;--- alloc all resources
;--- inp: ecx = flat selector, use it but do not release
;--- inp: dl = 1 if client runs as 32bit, 0 = 16bit
;-------------------------------------------------------------

VideoInit proc far stdcall public uses ds es ebx edi

	mov ebx, cs
	mov ax, 000Ah	; get CS alias
	int 31h
	jc exit
	mov ds, eax
	assume ds:DGROUP
	push ds
	pop es
	mov [__csalias], eax
	mov [__flatsel], ecx
	mov bIs32, dl
;--------------------------- get INT 10 vector and save it, because
;--------------------------- it cannot be used later when a debuggee is 
;--------------------------- active and has control over all INTs
	mov bl, 10h
	mov ax, 204h
	int 31h
	test byte ptr bIs32, 1
	jz @F
	mov dword ptr oldint10+0, edx
	mov word ptr oldint10+4, cx
	jmp int10saved
@@:
	mov word ptr oldint10+0, dx
	mov word ptr oldint10+2, cx
	mov al, 66h
	mov prefix1, al
	mov prefix2, al
int10saved:
;--------------------------- get INT 31 vector as well
	mov bl, 31h
	mov ax, 204h
	int 31h
	test byte ptr bIs32, 1
	jz @F
	mov dword ptr oldint31+0, edx
	mov word ptr oldint31+4, cx
	jmp int31saved
@@:
	mov word ptr oldint31+0, dx
	mov word ptr oldint31+2, cx
	mov al, 66h
	mov prefix3, al
	mov prefix4, al
int31saved:
;--------------------------- get VESA save/restore state buffer
;--------------------------- in DOS conventional memory
	call GetSaveStateBuffer
	jc exit
;--------------------------- get video memory save buffer (flat address)
	call AllocDbgeeSaveMem	; modifies esi, edi
	jc exit
;--------------------------- if debugger's text video mode shouldn't
;--------------------------- be set by BIOS, save debuggers state here
ife ?BIOSMODESET
;--------------------------- if charset is load manually, get the font ptrs
  if ?LOADFONTS
	call GetFonts
  endif
	mov edx, 3CEh
	in al, dx
	mov cl, al
	mov al, 6
	out dx, al
	inc edx
	in al, dx
	mov ch, al
	dec edx
	mov al, cl
	out dx, al
	push ecx
	test ch, 1
	jz @F
;--- debugger has been launched in graphics mode
	call SaveGraphicScreen
	mov ax, 4F02h
	mov bx, 0083h
	int 10h
@@:
	call InitDebuggerVideoState
	pop ecx
	test ch,1
	jz @F
	call RestoreGraphicScreen
@@:
endif
exit:
	ret
VideoInit endp

;-------------------------------------------------------------
;--- clear all resources
;-------------------------------------------------------------

VideoDone proc far stdcall public uses esi edi

;-------------------------------- debuggee video memory save buffer
	push cs:[dwHdlDbgeeSave]
	pop di
	pop si
	mov ax, 502h
	int 31h
ife ?BIOSMODESET
	push cs:[dwHdlDbgerSave]
	pop di
	pop si
	mov ax, 502h
	int 31h
endif
;-------------------------------- VESA state save buffer (low memory)
	mov edx, cs:[dwSaveBufSel]
	mov ax, 101h
	int 31h
;-------------------------------- DGROUP selector
	mov ebx, cs:[__csalias]
	mov ax, 1
	int 31h
	ret
VideoDone endp

;--- LibEntry

LibEntry proc far
	mov ax, 1
	db 66h			; LibEntry is called as far16 proc!
	ret
LibEntry endp

	end LibEntry

