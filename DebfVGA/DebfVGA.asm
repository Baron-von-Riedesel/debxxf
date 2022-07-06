
;--- handle debuggee in graphic mode with standard VGA BIOS
;--- this code is called by both the 16 and 32bit version
;--- of the debugger, so stack may be 16 or 32bit too!

	.386
	.MODEL SMALL
	option proc:private
	option casemap:none

?USEBIOSSAVE	equ 1	; 1=use BIOS to save/restore video register
						; 0=save/restore video register by hand
?USEBIOSTEXT	equ 0	; 1=use BIOS to set debugger's text mode
						; 0=save/load video register
?USEBIOSGRAPH	equ 0	; 1=use BIOS to set debuggee's graphic mode
						; 0=save/load video register
?SCREENOFF		equ 1	; 1=turn off screen during memory transfer
?LOADFONTS		equ 1	; 1=load font by hand into map 2

;----------------------------------------------------------------

if ?USEBIOSTEXT
?SAVEFULL		equ 1	;1=save all 4 planes (256 kB)
else
?SAVEFULL		equ 0	;0=save 8kB of plane 2 and 16 kB text page
endif

if ?SAVEFULL
?PLANE01SIZE	equ 10000h
?PLANE2SIZE		equ 10000h
?PLANE3SIZE		equ 10000h
?TEXTPAGESIZE	equ 0h
else
?PLANE01SIZE	equ 0h
?PLANE2SIZE		equ 2000h
?PLANE3SIZE		equ 0
?TEXTPAGESIZE	equ 4000h	;just save content of text page
endif
?TOTALSIZE		equ ?PLANE01SIZE * 2 + ?PLANE2SIZE + ?PLANE3SIZE + ?TEXTPAGESIZE

ife ?USEBIOSSAVE
  if ?USEBIOSGRAPH
?NUMDACREGS		equ 100h
  else
?NUMDACREGS		equ 40h	;if video mode isnt set thru BIOS, only 64 DACS
						;are destroyed
  endif

?NUMATTRREGS	equ 15h	;attribute controller registers to save

?SAVEREGSIZE	equ ?NUMDACREGS * 3 + 100h
endif

DGROUP group _TEXT, _DATA		;put everything in one segment

	.data

__csalias		dd 0
__flatsel		dd 0	;comes from debugger, dont free it
pDbgeeSaveMem	dd 0	;linear address save buffer
dwHdlDbgeeSave	dd 0	;DPMI handle save buffer
oldint10		df 0
if ?LOADFONTS
dwFont8x8		dd 0
dwFont8x14		dd 0
dwFont8x16		dd 0
savedregs		db 12 dup (?)
endif
bIs32			db 0    ;1=running as 32bit client
		align 4
                
ife ?USEBIOSTEXT
pDbgerSaveState	dd 0	;linear address debugger's text mode save state
dwHdlDbgerSave	dd 0	;DPMI handle
else
wVideoMode		dw 83h	;text video mode to set for debugger
endif

if ?USEBIOSGRAPH
clVMode			db 0	;saved debuggee's video mode
endif

if ?USEBIOSSAVE
		align 4
dwSaveBufSel	dd 0	;selector save state buffer
dwSaveBufSize	dd 0	;size in bytes of save state buffer
oldint31		df 0
wSaveBufSeg		dw 0	;segment save state buffer
else
bCols			db 0	;saved BIOS 449h text cols
bRows			db 0	;saved BIOS 484h text rows-1
bCharHeight		db 0	;saved BIOS 485h

graphconst label dword
wCRTport dd 3D4h,19h	;3d4h/3b4H: CRT controller
	dd 3C4h,5		;3c4h: sequencer
	dd 3CEh,9		;3ceh: graphic controller
	dd 0

	.data?
DGROUP	group _BSS

DbgeeSaveRegs	db ?SAVEREGSIZE dup (?)
  ife ?USEBIOSTEXT
DbgerSaveRegs	db ?SAVEREGSIZE dup (?)
  endif
endif
	.code
        
_Int10:
prefix1 label byte
	nop
	pushfd
prefix2 label byte
	nop
	call [oldint10]
	ret
if ?USEBIOSSAVE        
_Int31:					;--- call saved int 31h vector
prefix3 label byte
	nop
	pushfd
prefix4 label byte
	nop
	call [oldint31]
	ret
endif

if ?USEBIOSSAVE

RMCS    struct          ;real mode call structure
rEDI    dd ?            ;0
rESI    dd ?            ;4
rEBP    dd ?            ;8
RESERVE dd ?            ;12
rEBX    dd ?            ;16
rEDX    dd ?            ;20
rECX    dd ?            ;24
rEAX    dd ?            ;28
rFlags  dw ?
rES     dw ?
rDS     dw ?
rFS     dw ?
rGS     dw ?
rIP     dw ?
rCS     dw ?
rSP     dw ?
rSS     dw ?
RMCS    ends

	.data

	align 4

rmcs RMCS <>        

	.code

;--- call VGA save/restore state
;--- inp: DL=00/01/02
        
vga1c proc        
	mov edi, offset rmcs
	xor ecx, ecx
	mov dword ptr [edi].RMCS.rSP, ecx
	mov ax, wSaveBufSeg
	mov [edi].RMCS.rES, ax
	mov [edi].RMCS.rFlags, 202h
	mov word ptr [edi].RMCS.rECX, 0111b	; save/restore hw, bios, colors
	mov word ptr [edi].RMCS.rEBX, 0000h
	mov dh, 1Ch
	mov word ptr [edi].RMCS.rEAX, dx
	mov bx, 0010h
	mov ax, 300h
	call _Int31
	jc error
	cmp byte ptr [edi].RMCS.rEAX,1Ch	;supported?
	jnz error
	mov ebx, [edi].RMCS.rEBX
	clc
	ret
error:
	stc
	ret
vga1c endp

endif

ife ?USEBIOSSAVE

;--- save attribute controler register (15h bytes)

saveattr proc stdcall
	mov edx, wCRTport	;3B4/3D4
	add dl, 6			;3BA/3DA
	in al, dx
	mov dl, 0C0h		;3C0
	mov ecx, ?NUMATTRREGS
	mov ah, 00
@@:
	mov al, ah
	out dx, al
	inc edx
	in al, dx
	stosb
	dec edx
	out dx, al		;another out to select index register
	inc ah
	loop @B
	mov al, 20h
	out dx, al
	ret
saveattr endp

;*** load attribute controler register ***

loadattr proc stdcall
	mov edx, wCRTport
	add dl, 6			;3BA/3DA
	in al,dx
	mov dl, 0C0h		;3C0
	mov ecx, ?NUMATTRREGS
	mov ah, 0
@@:
	mov al, ah
	out dx, al
	lodsb
	out dx, al
	inc ah
	loop @B
	mov al, 20h
	out dx, al
	ret
loadattr endp

;--- save DAC registers (300h bytes)
;--- 3C7: palette address (read mode)
;--- 3C9: palette data register
;--- 3C6: pel mask

savedac proc stdcall
	mov dx, 3C7h
	mov al, 0
	out dx, al		;puts DAC in read mode
	inc edx
	inc edx
	mov ecx, ?NUMDACREGS	;now all registers can be read 
@@:
	in al, dx
	stosb
	in al, dx
	stosb
	in al, dx
	stosb
	loop @B
	ret
savedac endp

;--- load DAC registers (300h bytes)
;--- 3C8: palette address (write mode)

loaddac proc stdcall
	mov dx, 3C8h
	mov al, 0
	out dx, al
	inc edx
	mov ecx, ?NUMDACREGS
@@:
	lodsb
	out dx, al
	lodsb
	out dx, al
	lodsb
	out dx, al
	loop @B
	ret
loaddac endp

;--- save vga register state
;--- input: EDI = save buffer
;--- after this call EDI will point to save buffer

savegraphregister proc stdcall

	pushfd
	cli
	cld

;--- save misc output register 

	mov edx, 3cch
	in al, dx
	stosb
	mov dl,0B4h
	test al,1
	jz @F
	mov dl,0D4h
@@:        
	mov wCRTport, edx
	mov esi, offset graphconst
nextitem:
	lodsd
	and eax, eax
	jz exit
	mov edx, eax
	lodsd
	mov ecx, eax
	mov ah, 0
	in al, dx
	stosb				;save index register itself
@@:
	mov al, ah
	out dx, al
	inc edx
	in al, dx
	stosb
	dec edx
	inc ah
	loop @B
	jmp nextitem
exit:
	call saveattr
	call savedac
	pop eax
	test ah, 2
	jz @F
	sti
@@:        
	ret
        
savegraphregister endp

;--- restore vga register state
;--- input: ESI = save buffer
;--- after this call ESI will point to save buffer

loadgraphregister proc stdcall

	pushfd
	cli
	cld

;--- restore misc output register 

	lodsb
	mov dx, 3c2h
	out dx, al

	mov dl, 0B4h
	test al, 1
	jz @F
	mov dl, 0D4h
@@:        
	mov wCRTport, edx
        
	mov al, 11h
	out dx, al
	inc edx
	in al, dx
	and al, 7Fh				 ;clear write protection for 0-7 CRT
	out dx, al
        
	mov edi, esi

	mov esi, offset graphconst
nextitem:
	lodsd
	and eax, eax
	jz exit
	mov edx, eax
	lodsd
	mov ecx, eax
	mov al, [edi]
	inc edi
	push eax
	mov al, 0
@@:
	mov ah, [edi]
	out dx, ax
	inc edi
	inc al
	loop @B
	pop eax
	out dx, al			;restore the index register itself
	jmp nextitem
exit:
	mov esi, edi
	call loadattr
	call loaddac
	pop eax
	test ah, 2
	jz @F
	sti
@@:
	ret
        
loadgraphregister endp

endif	;!?USEBIOSSAVE

;*** set planes: changes eax edx  ***

setplane0 proc
	mov ah, 0
setplane::
	mov dx, 3ceh
	mov al, 4		;"read map select"
	out dx, ax
	mov dl, 0c4h
	mov cl, ah
	mov ah, 1
	shl ah, cl
	mov al, 2		;"map mask" register:select map to write to
	out dx, ax
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
        
;---  setevenmode: changes edx, eax

setevenmode proc
	mov dx, 3ceh
	mov ax, 1		;"enable set/reset" register
	out dx, ax
	mov al, 5		;"graphics mode" register
	out dx, al
	inc edx
	in al, dx
	and al, 084h 	;reset "odd/even mode", set "read mode 0"
	out dx, al		;set "write mode" 0

	mov dl, 0ceh
	mov al, 6		;"miscellaneous" register
	out dx, al
	inc edx
	in al, dx
	and al, 0F0h	;reset "odd/even"
	or al, 5		;set addr=A0000h,64K, set graphic mode
	out dx, al

	mov dl, 0ceh
	mov al, 8		;"bit mask" register: all bits to change 
	mov ah, 0FFh
	out dx, ax

	mov dl, 0c4h
	mov al, 4
	out dx, al
	inc edx
	in al, dx
	and al, not 8	;reset "chain 4"
	or al, 4+2		;set odd/even=4, set extended memory=2	
	out dx, al
	ret
setevenmode endp

;*** save video memory ***
;--- inp: EDI = save buffer

savevidmem proc stdcall uses ds es

	cld
	mov es, [__flatsel]
	mov ds, [__flatsel]
	call setevenmode
if ?PLANE01SIZE
	call setplane0				;character plane
	mov esi, 0A0000h
	mov ecx, ?PLANE01SIZE/4
	rep movsd
	call setplane1				;attribute plane
	mov esi, 0A0000h
	mov ecx, ?PLANE01SIZE/4
	rep movsd
endif
	call setplane2
	mov esi, 0A0000h 			;charset plane
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

;--- (re)load video memory
;--- inp: ESI=pBuffer

loadvidmem proc stdcall uses ds es

	cld
	mov es, [__flatsel]
	mov ds, [__flatsel]
	call setevenmode
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
AllocDbgeeSaveMem	endp        

;--- the vesa save buffer resides in conv. memory, but
;--- we need 2 buffers ( for debuggee and debugger state )
;--- so the buffer contents must be swapped.

SwapSaveBuf proc  uses ds es      

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

if ?LOADFONTS

savetab label word
	dw 3c4h
	db 2, 4, -1
	dw 3ceh
	db 1, 4, 5, 6, 8, -1
	dw -1

saveregs proc
	lea esi, savetab
nextitem:
	lodsw
	mov edx, eax
	cmp ax, -1
	jz done
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
nextitem:
	lodsw
	mov edx, eax
	cmp ax, -1
	jz done
nextitem2:
	lodsb
	cmp al, -1
	jz nextitem
	mov ah, [edi]
	out dx, ax
	inc edi
	jmp nextitem2
done:
	ret
loadregs endp


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

SaveLoadTextPage proc uses ds es
	mov cl, al
	mov edi, pDbgeeSaveMem
	add edi, ?PLANE2SIZE + ?PLANE3SIZE
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
;	mov ecx, ?TEXTPAGESIZE/4
	mov ecx, ds:[44Ch]
	shr ecx, 2
	rep movsd
	ret
SaveLoadTextPage endp

endif

;--- set text mode for debugger

SetDebuggerVideoMode proc far stdcall public uses ds es esi edi

	mov ds, cs:[__csalias]
	push ds
	pop es
ife ?USEBIOSTEXT
  ife ?USEBIOSSAVE
	mov esi, offset DbgerSaveRegs
	invoke loadgraphregister
	push es
	mov es,[__flatsel]
	mov al,bCols
	mov es:[44Ah],al
	mov al,bRows
	mov es:[484h],al
	mov al,bCharHeight
	mov es:[485h],al
  else
	call SwapSaveBuf
	mov dl, 2				;restore state
	call vga1c
	call SwapSaveBuf
  endif
	push ds
	mov ds, [__flatsel]
	mov cl, ds:[485h]
	pop ds
  if ?LOADFONTS
	mov eax, dwFont8x14
	cmp cl,14
	jz isok
	mov eax, dwFont8x8
	cmp cl,8
	jz isok
	mov eax, dwFont8x16
isok:
	call LoadFontInPlane2
  else
	mov al,1
	cmp cl,14
	jz isok
	mov al,2
	cmp cl,8
	jz isok
	mov al,4
isok:
	mov ah,11h			;load charset 0
	mov bl,0
	call _Int10
  endif
else
	mov ax,wVideoMode
	cmp ah,00
	jz isnormalmode
	mov ebx,eax
	or bh, 80h			; dont clear video memory
	mov ax,4f02h		; to be fixed: this is a VESA call!
isnormalmode:		 
	call _Int10
endif
if ?TEXTPAGESIZE
	mov al,0
	call SaveLoadTextPage
endif
	ret

SetDebuggerVideoMode endp

;--- save debuggee graphic video state

SaveGraphicScreen proc far stdcall public uses ds es

	mov ds, cs:[__csalias]
	push ds
	pop es
ife ?USEBIOSSAVE
	mov edi, offset DbgeeSaveRegs
	invoke	savegraphregister			;save register
	push ds
	mov ds, [__flatsel]
	mov ax, ds:[449h]
	stosw								;EDI -> save buffer
  if ?USEBIOSGRAPH
	mov clVMode, al
  endif
	mov ax, ds:[450h]
	stosw
	mov ax, ds:[484h]
	stosw
	pop ds
else
	mov dl, 1			;save VGA state
	call vga1c
endif
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
	invoke savevidmem
@@:  
if ?SCREENOFF
	pop eax
	mov ah, al
	mov dx, 3c4h
	mov al, 1
	out dx, ax
endif
	ret

SaveGraphicScreen endp

;--- restore debuggee graphic video state

RestoreGraphicScreen proc far stdcall public uses ds es

	mov ds, cs:[__csalias]
	push ds
	pop es
if ?USEBIOSGRAPH
	mov al,[clVMode]
	or al,80h
	mov ah,00
	call _Int10
endif
if ?SCREENOFF
	mov dx,3c4h
	mov al,1
	out dx,al
	inc dx
	in al,dx
	or al,20h
	out dx,al
endif
if ?TEXTPAGESIZE
	mov al,1
	call SaveLoadTextPage
endif
	mov esi, pDbgeeSaveMem
	and esi, esi
	jz @F
	invoke loadvidmem				;reload saved video memory
@@:
ife ?USEBIOSSAVE
	mov esi, offset DbgeeSaveRegs
	invoke	loadgraphregister
	mov es,[__flatsel]
	lodsw							;ESI -> save buffer
	mov es:[449h],ax
	lodsw
	mov es:[450h],ax
	lodsw
	mov es:[484h],ax
else
	mov dl,02				;load VGA state
	call vga1c
endif
	ret

RestoreGraphicScreen endp        

;--- save debugger's video state to avoid BIOS mode set call

InitDebuggerVideoState proc stdcall uses ds es

ife ?USEBIOSTEXT
  ife ?USEBIOSSAVE
	mov edi, offset DbgerSaveRegs
	invoke	savegraphregister
	mov es,[__flatsel]
	mov al, es:[44Ah]
	mov bCols,al
	mov ax, es:[484h]
	mov bRows,al
	mov bCharHeight,ah
  else
	cmp pDbgerSaveState,0
	jnz @F
	mov bx, word ptr dwSaveBufSize+2
	mov cx, word ptr dwSaveBufSize+0
	mov ax, 501h
	int 31h
	jc exit
	mov word ptr dwHdlDbgerSave+0,di
	mov word ptr dwHdlDbgerSave+2,si
	mov word ptr pDbgerSaveState+0,cx
	mov word ptr pDbgerSaveState+2,bx
@@:
;	call SwapSaveBuf
	mov dl,1			;save state
	call vga1c
	call SwapSaveBuf
exit:
  endif
endif
	ret
InitDebuggerVideoState endp

if ?LOADFONTS

;--- get linear addresses of fonts 8x8, 8x14, 8x16

fontparms label byte
	db 2
	dd offset dwFont8x14
	db 3
	dd offset dwFont8x8
	db 6
	dd offset dwFont8x16

GetFonts proc uses esi
	mov ecx, 3
	mov esi, offset fontparms
nextitem:
	push ecx
	lodsb
	mov edi, offset rmcs
	mov dword ptr [edi].RMCS.rSP,0
	mov byte ptr [edi].RMCS.rEBX+1,al		;8x14
	mov word ptr [edi].RMCS.rEAX,1130h
	mov bx,0010h
	mov cx,0000h
	mov ax, 300h
	call _Int31
	movzx edx, [edi].RMCS.rES
	shl edx, 4
	movzx ecx, word ptr [edi].RMCS.rEBP
	add edx, ecx
	lodsd
	mov [eax], edx
	pop ecx
	loop nextitem
	clc
error:
	ret
GetFonts endp

endif

if ?USEBIOSSAVE

;--- get a realmode buffer to save VGA state

GetSaveStateBuffer proc

	mov dl,0				;get size of VGA save/restore buffer
	call vga1c
	jc exit
	movzx eax,bx
	shl eax, 6
	mov dwSaveBufSize, eax	;size in Bytes
	shl bx, 2				;convert 64 byte blocks to paragraphs
	mov ax, 100h
	int 31h
	jc exit
	mov dwSaveBufSel, edx
	mov wSaveBufSeg, ax
exit:
	ret
GetSaveStateBuffer endp        

endif

;--- register API:
;--- ecx = flatsel
;--- dl = bIs32

VideoInit proc far stdcall public uses ds es ebx edi

	mov ebx, cs
	mov ax, 000Ah
	int 31h
	mov ds, eax
	assume	ds:DGROUP
	push ds
	pop es
	mov [__csalias], eax
	mov [__flatsel], ecx
	mov bIs32, dl
	mov ax, 204h
	mov bl, 10h
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
if ?USEBIOSSAVE
	mov ax, 204h
	mov bl, 31h
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
;--------------------------- get VGA save/restore state buffer
;--------------------------- in DOS conventional memory
	call GetSaveStateBuffer
	jc exit
endif
	call AllocDbgeeSaveMem
	jc exit
ife ?USEBIOSTEXT
	call InitDebuggerVideoState
  if ?LOADFONTS
	call GetFonts
  endif
endif
exit:
	ret
VideoInit endp

VideoDone proc far stdcall public uses esi edi

	push dword ptr cs:[dwHdlDbgeeSave]
	pop di
	pop si
	mov ax, 502h
	int 31h
ife ?USEBIOSTEXT
	push cs:[dwHdlDbgerSave]
	pop di
	pop si
	mov ax, 502h
	int 31h
endif
if ?USEBIOSSAVE
;-------------------------------- VGA state save buffer (low memory)		
	mov edx, cs:[dwSaveBufSel]
	mov ax, 101h
	int 31h
endif
	mov ebx,cs:[__csalias]
	mov ax,1
	int 31h
	ret
VideoDone endp

;--- LibEntry

LibEntry proc far
	mov ax,1
	db 66h		;LibEntry is called as far16 proc!
	ret
LibEntry endp

	end LibEntry
