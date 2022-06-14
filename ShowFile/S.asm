
	.286
	.model small
	option casemap:none
	option proc:private
	.386

	include bios.inc
	include showfile.inc

?IGNORE1FF equ 1

setcsrpos proto
_hread  proto pascal :word, :ptr far32, :dword
_hwrite proto pascal :word, :ptr far32, :dword

	.data

; Status line

statline  DB "Line: "
lineno    db "        "
statfile  DB 65 dup (" ")
lstatfile equ $ - statfile
          db  13

tFile     db " File: "
ltfile    equ $ - tFile

stathelp  DB "ESC=Quit F2="
stathlpf2 db "Wrap   "
          db "F4=Hex"
          db " F5="
stathlpf5 db "CR2ã "
          db "F9=Print "
statf10   db "        "
          db " Move:   PgUp PgDn Home End"
          db  13

sHexHelp  DB "ESC=Quit  F2="
          db "Edit  "
          db "F4=Ascii  "
statf10x  db "        "
          db "  Move:   PgUp PgDn Home End"
          db  13

altpf2    db "Unwrap"
laltpf2   dw $ - altpf2

altpf5    db "CR2  "
laltpf5   dw $ - altpf5

altpf10   db "F10=Next"
laltpf10  equ $ - altpf10


linenum   DD      1

; Variables for screen handling

cell      LABEL   WORD          ; Cell (character and attribute)
char      DB      " "           ; Initialize to space
attr      DB      ?             ; Attribute

columns   db      80            ; Number of columns
rows      Dw      24            ; Number of rows - status line takes one more
fname     dd      ?
modesaved DB      ?             ; Initial mode
pag       DB      ?             ; Initial display page
newvid    DB      0             ; Video change flag
cga       DB      1             ; CGA flag - default yes
fDispCR   DB      0
vidOfs    dw      0
fMultMode db      0
wCrt		DW	0
vidadr    	DW  0               ; Video buffer address - default CGA
saveptr		dw  0				;selector of buffer to save screen content
bCsrState 	db	0

if ?USEPREDEFSEGS
color     EQU     <__B800H>
mono      EQU     <__B000H>
endif

allattr   label dword
statatr   DB      017h          ; status line Color default
scrnatr   DB      007h          ; screen Color default - white on blue
prefatr   DB      007H          ; prefix
editatr   DB      00Fh

bwstat    EQU     070h          ; B&W default - black on white
bwscrn    EQU     007h          ; B&W default - white on black
bwedit    equ     00Fh

; Variables for buffer and file handling

buffer    LABEL   DWORD
pbuffer   DD      0             ; Position in buffer (offset)
sbuffer   DW      ?             ; Base of buffer (segment)
StartBuffer dd    0				; offset start buffer
fsize     DD      ?             ; File size after dosopen
dpmihdl   dd      ?				; dpmi handle for buffer
savefileexit df   0
fHex      db      1
fLineWrap db      0
fcb1      db      30 dup (?)
fcbname   db      13 dup (?)

; Call table

; Extended key codes
exkeys  DB 71, 72, 73, 79, 80, 81	; home,up,pgup,end,down,pgdn
		DB 60, 62, 63, 67, 68		; F2,F4,F5,F9,F10
		DB 119,132,117,118			; c-home,c-pgup,c-end,c-pgdn
lexkeys EQU $-exkeys				; Table of keys
extable DW homek
		DW upk
		DW pgupk
		DW endk
		DW downk
		DW pgdnk
		DW switchwrp
		DW switchhex
		DW switchmk
		DW printfile
		DW switchfile
		DW homek
		DW pgupk2
		DW endk
		DW pgdnk2
		DW nonek


	.code

ife ?DLL
	public mainproc
endif

getkey proc public
	mov ah, 0
	int 16h
	ret
getkey endp

;*** ax = filehandle ***
;*** es:edx = filename ***

handlefile proc

local fhandle: word

	mov fhandle, ax			; save handle

; Copy file name to status line

	mov ax, es
	push ds
	pop es
	mov di, OFFSET statfile	; Load status line as destination
	mov cx, ltfile
	mov si, offset tFile
	cld
	rep movsb
	mov ds, ax
	mov cx, 65
	mov esi, edx
copy:
	@useext
	lodsb
	stosb
	or al,al
	loopne copy
	dec di
	mov al,13
	stosb
	push es
	pop ds

; Check file size
	mov bx, fhandle
	xor cx, cx
	xor dx, dx
	mov ax,4202h
	int 21h
	push dx
	push ax
	xor dx, dx
	mov ax, 4200h
	int 21h
	pop eax
	mov [fsize],eax
	add eax, 2 				; 2 extra bytes

;--- alloc DPMI memory
	push eax
	pop cx
	pop bx
	mov ax, 501h
	int 31h
	jc main_er2
	mov word ptr [dpmihdl+0], di
	mov word ptr [dpmihdl+2], si

;--- create a selector to access the memory

	push bx
	push cx
	mov cx, 1
	mov ax, 0
	int 31h
	jc main_er4
	mov sbuffer, ax
	mov bx, ax
	pop dx
	pop cx
	mov ax,7		; set base
	int 31h
	jc main_er4
	mov eax, [fsize]
	inc eax
	push eax
	pop dx
	pop cx
	test cx, 0FFF0h	; > 1MB?
	jz @F
	or dx, 0FFFh	; then use page granularity
@@:
	mov ax,8		; set limit
	int 31h
	jc main_er4

;--- read the file in memory block

	mov ax, sbuffer
	xor edx, edx
	invoke _hread, fhandle, ax::edx, fsize
	inc eax
	jz main_er3
	dec eax
	push eax
	call closefile
	pop eax
	mov edi, eax				; Load file length
	push es						; Save ES and load buffer segment
	mov es, sbuffer
								; Set EOF marker
	mov word ptr es:[edi],LF*100h+CR
	pop es
	clc
	ret
main_er2:
	call closefile
	mov ax, 2
	stc
	ret
main_er3:
	call closefile
	mov ax, 3
	stc
	ret
main_er4:
	call closefile
	mov ax, 4
	stc
	ret
closefile:
	mov bx, fhandle
	mov ah, 3Eh
	int 21h
	retn

handlefile endp

freebuffer:
	pusha
	mov bx, [sbuffer]
	mov ax, 0001h
	int 31h
	mov si, word ptr [dpmihdl+2]
	mov di, word ptr [dpmihdl+0]
	mov ax, 0502h
	int 31h
	popa
	retn

if ?DLL
DllEntry:
	and eax, eax
	jz @F
	mov allattr, eax
@@:
	xor eax, eax
	mov pbuffer, eax
	inc eax
	mov linenum, eax
endif

;*** input: es:dx = filename
;***        ah=1 -> wildcard in filename

mainproc:
	push bp
	mov bp, sp
	push 0					; rc=0

	movzx edx, dx
	cld
	mov word ptr fname+0, dx
	mov word ptr fname+2, es
	mov fMultMode, ah
	and ah, ah
	jz mainproc_1
	push es
	push ds
	pop es
	mov si, offset altpf10
	mov di, offset statf10
	mov cx, laltpf10
	rep movsb
	mov si, offset altpf10
	mov di, offset statf10x
	mov cx, laltpf10
	rep movsb
	pop es
	push edx
	mov edx, offset fcb1
	mov ah, 1Ah
	int 21h
	pop edx

	push ds
	xor cx,cx
	push es
	pop ds
	mov ah, 4Eh
	int 21h
	pop ds

	jc main_er1
	mov dx, offset fcbname
	push ds
	pop es
mainproc_1:
	push ds
	push es
	pop ds
	mov ax, 3d20h
	int 21h
	pop ds
	jc main_er1
mainproc_2:
	call handlefile
	jc quit1
	call memoryproc
	jmp quit
quit1:
	mov [bp-2], ax
quit:
	call freebuffer
	jmp @F
main_er1:
	mov byte ptr [bp-2], 1
@@:
	cmp newvid, 1				; Restore video?
	jne thatsall				; No?
	@SetMode modesaved			; Restore video mode, page, and cursor
	@SetPage pag
thatsall:
	mov ax, [bp-2]
	mov sp, bp
	pop bp
	ret

if ?DLL
DllEntry2:						; entry "show memory"
	mov [fsize],eax 		; maximaler offset
	mov [StartBuffer], ecx	; minimaler offset
	mov [sbuffer], dx		; segment
	cmp ebx, eax
	jb @F
	mov ebx, ecx
@@:
	cmp ebx, ecx
	jnb @F
	mov ebx, ecx
@@:
	mov [pbuffer], ebx		; aktueller zeiger
	mov eax,1
	mov linenum, eax
	mov di, offset statfile
	push ds
	pop es
	mov cx, lstatfile
	mov al, ' '
	rep stosb
	xor eax, eax
	mov fname, eax
endif

memoryproc:
							; set video mode
	call vidcheck

							; Display first page
	xor eax,eax 			; Start at 0
	push eax
	call pager
nextkey:					;*** programm loop ***
	call getkey
nextkey2:
	cmp al,0				; Is it a null?
	jne normal
	mov al,ah
	push es
	push ds					; Load DS into ES
	pop es
	mov di,OFFSET exkeys	; Load address and length of key list
	mov cx,lexkeys+1
	repne scasb				; Find position
	pop es
	sub di,(OFFSET exkeys)+1; Point to key
	shl di,1				; Adjust pointer for word addresses
	call extable[di] 		; Call procedure
	jmp nextkey
normal:
	cmp al,27				; Is it ESCAPE?
	jne nextkey 			; No? Ignore unknown command
exit:
	call setcsron
	call restorescreen
	ret

;*** key commands ***

homek:
	mov eax, StartBuffer
	mov pbuffer, eax 		; HOME - set position to 0
	push eax
	mov linenum, 1
	call pager
	retn

upk:
	mov eax, -1				; UP - scroll back 1 line
	push eax
	call pager
	retn

pgupk:
	mov ax, rows 			; PGUP - Page back
	movzx eax, ax
	neg eax
	push eax
	call pager
	retn

pgupk2:
	mov ax, rows 			; C-PGUP - Page back
	shr ax, 1
	movzx eax, ax
	neg eax
	push eax
	call pager
	retn

endk:
	mov eax,fsize			; END - Get last byte of file
	mov pbuffer,eax 		; Make it the file position
	mov linenum,-1			; Set illegal line number as flag
	mov ax,rows 			; Page back
	movzx eax,ax
	neg eax
	push eax
	call pager
	retn

downk:
	mov eax, 1				; DOWN - scroll forward 1 line
	push eax
	call pager
	retn

pgdnk:
	mov ax, rows
	movzx eax, ax
	push eax 				; PGDN - page forward
	call pager
	retn

pgdnk2:
	mov ax, rows
	shr ax, 1
	movzx eax,ax
	push eax 				; C-PGDN
	call pager
	retn

switchmk:
	test byte ptr fHex, 1
	jnz @F
	xor byte ptr fDispCR, 1
	mov si, offset stathlpf5
	mov di, offset altpf5
	mov cx, laltpf5
	call switchtxt
	mov eax, 0
	push eax
	call pager
@@:
	retn
printfile:
	push ds
	mov ecx,fsize
	jecxz printfile_ex
;	int 3
	xor esi,esi
	mov ds,sbuffer
if ?IGNORE1FF
	cmp byte ptr [esi],0Ch	; FF am anfang ignorieren
	jnz @F
	lodsb
	jmp printfile_2
endif
@@:
	@useext
	lodsb
	mov dl, al
	mov ah, 05
	int 21h
printfile_2:
	@useext
	loop @B
	cmp dl, 0Ch
	jz printfile_ex
	mov dl, 0Ch
	mov ah, 05h
	int 21h
printfile_ex:
	pop ds
	retn
switchfile:
	cmp byte ptr fMultMode, 0
	jz switchfile_ex
	mov ah, 4Fh
	int 21h
	jnc @F
	mov edx, offset fcb1
	push ds
	lds dx, fname
	mov cx, 0
	mov ah, 4Eh
	int 21h
	pop ds
	jc switchfile_ex
@@:
	mov dx, offset fcbname
	mov ax, 3D00h + 40h
	int 21h
	jc switchfile_ex
	call freebuffer
	push ds
	pop es
	call handlefile
	jc switchfile_er
	xor eax, eax
	mov pbuffer, eax
	push eax
	call pager
switchfile_ex:
	retn
switchfile_er:
	pop ax
	retn
switchhex:
	xor byte ptr fHex,1
	mov eax,0
	push eax
	call pager
	retn
switchwrp:
	test byte ptr fHex, 1
	jnz edit
	xor byte ptr fLineWrap, 1
	mov si, offset stathlpf2
	mov di, offset altpf2
	mov cx, laltpf2
	call switchtxt
	mov eax, 0
	push eax
	call pager
	retn
edit:
	test byte ptr [fHex], 1
	jz @F
	call setcsron
	call _edit
	call setcsroff
	push dword ptr 0
	call pager
@@:
	retn

if 1
savefile proc public
	mov eax, fname
	or eax, eax
	jz savefile_er0

	push ds
	lds dx, fname
	movzx edx, dx
	xor cx, cx
	mov ah, 3Ch
	int 21h
	pop ds
	jc savefile_er

	push ax
	mov bx, ax
	mov cx, sbuffer
	xor edx, edx
	invoke _hwrite, bx, cx::edx, fsize
	pop bx
	push eax
	mov ah, 3Eh
	int 21h
	pop eax
savefile_er:
	retn

savefile_er0:
	mov ax, word ptr savefileexit+4
	and ax, ax
	jz @F

	mov dx,rows 			; Load last row and first column
	inc dx
	xchg dl,dh
	mov cx,dx				; Make row the same
	mov dl,columns
	dec dl
	@Scroll 0

	mov dh,byte ptr rows
	inc dh
	mov dl,00
	call setcsron
	call fword ptr savefileexit
	call setcsroff
@@:
	retn
savefile endp
endif


nonek:
	retn					; Ignore unknown key

switchtxt:
	mov al, [si]
	xchg al, [di]
	mov [si], al
	inc si
	inc di
	loop switchtxt
	retn

; Adjust for current mode and and video adapter

vidcheck proc near uses es

	mov cx, __0040H
	mov es, cx
	cmp wCrt, 0 			; columns, rows, vidOfs must be set
	jnz usedefaults
if 1
	call isEGA				; EGA (or VGA)?
	or ax,ax				; If 0 must be CGA or MA
	je @F					; Leave default
	mov rows, ax 			; Load rows
	dec cga 				; Not CGA
@@:
	@GetMode				; Get video mode
	mov modesaved, al		; Save initial mode and page
	mov pag, bh
endif
	mov ax, es:[63h]
	mov wCrt, ax
	mov bx, es:[4Eh] 		; aktuelle Startadresse der Page
	mov ah, es:[4Ah] 		; spalten
;	mov dl, es:[49h] 		; mode
	mov cl, es:[84h] 		; zeilen
	mov ch, 0
	mov columns, ah			; spalten
	dec cx					; letzte zeile reservieren
	mov rows, cx
	mov vidOfs, bx
usedefaults:
	cmp wCrt, 03B4h
	mov ax, color
	jne graphchk			; No? Check graphics
	mov statatr, bwstat		; Set B&W defaults for status line
	mov scrnatr, bwscrn		;   and screen background
	mov prefatr, bwscrn
	mov editatr, bwedit
	mov ax, mono
graphchk:					; color
	mov vidadr, ax			; Load mono address

	call getcsrstate
	call setcsroff
	call savescreen
	ret
vidcheck endp

savescreen proc uses ds
	mov al,byte ptr rows
	inc al
	inc al
	mov ah,columns
	mul ah
	inc ax
	mov cx, ax
	add ax,ax
	movzx ebx,ax
	shr ebx,4
	inc bx
	mov ah,48h
	int 21h
	jc ss_exit
	mov saveptr,ax
	mov es, ax
	mov si, vidOfs
	mov ds, vidadr
	xor di, di
	mov ax, cx
	stosw
	rep movsw
ss_exit:
	ret
savescreen endp

restorescreen proc
	mov bx, saveptr
	and bx, bx
	jz rs_exit
	mov es, vidadr
	mov di, vidOfs
	xor si, si
	push ds
	mov ds, bx
	lodsw
	mov cx, ax
	rep movsw
	pop ds
	mov es, saveptr
	mov ah, 49h
	int 21h
rs_exit:
	ret
restorescreen endp

getcsrstate proc
	mov dx, wCrt
	mov al, 0Ah
	out dx, al
	inc dx
	in al, dx
	mov bCsrState, al
	ret
getcsrstate endp

setcsroff proc
	mov dx, wCrt
	mov ah, bCsrState
	and ah, 9Fh
	or ah, 20h
	mov al, 0Ah
	out dx, ax
	ret
setcsroff endp

setcsron proc
	mov dx, wCrt
	mov ah, bCsrState
	mov al, 0Ah
	out dx, ax
	ret
setcsron endp

;--- set csr pos in cx, bl

setcsrpos proc public

	mov ah, cl
	mov al, columns
	mul ah
	movzx cx, ch
	add ax, cx
	mov cx, vidOfs
	shr cx, 1
	add ax, cx
	mov cl, al
	mov dx, wCrt

	mov al, 0Eh
	out dx, ax
	inc al
	mov ah, cl
	out dx, ax

	ret
setcsrpos endp

	END

