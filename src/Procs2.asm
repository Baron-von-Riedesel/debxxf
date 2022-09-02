
	.386
if ?FLAT
	.MODEL FLAT
else
	.MODEL TINY
endif
;	option proc:private
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
	include rmdbghlp.inc
if ?WINDOWS
	include winnt.inc
endif
	include extern32.inc
	include extern16.inc

;GetEnvironmentVariableA proto stdcall :ptr BYTE, :ptr BYTE, :DWORD
memcpy proto stdcall :ptr, :ptr, :DWORD

if ?WINDOWS

VMEXEC struct
pifflags dd ?		;00 DWORD (pif)-flags
					;	0=exclusive
					;	1=background
dispflg  dd ?		;04 DWORD display flags
					;	0=emulate text mode
					;	1=monitor text port
					;	2=monitor low graphics port
					;	3=monitor high graphics port
pgmpath df ?		;08 FWORD pgmpath
pgmparm df ?		;14 FWORD parameter
curdir	df ?		;20 FWORD curdir
mainmir dw ?		;26 main mem min
mainmax dw ?		;28 main mem max
fgprior dw ?		;30 foreground priority
bgprior dw ?		;32 background priority
emsmax	dw ?		;34 maximum EMS
emsmin	dw ?		;36 minimum EMS
xmsmax	dw ?		;38 maximum XMS
xmsmin	dw ?		;40 minimum XMS
res1	dw ?
res2	dw ?
VMEXEC ends

endif

	.data

outmodefn dd 0			;filename OUTMFN
inpmodefn dd 0			;filename INPMFN

fcprocadr df 0			;funktionsadresse _farcall
fctmpvar  dd 0			;stackpointer _farcall
fcType	  db 0			;adresstyp _farcall (FPTR/LPTR)
lastint	  db 0

dwPageAddr	dd 0
dwPageLines dd 10h

deebp	  dd 0			;fuer RS

filllength db 1 		;fuer _fill

	.code

;------ rs drive:byte, sector:dword,numsectos:dword,addr:ADDR

_readsecs proc c pb:PARMBLK

local	dehandle:dword
local	deaddr:dword
local	deaddrs:dword
local	delaenge:dword
local	dedrive:dword
local	packetv:DISKIO

	call SaveState
	jc readsecs_erx

	xor eax, eax
	mov dehandle, eax
	lea edi, packetv
	mov eax, pb.dwOffs1			  ;drive
	cmp eax, 100h
	jb @F
	@errorout ERR_INVALID_DRIVE_SPEC
	jmp readsecs_er
@@:
	mov dedrive, eax
	mov eax, pb.dwOffs2			; sector
	mov [edi.DISKIO.startsec], eax
	mov eax, pb.p3.dwOffs		; sectors
	cmp eax, 10000h
	jb @F
	@errorout ERR_INVALID_SECTORS
	jmp readsecs_er
@@:
	mov [edi.DISKIO.sectors],ax
	shl eax,9					; sectors -> bytes
	mov delaenge,eax
	and eax,eax
	jnz @F
	@errorout ERR_INVALID_SECTORS
	jmp readsecs_er
@@:
	mov 	ecx,pb.p4.dwOffs	; address
	mov 	deaddr,ecx
	mov 	ecx,pb.p4.dwSeg
	and		ecx,ecx
	jnz		@F
	mov		ecx,[r1.rDS]
@@:
	mov 	deaddrs,ecx
	cmp 	pb.p4.bType, __VOID__	;parm 4 angegeben?
	jnz 	readsecs_1
	push	eax							;falls nicht, block ueber DPMI allokieren
	pop 	cx
	pop 	bx
	mov 	ax,0501h
	@DpmiCall
	jnc 	@F
	@errorout ERR_MEMORY_ERROR
	jmp 	readsecs_er
@@:
	push	si
	push	di
	pop		dehandle
	push	bx
	push	cx
	pop		deaddr
	mov 	eax,[__flatsel]
	mov 	deaddrs,eax
readsecs_1:
	invoke	getbaser,deaddrs
	add 	eax,deaddr
if ?32BIT eq 0
	invoke	setworkselbase,eax
	mov 	eax,delaenge
	dec 	eax
	invoke	setworksellimit,eax
else
	mov 	deaddr,eax
endif
	invoke	lockdrive,dedrive
@@:
	lea 	ebx,packetv
if ?32BIT
	mov 	eax,deaddr
	mov 	[ebx.DISKIO.buffofs],eax
	mov 	eax,[__flatsel]
else
	xor 	eax,eax
	mov 	[ebx.DISKIO.buffofs],ax
	mov 	eax,[worksel]
endif
	mov 	[ebx.DISKIO.buffseg],ax
	mov 	al,byte ptr dedrive 		;0=A,1=B usw.
	mov 	ecx,-1
	int 	25h
	@adjint25sp
	jnc 	@F

	mov 	ax,7305h					;vielleicht ists ein FAT32 drive
	lea 	ebx,packetv
	mov 	dl,byte ptr dedrive
	inc 	dl							;hier wird ab 1 gezaehlt
	mov 	si,0						;read sectors
	mov 	ecx,-1
	@DosCall
	jnc 	@F

	push	eax
	@errorout ERR_DISK_READ
	pop		ecx							;adjust esp
	jmp 	readsecs_er
@@:
	cmp 	pb.p4.bType, __VOID__
	jz		@F
	push	pb.p3.dwOffs
	@errorout MSG_SECTORS_READ
	pop		ecx							;adjust esp
	jmp 	readsecs_exx
@@:
	mov 	eax,offset desave
	mov 	dword ptr [savefileexit+0],eax
	mov 	word ptr [savefileexit+4],cs
	mov 	[deebp],ebp
	push	__DWORD__ shl 16
	push	0
	push	delaenge
	push	__FPTR__ shl 16
if ?32BIT
	push	[__flatsel]
	push	deaddr
else
	push	[worksel]
	push	0
endif
	push	2				   ;2 argumente
	call	_type
	add 	esp,7*4

	invoke	unlockdrive,dedrive
	xor 	eax,eax
	mov 	dword ptr [savefileexit+0],eax
	mov 	word ptr [savefileexit+4],ax
readsecs_ex:
	mov 	eax,dehandle
	and 	eax,eax
	jz		readsecs_er
	push	eax
	pop 	di
	pop 	si
	mov 	ax,0502h
	@DpmiCall
readsecs_er:
readsecs_exx:
	call	LoadState
readsecs_erx:
	ret

desave:
	pushad
	push	ds
	push	es
	mov 	ds,cs:[__csalias]
	push	ds
	pop 	es
	mov 	ebp,[deebp]
	call SaveState
	jc		desave_ex

	mov 	al,byte ptr dedrive
	lea 	ebx,packetv
	mov 	ecx,-1
	int 	26h
	@adjint25sp
	jnc 	@F

	mov 	ax,7305h					;vielleicht ists ein FAT32 drive
	lea 	ebx,packetv
	mov 	dl,byte ptr dedrive
	inc 	dl							;hier wird ab 1 gezaehlt
	mov 	si,6001h					;write sectors
	mov 	ecx,-1
	@DosCall
	jnc 	@F

	push	eax
	@errorout ERR_DISK_WRITE
	pop		ecx							;adjust esp
	call	_getchar
@@:
	invoke	_crout
	call	LoadState
desave_ex:
	pop 	es
	pop 	ds
	popad
	retf

_readsecs endp

;------ ws drive:byte, sector:dword,numsectos:dword,addr:ADDR

_writesecs proc c pb:PARMBLK

local	deaddr:dword
local	deaddrs:dword
local	delaenge:dword
local	dedrive:dword
local	packetv[12]:byte

	call SaveState
	jc		writesecs_erx

	lea 	edi,packetv
	mov 	eax,pb.dwOffs1			; drive
	cmp 	eax,100h
	jb		@F
	@errorout ERR_INVALID_DRIVE_SPEC
	jmp 	writesecs_er
@@:
	mov 	dedrive,eax
	mov 	eax,pb.p2.dwOffs		; sector
	mov 	[edi.DISKIO.startsec],eax
	mov 	eax,pb.p3.dwOffs		; sectors
	cmp 	eax,10000h
	jb		@F
	@errorout ERR_INVALID_SECTORS
	jmp 	writesecs_er
@@:
	mov 	[edi.DISKIO.sectors],ax
	shl 	eax,9				   ;sectors -> bytes
	mov 	delaenge,eax
	and 	eax,eax
	jnz 	@F
	@errorout ERR_INVALID_SECTORS
	jmp 	writesecs_er
@@:
	mov 	eax,pb.p4.dwOffs
	mov 	deaddr,eax
	mov 	eax,pb.p4.dwSeg
	mov 	deaddrs,eax

	invoke	getbaser,deaddrs
	add 	eax,deaddr
if ?32BIT
	mov 	deaddr,eax
else
	invoke	setworkselbase,eax
	mov 	eax,delaenge
	dec 	eax
	invoke	setworksellimit,eax
endif
;	invoke	lockdrive,dedrive

	lea 	ebx,packetv
if ?32BIT
	mov 	eax,deaddr
	mov 	[ebx.DISKIO.buffofs],eax
	mov 	eax,[__flatsel]
else
	xor 	eax,eax
	mov 	[ebx.DISKIO.buffofs],ax
	mov 	eax,[worksel]
endif
	mov 	[ebx.DISKIO.buffseg],ax

	mov 	al,byte ptr dedrive
	mov 	ecx,-1
	int 	26h
	@adjint25sp
	jnc 	@F

	mov 	ax,7305h					;may be a FAT32 drive?
	lea 	ebx,packetv
	mov 	dl,byte ptr dedrive
	inc 	dl							;hier wird ab 1 gezaehlt
	mov 	si,6001h					;write sectors
	mov 	ecx,-1
	@DosCall
	jnc 	@F

	push	eax
	@errorout ERR_DISK_WRITE
	pop		ecx							;adjust esp
	jmp 	writesecs_er
@@:
	push	pb.p3.dwOffs
	@errorout MSG_SECTORS_WRITE
	pop		ecx							;adjust esp
	invoke	unlockdrive,dedrive
writesecs_er:
	call	LoadState
writesecs_erx:
	ret
_writesecs endp

?LOCKPARM = 4	;lock level (win98 only permits 0 and 4)

lockdrive proc stdcall drive:dword

	mov bl,byte ptr drive		  ;lock volume
	inc bl
	mov bh, ?LOCKPARM
	mov ax, 440Dh
	mov cx, 084Ah
	mov dx, 0001h
	@DosCall
	jnc @F
if ?LOCKPARM
	mov ax, 440Dh
	mov bh, 00
	@DosCall
	jnc @F
endif
	movzx eax,ax
	push eax
	@errorout WRN_VOLUME_CANNOT_BE_LOCKED
	pop eax
@@:
	ret
lockdrive endp

unlockdrive proc stdcall drive:dword

	mov bl,byte ptr drive		  ;unlock volume
	inc bl
	mov bh, ?LOCKPARM
	mov ax, 440Dh
	mov cx, 086Ah
	mov dx, 0001h
	@DosCall
	jnc @F
if ?LOCKPARM
	mov ax,440Dh
	mov bh,00
	@DosCall
	jnc @F
endif
	@errorout WRN_VOLUME_CANNOT_BE_UNLOCKED
@@:
	ret
unlockdrive endp

_lockdrive proc c pb:PARMBLK

	invoke lockdrive,pb.dwOffs1
	ret
_lockdrive endp

_unlockdrive proc c pb:PARMBLK

	invoke unlockdrive,pb.dwOffs1
	ret
_unlockdrive endp


;*** return: ebx -> iobitmap, eax=limit ***

getiopbitmap proc stdcall

	movzx eax,[rTR]
	invoke getbaser,eax
	and eax,eax
	jnz @F
	@errorout ERR_GETTING_TSS_BASE
	stc
	ret
@@:
	push eax
	mov bx,[rTR]
	call getlimitr
	pop ebx
	add eax,ebx				;max for io bitmap
	mov cx,@flat:[ebx+66h]
	movzx ecx,cx
	add ebx,ecx
	ret
getiopbitmap endp

_iop proc c pb:PARMBLK

local	ebxmax:dword

	call	getiopbitmap
	jc		iop_ex
	mov 	ebxmax,eax
	mov 	edx,pb.dwOffs1	;now ebx has flat addr of io bitmap
	movzx	ecx,dl
	and 	cl,07h
	shr 	edx,3
	add 	ebx,edx
	cmp		ebx, ebxmax
	ja		iop_er
	cmp 	byte ptr pb.wArgc,2
	jnb 	iop_2
	bt		@flat:[ebx],ecx
	setc	al
	movzx	eax,al
	invoke printf, CStr("permission bit for port %X=%X",lf), pb.dwOffs1, eax
	jmp 	iop_ex
iop_2:
	mov 	eax,pb.dwOffs2
	cmp 	eax,1
	ja		iop_er
	jz		iop_3
	btr 	@flat:[ebx],ecx
	jmp 	iop_ex
iop_3:
	bts 	@flat:[ebx],ecx
	jmp 	iop_ex
iop_er:
	@errorout ERR_VALUE_EXCEEDS_TSS_LIMIT
iop_ex:
	ret
_iop endp

;--- display IO-bitmap

_iorestr proc c pb:PARMBLK

local	ebxmax:dword

	call	getiopbitmap
	jc		iorestr_ex
	mov 	ebxmax,eax
	mov 	edx,pb.dwOffs1
	shr 	edx,5
	add 	ebx,edx
	shl 	edx,5
	push	edx
	invoke printf, CStr("lin. Addr  Port   01234567 89ABCDEF 01234567 89ABCDEF",lf)
	invoke printchars, '-', eax, 1
	pop		edx
iorestr1:
	cmp 	ebx,ebxmax
	ja		iorestr_ex

	mov 	eax,@flat:[ebx]
	pushad
	invoke printf, CStr("[%08X] %04X: "), ebx, edx
	popad
	@mov 	ecx,32
	add 	edx,ecx
	add 	ebx,4
	pushad
iorestr2:
	test	cl,07
	jnz 	@F
	@putchr ' '
@@:
	shr 	eax,1
	jnc 	@F
	@putchr '*'
	jmp 	iorestr3
@@:
	@putchr '.'
iorestr3:
	loop	iorestr2
	invoke	_crout
	popad
	and 	dx,dx
	jnz 	iorestr1
iorestr_ex:
	ret
_iorestr endp

;--- TSS and DT cmds
;--- display current TSS

_tssout proc c pb:PARMBLK

	mov eax, ebx
	cmp cl, 1
	jnc @F
	pushad
	mov esi, offset tritem
	call regsout_2
	popad
	movzx eax, [rTR]
	invoke getbaser, eax
	jnc @F
	@errorout ERR_GETTING_TSS_BASE
	jmp notss
@@:
	mov ebx, eax
	@mov ecx, 68h/4
	mov esi, ebx
	mov edi, [pNearHeap]
	push ds
	mov ds, [__flatsel]
	rep movsd
	pop ds
	call regsout_tss
notss:
	ret
_tssout endp

;--- EPM cmd: display exception vectors (DPMI ax=0202h)
;--- syntax: EPM [exc#],[debuggercontext]

_excpm proc c pb:PARMBLK

local	anz:byte
local	excnr:dword

	cmp cl, __VOID__
	mov ecx, 1
	jnz @F
	@mov ebx, 0
	mov ecx, 20h
@@:
	cmp bl, 20h
	jnb excpmex
	mov bh, 00
	mov excnr, ebx
	mov anz, 00
excpm1:
	push ecx
	mov ebx, excnr
	mov ax, 0202h
	.if pb.dwOffs2
		@DpmiCall
	.else
		int 31h
	.endif
	mov al,' '
	jnc @F
	mov al,'*'
@@:
	movzx ecx, cx
	movzx eax, al
if ?32BIT
	invoke printf, CStr("%4X %4X:%08X%c "), excnr, ecx, edx, eax
else
	movzx edx,dx
	invoke printf, CStr("%4X %4X:%04X%c "), excnr, ecx, edx, eax
endif
	test anz,1
	jz @F
	invoke _crout
@@:
	inc anz
	inc excnr
	pop ecx
	loop excpm1
excpmex:
	ret
_excpm endp

;--- IPM cmd
;--- display interrupt vectors (DPMI + DOS) 

_intpm proc c pb:PARMBLK

local	anz:dword
local	intnr:dword

	cmp cl,__VOID__
	jnz @F
	xor ebx, ebx
@@:
	movzx ebx,bl
	mov intnr,ebx
	@mov ecx,20h
	mov dword ptr anz,0
nextitem:
	push ecx
	mov bl, byte ptr intnr
	mov ax, 204h
	.if (pb.dwOffs2)
		@DpmiCall
	.else
		int 31h
	.endif
	jnc @F
	mov eax, CStr("!")
	jmp intpm_1
@@:
	lar eax, ecx
	mov eax, CStr(" ")
	jz intpm_1
	mov eax, CStr("*")
intpm_1:
	movzx ecx,cx
if ?32BIT
	invoke printf, CStr("%4X %4X:%08X %s "), intnr, ecx, edx, eax
else
	movzx edx,dx
	invoke printf, CStr("%4X %4X:%04X %s "), intnr, ecx, edx, eax
endif
	push es
	mov al,byte ptr intnr
	mov ah,35h
	.if (pb.dwOffs2)
		@DosCall
	.else
		int 21h
	.endif
	mov eax, es
	movzx eax, ax
	pop es
if ?32BIT
	invoke printf, CStr("(%4X:%08X) "), eax, ebx
else
	movzx ebx,bx
	invoke printf, CStr("(%4X:%04X) "), eax, ebx
endif
intpm_2:
	test byte ptr anz,1
	jz @F
	invoke _crout
@@:
	inc anz
	inc intnr
	pop ecx
	dec ecx
	jnz nextitem
	ret
_intpm endp

;--- IPMS cmd

_setintpm proc c pb:PARMBLK

	cmp pb.p1.bType, __CONST__
	jnz setintpm_er
	cmp pb.p2.bType, __FPTR__
	jnz setintpm_er
	mov ebx, pb.dwOffs1
	cmp ebx, 100h
	jnb setintpm_er
	mov edx, pb.dwOffs2
	mov ecx, pb.wSeg2
	mov ax, 205h
	@DpmiCall
	jnc setintpm_ex
	@errorout ERR_FROM_DPMI
	jmp setintpm_ex
setintpm_er:
	@errorout ERR_WRONG_PARAM
setintpm_ex:
	ret
_setintpm endp

;--- IRM cmd

_intrm proc c pb:PARMBLK		; display rm interrupt vectors

local	anz:dword
local	intnr:dword

	cmp cl, __VOID__
	jnz @F
	xor ebx, ebx
@@:
	movzx ebx,bl
	mov intnr, ebx
	@mov ecx, 20h
	mov word ptr anz,0000
intrm1:
	push ecx
	mov bl,byte ptr intnr
	mov ax, 200h
	@DpmiCall
	jnc @F
	@putchr '*'
@@:
	mov ebx, intnr
if ?FLAT        
	movzx eax, word ptr cs:[ebx*4]
	mov bx, cs:[ebx*4+2]
else
	movzx eax, word ptr @flat:[ebx*4]
	mov bx, @flat:[ebx*4+2]
endif        
	movzx ecx,cx
	movzx edx,dx
	invoke printf, CStr("%4X %4X:%04X (%4X:%04X)  "), intnr, ecx, edx, ebx, eax
	test dword ptr anz,1
	jz @F
	invoke  _crout
@@:
	inc dword ptr anz
	inc dword ptr intnr
	pop ecx
	loop intrm1
	ret
_intrm endp

;--- IRMS cmd

_setintrm proc c pb:PARMBLK

	cmp pb.p1.bType, __CONST__
	jnz setintrm_er
	cmp pb.p2.bType, __RMLPTR__
	jnz setintrm_er
	mov ebx, pb.dwOffs1
	cmp ebx, 100h
	jnb setintrm_er
	mov edx, pb.dwOffs2
	mov ecx, pb.wSeg2
	mov ax, 201h
	@DpmiCall
	jnc setintrm_ex
	@errorout ERR_FROM_DPMI
	jmp setintrm_ex
setintrm_er:
	@errorout ERR_WRONG_PARAM
setintrm_ex:
	ret
_setintrm endp

	.const
        
meminfcap label byte
	db "                   Pages(hex) Pages(dec)    kB(dec)",lf
	db "---------------------------------------------------",lf
	db 0
        
meminf label byte
	db "max. block           %8X %10d %10d",lf,0
SIZEMEMINF equ $ - meminf
	db "max. block, unlocked %8X %10d %10d",lf,0
	db "max. block, lockable %8X %10d %10d",lf,0
	db "linear address space %8X %10d %10d",lf,0
	db "unlocked phys.memory %8X %10d %10d",lf,0
	db "free phys. memory    %8X %10d %10d",lf,0
	db "total phys. memory   %8X %10d %10d",lf,0
	db "free address space   %8X %10d %10d",lf,0
	db "swap file size       %8X %10d %10d",lf,0

	.code

;--- function MEMinfo 
;--- display DPMI memory info

_meminfo proc c pb:PARMBLK

local membuf[72]:byte

	invoke printf, addr meminfcap
	lea edi, membuf
	mov ax, 500h
	@DpmiCall
	mov ecx, 9
	lea esi, meminf
nextitem:
	push ecx
	mov eax,[edi]
	cmp ecx, 9
	jnz @F
	shr  eax, 10
	push eax
	shr eax, 2
	push eax
	push eax
	jmp mi_1
@@:
	shl eax, 2
	push eax 						;kB
	push [edi]						;Pages
	push [edi]						;pages
mi_1:
	push esi
	call printf
	add esp, 4*4
	add esi, SIZEMEMINF
	add edi, 4
	pop ecx
	loop nextitem
	ret
_meminfo endp

;--- DPMI function

_dpmiinfo proc c pb:PARMBLK

if ?FLAT
local   szVendor[128]:byte
endif

	mov ebx,offset dpmivartab
	mov eax,offset dpmivartabend
	mov ch,0
	call symtout
if ?FLAT
	lea edi, szVendor
	mov byte ptr [edi+2],0
	mov ax, 0401h
	@DpmiCall
	jc exit
	movzx ecx, ax
	invoke printf, CStr("0401 capabilities: %X, vendor: %s",lf), ecx, addr szVendor+2
exit:
endif
	ret
_dpmiinfo endp

teststrictmode:
	test [fMode], FMODE_STRICT
	jnz @F
	clc
	ret
@@:
	@errorout ERR_NOT_IN_STRICT_MODE
	stc
	ret

;--- IDT and DI cmds
;--- display IDT

_idtout proc c pb:PARMBLK

	pushad
	mov esi, offset idtitem
	call regsout_2
	popad

	call teststrictmode
	jc exit
	cmp cl, __VOID__
	jnz @F
	movzx ebx, [lastint]
@@:
	shl ebx, 3
	invoke printf, CStr("Int Address       Type",lf)
	add eax, 24
	invoke printchars, '-', eax, 1
	mov ecx, 10h
	cmp pb.p2.bType, __CONST__
	jnz @F
	mov ecx, pb.dwOffs2
@@:
nextitem:
	cmp bx, word ptr [rIDTR+0]
	jnc exit
	push ebx
	push ecx
	mov eax, ebx
	shr eax, 3
	add ebx, dword ptr [rIDTR+2]
	movzx esi, word ptr @flat:[ebx+4]
	mov dx, @flat:[ebx+6]
	shl edx, 16
	mov dx, @flat:[ebx+0]
	movzx ecx, word ptr @flat:[ebx+2]
	invoke printf, CStr(" %02X %04X:%08X %04X "),eax,ecx,edx,esi
	mov eax, esi
	shr eax, 8
	call print_descattr
	mov eax,esi
	shr eax, 13
	and al, 03
	invoke printf, CStr(", DPL=%X"),eax
	invoke _crout

	pop ecx
	pop ebx
	add ebx, 8
	cmp ebx, 800h
	loopnz nextitem
	shr ebx, 3
	mov lastint, bl
exit:
	ret
_idtout endp

if 0; handled by function _fpregsout now

_floatregs proc c pb:PARMBLK

	test byte ptr [f80x87],1
	jnz @F
	@errorout ERR_NO_NUMBER_CRUNCHER
	jmp floatregs_ex
@@:
	mov esi,offset frsave
	lodsd
	mov ecx, eax
	lodsd
	mov edx, eax
	lodsd
	invoke printf, CStr("control word=%X status word=%X tag word=%X",lf), ecx, edx, eax
	lodsd
	mov ecx, eax
	lodsw
	movzx ebx, ax
	lodsw
	movzx edx, ax
	lodsd
	mov edi, eax
	lodsd
	invoke printf, CStr("fip=%X fcs=%X opcode=%X foo=%X fos=%X",lf), ecx, ebx, edx, edi, eax

	@mov ecx, 8
	mov dl,0
floatregs1:
	push ecx
	push edx
	movzx ebx, dl
	add bl, '0'
	lodsd
	mov ecx, eax
	lodsd
	mov edx, eax
	lodsw
	movzx eax, ax
	invoke printf, CStr("st(%u)=%X.%08X.%08X "), ebx, eax, edx, ecx
	pop edx
	pop ecx
	inc dl
	test dl,1
	jnz @F
	call _crout
@@:
	loop floatregs1
floatregs_ex:
	ret
_floatregs endp
endif

;*** Input function

_input proc c pb:PARMBLK
	mov edx, pb.dwOffs1
	in al, dx
	movzx eax, al
dispinput::        
	movzx edx, dx
	invoke printf, CStr("%X:%X",10), edx, eax
	ret
_input endp

_inpw proc c pb:PARMBLK
	mov edx, pb.dwOffs1
	in ax, dx
	movzx eax, ax
	jmp dispinput
_inpw endp

_inpd proc c pb:PARMBLK
	mov edx, pb.dwOffs1
	in eax, dx
	jmp dispinput
_inpd endp


loadtempregs:
	mov ebx, offset tregs
	mov eax, [ebx.MYREGSTR.rEax]
	push [ebx.MYREGSTR.rEbx]
	mov ecx, [ebx.MYREGSTR.rEcx]
	mov edx, [ebx.MYREGSTR.rEdx]
	mov esi, [ebx.MYREGSTR.rEsi]
	mov edi, [ebx.MYREGSTR.rEdi]
	mov ebp, [ebx.MYREGSTR.rEbp]
	mov gs, [ebx.MYREGSTR.rGS]
	mov fs, [ebx.MYREGSTR.rFS]
	mov es, [ebx.MYREGSTR.rES]
	mov ds, [ebx.MYREGSTR.rDS]
	pop ebx
	ret
savetempregs:
	push ebx
	mov  ebx, offset tregs
	mov  [ebx.MYREGSTR.rES], es
	mov  [ebx.MYREGSTR.rFS], fs
	mov  [ebx.MYREGSTR.rGS], gs
	mov  [ebx.MYREGSTR.rEax], eax
	pop  [ebx.MYREGSTR.rEbx]
	mov  [ebx.MYREGSTR.rEcx], ecx
	mov  [ebx.MYREGSTR.rEdx], edx
	mov  [ebx.MYREGSTR.rEsi], esi
	mov  [ebx.MYREGSTR.rEdi], edi
	mov  [ebx.MYREGSTR.rEbp], ebp
	pushfd
	pop  [ebx.MYREGSTR.rEfl]
;	lea  eax,[esp+4]
;	mov  [tregs.rEsp],eax
	ret
tregs2rmcb:			;copy temp regs to RMCS structure
	pushad
	mov esi, offset tregs
	mov ecx, 8
	push esi
	rep movsd
	pop esi
	xor eax,eax
	stosw							;flags
	mov eax, [esi.MYREGSTR.rDS]
	shl eax, 16
	mov ax, word ptr [esi.MYREGSTR.rES]
	stosd							;ES, DS
	mov eax, [esi.MYREGSTR.rGS]
	shl eax, 16
	mov ax, word ptr [esi.MYREGSTR.rFS]
	stosd							;FS, GS
	mov eax, [esi.MYREGSTR.rCS]
	shl eax, 16
	mov ax, word ptr [esi.MYREGSTR.rEip]
	stosd							;CS:IP
	mov eax, [esi.MYREGSTR.rSS]
	shl eax, 16
	mov ax, word ptr [esi.MYREGSTR.rEsp]
	stosd							;SS:SP
	popad
	ret
rmcb2tregs:
	pushad
	mov edi, offset tregs
	mov ecx, 8
	push edi
	rep movsd
	pop edi
	lodsw
	mov word ptr [edi.MYREGSTR.rEfl], ax
	lodsw
	mov word ptr [edi.MYREGSTR.rES], ax
	lodsw
	mov word ptr [edi.MYREGSTR.rDS], ax
	lodsw
	mov word ptr [edi.MYREGSTR.rFS], ax
	lodsw
	mov word ptr [edi.MYREGSTR.rGS], ax
	lodsw
	mov word ptr [edi.MYREGSTR.rEip], ax
	lodsw
	mov word ptr [edi.MYREGSTR.rCS], ax
	lodsw
	mov word ptr [edi.MYREGSTR.rEsp], ax
	lodsw
	mov word ptr [edi.MYREGSTR.rSS], ax
	popad
	ret

;*** FCALL cmd

_farcall proc c pb:PARMBLK

	pushad
	mov eax, pb.dwOffs1
	mov dword ptr fcprocadr, eax
	mov eax, pb.wSeg1
	cmp cl, __FPTR__
	jz farcall_1
	cmp cl, __LPTR__
	jz farcall_2
	jmp farcallex
farcall_1:
	mov word ptr fcprocadr+4, ax
	jmp farcall_3
farcall_2:
	mov word ptr fcprocadr+2, ax
farcall_3:
	mov al, pb.p1.bType
	mov fcType, al

	call SaveState

	push dword ptr [pb.p4.dwOffs]
	push dword ptr [pb.p3.dwOffs]
	push dword ptr [pb.p2.dwOffs]
	call loadtempregs
	cmp byte ptr cs:[fcType],__FPTR__
	jz @F
	db 66h
@@:
	call fword ptr cs:[fcprocadr]

	push ds
	mov ds, cs:[__csalias]
	pop [tregs.rDS]
	call savetempregs

	call LoadState
	popad
farcallex:
	ret
_farcall endp

;*** farcallrm

_farcallrm proc c pb:PARMBLK

local myrmcs:RMCS

	pushad
	cmp cl,__RMLPTR__
	jnz farcallrm_ex

	mov eax,pb.dwOffs1
	mov [tregs.rEip], eax
	mov eax,[a1.dwSeg]
	mov [tregs.rCS], eax

	call SaveState

	lea edi, myrmcs
	call tregs2rmcb

	mov bh, 00
	xor ecx, ecx
	mov ax, 301h
	@DpmiCall

	lea esi, myrmcs
	call rmcb2tregs

	call LoadState
farcallrm_ex:
	ret
_farcallrm endp

parm2tregs proc stdcall

	mov ebx, offset tregs
;	cmp byte ptr [esi.PARMBLK.wType5], __VOID__
	cmp [esi].PARMBLK.p5.bType, __VOID__
	jz @F
;	mov eax, [esi.PARMBLK.dwOffs5]
	mov eax, [esi].PARMBLK.p5.dwOffs
	mov [ebx.MYREGSTR.rEdx],eax
@@:
	cmp [esi.PARMBLK.p4.bType], __VOID__
	jz @F
	mov eax, [esi.PARMBLK.p4.dwOffs]
	mov [ebx.MYREGSTR.rEcx],eax
@@:
	cmp [esi.PARMBLK.p3.bType], __VOID__
	jz @F
	mov eax, [esi.PARMBLK.p3.dwOffs]
	mov [ebx.MYREGSTR.rEbx],eax
@@:
	cmp [esi.PARMBLK.p2.bType], __VOID__
	jz @F
	mov eax, [esi.PARMBLK.p2.dwOffs]
	mov [ebx.MYREGSTR.rEax],eax
@@:
	ret
parm2tregs endp

if ?FLAT
	.data

intproc db 0CDh
intc    db 00h
        db 0C3h

	.code

endif

;*** INTcall function

_intcall proc c pb:PARMBLK

local   pInt:DWORD

	mov eax,[a1.dwOfs]
	cmp eax,0100h
	jnc intcallex
	mov ds:intc,al
	lea esi,pb
	call parm2tregs

	call SaveState
	call switch2hoststack
if ?32BIT
	mov eax,esp
else
	movzx eax,sp
endif
	mov [dwTmp],eax
	call loadtempregs
if ?FLAT
	db 2Eh
	call near32 ptr intproc
else
	db 0CDh
intc db 0
endif
	push ds
	mov ds,cs:[__csalias]
	pop [tregs.rDS]
	call savetempregs
	push ds
	pop es
if ?32BIT
	mov eax, esp
else
	movzx eax, sp
endif
	mov ebx,[dwTmp]
	mov esp,ebx
	call switch2debuggerstack
	cmp eax,ebx
	jz @F
	invoke printf, CStr("SP has changed from %X to %X",lf),eax,ebx
@@:
	call LoadState
intcallex:
	ret
_intcall endp

;*** INTRM function

_intcallrm proc c pb:PARMBLK

local myrmcs:RMCS

	mov eax,[a1.dwOfs]
	cmp eax,0100h
	jnc intcallrm_ex

	call parm2tregs

	lea edi,myrmcs
	call tregs2rmcb

	call SaveState

	lea edi, myrmcs
	mov bx,word ptr pb.dwOffs1
	xor ecx, ecx
	mov ax, 300h
	@DpmiCall

	lea esi, myrmcs
	call rmcb2tregs

	call LoadState
intcallrm_ex:
	ret
_intcallrm endp

pageflgs proc stdcall

local	flgs:dword

	push ebx
	mov  flgs,eax
;	@putchr '/'

	test byte ptr flgs,40h
	jnz  @F
	@putchr 'c' 			 ;Clean
	jmp  pageflgs1
@@:
	@putchr 'D' 			 ;dirty
pageflgs1:
	test byte ptr flgs,20h
	jnz  @F
	@putchr 'u' 			 ;unaccessed
	jmp  pageflgs2
@@:
	@putchr 'A' 			 ;accessed
pageflgs2:
	test byte ptr flgs,04h
	jnz  @F
	@putchr 's' 			 ;supervisor
	jmp  pageflgs3
@@:
	@putchr 'U' 			 ;user
pageflgs3:
	test byte ptr flgs,02h
	jnz  @F
	@putchr 'r'
	jmp  pageflgs4
@@:
	@putchr 'W'
pageflgs4:
	test byte ptr flgs,01h
	jnz  @F
	@putchr 'n'
	jmp  pageflgs5
@@:
	@putchr 'P'
pageflgs5:
	pop  ebx
	ret
pageflgs endp

;--- display 1 page directory entry in 1 line 
;--- inp: dx=index for page directory (0000-03FF)
;--- inp: eax=value of entry (phys page + flags)

pagedirout proc stdcall

	mov edi, eax
	movzx eax, dx
	shl eax, 22
	mov ebx, eax
	add ebx, 3FFFFFh
	mov ecx, edi
	shr ecx, 12
	movzx edx, di
	and dx,0FFFh
	invoke printf, CStr("%08X-%08X %05X %03X-"), eax, ebx, ecx, edx

	mov eax, edi
	call pageflgs

	push edi        
	pop cx
	pop bx
	and cx,0F000h
	xor esi, esi
	mov di,1000h
	mov ax, 800h
	@DpmiCall 				;map entry to linear address
	jc notmapped
	push bx
	push cx
	pop eax
	mov esi, eax
	mov ebx, eax
	mov ecx,400h
	xor edx,edx
nextitem:
	lodsd @flat:[dwPageAddr]
	test al,1
	jz @F
	inc edx
@@:        
	loop nextitem
	mov esi, edx
	invoke printf, CStr(" %08X %5u"),ebx, edx
notmapped:
	invoke _crout
	ret
pagedirout endp

;*** get linear address of page directory

	.DATA
lastcr3 dd -1
pdaddr	dd 0

	.CODE

getpagediraddr proc stdcall uses esi edi

	xor al,al
	invoke getcr0, 0
	and eax, eax		;if paging is active, bit 31 is 1
	js @F
	@errorout ERR_PAGING_NOT_ACTIVE
	stc
	jmp exit
@@:
	xor al, al
	invoke getcr3, 0
	cmp eax, lastcr3
	jnz @F
	mov eax, [pdaddr]
	jmp exit
@@:
	mov lastcr3, eax
	push eax
	pop cx
	pop bx
	and cx, 0F000h
	mov si, 0000h
	mov di, 0001h
	mov ax, 0800h
	@DpmiCall
	jnc @F
	@errorout ERR_LINADDR_UNAVAILABLE
	mov eax, -1
	mov lastcr3, eax
	stc
	jmp exit
@@:
	push bx
	push cx
	pop eax
	mov pdaddr, eax
	clc
exit:
	ret
        
getpagediraddr endp

;*** PD cmd/DPD cmd: display page directory

_pagedir proc c pb:PARMBLK

local	dwPages:DWORD
local	dwLineSize:DWORD

	call getpagediraddr
	jc exit
	push eax
	invoke printf, CStr("linear address page directory: %08X",lf), eax
	invoke printf, CStr("addr region       phys  attr      linear   pages",lf)
	mov dwLineSize, eax
	invoke printchars, '-', eax, 1
	pop eax
	mov ecx, -1
	cmp pb.p2.bType, __CONST__
	jnz @F
	mov ecx, [pb.dwOffs2]
@@:
	mov ebx, eax
	mov eax, [pb.dwOffs1]
	shr eax, 20		   ;
	add ebx, eax
	shr eax, 2
	mov edx, eax
	mov dwPages, 0
pagedir1:						;<---
	mov eax,@flat:[ebx]
	test al,1
	jz pagedir2
	pushad
	call pagedirout
	add dwPages, esi
	popad
	dec ecx
	jecxz done
pagedir2:
	add ebx,4
	inc edx
	cmp dx,400h
	jnz pagedir1		;--->
done:        
	invoke printchars, '-', dwLineSize, 1
	invoke printf, CStr("%48u",lf),dwPages
exit:
	ret
_pagedir endp

;*** adresse page table ermitteln
;*** out: C bei Fehler
;***	  eax=lineare page table adr

getptaddr proc stdcall uses esi edi ebx ecx adresse:dword

	call getpagediraddr
	jc exit
	mov ebx, adresse
	shr ebx, 20			; lineare adresse -> pagedirentry
	and bl, 0FCh
	add ebx, eax 		; adresse pagedir addieren
	mov eax,@flat:[ebx]	; eintrag in pagedir
	test al,1			; pagetable present?
	jnz @F
	push adresse
	@errorout ERR_NOVALID_PAGEDIR
	pop eax
	stc
	jmp exit
@@:
	and ax, 0F000h		; pagestart -> bx:cx
	push eax
	pop cx
	pop bx
	mov si, 0000h
	mov di, 1000h		; 4k Bereich (pagetable) mappen
	mov ax, 0800h
	@DpmiCall
	jnc @F
	@errorout ERR_LINADDR_PAGEDIR
	stc
	jmp exit
@@:
	push bx
	push cx
	pop eax
	clc
exit:
	ret
getptaddr endp

;*** flags in pagetable setzen
;*** rc: alte flags in eax

setptentry proc stdcall uses esi edi adresse:dword,newflags:dword,maske:dword

	invoke getptaddr, adresse
	jc exit
	mov edi, eax
	mov esi, adresse
	shr esi, 10
	and esi, 00000FFCh
	mov eax, @flat:[edi+esi]
	mov ecx, maske
	jecxz exit
	mov edx, newflags
	and edx, ecx 			; in newflags relevante bits set
	xor ecx, -1
	and eax, ecx 			; in entry relevante bits reset
	or eax, edx 			; wert neuer entry
	cmp eax,@flat:[edi+esi]
	jz exit
	xchg eax,@flat:[edi+esi]
	clc
exit:
	ret
setptentry endp


getptentry proc stdcall adresse:dword
	invoke setptentry, adresse, 0, 0
	ret
getptentry endp

;--- PFlags command

_getptentry proc c pb:PARMBLK

	invoke setptentry, pb.p1.dwOffs, pb.p2.dwOffs, pb.p3.dwOffs
	jc exit
	mov ecx, eax
	shr ecx, 12
	and eax, 0FFFh
	invoke printf, CStr("phys entry=%X, flags=%X",10), ecx, eax
exit:
	ret
_getptentry endp

;*** PT cmd/DP cmd: display page table

_pagetab proc c uses edi pb:PARMBLK

local actentry:dword

	mov eax, [dwPageAddr]
	cmp [pb.p1.bType], __VOID__
	jz @F
	mov eax, [pb.dwOffs1]
	and ax, 0F000h
@@:
	mov ecx, [dwPageLines]
	cmp pb.p2.bType, __VOID__
	jz @F
	mov ecx, [pb.dwOffs2]
@@:
	mov [dwPageLines], ecx
	shl ecx, 2
	mov edi, eax
nextline:							;<----
	invoke getptaddr, edi
	jc exit
	mov ebx, eax
	push ecx
	invoke printf, CStr("page table at %X",lf), ebx
	pop ecx
	mov eax, edi 			;
	shr eax, 10				;adresse -> pages*2 (=offset in pagetab)
	and eax, 0FFFh
	add ebx, eax

	and di, 0F000h
nextitem:                           ;<----
	push ecx
	test cl, 03
	jnz @F

	movzx eax, bx
	and ah, 0Fh
	invoke printf, CStr("%03X %08X: "), eax, edi
@@:
	push ecx
	mov cl, 0
	mov eax, @flat:[ebx+0]
	mov actentry, eax
	cmp pb.p3.bType, __VOID__
	jz @F
	cmp pb.p4.bType, __VOID__
	jz @F
	push eax
	mov edx, [pb.p3.dwOffs]	;mask: what bits are relevant
	and eax, edx
	mov ecx, [pb.p4.dwOffs]
	and ecx, edx
	sub ecx, eax
	pop eax
	jz @F
	inc cl					; selected
@@:
	cmp cl, 0
	pop ecx
	jz @F
	@stroutc "---------,"
	jmp pagetab3
@@:
	movzx ecx, ax
	and ch, 0Fh
	shr eax, 12
	invoke printf, CStr("%5X-%03X,"), eax, ecx
pagetab3:
	mov eax, actentry
	call pageflgs
	@putchr ' '
	pop ecx
	add edi, 1000h
	dec ecx
	jz done
	test cl, 3h
	jnz @F
	invoke  _crout
@@:
	mov eax, ebx
	add ebx, 4
	and ah, 10h
	xor ah, bh
	test ah, 10h
	jz nextitem
	jmp nextline
done:
	invoke _crout
	mov [dwPageAddr], edi
exit:
	ret
_pagetab endp

	.const
pagetypes label dword
	dd CStr("uncommitted")
	dd CStr("committed")
	dd CStr("mapped")
	dd CStr("???")
	.code

;--- GPA command
;--- get pageing attributes
;--- 1. mem handle, 2. offset, 3.pages

_getpageattr proc c uses edi pb:PARMBLK

local dwESP:dword

	mov dwESP, esp
	cmp [pb.p1.bType], __CONST__
	jnz error2
	cmp [pb.p2.bType], __CONST__
	jnz error2
	cmp [pb.p3.bType], __CONST__
	jnz error2
	mov esi, pb.p1.dwOffs
	mov ebx, pb.p2.dwOffs
	mov ecx, pb.p3.dwOffs
	cmp ecx, 16*16
	ja error2
	sub esp, ecx
	sub esp, ecx
	@loadesp edx
	mov edi, edx
	mov ax, 0506h
	@DpmiCall
	jc error
	.while (ecx)
		push ecx
		push edi
		movzx eax, word ptr [edi]
		mov edx, eax
		and edx, 3
		mov ecx, [edx*4+offset pagetypes]
		mov edx, CStr("r/w")
		test al, 8
		jnz @F
		mov edx, CStr("r/o")
@@:
		mov esi, CStr("")
		mov edi, CStr("")
		test al, 10h
		jz nodirtyacc
		mov esi, CStr("NoAccess")
		mov edi, CStr("Clean")
		test al, 20h
		jz @F
		mov esi, CStr("Access")
@@:
		test al, 40h
		jz @F
		mov edi, CStr("Dirty")
@@:
nodirtyacc:
		invoke printf, CStr("%8X: %4X %s %s %s %s",10), ebx, eax, edx, ecx, esi, edi
		pop edi
		inc edi
		inc edi
		add ebx, 1000h
		pop ecx
		dec ecx
	.endw
	jmp exit
error2:
	@errorout ERR_WRONG_PARAM
	jmp exit
error:
	@errorout ERR_FROM_DPMI
exit:
	mov esp, dwESP
	ret
_getpageattr endp

;--- set pageing attributes
;--- 1. mem handle, 2. offset, 3.pages, 4.attribute

_setpageattr proc c uses edi pb:PARMBLK

local dwESP:dword

	cmp [pb.p1.bType], __CONST__
	jnz error2
	cmp [pb.p2.bType], __CONST__
	jnz error2
	cmp [pb.p3.bType], __CONST__
	jnz error2
	mov esi, pb.p1.dwOffs
	mov ebx, pb.p2.dwOffs
	mov ecx, pb.p3.dwOffs
	cmp ecx, 16*16
	ja error2
	mov dwESP, esp
	sub esp, ecx
	sub esp, ecx
	@loadesp edx
	mov edi, edx
	push ecx
	mov eax, pb.p4.dwOffs
	rep stosw
	pop ecx
	mov ax, 0507h
	@DpmiCall
	mov esp, dwESP
	jc error
	jmp exit
error2:
	@errorout ERR_WRONG_PARAM
	jmp exit
error:
	@errorout ERR_FROM_DPMI
exit:
	ret
_setpageattr endp

;*** UNCOMM: display uncommited pages of a segment

_uncommited proc c pb:PARMBLK

local seglimit:dword
local segbase:dword
local myseg:dword
local ptentry:dword

	verr bx
	jnz segptab_ex
	lsl eax, ebx
	mov myseg, ebx
	inc eax
	mov ecx, eax
	shr eax, 12
	test cx, 0FFFh
	jz @F
	inc eax
@@:
	mov seglimit, eax
	invoke getbaser, ebx
	jc segptab_ex
	mov segbase, eax
	mov edi, eax
	movzx ebx, bx
	invoke printf, CStr("uncommited pages in segment %X",lf), ebx
	invoke printf, CStr("Offset   linear   ^pagetab PT entry",lf)
	invoke printchars, '-', eax, 1
	mov ecx, seglimit
	mov esi, segbase
	and esi, 0FFFFF000h
	xor ebx, ebx
segptab_0:						;<----
	test bx, 0FFFh
	jnz segptab_2
@@:
	invoke getptaddr, esi		;adresse der zugehoerigen pagetable
	jnc @F
	add esi, 400000h
	sub ecx, 400h
	ja @B
	jmp segptab_ex
@@:
	mov ebx, eax
	mov eax, esi
	shr eax, 10				;adresse -> pages*2 (=offset in pagetab)
	and eax, 0FFFh
	add ebx, eax
segptab_2:
	mov gs,[__flatsel]
	mov eax, @flat:[ebx]
	test eax, 1
	jnz @F
	mov ptentry, eax
	mov eax, esi
	sub eax, segbase
	call eaxout
	@putchr ' '
	@dwordout esi
	@putchr ' '
	@dwordout ebx
	@putchr ' '
	mov eax, ptentry
	shr eax, 16
	@wordout eax
	mov eax, ptentry
	mov al, ah
	shr al, 4
	call __nibout
	@putchr ' '
	@putchr '['
	mov eax, ptentry
	mov al,ah
	call __nibout
	mov eax, ptentry
	invoke	_hexout
	@putchr ','
	mov eax, ptentry
	call pageflgs
	@putchr ']'
	invoke _crout
@@:
	add esi, 1000h
	add ebx, 4
	dec ecx
	jnz segptab_0
segptab_ex:
	ret
_uncommited endp

;*** PMAP: map a physical region into linear address region

_physmap proc c pb:PARMBLK

	cmp byte ptr pb.wArgc,2
	jnb @F
	mov eax,1000h
	mov pb.dwOffs2,eax
@@:
	push dword ptr pb.dwOffs1
	pop cx
	pop bx
	mov si,word ptr pb.dwOffs2+2
	mov di,word ptr pb.dwOffs2+0
	mov ax,0800h
	@DpmiCall
	jnc @F
	@errorout ERR_UNMAPPED_ADDRESS
	jmp linearex
@@:
	mov eax,pb.dwOffs1
;;	and ah,0Fh
;;	or  cx,ax
	push bx
	push cx
	push pb.dwOffs1
	@errorout MSG_PHYS_ADDR_MAPPED
	add esp, 8
linearex:
	ret
_physmap endp

;*** PUNMAP command: unmap a physical region

_physunmap proc c pb:PARMBLK

	push dword ptr pb.dwOffs1
	pop cx
	pop bx
	mov ax,0801h
	@DpmiCall
	jnc @F
	@errorout ERR_FROM_DPMI
@@:
exit:
	ret
_physunmap endp

;*** output funktion ***

_outp proc c pb:PARMBLK
	mov dx, word ptr pb.dwOffs1
	mov al, byte ptr pb.dwOffs2
	out dx, al
	ret
_outp endp

_outpw proc c pb:PARMBLK
	mov dx, word ptr pb.dwOffs1
	mov ax, word ptr pb.dwOffs2
	out dx, ax
	ret
_outpw endp

_outpd proc c pb:PARMBLK
	mov dx, word ptr pb.dwOffs1
	mov eax, dword ptr pb.dwOffs2
	out dx, eax
	ret
_outpd endp

;*** INPMode function

inpmode proc c tvalue:dword

	cmp al,1
	jnz inpmode_2
	mov eax,[__inpstream]
	cmp eax,5
	jb @F
	call MarkDOSused
	@close eax
@@:
	mov al, byte ptr tvalue
	test al, _DOSINP
	jz inpmode_1
	mov edx, [inpmodefn]
	and edx, edx
	jz inpmode_1
	cmp byte ptr [edx], 0
	jz inpmode_1
	call  MarkDOSused
if ?32BIT eq 0
	invoke CopyString2TmpHeap, edx
	mov edx, eax
endif
	mov ax, 3D20h			;open r/o, deny write
	@DosCall
	jnc @F
	movzx eax, ax
	push eax
	push [inpmodefn]
	@errorout ERR_FILE_OPEN
	add esp, 2*4
	jmp inpmode_ex
@@:
	movzx eax, ax
	mov [__inpstream], eax
	invoke printf, CStr("file handle for DOS input is %X",lf), eax
	mov al,byte ptr tvalue
inpmode_1:
	mov [__inpmode],al
	call installirqcom
	call installirqkbd
inpmode_2:
	mov al,[__inpmode]
	clc
inpmode_ex:
	ret
inpmode endp

;*** OUTMode function

outmode proc c tvalue:dword

	cmp al, 1
	jnz outmode_2
	mov eax, [__stream]		;__stream is the file handle for DOS output ( out_dosc.asm )
	cmp eax, 5				; fixme: whether it's a file can't be detected by the fh#!!!
	jb @F
	call MarkDOSused
	@close eax
	mov [__stream], 1
@@:
	and [fStat], not FSTAT_DOSOUT

	mov al, byte ptr tvalue
	test al, _DOSOUT
	jz outmode_1
	mov edx, [outmodefn]
	and edx, edx
	jz outmode_1
	cmp byte ptr [edx],0
	jz outmode_1
	call MarkDOSused
if ?32BIT eq 0
	invoke CopyString2TmpHeap, edx
	mov edx, eax
endif
	mov ah,3ch
	mov cx, 0		;normal
	@DosCall
	jnc @F
	push edx
	push [outmodefn]
	@errorout ERR_FILE_CREATE
	add esp, 2*4
	jmp outmode_ex
@@:
	or [fStat], FSTAT_DOSOUT
	movzx eax, ax
	mov [__stream], eax
	invoke printf, CStr("file handle for DOS output is %X",lf), eax
	mov al, byte ptr tvalue
outmode_1:
	mov [__outmode], al
	call installirqcom
	call installirqmou
outmode_2:
	mov   al, [__outmode]
	clc
outmode_ex:
	ret
outmode endp

;*** fill function ***

_fill proc c public uses es pb:PARMBLK

	push [excexit]
	mov [excexit], offset errorexit
	cmp byte ptr pb.wArgc,4
	jb fill_1
	mov eax,pb.p4.dwOffs
	cmp eax,4					;BYTE=1,WORD=2,DWORD=4
	jbe @F
	@errorout ERR_WRONG_PARAM
	ret
@@:
	mov [filllength],al
fill_1:
if 0
	test [fDump], FDT_SWITCHSCREEN
	jz @F
	call SwitchToDebuggeeScreen
@@:
endif
	cld
	mov eax,pb.wSeg1
	and eax,eax
	jnz @F
	mov eax,[r1.rDS]
@@:
	mov edi,pb.dwOffs1
	mov ecx,pb.dwOffs2
if ?LOADVDD
	cmp [hVDD],-1
	jz @F
	invoke VerifyAddress, eax::edi, 2
	jc errorexit
@@:
endif
	mov es,eax
	mov eax,pb.p3.dwOffs
	mov dl,filllength
	movzx edx,dl
	cmp dl,4
	jz filldword
	cmp dl,3
	jz fill3byte
	push ax
	shl eax,16
	pop ax
	cmp dl,2
	jz fillword
	cmp dl,1
	jz fillbyte
	jmp exit
fill3byte:
	cmp ecx,edx
	jbe fillword
	stosd
	dec edi
	sub ecx,edx
	jmp fill3byte
filldword:
	mov dl,cl
if ?WINDOWS
	call _Stosb
else
	shr ecx,2
	rep stosd
endif
	mov cl,dl
	and cl,3
fillword:
if ?WINDOWS
	call _Stosb
else
	shr ecx,1
	rep stosw
endif
	adc ecx,ecx
	shr eax,16
fillbyte:
if ?WINDOWS
	call _Stosb
else
	rep stosb
endif
	jmp exit
errorexit:
	@errorout ERR_ACCESS_CAUSED_EXCEPTION
exit:
	pop [excexit]
if 0
	test [fDump], FDT_SWITCHSCREEN
	jz @F
	call SwitchToDebuggerScreen	; fill cmd
@@:
endif
	ret
_fill endp

;*** Move function

_move proc c uses ds es pb:PARMBLK

	mov eax, pb.wSeg2		;target
	cmp pb.p2.bType, __RMLPTR__
	jnz @F
	shl eax, 4
	add pb.dwOffs2, eax
	mov eax, @flat
@@:
	and eax, eax
	jnz @F
	mov eax, [r1.rDS]
@@:
	mov es, eax
	mov eax, pb.wSeg1		;source
	and eax, eax
	jnz @F
	mov eax, [r1.rDS]
@@:
	mov ds, eax
	invoke memcpy, pb.p2.dwOffs, pb.p1.dwOffs, pb.p3.dwOffs
	ret
_move endp

;--- .DOS command

	@cmdproc

_getdosparms proc c public pb:PARMBLK

	mov ebx, offset dosvartab
	mov eax, offset dosvartabend
	mov ch, _RDONLY_
	call symtout
	ret
_getdosparms endp

_getenvironment proc c public pb:PARMBLK

	call getpsp
	mov fs,eax
	mov ax,fs:[002Ch]
	and ax,ax
	jnz @F
	@errorout ERR_NO_ENVIRON
	jmp getenviron_ex
@@:
	xor esi,esi
	verr ax					;das Environment kann Segment oder
	jz @F					;Selektor sein!
	movzx esi,ax
	shl esi,4
	mov eax,@flat
@@:
	mov fs,eax
@@:
	lods byte ptr fs:[0]
	and al,al
	jz @F
getenvironment_1:
	@putchr al
	jmp @B
@@:
	invoke _crout
	lods byte ptr fs:[0]
	and al,al
	jnz getenvironment_1
getenviron_ex:
	ret
_getenvironment endp

	@cmdprocend

;--- launching a DOS box 
;--- works in win9x only, so deactivated

ife ?WINDOWS

if ?WINDOWS

	.data

vmstr VMEXEC <1,0,0,0,0,640,640,100,50,0,0,0,2048,0,0>
cmdparms  db 00
cmdcurdir db "C:\",00

	.code

startdosbox proc pascal cmdpath:dword

local lpproc:PF16

if 0
	mov ax,1600h
	@callint2F
	cmp al,00
	stc
	jz startdosbox_ex
endif
	xor edi,edi
	mov es,edi
	mov bx,0017h			;get shell pm entry
	mov ax,1684h
	@callint2F
	jc startdosbox_ex
	mov word ptr lpproc+0,di
	mov word ptr lpproc+2,es
	push ds
	pop es
	cmp lpproc,0
	jz startdosbox_ex

	mov edi,offset vmstr
	mov eax,cmdpath
	mov ecx,ds
	mov dword ptr [edi.VMEXEC.pgmpath+0],eax
	mov word ptr [edi.VMEXEC.pgmpath+4],cx

	mov dword ptr [edi.VMEXEC.pgmparm+0],offset cmdparms
	mov word ptr [edi.VMEXEC.pgmparm+4],cx

	mov dword ptr [edi.VMEXEC.curdir+0],offset cmdcurdir
	mov word ptr [edi.VMEXEC.curdir+4],cx

	mov edx, 3
	call lpproc
	jc startdosbox_ex

	push eax
	@stroutc "vm-handle="
	call eaxout
	invoke _crout
	pop ebx
	mov gs, [__flatsel]
	mov bx, @flat:[ebx+12]
	mov ax, 168Bh			 ; set focus
	@callint2F
	clc
startdosbox_ex:
	ret
startdosbox endp

endif

if ?FLAT
GetEnvironmentVariable proc stdcall public pszName:ptr byte, pBuffer:ptr byte, nSize:DWORD
else
GetEnvironmentVariable proc stdcall public uses ds es pszName:ptr byte, pBuffer:ptr byte, nSize:DWORD
endif

local keysize:dword
local pEnv:dword

	invoke strlen, pszName
	mov keysize, eax

;--- use environment of debugger!

if 1
	mov ebx,[_environ]
else
	mov ah, 62h
	@DosCall
	push ds
	mov ds, ebx
	mov bx, ds:[2ch]
	pop ds
endif
	mov ax, 6
	@DpmiCall
	jc error		;C if environment is NULL

	push cx
	push dx
	pop edi
ife ?FLAT
	push @flat
	pop es
endif
	.while (byte ptr @flat:[edi])
		mov esi,pszName
		mov ecx,keysize
		mov edx, edi
		repz cmpsb
		.if (ZERO? && byte ptr @flat:[edi] == '=')
			inc edi
			mov esi,edi
			mov edi,pBuffer
			mov ecx,nSize
ife ?FLAT
			mov eax,es
			push ds
			pop es
			mov ds,eax
endif
@@:
			lodsb
			stosb
			and al,al
			loopnz @B
			mov eax, edi
			sub eax, pBuffer
			dec eax
			jmp done
		.endif
		mov edi, edx
		mov al, 0
		mov ecx, -1
		repnz scasb
	.endw
error:
	xor eax, eax
done:
	ret
GetEnvironmentVariable endp

;--- .SHell command

_dosshell proc c pb:PARMBLK

local	cmdpath[260]:byte
local	myCmdline:dword

	mov esi,pb.dwOffs1
	call preparecmdline
	mov myCmdline,ebx
	invoke GetEnvironmentVariable, CStr("COMSPEC"), addr cmdpath, sizeof cmdpath
	and eax, eax
	jnz @F
	@errorout ERR_COMSPEC_NOT_FOUND
	jmp exit
@@:
if ?WINDOWS
	lea eax,cmdpath
	push eax
	call startdosbox
	jnc exit
endif
	call SaveState
	jc exit

	xor eax, eax
	call rmbreak

	mov ebx,offset params
	mov eax,myCmdline
	xor ecx,ecx
	mov edx,offset fcb
if ?32BIT
	mov [ebx+0],eax 		  ;parameter
	mov [ebx+4],ss
	mov [ebx+8],edx 		  ;fcb1
	mov [ebx+12],ds
	mov [ebx+16],edx		  ;fcb2
	mov [ebx+20],ds
else
	mov [ebx+0],cx			  ;environment
	mov ecx,ds
	push cx
	push dx
	pop edx
	mov [ebx+2],ax			  ;parameter
	mov [ebx+4],ss
	mov [ebx+6],edx 		  ;fcb1
	mov [ebx+10],edx		  ;fcb2
endif
	push ss
	pop es
	lea edx,cmdpath
	call switch2hoststack
	mov ax,4B00h
	@DosCall
	push ds
	pop es
	@loadflat
	call switch2debuggerstack
	jnc dosshell_1
;	mov ah,4Dh
;	@DosCall
	movzx eax,ax
	push eax
	@errorout ERR_SHELL_EXEC_FAILED	
	pop eax
	jmp dosshell_2
dosshell_1:
	movzx eax,ax
	invoke printf, CStr("rc from shell: %X",lf), eax
dosshell_2:
	mov eax,1
	call rmbreak
	call LoadState
	call checkpmints
exit:
	ret
_dosshell endp

endif

;--- set flag NOTACTIVE in RMDBGHLP
;--- EAX=0: reset
;--- EAX=1: set

rmbreak proc stdcall public
	cmp [__RmDS], 0
	jz @F
	push ds
	mov ds, [__RmDS]
	assume ds:_TEXT16
	.if (eax)
		and ds:[DEBRMVAR.bFlags], not FLRM_NOTACTIVE
	.else
		or ds:[DEBRMVAR.bFlags], FLRM_NOTACTIVE
	.endif
	pop ds
	assume ds:@data
@@:
	ret
rmbreak endp

	@cmdproc

_getfpustatus proc c pb:PARMBLK
	cmp cl,1
	jb getstat
	mov ax,0e01h
	@DpmiCall
	jc error
getstat:
	mov ax,0E00h
	@DpmiCall
	jc error
	movzx eax, ax
	invoke printf, CStr("Status=%X",lf), eax
	jmp done
error:
	@errorout ERR_FROM_DPMI
done:
	ret

_getfpustatus endp

ife ?WINDOWS

;--- .LDR cmd
;--- syntax: .LDR 1|0 [,mask] [,flags]  ( see environment variable DPMILDR=xxx )

_loader proc c public pb:PARMBLK

	cmp al, 0
	jz dispstatus
	mov bLdr, bl
	cmp al, 2
	jb @F
	mov eax, pb.p2.dwOffs
	mov wLdrMask, ax
	jz @F
	mov eax, pb.p3.dwOffs
	mov wLdrFlgs, ax
@@:
if ?LDRDISABLE
	mov bl, [bLdr]
	mov ax, 4b91h
	@DosCall
endif
	ret
dispstatus:
	movzx eax, bLdr
	movzx edx, wLdrMask
	movzx ecx, wLdrFlgs
	invoke printf, CStr("Loader active: %u, mask=%X, flags=%X",lf), eax, edx, ecx
	ret
_loader endp

;--- enable/disable DPMILD16/32.
;--- called when a debuggee is loaded and on exit

ife ?LDRDISABLE
setloaderstate proc stdcall public
	pushad
	mov bl, [bLdr]
	mov ax, 4b91h
	@DosCall
	popad
	ret
setloaderstate endp

enableloader proc stdcall public
	pushad
	mov bl, 1
	mov ax, 4b91h
	@DosCall
	popad
	ret
enableloader endp
endif

endif

	@cmdprocend

	end

