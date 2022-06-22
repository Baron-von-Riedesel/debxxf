
;--- write debxxf.ini
;--- not used by deb16fw

	.386
if ?FLAT
	.MODEL FLAT
else
	.MODEL TINY
endif
	option proc:private
	option casemap:none

HFILE_ERROR equ -1
?BUFSIZE	equ 4000h

	.nolist
	include const.inc
	include ascii.inc
	include function.inc
	include debxxfd.inc
	include extern32.inc
	.list

@flatprefix macro
ife ?FLAT
	db 65h	;is GS prefix
endif
endm

@flatstosb macro
if ?FLAT
	stosb
else
	mov @flat:[edi],al
	inc edi
endif
endm

@flatstosw macro
if ?FLAT
	stosw
else
	mov @flat:[edi],ax
	inc edi
	inc edi
endif
endm

@flatlodsb macro
	@flatprefix
	lodsb
endm

@movsb2flat macro
if ?FLAT
	movsb
else
	lodsb
	@flatstosb
endif
endm

	.CODE

ToLower proc
	cmp al,'A'
	jc @F
	cmp al,'Z'
	ja @F
	or al,20h
@@:
	ret
ToLower endp

;--- check if 2 strings are equal
;--- edi points to flat memory
;--- edi points to string

check proc uses esi

check_0:
	mov al,@flat:[edi]
	call ToLower
	mov ah,al
	mov al,[esi]
	call ToLower
	inc esi
	inc edi
	cmp al,ah
	jz check_0
	dec esi
	dec edi
	mov al,[esi]
	ret
check endp

skipline proc
nextchar:
	mov al, @flat:[edi]
	cmp al, 10
	jz done
	cmp al, 0
	jz doneall
	inc edi
	jmp nextchar
done:
	inc edi
doneall:
	ret
skipline endp

skipline2 proc
	.while (byte ptr @flat:[esi])
		@flatlodsb
		.break .if (al == 10)
	.endw
	ret
skipline2 endp

copykeyname proc

	dec esi
	mov ebx, ecx
	mov edx, edi
next:
	@flatlodsb
	cmp al,'='
	jz iskey
	cmp al,13
	jz done
	cmp al, 0
	jz done2
	stosb
	dec ecx
	jnz next
done2:
	dec esi
done:
	mov edi, edx
	mov ecx, ebx
	jmp exit
iskey:
	mov al,0
	stosb
	dec ecx
exit:
	ret
copykeyname endp

;--- copy all keys in a section to edi, max size ecx
;--- end is indicated by 2 00 bytes
;--- esi = flat
;--- edi = std

getallkeys proc
	jecxz done
	dec ecx
	.while (ecx && byte ptr @flat:[esi])
		@flatlodsb
		.if (al == ';')
			;
		.elseif (al == 13)
		.else
			call copykeyname
		.endif
		call skipline2
	.endw
	mov al,0
	stosb
done:
	ret
getallkeys endp

;--- esi = flat

copysectionname proc
next:
	@flatlodsb
	cmp al,']'
	jz done
	cmp al,13
	jz done
	cmp al, 0
	jz done2
	stosb
	dec ecx
	jnz next
	dec edi
	inc ecx
	jmp done
done2:
	dec esi
done:

	mov al,0
	stosb
	dec ecx
	ret
copysectionname endp

;--- copy all section names to edi, max size ecx
;--- end is indicated by 2 00 bytes

getallsections proc

	jecxz done
	dec ecx
	.while (ecx && byte ptr @flat:[esi])
		@flatlodsb
		.if (al == '[')
			call copysectionname
		.endif
		call skipline2
	.endw
	mov al,0
	stosb
done:
	ret
getallsections  endp

;--- section -> esi
;--- file -> edi

searchsection proc

	mov ecx, -1
	.while (byte ptr @flat:[edi])
		.if (byte ptr @flat:[edi] == '[')
			inc edi
			call check
			cmp ah,']'
			jz done
		.endif
		call skipline
	.endw
error:
	stc
	ret
done:
	clc
	ret

searchsection endp

;--- esi -> entry
;--- edi -> file

searchentry proc

	mov ecx, -1
	.while (byte ptr @flat:[edi])
		.break .if (byte ptr @flat:[edi] == '[')
		call check
		cmp ah,'='
		jz done
		call skipline
	.endw
error:
	stc
	ret
done:
	ret
searchentry endp


GetPrivateProfileStringA proc stdcall public uses esi edi ebx lpAppName:ptr byte,
		lpKeyName:ptr byte, lpDefault:ptr byte, retbuff:ptr byte, bufsize:dword, filename:ptr byte

local	rc:dword
local	pMem:dword

if _TRACE_
	mov  ecx, lpAppName
	.if (!ecx)
		mov ecx, CStr("NULL")
	.endif
	mov  edx, lpKeyName
	.if (!edx)
		mov edx, CStr("NULL")
	.endif
	@tprintf <"GetPrivateProfileStringA(%s,%s,%s,%s)",lf>,ecx,edx,lpDefault,filename
endif

	xor eax, eax
	mov rc, eax

	mov pMem, eax
	invoke allocflat, ?BUFSIZE
	jc copydefault
	mov pMem, eax
	mov edx, filename
	mov ax, 3d20h
	@DosCall
	mov ebx,-1
	jc copydefault
	mov ebx, eax
	mov ah, 3Fh
	mov edx, pMem
	mov ecx, ?BUFSIZE-1
ife ?FLAT
	push ds
	@loadflat
  if ?32BIT
	push @flat
	pop ds
  else
	invoke setworkselbase, edx   ; set base+limit of worksel
	xor edx, edx
	mov ds, worksel
  endif
endif
	@DosCall
ife ?FLAT
	pop ds
endif
	jc copydefault
ife ?32BIT
	movzx eax, ax
endif

	mov edi, pMem
	mov byte ptr @flat:[edi+eax],0

	mov esi,lpAppName			; section suchen
	.if esi
		call searchsection
		jc copydefault
	.else
		mov esi, edi
		mov ecx,bufsize
		mov edi,retbuff
		call getallsections
		mov rc, eax
		jmp exit
	.endif

	mov esi, lpKeyName
	.if esi
		call searchentry
		jc copydefault
		jmp copyvalue
	.else
		mov esi, edi
		mov ecx,bufsize
		mov edi,retbuff
		call getallkeys
		mov rc, eax
	.endif

	jmp exit

copyvalue:
	mov ah, 13
	mov esi, edi
	inc esi
	cmp byte ptr @flat:[esi],'"'
	jnz @F
	inc esi
	mov ah,'"'
@@:
	mov edi, retbuff
	mov ecx, bufsize
	jecxz cd2
	dec ecx
@@:
	@flatlodsb
	cmp al,ah
	jz @F
	cmp al,13
	jz @F
	stosb
	loopnz @B
@@:
	mov al,0
	stosb
	sub edi, retbuff
	dec edi
	mov rc, edi
	jmp exit

copydefault:
	mov esi, lpDefault
	mov edi, retbuff
	mov ecx, bufsize
	jecxz cd2
cd1:
	lodsb
	stosb
	and al,al
	loopnz cd1
	.if (!ecx)
		dec edi
		mov al,0
		stosb
	.endif
	sub edi, retbuff
	dec edi
	mov rc, edi
cd2:

exit:
	.if (ebx != -1)
		mov ah,3Eh
		@DosCall
	.endif
	.if (pMem)
		invoke freeflat, pMem
	.endif
	mov eax,rc
	ret

GetPrivateProfileStringA endp


WritePrivateProfileStringA proc stdcall public uses esi edi ebx lpAppName:ptr byte,
		lpKeyName:ptr byte, lpValue:ptr byte, filename:ptr byte

local   rc:dword
local   sel:dword
local   pMem:dword

if _TRACE_
	mov ecx, lpAppName
	.if (!ecx)
		mov ecx, CStr("NULL")
	.endif
	mov edx, lpKeyName
	.if (!edx)
		mov edx, CStr("NULL")
	.endif
	mov eax, lpValue
	.if (!eax)
		mov eax, CStr("NULL")
	.endif
	@tprintf <"WritePrivateProfileStringA (%s,%s,%s,%s)",lf>, ecx, edx, eax, filename
endif
	mov eax, HFILE_ERROR
	mov rc, eax

	@loadflat

	mov pMem, 0
	invoke allocflat, ?BUFSIZE
	jc exit
	mov pMem, eax
	mov edx, filename
	mov ax, 3D22h		;r/w, deny write
	@DosCall
	jnc @F
	mov ah, 3Ch
	mov cx, 0
	@DosCall
	mov ebx, -1
	jc exit
@@:
	movzx ebx, ax
	mov edx, pMem
	mov ecx, ?BUFSIZE-1
ife ?FLAT
	push ds
  if ?32BIT
	push @flat
	pop ds
  else
	invoke setworkselbase, edx
	xor edx, edx
	mov ds, worksel
  endif
endif
	mov ah, 3Fh
	@DosCall
ife ?FLAT
	pop ds
endif
ife ?32BIT
	movzx eax, ax
endif
	mov edi, pMem
	mov byte ptr @flat:[edi+eax], 0

	mov esi,lpAppName
	.if (esi)
		call searchsection
		jc insertnewsection
	.else
;-------------------------------- all three entries = NULL -> flush cache
		.if ((!lpKeyName) && (!lpValue))
			jmp done
		.endif
;-------------------------------- this is an error
		jmp done
	.endif

	mov esi,lpKeyName
	.if (esi)
		call searchentry
		jc insertnewkey
	.else
;-------------------------------- delete this section
		jmp deletesection
	.endif

	mov esi,lpValue
	.if (!esi)
		jmp deletekey
	.endif

replacevalue:
	inc edi
	invoke strlen, esi
	lea eax, [edi+eax]
	push edi
	push eax
	xor cl,cl
	.while (1)
		mov al, @flat:[edi]
		.break .if ((al == 0) || (al == 13))
		cmp al, [esi]
		jz @F
		mov cl,1
@@:
		.if (byte ptr [esi])
			inc esi
		.endif
		inc edi
	.endw
;---------------------------------------- value hasnt changed, do nothing
	.if ((cl == 0) && (byte ptr [esi] == 0))
		pop eax
		pop eax
		sub eax, pMem
		mov rc, eax
		jmp done
	.endif
	pop esi

	xchg esi, edi
@@:
	@flatlodsb
	@flatstosb
	cmp al,0
	jnz @B

	pop eax
	mov edi, eax
	sub eax, pMem
	mov rc, eax
	mov esi, lpValue
	.while (byte ptr [esi])
		@movsb2flat
	.endw
	jmp rewritefile
deletesection:
;------------------------------ not implemented yet
	jmp done
deletekey:
;------------------------------ not implemented yet
	jmp done
insertnewsection:
	.if (!lpKeyName)
;------------------------------ section does not exist and should be deleted
		jmp done
	.endif
	mov ax, 0A0Dh
	@flatstosw
	mov al,'['
	@flatstosb
	mov esi, lpAppName
	.while (byte ptr [esi])
		@movsb2flat
	.endw
	mov al,']'
	@flatstosb
	mov ax,0A0Dh
	@flatstosw
	mov byte ptr @flat:[edi],0
insertnewkey:

	mov eax, edi
	sub eax, pMem
	mov rc, eax

	.if (!lpValue)
		jmp done
	.endif
	mov esi, lpKeyName
	.while byte ptr [esi]
		@movsb2flat
	.endw
	mov al, '='
	@flatstosb
	mov esi, lpValue
	.while byte ptr [esi]
		@movsb2flat
	.endw
	mov ax,0A0Dh
	@flatstosw
	mov byte ptr @flat:[edi],0

rewritefile:
;---------------------- rewrite the whole file here

	mov ax, 4200h
	mov cx, 0
	mov dx, 0
	@DosCall

	mov edx, pMem
	mov ecx, edx
	.while byte ptr @flat:[ecx]
		inc ecx
	.endw
	sub ecx, edx
ife ?FLAT
	push ds
 if ?32BIT
	push @flat
	pop ds
 else
	mov ds, worksel
	xor edx, edx
 endif
endif
	mov ah, 40h
	@DosCall
ife ?FLAT
	pop ds
endif

done:

exit:
	.if ebx != -1
		mov ah,3Eh
		@DosCall
	.endif
	.if (pMem)
		invoke freeflat, pMem
	.endif
	mov eax,rc
	ret
WritePrivateProfileStringA endp

	end

