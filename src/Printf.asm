
;--- some CRT functions
;--- printf
;--- sprintf
;--- vsprintf
;--- memcpy
;--- strcpy
;--- strcmp
;--- strlen

	.386
if ?FLAT
	.MODEL FLAT
else
	.MODEL TINY
endif
	option proc:private
	option casemap:none

	.nolist
	include const.inc
	include ascii.inc
	include function.inc
	include debxxfd.inc
	include putf.inc
	include isvbop.inc
	include extern32.inc
if ?WINDOWS
	include extern16.inc
endif
	.list

?DOSTRACE equ 1	; trace displays to DOS

	.data?

;--- use a static buffer for printf so it will work with SS =! DS

buffer	db 128 dup (?)

	.code

strcpy proc stdcall public uses esi edi strg1:ptr byte, strg2:ptr byte

	mov edi,strg2	; source string
	xor al,al
	@mov ecx,-1
	repne scasb
	inc ecx
	neg ecx
	mov eax,ecx 	; rc = string length (incl 00)
	mov edi,strg1
	mov esi,strg2
	mov dl,cl
	shr ecx,2
	rep movsd
	mov cl,dl
	and cl,3
	rep movsb
	ret

strcpy endp

strcmp proc stdcall public uses esi edi strg1:ptr byte, strg2:ptr byte

	mov esi,strg1
	mov edi,strg2
	xor eax,eax
	@mov ecx,-1
	repne scasb
	not ecx
	sub edi,ecx
	repz cmpsb
	je @F
	sbb eAX,eAX
	sbb eAX,-1
@@:
	ret
strcmp endp

strlen proc stdcall public pStr:ptr byte

	mov edx,edi
	mov edi,pStr
	xor ecx,ecx
	dec ecx
	mov al,00
	repne scasb
	not ECX
	dec ECX
	mov eax,ecx
	mov edi,edx
	ret

strlen endp

memcpy proc stdcall public uses esi edi dest:ptr byte, src:ptr byte, count:dword

	mov edi,dest
	mov esi,src
	mov ecx,count
	mov dl,cl
	shr ecx,2
	rep movsd
	mov cl,dl
	and cl,3
	rep movsb
	mov eax,dest
	ret

memcpy endp

;--- DS may be != SS here

printf proc c public uses esi formstr:ptr byte,nextparms:VARARG

	invoke vsprintf, addr buffer, formstr, addr nextparms
	mov  esi, offset buffer
	push eax
nextchr:
	lodsb
	and al, al
	jz exit
	cmp al, lf			 ;translate LF to CR/LF
	jnz @F
	@putchr cr
@@:
	@putchr al
	jmp nextchr
exit:
	pop eax
	ret

printf endp

sprintf proc c public pBuffer:ptr byte, formstr:ptr byte, nextparms:VARARG

	lea ecx, nextparms
	invoke vsprintf, pBuffer, formstr, ecx
	ret

sprintf endp

;--- convert long to string
;--- ltoa( long n, char * s, base n );

ltoa PROC c uses ebx esi edi number:dword, tbuffer:ptr byte, base:dword

	mov eax, number
	mov ebx, tbuffer
	mov edi, base
	mov ch,0
	cmp edi,-10
	jne @F
	mov edi,10
	and eax,eax
	jns @F
	neg eax
	mov ch,'-'
@@:
	add ebx,10
	mov BYTE PTR ss:[ebx],0
	dec ebx
@@nextdigit:
	xor edx, edx
	div edi
	add dl,'0'
	cmp dl,'9'
	jbe @F
	add dl,7+20h
@@:
	mov ss:[ebx],dl
	dec ebx
	and eax, eax
	jne @@nextdigit
	cmp ch,0
	je @F
	mov ss:[ebx],ch
	dec ebx
@@:
	inc ebx
	mov eax, ebx
	ret

ltoa ENDP

;--- DS may be != SS here

vsprintf proc c public uses esi edi ebx pBuffer:ptr byte, fmt:ptr byte, args:ptr dword

local flag:byte
local longarg:byte
local size_:dword
local precision:dword
local pPrec:dword
local fillchr:byte
local szTmp[12]:byte

	mov ebx, args
	mov edi, pBuffer
nextchar2:
	mov esi, fmt
nextchar:
	lodsb
	or al,al
	je done
	cmp al,'%'
	je formatitem
	stosb
	jmp nextchar
done:
	stosb
	mov eax, edi
	dec eax
	sub eax, pBuffer
	mov edx, ebx        ; for tprintf(), to clean-up stack
	ret 

formatitem:
	push nextchar2
	xor edx,edx
	mov [longarg], dl
	mov al, 1
	mov cl,' '
	cmp BYTE PTR [esi], '-'
	jne @F
	dec al
	inc esi
@@:
	mov [flag], al
	cmp BYTE PTR [esi], '0'
	jne @F
	mov cl, '0'
	inc esi
@@:
	mov [fillchr], cl

	.while ( byte ptr [esi] >= '0' && byte ptr [esi] <= '9' )
		lodsb
		sub al,'0'
		movzx eax, al
		imul ecx, edx, 10
		add eax, ecx
		mov edx, eax
	.endw

	mov [size_], edx
	cmp BYTE PTR [esi],'.'
	jne @F
	inc esi
	xor edx, edx
	.while ( byte ptr [esi] >= '0' && byte ptr [esi] <= '9' )
		lodsb
		sub al,'0'
		movzx eax, al
		imul ecx, edx, 10
		add eax, ecx
		mov edx, eax
	.endw
	mov [precision], edx
	mov [pPrec], offset numprec
	or [flag], 2
@@:
	cmp BYTE PTR [esi],'l'
	jne @F
	mov [longarg], 1
	inc esi
@@:
	lodsb
	mov [fmt], esi
	cmp al,'x'
	je handle_x
	cmp al,'X'
	je handle_x
	cmp al,'d'
	je handle_d
	cmp al,'u'
	je handle_u
	cmp al,'s'
	je handle_s
	cmp al,'c'
	je handle_c
	and al,al
	jnz @F
	pop ecx
	jmp done
handle_c:
	mov eax, ss:[ebx]
	add ebx, 4
@@:
	stosb
	retn

handle_s:
	mov [pPrec], offset strprec
	mov esi, ss:[ebx]
	add ebx, 4
	cmp [longarg], 0
	je print_string
	push ds
	mov ds, ss:[ebx]
	add ebx, 4
	call print_string
	pop ds
	retn
handle_d:
handle_i:
	mov ecx, -10
	jmp @F
handle_u:
	mov ecx, 10
	jmp @F
handle_x:
	mov ecx, 16
@@:
	mov eax, ss:[ebx]
	add ebx, 4
	cdq
	cmp [longarg], 0
	je @F
	mov edx, ss:[ebx]
	add ebx, 4
@@:
	lea esi, szTmp
	invoke ltoa, eax, esi, ecx
	mov esi, eax
	push ds
	push ss
	pop ds
	call print_string
	pop ds
	retn

print_string:		; copy string DS:ESI to ES:EDI
	mov eax, esi
	.while byte ptr [eax]
		inc eax
	.endw
	sub eax, esi
	mov ecx, size_
	sub ecx, eax
	jnc @F
	xor ecx, ecx
@@:
	test flag, 1
	jz @F
	mov al, [fillchr]
	rep stosb
@@:
	test [flag], 2  ; precision set?
	jz @F
	call [pPrec]
@@:
	lodsb
	stosb
	and al, al
	jnz @B
	dec edi
	mov al, [fillchr]
	rep stosb
	retn

numprec:	; precision for numbers
	cmp [precision], eax
	jnc @F
	.while eax > [precision] && byte ptr [esi] == '0'
		dec eax
		inc esi
	.endw
@@:
	retn
strprec:	; precision for strings
	mov ecx, [precision]
@@:
	lodsb
	stosb
	and al, al
	loopnz @B
	jnz @F
	dec edi
@@:
	mov eax, [size_]
	sub eax, [precision]
	inc ecx
	add eax, ecx
	js @F
	mov ecx, eax
	mov al,' '
	rep stosb
@@:
	pop ecx    ; skip rest of print_string
	retn

vsprintf ENDP

if _TRACE_

;--- printf for trace log
;--- ebp-4: formatstring
;--- ebp-8: returnaddr
;--- ebp-12: efl

tprintf proc stdcall public
	pushfd
	pushad
	push ds
	push es
	mov ds,cs:[__csalias]
	push ss
	pop es
	lea ebp, [esp+2*4+8*4+3*4]
ife ?32BIT
	movzx ebp,bp
endif
	sub esp, 100
	@loadesp edx
	invoke vsprintf, edx, dword ptr [ebp-4], ebp
	mov ecx, [ebp-8]        ; get caller's return address
	sub edx, 4
	mov ss:[edx], ecx		; and put it where the last argument has been read
	mov [ebp-8], edx        ; set the value for "pop esp" below
ife ?WINDOWS
	@loadesp esi
 if ?LOADVDD
	mov eax, [hVDD]
	cmp eax, -1
	jz @F
	push ds
	push ss
	pop ds
	mov cx, 7
  if ?FLAT
	invoke RunBop, 2
  else
	DispatchCall 
  endif
	pop ds
	jmp done
@@:
 endif
 if ?DOSTRACE
	mov al, [__outmode]
	push eax
	mov [__outmode], _DOSOUT
 endif
nextchr:
	lodsb ss:[esi]
	and al,al
	jz exit
	cmp al,lf
	jnz @F
	dec [cLines]  ; never stop for trace msgs
	@putchr cr
	mov al,lf
@@:
	@putchr al
	jmp nextchr
exit:
 if ?DOSTRACE
	pop eax
	mov [__outmode], al
 endif
else
	mov eax, ss
	mov dx, sp
	@savewinsegregs
	push ax
	push dx
	call _OutputDebugString
	@restorewinsegregs
endif
done:
	add esp, 100
	pop es
	pop ds
	popad
	popfd
	pop esp
	ret
tprintf endp

endif

end

