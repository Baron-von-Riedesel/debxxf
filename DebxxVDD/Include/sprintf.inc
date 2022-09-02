
;--- simple sprintf implementation for DOS, Windows
;--- supports:
;--- %x  : dword
;--- %lx : qword 
;--- %u  : dword
;--- %lu : qword 
;--- %d  : sdword
;--- %ld : sqword 
;--- %s  : near32 string out
;--- %c  : character

	.386

;--- i64toa(long long n, char * s, int base);
;--- convert 64-bit long long to string

i64toa PROC stdcall uses esi edi ebx number:qword, outb:ptr, base:dword

	mov ch,0
	mov edi, base
	mov eax, dword ptr number+0
	mov esi, dword ptr number+4
	cmp edi,-10
	jne @F
	neg edi
	and esi,esi
	jns @F
	neg esi
	neg eax
	sbb esi,0
	mov ch,'-'
@@:
	mov ebx,outb
	add ebx,22
	mov byte ptr [ebx],0
@@nextdigit:
	dec ebx
	xor edx,edx
	xchg eax,esi
	div edi
	xchg eax,esi
	div edi
	add dl,'0'
	cmp dl,'9'
	jbe @F
	add dl,7+20h
@@:
	mov [ebx],dl
	mov edx, eax
	or edx, esi
	jne @@nextdigit
	cmp ch,0
	je @F
	dec ebx
	mov [ebx],ch
@@:
	mov eax,ebx
	ret

i64toa ENDP

sprintf PROC c uses ebx esi edi buffer:ptr sbyte, fmt:ptr sbyte, args:vararg

local flag:byte
local longarg:byte
local size_:dword
local fillchr:dword
local szTmp[24]:byte

	lea ebx, args
	mov edi, buffer
@@L335:
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
	mov byte ptr [edi],0
	mov eax, edi
	sub eax, buffer
	ret 

formatitem:
	push offset @@L335
	xor edx, edx
	mov [longarg], dl
	mov al,1
	mov cl,' '
	cmp BYTE PTR [esi],'-'
	jne @F
	dec al
	inc esi
@@:
	mov [flag], al
	cmp BYTE PTR [esi],'0'
	jne @F
	mov cl,'0'
	inc esi
@@:
	mov [fillchr], ecx

	.while ( byte ptr [esi] >= '0' && byte ptr [esi] <= '9' )
		lodsb
		sub al,'0'
		movzx eax,al
		imul ecx,edx,10		;ecx = ebx * 10
		add eax,ecx
		mov edx,eax
	.endw

	mov [size_],edx
	xor edx, edx
	cmp BYTE PTR [esi],'l'
	jne @F
	mov [longarg],1
	inc esi
@@:
	lodsb
	mov [fmt],esi
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
	pop eax
	jmp done
handle_c:
	mov eax,[ebx]
	add ebx, 4
@@:
	stosb
	retn

handle_s:
	mov esi,[ebx]
	add ebx,4
	jmp print_string
handle_d:
handle_i:
	mov ecx,-10
	jmp @F
handle_u:
	mov ecx, 10
	jmp @F
handle_x:
	mov ecx, 16
@@:
	xor edx,edx
	mov eax,[ebx]
	add ebx,4
	cmp longarg,1
	jnz @F
	mov edx,[ebx]
	add ebx,4
	jmp printnum
@@:
	and ecx,ecx
	jns @F
	cdq
@@:
printnum:
	lea esi, szTmp
	invoke i64toa, edx::eax, esi, ecx
	mov esi, eax

print_string:		;print string ESI
	mov eax, esi
	.while byte ptr [esi]
		inc esi
	.endw
	sub esi, eax
	xchg eax, esi
	mov ecx, size_
	sub ecx, eax
	jnc @F
	xor ecx, ecx
@@:
	.if flag == 1
		push eax
		mov eax, [fillchr]
		rep stosb
		pop eax
	.endif
	push ecx
	mov ecx, eax
	rep movsb
	pop ecx
	mov eax, [fillchr]
	rep stosb
	retn

sprintf ENDP


