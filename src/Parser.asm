
;--- parse expressions

	.386
if ?FLAT
	.MODEL FLAT
else
	.MODEL TINY
endif
	option proc:private
	option casemap:none

	include const.inc
	include ascii.inc
	include function.inc
	include debxxfd.inc
	include dpmi.inc
	include putf.inc
	include errors.inc
	include extern32.inc

?PARMPROT = 0
OP_AND	equ "DNA"
OP_OR	equ "RO"
OP_XOR	equ "ROX"

;--- what does the parser?
;--- GetStringToken: gets a string token from a string without further checks
;--- GetParameter: get tokens from a string until ended

	.code

;*** transform AL to caps

kl2gr proc near stdcall public
	cmp   al,'a'
	jc	  gross1
	cmp   al,'z'+1
	jnc   gross1
	sub   al,'a' - 'A'
gross1:
	ret
kl2gr endp

	.const

opctab label byte
		db '(',')','[',']',':','=','>','<','&','|'
		db '!','/','*','+','-'
lopctab equ $ - offset opctab

	.code

trnchk proc near	; rc=C if no separator
	push	edi
	push	ecx
	mov 	edi,offset opctab
	mov 	ecx,lopctab
	cld
	repnz	scasb
	pop 	ecx
	pop 	edi
	jz		exit
trnchk1::					;<--- entry for argument separator
	cmp 	al,00
	jz		exit
	cmp 	al,','
	jz		exit
	cmp 	al,';'
	jz		exit
trnchk2::
	cmp 	al,' '
	jz		exit
	cmp 	al,9
	jz		exit
	stc
exit:
	ret
trnchk endp


ignsp proc near
	lodsb
	cmp 	al,' '
	jz		ignsp
	dec 	esi
	ret
ignsp endp

;*** string into caps

transform proc stdcall public pString:dword

	push	esi
	push	eax
	mov 	esi,pString
@@:
	lodsb
	and 	al,al
	jz		@F
	call	kl2gr
	mov 	[esi-1],al
	jmp 	@B
@@:
	pop 	eax
	pop 	esi
	ret
transform endp

;*** read a hex number
;*** in: # digits in al
;***	 ^string in esi
;*** out: number in eax

gethex proc near
	xor 	edx,edx
	mov 	cl,al
	xor 	eax,eax
gethz2:
	lodsb
	sub 	al,'0'
	cmp 	al,9
	jbe 	@F
	or		al,20h
	sub 	al,'a'- ('0' + 10)
@@:
	shl 	edx,4
	add 	edx,eax
	dec 	cl
	jnz 	gethz2
	mov 	eax,edx
	@tprintf <"valid number %X",lf>, eax
	ret
gethex endp

getdez proc c
	xor 	edx,edx
	mov 	cl,al
	xor 	eax,eax
getdez2:
	lodsb
	sub 	al,'0'
	lea		edx,[edx*4+edx]
	lea		edx,[edx*2+eax]
	dec 	cl
	jnz 	getdez2
	mov 	eax,edx
	ret
getdez endp

;*** convert string in number
;*** input: ^string in esi
;*** out: NC, zahl in eax

getnumber proc stdcall public
	push	esi
	push	ecx
	mov 	cl,0
	mov 	ah,0
getnumber_0:
	mov 	al,[esi]
	mov		ch,al
	cmp 	al,'0'
	jb		getnumber_3
	cmp 	al,'9'
	jbe 	@F
	cmp 	al,'A'
	jb		getnumber_3
	or		al,20h			;in kleinbuchstaben umwandeln!
	cmp 	al,'f'
	ja		getnumber_3
	mov 	ah,1
@@:
	inc 	esi
	inc 	cl
	jmp 	getnumber_0
getnumber_3:
	cmp 	cl,0			;keine ziffern
	jz		@F
	mov		al,ch			;restore char
	call	trnchk			;gueltiges ende?
	jz		getnumber_1
	cmp 	ah,0
	jnz 	@F
	cmp 	al,'T'
	jnz 	@F
	mov 	al,cl
	pop 	ecx
	pop 	esi
	call	getdez
	inc 	esi
	ret
@@:
	@tprintf <"invalid number, ecx=%X",lf>,ecx
	pop 	ecx
	pop 	esi
	stc
	ret
getnumber_1:
	mov 	al,cl
	pop 	ecx
	pop 	esi
	jmp 	gethex
getnumber endp

if ?PARMPROT
protnewvar:
	pushad
	push eax
	@stroutc "new var:"
	@dwordout esi
	@stroutc " text="
	mov ebx,[esi.SYMBOL.pText]
	invoke __stroutebx
	@stroutc " next: "
	@dwordout [esi-4]
	@stroutc  " addrvalue: "
	@dwordout [esi.SYMBOL.dwProc]
	@stroutc  " value: "
	@dwordout ecx
	@dwordout edx
	pop eax
	@dwordout eax
	@cr_out
	popad
	ret
endif

checkifnewvar proc stdcall pText:dword

local	newvar[4+sizeof SYMBOL]:byte
local	value[3]:dword

	mov 	ax,[esi]
	cmp 	al,'='
	jnz 	checkifnewvar_er
	mov 	al,ah
	cmp 	al,'"'
	jz		@F
	call	trnchk
	jz		checkifnewvar_er
@@:
	mov 	edx,pText
	mov 	al,[edx]
	cmp 	al,'@'
	jb		checkifnewvar_er
	inc 	esi
	invoke	GetExpression,__STRING__
	mov 	[value+0],eax
	mov 	[value+4],edx
	mov 	[value+8],ecx
	cmp 	cl,-1				 ;undefiniert?
	jz		checkifnewvar_er
	cmp 	cl,__STRING__
	jnz 	@F
	invoke	PutStringInHeap,value
	mov 	value,eax
@@:
	push	ecx
	invoke	PutStringInHeap,pText		;bezeichner der variablen -> heap
	pop 	ecx
	lea 	ebx,newvar+4
	mov 	[ebx.SYMBOL.pText],eax	  ;im descriptor sichern
	xor 	eax,eax
	mov 	[ebx.SYMBOL.bType],cl
	mov 	[ebx.SYMBOL.bType2],al

	call	gettypesize
	lea 	ecx,value
	invoke	PutBytes2Heap,ecx,eax		;wert auf heap sichern

	mov 	[ebx.SYMBOL.dwProc],eax
	mov 	eax,[dwUserSymbols]
	sub 	ebx,4
	mov 	[ebx],eax

	invoke	PutBytes2Heap,ebx,sizeof SYMBOL+4

	mov 	[dwUserSymbols],eax
	push	esi
	lea 	esi,[eax+4]
	mov 	eax,value+0
	mov 	edx,value+4
	mov 	ecx,value+8
if ?PARMPROT
	call	protnewvar
endif
	cmp 	cl,__STRING__
	jz		@F
	call	wrinp				 ;nochmal setzen (jetzt korrekt)
@@:
	pop 	esi
	clc
	jmp 	checkifnewvar_ex
checkifnewvar_er:
	mov 	cl,__VOID__
	stc
checkifnewvar_ex:
	ret
checkifnewvar endp

gettypesize:
	mov 	eax,4
	cmp 	cl,__DWORD__
	jbe 	@F
	cmp 	cl,__STRING__
	jz		@F
	cmp 	cl,__CHAR__
	jz		@F
	mov 	eax,8
	cmp 	cl,__TBYTE__
	jnz 	@F
	mov 	eax,12
@@:
	ret

;*** symbol in near heap kopieren und in asciiz umwandeln ***

CopySym2Heap proc stdcall
	push	edi
	mov 	edi,[pNearHeap]
copysym2heap_1:
	lodsb
	call	trnchk
	jz		copysym2heap_ex
	stosb
	jmp 	copysym2heap_1
copysym2heap_ex:
	dec 	esi
	mov 	al,0
	stosb
	mov 	eax,edi
	xchg	eax,[pNearHeap]
	pop 	edi
	ret
CopySym2Heap endp

;*** parser: 1 parameter holen
;*** input:
;*** esi -> eingabestring
;*** output:
;*** eax  = value+0
;*** edx  = value+4
;*** ecx  = value+8
;*** ebx  = ^symboltabelle
;*** esi  = string

GetExpression proc stdcall public vartype:dword

local	varseg:dword
local	defseg:dword		;falls registerbezug, defaultseg eintragen
local	varptr:dword
local	tempvar:dword
local	espvar:dword

	xor 	eax,eax
	mov 	varseg,eax
	mov 	defseg,eax
	call	getexpressionx
getexpression_1:
	push	eax
	mov 	eax,[esi]
	and 	eax,0FFFFFFh
	cmp 	eax,OP_AND
	jnz 	@F
	mov 	al,[esi+3]
	call	trnchk
	jnz 	@F
	inc 	esi
	inc 	esi
	inc 	esi
	call	getexpressionx
	pop 	ecx
	and 	eax,ecx
	jmp 	getexpression_1
@@:
	mov 	eax,[esi]
	cmp 	ax,OP_OR
	jnz 	@F
	shr 	eax,16
	call	trnchk
	jnz 	@F
	inc 	esi
	inc 	esi
	call	getexpressionx
	pop 	ecx
	or		eax,ecx
	jmp 	getexpression_1
@@:
	mov 	eax,[esi]
	and 	eax,0FFFFFFh
	cmp 	eax,OP_XOR
	jnz 	@F
	mov 	al,[esi+3]
	call	trnchk
	jnz 	@F
	inc 	esi
	inc 	esi
	inc 	esi
	call	getexpressionx
	pop 	ecx
	xor 	eax,ecx
	jmp 	getexpression_1
@@:
	mov 	ax,[esi]
	cmp 	ax,"&&"
	jnz 	@F
	inc 	esi
	inc 	esi
	call	getexpressionx
	pop 	ecx
	mov 	edx,1
	cmp 	ecx,edx
	cmc
	sbb 	ecx,ecx
	cmp 	eax,edx
	cmc
	sbb 	eax,eax
	and 	eax,ecx
	jmp 	getexpression_1
@@:
	cmp 	ax,"||"
	jnz 	@F
	inc 	esi
	inc 	esi
	call	getexpressionx
	pop 	ecx
	mov 	edx,1
	cmp 	ecx,edx
	cmc
	sbb 	ecx,ecx
	cmp 	eax,edx
	cmc
	sbb 	eax,eax
	or		eax,ecx
	jmp 	getexpression_1
@@:
	pop 	eax
getexpression_ex:
	mov 	ecx,vartype
	mov 	edx,varseg
	and 	edx,edx
	jnz 	ge_1
	cmp 	cl,__DWORD__
	ja		ge_1
	mov 	edx,defseg		 ;schwacher segmentbezug (default register)
ge_1:
	mov 	ebx,varptr
	@tprintf <"GetExpression exit (eax=%X, ecx=%X, edx=%X)",lf>, eax, ecx, edx
	ret
divzero:        
	invoke printf,CStr("divide by zero error",lf)
	jmp		mains

getexpressionx:
	call	gettokena
getexpressionx_1:
	push	eax
	mov 	ax,[esi]
	cmp 	ax,"=="
	jnz 	@F
	inc 	esi
	inc 	esi
	call	gettokena
	pop 	ecx
	sub 	eax,ecx
	cmp 	eax,1
	sbb 	eax,eax
	jmp 	getexpressionx_1
@@:
	cmp 	ax,"=!"
	jnz 	@F
	inc 	esi
	inc 	esi
	call	gettokena
	pop 	ecx
	sub 	eax,ecx
	cmp 	eax,1
	sbb 	eax,eax
	xor 	eax,-1
	jmp 	getexpressionx_1
@@:
	cmp 	ax,"=>"
	jnz 	@F
	inc 	esi
	inc 	esi
	call	gettokena
	pop 	ecx
	xchg	eax,ecx
	cmp 	eax,ecx
	sbb 	eax,eax
	not 	eax
	jmp 	getexpressionx_1
@@:
	cmp 	ax,"=<"
	jnz 	@F
	inc 	esi
	inc 	esi
	call	gettokena
	pop 	ecx
	xchg	eax,ecx
	cmp 	eax,ecx
	sbb 	eax,eax
	jmp 	getexpressionx_1
@@:
	cmp 	al,">"
	jnz 	@F
	inc 	esi
	call	gettokena
	pop 	ecx
	cmp 	eax,ecx
	sbb 	eax,eax
	jmp 	getexpressionx_1
@@:
	cmp 	al,"<"
	jnz 	@F
	inc 	esi
	call	gettokena
	pop 	ecx
	cmp 	ecx,eax
	sbb 	eax,eax
	jmp 	getexpressionx_1
@@:
	cmp 	al,"="
	jnz 	@F
	inc 	esi
	invoke	GetExpression,__STRING__
	pop 	ebx 			   ;alter wert -> EBX
	call	varcheck		   ;typueberpruefung + zuweisung
	jmp 	getexpressionx_1
@@:
	pop 	eax
	retn

;*** return: wert in EAX ***

gettokena:
	call	gettokenb
gettokena_1:
	push	eax
	mov 	al,[esi]
	cmp 	al,'+'
	jnz 	@F
	inc 	esi
	call	gettokenb
	pop 	ecx
	add 	eax,ecx
	jmp 	gettokena_1
@@:
	cmp 	al,'-'
	jnz 	@F
	inc 	esi
	call	gettokenb
	mov 	ecx,eax
	pop 	eax
	sub 	eax,ecx
	jmp 	gettokena_1
@@:
	pop 	eax
	retn

;*** return: wert in EAX ***

gettokenb:
	call	gettoken
gettokenb_1:
	push	eax
	mov 	ax,[esi]
	cmp 	ax,'<<'
	jnz 	@F
	inc 	esi
	inc 	esi
	call	gettoken
	mov 	ecx,eax
	pop 	eax
	shl 	eax,cl
	jmp 	gettokenb_1
@@:
	cmp 	ax,'>>'
	jnz 	@F
	inc 	esi
	inc 	esi
	call	gettoken
	mov 	ecx,eax
	pop 	eax
	shr 	eax,cl
	jmp 	gettokenb_1
@@:
	pop 	eax
	retn

;*** return: wert in EAX ***

gettoken:
	call	gettokenx
gettoken_1:
	push	eax
	call	ignsp
	cmp 	al,'*'
	jnz 	@F
	inc 	esi
	call	gettokenx
	pop 	ecx
	imul	ecx
	jmp 	gettoken_1
@@:
	cmp 	al,'/'
	jnz 	@F
	cmp 	byte ptr [esi+1],al
	jz		@F
	inc 	esi
	call	gettokenx
	mov 	ecx,eax
	pop 	eax
	push	[excexit]
	mov		[excexit],offset divzero
	xor 	edx,edx
	idiv	ecx
	pop		[excexit]
	jmp 	gettoken_1
@@:
	mov 	eax,[esi]
	and 	eax,0FFFFFFh
	cmp 	eax,"DOM"
	jnz 	@F
	mov 	al,[esi+3]
	call	trnchk
	jnz 	@F
	inc 	esi
	inc 	esi
	inc 	esi
	call	gettokenx
	mov 	ecx,eax
	pop 	eax
	push	[excexit]
	mov		[excexit],offset divzero
	xor 	edx,edx
	idiv	ecx
	pop		[excexit]
	mov 	eax,edx
	jmp 	gettoken_1
@@:
	pop 	eax
	retn

;*** return: wert in EAX,typ in vartype ***

gettokenx::
	call	ignsp
	cmp 	al,'+'				;un„rer opcode +?
	jnz 	@F
	inc 	esi
	call	gettokenx_0
	retn
@@:
	cmp 	al,'-'
	jnz 	@F
	inc 	esi
	call	gettokenx_0
	neg 	eax
	retn
@@:
	cmp 	al,'!'
	jnz 	@F
	inc 	esi
	call	gettokenx_0
	cmp 	eax,1
	sbb 	eax,eax
	retn
@@:
gettokenx_0:
	mov 	al,[esi]
	cmp 	al,'&'			   ;real mode segment?
	jnz 	@F
	inc 	esi
	call	gettokenaddr
	cmp 	byte ptr vartype,__FPTR__
	jnz 	gettokenx_er1
	mov 	byte ptr vartype,__RMLPTR__
gettokenx_er1:
	cmp 	byte ptr vartype,__RMLPTR__
	jz		gettokenx_ex1
	mov 	byte ptr vartype,__STRING__
gettokenx_ex1:
	retn
@@:
	cmp 	al,'#'			   ;prot mode segment?
	jnz 	@F
	inc 	esi
	call	gettokenaddr
	cmp 	byte ptr vartype,__RMLPTR__
	jnz 	gettokenx_er2
	mov 	byte ptr vartype,__FPTR__
gettokenx_er2:
	cmp 	byte ptr vartype,__FPTR__
	jz		gettokenx_ex2
	mov 	byte ptr vartype,__STRING__
gettokenx_ex2:
	retn
@@:
	cmp 	al,'%'			   ;lineare adresse?
	jnz 	@F
	inc 	esi
	call	gettokenx_1
	cmp 	byte ptr vartype,__STRING__
	jz		error1
	mov 	byte ptr vartype,__FPTR__
	mov 	edx,[__flatsel]
	mov 	varseg,edx
error1:
	retn
@@:
gettokenaddr:
	call	gettokenx_1
	push	eax
	mov 	al,[esi]
	cmp 	al,':'
	jnz 	@F
	inc 	esi
	call	gettokenx_1
	xor 	edx,edx
	mov 	varptr,edx
	pop 	edx
	push	eax
	cmp 	byte ptr vartype,__STRING__
	jz		@F
	mov 	byte ptr vartype,__FPTR__
	test	[fEntry], FENTRY_REAL
	jz		gettokenaddr_1
	mov 	byte ptr vartype,__RMLPTR__
gettokenaddr_1:
	mov 	varseg,edx
@@:
	pop 	eax
	retn
gettokenx_1:
	xor 	eax,eax
	mov 	varptr,eax
	mov 	al,[esi]
	cmp 	al,'['
	jnz 	@F
	call	eklammern
	retn
@@:
	cmp 	al,'('
	jnz 	@F
	call	klammern
	retn
@@:
	call	getnumber						  ;zahl einlesen
	jnc 	setnumbertype
	mov 	al,[esi]
	cmp 	al,'"'
	jnz 	@F
	call	handlestring
	retn
@@:
	call	trnchk
	jz		gettokenx_err
	call	CopySym2Heap					;symbol -> near heap
	push	eax
	invoke	SearchStdSymbol,pSymtab,eax 	;symboltabelle durchsuchen
	pop 	edx 							;adresse text
	jc		@F								;nicht gefunden
	call	handlesym
	retn
@@:
	mov 	ecx,[dwUserSymbols]
	jecxz	@F
	invoke	SearchUserSymbol,ecx,edx
	jc		@F
	call	handlesym
	retn
@@:
	push	edx
	mov 	ebx,edx 						  ;jetzt geladene Symbole
	call	getmysymaddr					  ;durchsuchen
	pop 	edx
	and 	eax,eax
	jz		@F							;nicht gefunden -> error
	push	eax
	shr 	eax,16
	mov 	varseg,eax
	pop 	eax
	movzx	eax,ax
	mov 	byte ptr vartype,cl
	retn
@@:
	push	edx
	call	checkifnewvar
	jc		gettokenx_err
	mov 	vartype,ecx
	mov 	varseg,edx
	retn
gettokenx_err:: 								  ;syntax error
	mov 	byte ptr vartype,-1
	jmp 	getexpression_ex
gettokenx_ex:
	retn
setnumbertype:
	xor 	edx,edx
	mov 	cl,byte ptr vartype
	cmp 	cl,__STRING__
	jnz 	@F
	mov 	byte ptr vartype,__DWORD__
	retn
@@:
	cmp 	cl,__QWORD__
	jnz 	@F
	mov 	varseg,edx
	retn
@@:
	cmp 	cl,__TBYTE__
	jnz 	@F
	mov 	varseg,edx
@@:
	retn

handlestring:
	inc 	esi
	push	edi
	mov 	edi,[pNearHeap]
@@:
	lodsb
	and 	al,al
	jz		handlestring_2
	cmp 	al,'"'
	jz		handlestring_3
	cmp 	al,'\'
	jnz 	handlestring_1
	call	handlebackslash
handlestring_1:
	stosb
	jmp 	@B
handlestring_3:
	inc 	esi
	mov 	al,0
handlestring_2:
	stosb
	dec 	esi
	xchg	edi,[pNearHeap]
	mov 	eax,edi
	pop 	edi
	retn

;*** "[" aufgetreten: ***

eklammern:
	inc 	esi
	invoke	GetExpression,__STRING__ ;typ in CL
	cmp 	byte ptr [esi],']'
	push	[excexit]
	mov		[excexit],offset eklammern_er
	jnz 	gettokenx_err		   ;---> exit ist ok
	inc 	esi
	cmp 	cl,-1
	jz		eklammern_er
	cmp 	cl,__RMLPTR__
	jnz 	@F
	shl 	edx,4
	add 	eax,edx
	mov 	fs,[__flatsel]
	jmp 	eklammern_1
@@:
	cmp		cl,__CONST__
	jnz		@F
	mov		edx,ds
@@:        
	verr	dx
	jnz 	eklammern_er
	mov 	fs,edx
	lar 	ebx,edx
	lsl 	edx,edx
	and 	bh,0Ch
	cmp 	bh,4				   ;expand down?
	jnz 	@F
	cmp 	eax,edx
	jbe 	eklammern_er
	jmp 	eklammern_1
@@:
	cmp 	eax,edx
	ja		eklammern_er
	dec 	edx
	cmp 	eax,edx
	ja		eklammern_byte
	jz		eklammern_word
	dec 	edx
	cmp 	eax,edx
	jz		eklammern_er
eklammern_1:
	mov 	eax,fs:[eax]
	cmp 	byte ptr vartype,__STRING__
	jnz 	@F
	mov 	byte ptr vartype,__DWORD__
@@:
	jmp		ek_done
eklammern_byte:
	mov 	al,fs:[eax]
	cmp 	byte ptr vartype,__BYTE__
	jnz 	eklammern_er
	jmp		ek_done
eklammern_word:
	mov 	ax,fs:[eax]
	cmp 	byte ptr vartype,__WORD__
	ja		eklammern_er
	jmp		ek_done
eklammern_er:
	mov 	byte ptr vartype,__VOID__
ek_done:        
	pop		[excexit]
	retn


;*** "(" aufgetreten: ***

klammern:
	inc 	esi
	mov 	eax,[esi]
	cmp 	eax,"ETYB"
	jz		klammern_21
	cmp 	eax,"DROW"
	jz		klammern_22
	cmp 	eax,"DRWD"
	jz		klammern_24
klammern_1:
	invoke	GetExpression,__STRING__
	cmp 	byte ptr [esi],')'
	jnz 	gettokenx_err					  ;dieser exit ist ok
	mov 	byte ptr vartype,cl
	mov 	varseg,edx
	inc 	esi
	retn
klammern_21:
	mov 	al,__BYTE__
	jmp 	klammern_20
klammern_22:
	mov 	al,__WORD__
	jmp 	klammern_20
klammern_24:
	mov 	al,__DWORD__
klammern_20:
	cmp 	byte ptr [esi+4],')'
	jnz 	klammern_1
	add 	esi,5
	push	eax
	call	gettokenx
	pop 	ecx
	mov 	byte ptr vartype,cl
	cmp 	cl,__DWORD__
	jz		@F
	movzx	eax,ax
@@:
	cmp 	cl,__BYTE__
	jnz 	@F
	mov 	ah,0
@@:
	retn


checkptr:
	cmp 	cl,__FPTR__
	jz		@F
	cmp 	cl,__RMLPTR__
	jz		@F
	cmp 	cl,__LPTR__
@@:
	retn

;*** handle symbols
;*** inp: eax=symbol descriptor
;*** out: eax=value

handlesym:
	mov cl,[eax.SYMBOL.bType]
	test cl,_FNCALL_
if ?PARMPROT
	jz handlesym_0
else
	jz handlesym_0
endif
	mov espvar,esp
	mov tempvar,eax
	mov bl,[eax.SYMBOL.bType2]					;anzahl argumente
	test bl,_PARMOK_
	jz @F
	mov eax,[eax.SYMBOL.dwAddr] 				;diesen parameter immer
	and eax, eax
	jz @F
	mov eax,[eax]
	push eax
@@:
	xor eax,eax
	and bl,0Fh
	cmp bl,0
	jz symproc_3
	mov al,'('
handlesym_2:
	cmp [esi],al
	jnz gettokenx_err
	inc esi
	push ebx
	invoke GetExpression,__STRING__
	pop ebx
	call checkptr
	jnz @F
	push edx
@@:
	push eax 							;wert des ausdrucks
	cmp cl,-1
	jz handlesym_er1
	mov al,','
	dec bl
	jnz handlesym_2
	cmp byte ptr [esi],')'
	jnz handlesym_er1
	inc esi
symproc_3:
	xchg esi,tempvar
	mov ch,cl							;ch=typ des letzten parameters
	mov cl,[esi.SYMBOL.bType]
	mov al,0
	call [esi.SYMBOL.dwProc]
	mov esp,espvar
	mov varptr,esi

	mov esi,tempvar
	jc handlesym_er
	and cl,0AFh 						;reset _FNCALL_ _PARMOK_
	jmp handlesym_1
handlesym_er1:
	mov esp,espvar
handlesym_er:
if ?PARMPROT
	@stroutc "error getting symbol value, ecx="
	@dwordout ecx
	@cr_out
endif
	mov cl,-1
	mov byte ptr vartype,cl
	ret
handlesym_0:
	cmp byte ptr vartype,__STRING__
	jnz @F
	mov byte ptr vartype,cl 		;typ des operanden
@@: 								;esi -> auf symbolelement
	mov varptr,eax					;^ auf elem in symboltabelle
	mov eax, [eax.SYMBOL.dwProc] 	;adresse holen
	mov edx, [eax+4]
	cmp cl, __DWORD__
	ja @F
	invoke getdefseg,varptr
	mov defseg,edx
@@:
	mov bx,[eax+8]						  ;falls __TBYTE__
	mov eax,[eax]						  ;wert des symbols holen
handlesym_1:
	and cl,0DFh 						  ;reset _RDONLY_
	mov byte ptr vartype,cl 			  ;typ des operanden
if ?PARMPROT
	pushad
	invoke printf, CStr("sym value=%x:%x type=%x pSym=%x",lf), edx, eax, ecx, varptr
	popad
endif
	cmp cl,__LPTR__
	jnz @F
if 1
	mov edx,eax			 ;2 zeilen deaktiviert 24.11.99
	shr edx,16
endif
	movzx eax,ax
	mov word ptr varseg,dx
	retn
@@:
	cmp cl,__RMLPTR__
	jnz @F
if 1
	mov edx,eax			 ;2 zeilen deaktiviert 24.11.99
	shr edx,16
endif
	movzx eax,ax
	mov word ptr varseg,dx
	retn
@@:
	cmp cl,__FPTR__
	jnz @F
	mov word ptr varseg,dx
	retn
@@:
	cmp cl,__TBYTE__
	jnz @F
	mov varseg,edx
	mov word ptr vartype+2,bx
	retn
@@:
	cmp cl,__QWORD__
	jnz @F
	mov varseg,edx
	retn
@@:
	cmp cl,__WORD__
	ja handlesym_ex
	movzx eax,ax
	cmp cl,__BYTE__
	jnz handlesym_ex
	mov ah,00
handlesym_ex:
	retn


checktypes:
	cmp cl,__STRING__			;rechter operand string?
	jnz @F
	cmp cl,ch					;string nur an stringvariable
	jnz checktypes_er			;zuweisbar
	and esi,esi
	jz checktypes_er
	push ecx
	invoke PutStringInHeap,eax 	;string in far heap kopieren
	pop ecx
	clc
	retn
@@:
	cmp ch,__STRING__
	jz checktypes_er
	clc
	retn
checktypes_er:
	stc
	retn

;*** zuweisungs operator

;*** edx:eax=wert rechter operand (evtl in HIWORD(ecx) bytes 8+9) ***
;*** ebx=wert linker operand ***
;*** out: varseg:eax=neuer wert ***
;*** in vartype ist typ des linken operanden ***
;*** in cl ist typ des rechten operanden ***

?TRACE = 0

varcheck:
	@tprintf <"enter varcheck",lf>
	push	ebx
	push	esi
	cmp 	cl,-1
	jz		varcheck_1
	mov 	esi,varptr				;falls keine variable, ist nur
	and 	esi,esi 				;adresse moeglich
	jnz 	@F
	cmp 	byte ptr vartype,__STRING__
	jz		varcheck_1
	mov 	ch,cl
	mov 	byte ptr vartype,cl
	jmp 	varcheck_2
@@:
	mov 	ch,[esi.SYMBOL.bType]
	test	ch,_RDONLY_ 			;falls variable r/o, alten wert
	jnz 	varcheck_3
varcheck_2:
	call	checktypes
	jc		varcheck_1
	xchg	edx,varseg
if ?TRACE
	pushad
	mov cl,vartype
	@stroutc "call wrinp for "
	push eax
	and esi,esi
	jz @F
	mov ebx,[esi.SYMBOL.pText]
	invoke __stroutebx
@@:
	@stroutc ", new value="
	@dwordout varseg
	pop eax
	@dwordout eax
	@stroutc ", type="
	@dwordout ecx
	@cr_out
	popad
endif
	mov 	cl,ch
	and 	esi,esi
	jnz 	@F
	verw	dx
	jnz 	varcheck_3
	push	ds
	mov 	ds,edx
	mov 	edx,varseg
	xor 	esi,esi
	call	wrinp
	pop 	ds
	jmp 	varcheck_ex
@@:
	mov 	edx,varseg
	call	wrinp
	jmp 	varcheck_ex
varcheck_3:
	mov 	byte ptr vartype,__VOID__
	jmp 	varcheck_ex
varcheck_1:
	mov 	byte ptr vartype,-1
varcheck_ex:
	pop 	esi
	pop 	ebx
	retn

GetExpression endp

handlesemicolon proc
	inc esi
	mov al,[esi]
	and al,al
	jz exit
if 0       
	pushad
	mov edi,[pKbdBuff]
@@:
	lodsb
	stosb
	and al,al
	jnz @B
	popad
else
	mov pKbdBuff, esi
endif        
	or [fStat], FSTAT_BUFNOTEMPTY
exit:
	ret
handlesemicolon endp

;*** in grossbuchstaben umwandeln ***
;*** white spaces auf 1 zeichen begrenzen ***

preparestring proc stdcall public uses esi edi pString:dword

	mov 	esi,pString
	mov 	edi,[pNearHeap]
preparestring_0:
	lodsb
	call	kl2gr
	stosb
	and 	al,al
	jz		preparestring_ex
	cmp 	al,'"'
	jnz 	preparestring_1
	mov 	ah,al
@@:
	lodsb
	stosb
	and 	al,al
	jz		preparestring_ex
	cmp 	al,ah
	jnz 	@B
	jmp 	preparestring_0
preparestring_1:
	call	trnchk2
	jnz 	preparestring_0
	mov 	byte ptr [edi-1],' '
@@:
	lodsb
	call	trnchk2
	jz		@B
	dec 	esi
	jmp 	preparestring_0
preparestring_ex:
	mov 	eax,edi
	xchg	eax,[pNearHeap]
if ?PARMPROT
	pushad
	@stroutc lf,"prepared string: "
	@strout eax
	@cr_out
	popad
endif
	ret
preparestring endp

;--- put ECX bytes beginning at EAX in near heap, returns eax=pMem

PutBytesInNearHeap proc uses edi esi
	mov 	edi,[pNearHeap]
	mov 	esi,eax
	rep 	movsb
	mov 	al,0
	stosb
	mov 	eax,edi
	xchg	eax,[pNearHeap]
	ret
PutBytesInNearHeap endp

;*** get a parameter into an argument descriptor
;*** translate symbols into values
;*** out: EDX=string pointer behind parameter or NULL
;*** out: EAX=-1 if error, else ???

GetParameter proc stdcall public uses esi edi string:ptr byte, argdsc: ptr ARGDESC

local	args:dword

	xor 	eax,eax
	mov 	args,eax
	mov 	esi,string
	mov 	edi,argdsc
getparms_0:
	mov 	byte ptr [edi.ARGDESC.dwType],__VOID__

;	mov	 eax,[dsdef]
	xor 	eax,eax
	mov 	dword ptr [edi.ARGDESC.dwSeg],eax

	call	ignsp
	mov 	ax,[esi]
	cmp 	al,00
	jz		getparms_5				;string has ended
	cmp 	al,';'
	jz		getparms_4
	cmp 	ax,"//"
	jz		getparms_5
	cmp 	al,','
	jz		getnextparm
	mov		string,esi				;save current string pointer
	invoke	GetExpression,__STRING__
	cmp 	cl,-1
	jz		getparms_er
	mov 	[edi.ARGDESC.dwOfs],eax
	mov 	[edi.ARGDESC.dwPtr],ebx
	mov 	[edi.ARGDESC.dwSeg],edx
	mov 	[edi.ARGDESC.dwType],ecx
	mov 	ecx,esi
	mov		eax,string
	sub 	ecx,eax
	call	PutBytesInNearHeap		;put ECX bytes into near heap
	mov 	[edi.ARGDESC.dwPx],eax
getnextparm:
if ?PARMPROT
	call	parmprot
endif
	inc 	byte ptr args
	mov 	ax,[esi]
	cmp 	al,00
	jz		getparms_5
	cmp 	al,';'
	jz		getparms_4
	cmp 	al,','
	jnz 	@F
	inc 	esi
@@:
	cmp 	ax,"//"
	jz		getparms_5
	mov 	al,[esi-1]
	call	trnchk1			; , ; 00 TAB SPACE
	jz		getparms_ex
getparms_er:
;	@stroutc "syntax error",lf
	mov 	eax,-1
	jmp 	@F
getparms_4:
	call	handlesemicolon
getparms_5:
	xor 	esi,esi 		;string nicht weiter untersuchen
getparms_ex:
	mov 	eax,args
@@:
	mov 	edx,esi
	ret
GetParameter endp

if ?PARMPROT
parmprot:
	pushad
	movzx	eax,byte ptr [edi.ARGDESC.dwType]
	invoke printf, CStr("getp: edi=%x type=%x seg=%x"), edi, eax, [edi.ARGDESC.dwSeg]
	invoke printf, CStr(" ofs=%x ^sym=%x |%s|",lf), [edi.ARGDESC.dwOfs],\
		[edi.ARGDESC.dwPx], [edi.ARGDESC.dwPx]
	popad
	ret
endif

handlebackslash:
	mov 	dl,al
	lodsb
	cmp 	al,'n'
	jz		handle_1
	cmp 	al,'"'
	jz		handle_2
	cmp 	al,'\'
	jz		handle_3
	xchg	al,dl
	stosb
	xchg	al,dl
	ret
handle_1:
	mov 	al,cr
	stosb
	mov 	al,lf
	ret
handle_2:
	mov 	al,'"'
	ret
handle_3:
	ret

;*** get an item, dont try to parse further ***
;*** item is stored on near heap
;*** out: eax=???
;*** out: edx=string behind token (or NULL if done)

GetStringToken proc stdcall public string:ptr BYTE, argdsc:ptr ARGDESC

local	rc:dword

	pushad
	mov 	ebx,argdsc
	mov 	edi,[pNearHeap]
	mov 	[ebx.ARGDESC.dwPx],edi
	mov 	[ebx.ARGDESC.dwOfs],edi
	mov 	byte ptr [ebx.ARGDESC.dwType],__VOID__
	xor 	eax,eax
	mov 	[ebx.ARGDESC.dwSeg],eax
	mov 	[edi],al
	mov 	rc,eax
	mov 	esi,string

	call	ignsp
	and 	al,al
	jz		getastring_exx		;string done!
	cmp 	al,';'
	jnz 	@F
	call	handlesemicolon
	jmp 	getastring_exx
@@:
	mov 	ax,[esi]
	cmp 	ax,"//"
	jz		getastring_exx
	xor 	ecx,ecx
parseargs5:
	lodsb
	call	trnchk1
	jz		parseargs1		; string token done
	cmp 	al,'"'
	jz 		@F
	stosb
	inc 	ecx
	jmp 	parseargs5
@@:							; is a string enclosed in quotes
	mov 	ah,al
nextchar:
	lodsb
	and 	al,al
	jz		parseargs1		; done (missing ending quote)
	cmp 	al,ah
	jz		parseargs1x		; done
	cmp 	al,'\'
	jnz 	@F
	call	handlebackslash
@@:
	stosb
	inc 	ecx
	jmp 	nextchar
parseargs1:							;done string token
	mov 	ah,al
	cmp 	ah,','
	jz		@F
	dec 	esi
@@:
parseargs1x:						;done string token
	mov 	al,0
	stosb
	mov 	[pNearHeap],edi
	jecxz	@F
	mov 	rc,ebx
	mov 	byte ptr [ebx.ARGDESC.dwType],__STRING__
@@:
	mov 	string,esi
	jmp 	getastring_ex
getastring_exx:
	mov 	dword ptr [string],0	;
getastring_ex:
	;@tprintf <"GetStringToken: %s, eax=%X, edx=%X",10>, [ebx].ARGDESC.dwPx, rc, string
	popad
	mov 	eax,rc
	mov 	edx,string
	ret
GetStringToken endp

tstkl proc near stdcall public	;return C wenn kleinbuchstabe
	cmp al,'a'
	jc	tstkl1
	cmp al,'z'+1
	ret
tstkl1: and al,al
	ret
tstkl endp

;*** symbol in tabellen suchen
;*** rc: NC = gefunden, dann
;*** EAX -> symbol in tabelle

SearchStdSymbol proc stdcall public symtab:dword,symptr:dword

	push esi
	push edi
if 0
	@stroutc "enter SearchStdSymbol: >"
	mov 	eax,symptr
	@strout eax
	@stroutc "<",lf
endif
	mov  esi,symtab
syms1:									;<- weitersuchen
	cmp  byte ptr [esi.SYMBOL.pNext],00  ;tabelle zuende?
	jz	 syms31
	mov  edi,symptr
	mov  edx,esi
	mov  al,[esi.SYMBOL.bType]
	and  al,8Fh
	cmp  al,__VOID__				;dummy eintrag?
	jz	 syms3
	mov  esi,[esi.SYMBOL.pText]
if 0
	@stroutc "compare with >"
	@strout esi
	@stroutc "<",lf
endif
@@: 								;<---
	lodsb
	and  al,al
	jz	 syms2
	mov  ah,al						;originalwert AL retten
	call kl2gr						;(AL) in grossbuchstaben umwandeln
	scasb
	jz	 @B
	cmp  al,ah						;war aktuelles zeichen kleinbuchstabe?
	jz	 syms3				  ;falls nicht, weitersuchen
	dec  edi
syms2:
	cmp  byte ptr [edi],0			;eingegebenes symbol muss zuende sein!
	jz	 syms30 			  ;---> gefunden
syms3:
	movzx esi,byte ptr [edx.SYMBOL.pNext]
	cmp  esi,0FFh
	jz	 @F
	add  esi,edx
	jmp  syms1						;---> naechstes symbol
@@:
	mov  esi,[edx.SYMBOL.pText]
	jmp  syms1
syms31: 								;tabelle zuende
	stc
	jmp  symsex 			  ;---> nicht gefunden
syms30:
	mov  eax,edx
symsex:
	pop  edi
	pop  esi
	ret
SearchStdSymbol endp

;--- search command in userdefined macros
;--- return C if nothing found
;--- else NC and symbol in eax

SearchUserSymbol proc stdcall public uses edx esi edi symtab:dword,text:ptr byte

local	strlength:dword

	mov  edi,text
	mov  ecx,-1
	mov  al,0
	repnz scasb
	not  ecx
	mov  strlength,ecx
	@tprintf <"user syms, searching %s",lf>, text

	mov  ebx,symtab
nextitem: 								 ;<- weitersuchen
	add  ebx,4
	@tprintf <"user sym: %X addr bez: %X, inh bez: %s",lf>,ebx,\
		[ebx.SYMBOL.pText], [ebx.SYMBOL.pText]
	mov  edi,text
	mov  esi,[ebx.SYMBOL.pText]

	mov  ecx,strlength
	repz cmpsb
	jnz  ssyms3
	mov  al,[esi-1]
	cmp  al,00
	jnz  ssyms3 			  ;ansonsten weitersuchen
	mov  eax,ebx
	clc
	jmp  ssymsex			  ;gefunden --->
ssyms3:
	mov  ebx,[ebx-4]
	and  ebx,ebx
	jnz  nextitem 					;---> naechstes symbol
	@tprintf <"searchusersyms: nothing found",10>
	stc 							;fertig (nicht gefunden)
ssymsex:
	ret
SearchUserSymbol endp

	end

