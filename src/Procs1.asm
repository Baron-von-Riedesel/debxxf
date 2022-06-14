
	.386
if ?FLAT
	.MODEL FLAT
else
	.MODEL SMALL
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
	include toolhelp.inc
	include fcntl.inc
	include extern32.inc
	include extern16.inc
	include rmdbghlp.inc

	.data

dlarg    dd 0             ;letztes argument fuer DL/DG/LDT/GDT
editarg  dq 0
edittype db __BYTE__      ;aktueller Typ fuer 'edit'

searchtype db __BYTE__

CCONST segment
tUndefined db "Undefined",0
CCONST ends

	.const

segtypes label dword
	dd offset tUndefined				;0
	dd CCStr( "avl. 286 TSS" )			;1
	dd CCStr( "LDT" )					;2
	dd CCStr( "Busy 286 TSS" )			;3
	dd CCStr( "286 Call Gate" ) 		;4
	dd CCStr( "Task Gate" ) 			;5
	dd CCStr( "286 Intr Gate" ) 		;6
	dd CCStr( "286 Trap Gate" ) 		;7
	dd offset tUndefined				;8
	dd CCStr( "avl. 386 TSS" )			;9
	dd offset tUndefined				;A
	dd CCStr( "Busy 386 TSS" )			;B
	dd CCStr( "386 Call Gate" ) 		;C
	dd offset tUndefined				;D
	dd CCStr( "386 Intr Gate" ) 		;E
	dd CCStr( "386 Trap Gate" ) 		;F

	.code

display1 proc stdcall

	@tprintf <"type =%X, offs=%X, seg=%X",lf>, ecx, [ebx], eax
	mov eax, [esi.ARGDESC.dwSeg]
	mov ecx, [esi.ARGDESC.dwType]
	test cl,_FNCALL_
	jz @F
	mov esi, [esi.ARGDESC.dwPtr]
	mov ebx, [esi.SYMBOL.dwProc] ;register und interne variable
	jmp display1_1
@@:
	mov ch, cl
	and cl, 0Fh
	cmp cl, __LPTR__
	jnz @F
	mov [ebx+2], ax
@@:
	cmp cl, __RMLPTR__
	jnz @F
	mov [ebx+2], ax
@@:
	mov cl, ch
display1_1:
	call symout
	cmp cl, __VOID__
	jz display1_5
	cmp  cl, __CONST__
	ja display1_5
	pushad
	invoke printf,CStr(" (dez=%d) (asc="), dword ptr [ebx]
	popad
	cmp cl, 2
	jbe @F
	mov cl, 4
@@:
display1_3:
	mov al, [ebx]
	cmp al, ' '
	jnb display1_2
	mov al, '.'
display1_2:
	@putchr al
	inc ebx
	dec cl
	jnz display1_3
	@putchr ')'
display1_5:
	invoke _crout
display1_ex:
	ret
display1 endp

;--- ?? command

_display proc c public pb:PARMBLK

        mov  cl,byte ptr pb.wArgc
        and  cl,cl
        jz   display_ex
        lea  esi,a1
        lea  ebx,pb.dwOffs1
@@:
        push ecx
        push ebx
        call display1
        pop  ebx
        add  esi,size ARGDESC
        add  ebx,3*4            ;je 3 parameter pro argument
        pop  ecx
        dec  cl
        jnz  @B
display_ex:
        ret
_display endp

_myprintf proc c public pb:PARMBLK

        mov     ecx,pb.wArgc
        mov     esi,esp
        lea     ebx,pb.dwOffs2
        dec     ecx
        jecxz   myprintf1
@@:
        mov     eax,3*4
        mul     ecx
        push    [ebx+eax-3*4]
        loop    @B
myprintf1:
        push    pb.dwOffs1
        call    printf
        mov     esp,esi
        invoke	_crout
        ret
_myprintf endp

;*** variable: wert zuweisen (subroutine von EDIT)
;*** inp: esi=descriptor
;*** eax=neuer wert(Offset)
;*** edx=neuer wert(Segment)
;*** ebx=evtl. 2. wert
;*** DS ist nicht unbedingt == DGROUP!

wrinp_er:
        @errorout ERR_TOO_FEW_PARAMETERS
        ret
wrinp_er2:
        add  esp,8
        @errorout ERR_FROM_FUNCTION
        ret

wrinp   proc stdcall public

        test cl,_FNCALL_
        jz   @F
        dec  ch                  ;variablenadresse weg
        cmp  ch,cs:[esi.SYMBOL.bType2] ;braucht funktion ein argument?
        jbe  wrinp_er
        mov  edx,eax       ;edx=dwOffs2,ebx=p3.dwOffs
        mov  al,1          ;modus: wert setzen
        push ebx
        push edx
        call dword ptr cs:[esi.SYMBOL.dwProc]
        jc   wrinp_er2
        add  esp,8
        ret
@@:
        and  esi,esi
        jz   @F
        mov  ebx,[esi.SYMBOL.dwProc]
@@:
        cmp  cl,__BYTE__
        jz   wrinp_b
        cmp  cl,__WORD__
        jz   wrinp_w
        cmp  cl,__DWORD__
        jz   wrinp_dw
;        cmp  cl,__CONST__
;        jz   wrinp_dw
        cmp  cl,__CHAR__
        jz   wrinp_b
        cmp  cl,__BOOL__
        jz   wrinp_bool
        cmp  cl,__STRING__
        jz   wrinp_dw
        cmp  cl,__LPTR__
        jz   wrinp_lptr
        cmp  cl,__RMLPTR__
        jz   wrinp_lptr
        cmp  cl,__FPTR__
        jz   wrinp_fw
        cmp  cl,__FWORD__
        jz   wrinp_fw
        cmp  cl,__QWORD__
        jz   wrinp_qw
        cmp  cl,__TBYTE__
        jz   wrinp_tb
if 0    
        cmp  cl,__OWORD__
        jz   wrinp_ow
endif  
        ret
wrinp_b:
        mov [ebx],al
        ret
wrinp_w:
        mov [ebx],ax
        ret
wrinp_dw:
        mov [ebx],eax
        ret
wrinp_lptr:
        mov [ebx+0],ax
        mov [ebx+2],dx
        ret
wrinp_fw:
        mov [ebx+0],eax
        mov [ebx+4],dx
        ret
wrinp_bool:
        cmp al,1
        cmc
        sbb al,al
        mov [ebx],al
        ret
wrinp_qw:
        mov [ebx+0],eax
        mov [ebx+4],edx
        ret
wrinp_tb:
        call wrinp_qw
        push ecx
        shr  ecx,16
        mov  [ebx+8],cx
        pop  ecx
        ret
wrinp   endp

;*** edit funktion ***

	@cmdproc
        
_editb	proc c public pb:PARMBLK
	mov [edittype],__BYTE__
	jmp _edit
_editb endp

_editw proc c public pb:PARMBLK
	mov [edittype],__WORD__
	jmp _edit
_editw	endp

_editd proc c public pb:PARMBLK
	mov [edittype],__DWORD__
;	jmp _edit
_editd endp

	@cmdprocend

_edit proc c public pb:PARMBLK

	mov  esi, [a1.dwPtr]
	and  esi, esi		  ;variable?
	jz	 @F
	mov  cl, [esi.SYMBOL.bType] ;muá hier restauriert werden, da flag
@@: 						;funktionsaufruf nicht mehr aktuell
	cmp  cl, __VOID__
	jnz  @F
	mov  ebx, dword ptr editarg+0	 ;voriges argument
	mov  di, word ptr editarg+4
	jmp  edit_3
@@:
	mov ebx, [pb.dwOffs1]
	test cl, _RDONLY_	  ;nur lese symbol?
	jnz edit3			  ;dann exit
	mov al, cl
	and al, 0fh
	cmp al, __FPTR__
	jz edit_1
	cmp al, __RMLPTR__
	jz edit_1
	cmp al, __LIST__
	jz edit2
	cmp al, __CONST__	  ;typ konstante (=Addr)?
	jb edit2
	and esi, esi		  ;strings nur editierbar wenn variable
	jnz edit2
	ja edit3			  ;__STRING__,__LSTRING__ nicht editierbar

edit_1: 				  ;<--- __CONST__ ,__FPTR__,__RMLPTR__
	mov eax, [pb.wSeg1]
	and eax, eax
	jnz @F
	mov eax, [r1.rDS]
@@:
	mov edi, eax
	verw ax
	jz edit_3 		  ;selector writeable?
	invoke getbaser, eax
	jnc @F
	@errorout ERR_PAGE_NOT_WRITEABLE
	jmp exit
@@:
	add  ebx, eax
	mov  edi, [__flatsel]
edit_3: 					  ;jetzt in di:ebx adresse
	mov es, edi
	mov edi, ebx
	mov esi, pb.dwOffs2
	cmp pb.p2.bType, __STRING__
	jz edit_328
	lea esi, pb.dwOffs2
	movzx ecx, [edittype]
	cmp cl, __BYTE__
	jz edit_32x
	cmp cl, __WORD__
	jz edit_32x
	cmp cl, __DWORD__
	jz edit_32x
	mov es, ebx
	@errorout ERR_UNKNOWN_TYPE
	jmp  exit
edit_328:					;string
	xor  ecx, ecx
@@:
	mov  al, [esi+ecx]
	and  al, al
	jz	 edit_32x
	inc  ecx
	jmp  @B
edit_32x:
if ?LOADVDD
	cmp [hVDD], -1
	jz @F
	invoke VerifyAddress, edi::ebx, 2
	jc error
@@:
endif
	push excexit
	mov excexit, offset editerr
	mov eax, esp
if ?WINDOWS
	call _Movsb
else
	rep movsb
endif
	mov dword ptr editarg+0, edi
	mov word ptr editarg+4, es
	push ds
	pop es
	jmp exit
editerr:
	mov  esp, eax
	pop  excexit
error:
	push ds
	pop es
	@errorout ERR_PAGE_NOT_WRITEABLE
	jmp exit
edit3:
	@errorout ERR_STRINGS_READONLY	;strings nicht editierbar
	jmp exit
edit2:								;Listen,Variablen usw.
	mov esi, [a1.dwPtr]
	mov eax, pb.p2.dwOffs
	mov ebx, pb.p3.dwOffs
	mov edx, pb.wSeg2
	mov ch, byte ptr pb.wArgc
	and cl, 1fh
	call wrinp
exit:
	ret
_edit endp


;*** string suchen: esi-> string, fs:edi-> bereich, ecx=laenge bereich
;*** out: C wenn nicht gefunden oder stringlaenge=0
;***      ansonsten pointer in edi

searchstring proc stdcall

	mov ebx, ecx

	push edi
	mov edi, esi 		 ;ende des strings suchen
	mov al,0
	mov ecx, -1
	repnz scasb
	not ecx
	dec ecx
	mov edx, ecx
	pop edi
	jecxz searchstring_er
	mov ecx, ebx
searchstring_1:
	mov al, [esi]
	push es
	push fs
	pop es
	repnz scasb
	pop es
	jnz searchstring_er
	mov eax, edi
	dec edi
	cmp ecx, edx
	jb searchstring_er
	push esi
	push ecx
	mov ecx, edx
	push es
	push fs
	pop es
	repz cmpsb
	pop es
	pop ecx
	pop esi
	mov edi,eax
	jnz searchstring_1
	dec edi
	clc
	ret
searchstring_er:
	stc
	ret
searchstring endp

;*** search - byte, word, dword oder string suchen ***

	@cmdproc

_searchb proc c public pb:PARMBLK
	mov [searchtype], __BYTE__
	jmp _search
_searchb endp

_searchw proc c public pb:PARMBLK
	mov [searchtype], __WORD__
	jmp _search
_searchw endp

_searchd proc c public pb:PARMBLK
	mov [searchtype], __DWORD__
_searchd endp

	@cmdprocend

_search proc c public pb:PARMBLK

local sstrlen:dword

	mov ecx,pb.dwOffs2
	mov eax,[pb.wSeg1]
	and eax,eax
	jnz @F
	mov eax,[r1.rDS]
@@:
	mov fs,eax
	cmp pb.p2.bType, __VOID__
	jnz @F
	mov ebx,fs
	call getlimitr
	jc search_ex
	mov ecx,eax
	sub ecx,pb.dwOffs1
	cmp ecx,-1
	jz @F
	inc ecx
@@:
	mov edi, pb.dwOffs1
	cmp pb.p3.bType, __STRING__
	jz search_2
	mov eax, pb.p3.dwOffs
	mov dl, [searchtype]
	dec edi
search_0:
	push es
	push fs
	pop es
	inc edi
	repnz scasb
	pop es
	jnz search_ex
	dec edi
	cmp dl,__BYTE__
	jz search_1
	jecxz search_ex
	cmp ax, fs:[edi]
	jnz search_0
	cmp dl,__WORD__
	jz search_1
	cmp ecx,3
	jb search_ex
	cmp eax, fs:[edi]
	jnz search_0
	jmp search_1
search_2:						;string suchen
	mov esi, pb.p3.dwOffs
	call searchstring
	jc search_ex
search_1:
if 0
	mov eax, fs
	@wordout eax
	@putchr ':'
	@dwordout edi
	invoke _crout
else
	push edi
	@errorout MSG_ITEM_FOUND
	pop ecx
endif
search_ex:
	ret
_search endp

_allocdos proc c public pb:PARMBLK

;local handle:dword

	mov al, pb.p1.bType
	cmp al, __CONST__
	jnz exit
	mov bx, word ptr pb.dwOffs1
	mov ax, 100h
	@DpmiCall
	jnc allocdos1
	invoke printf, CStr("Error: %04X, BX=%04X",lf), eax, ebx
	jmp exit
allocdos1:
	invoke printf, CStr("Segment=%04X, Selector=%04X",lf), eax, edx
exit:
	ret
_allocdos endp

_freedos proc c public pb:PARMBLK

	mov al, pb.p1.bType
	cmp al, __CONST__
	jnz exit
	mov dx, word ptr pb.dwOffs1
	mov ax, 0101h
	@DpmiCall
	jnc freedos1
	invoke printf, CStr("Error: %04X",lf), eax
freedos1:
exit:
	ret
_freedos endp

;--- REALLOCDOS command

_reallocdos proc c public pb:PARMBLK

;local handle:dword

	mov al, pb.p1.bType
	cmp al, __CONST__
	jnz exit
	mov al, pb.p2.bType
	cmp al, __CONST__
	jnz exit
	mov edx, pb.dwOffs1		;selector
	mov ebx, pb.dwOffs2		;neue anzahl paragraphen
	mov ax, 102h
	@DpmiCall
	jnc reallocdos1
	invoke printf, CStr("Error %04X, BX=%04X",lf), eax, ebx
	jmp exit
reallocdos1:
	invoke printf, CStr("ok",lf)
exit:
	ret
_reallocdos endp

;*** eax bytes ueber dpmi allokieren ***

allocdpmimem proc stdcall public uses ebx esi edi
	push eax
	pop cx
	pop bx
	mov ax, 501h
	@DpmiCall
	jnc @F
	@errorout ERR_FROM_DPMI
	stc
	jmp exit
@@:
	push bx
	push cx
	pop ebx
	push si
	push di
	pop esi
	invoke printf, CStr("address=%X, handle=%X",lf), ebx, esi
	mov eax,ebx
	clc
exit:
	ret
allocdpmimem endp

;*** memory allokieren DPMI 0501 ***

_allocmem proc c public pb:PARMBLK

	pushad
	mov al, pb.p1.bType
	cmp al, __CONST__
	jnz exit

	mov eax, pb.dwOffs1
	call allocdpmimem
exit:
	popad
	ret
_allocmem endp

;*** memory allokieren DPMI 0504 ***

_allocmemx proc c public pb:PARMBLK

	pushad
	xor ebx, ebx
	mov al, pb.p1.bType
	cmp al, __VOID__
	jz @F
	cmp al, __CONST__
	jnz exit
	mov ebx, pb.dwOffs1
@@:
	mov al, pb.p2.bType
	cmp al, __CONST__
	jnz exit
	mov ecx, pb.p2.dwOffs
	mov edx, 1
	mov al, pb.p3.bType
	cmp al, __CONST__
	jnz @F
	mov edx, pb.p3.dwOffs
@@:
	mov ax, 0504h
	@DpmiCall
	jc error
	invoke printf, CStr("address=%X, handle=%X",lf), ebx, esi
	jmp exit
error:
	@errorout ERR_FROM_DPMI
exit:
	popad
	ret
_allocmemx endp

_reallocmem proc c public pb:PARMBLK

	pushad
	mov al, pb.p1.bType
	cmp al, __CONST__
	jnz exit
	mov si, word ptr pb.dwOffs1+2
	mov di, word ptr pb.dwOffs1+0
	mov bx, word ptr pb.dwOffs2+2
	mov cx, word ptr pb.dwOffs2+0
	mov ax, 503h
	@DpmiCall
	jc error
	invoke printf, CStr("address=%X, handle=%X",lf), bx::cx, si::di
	jmp exit
error:
	@errorout ERR_FROM_DPMI
exit:
	popad
	ret
_reallocmem endp

_reallocmemx proc c public pb:PARMBLK

	pushad
	mov esi, pb.p1.dwOffs
	mov ecx, pb.p2.dwOffs
	mov edx, pb.p3.dwOffs
	and dl, not 2 
	mov ax, 505h
	@DpmiCall
	jc error
	invoke printf, CStr("address=%X, handle=%X",lf), ebx, esi
	popad
	ret
error:
	@errorout ERR_FROM_DPMI
	popad
	ret

_reallocmemx endp

;*** memory freigeben ***

_freemem proc c public pb:PARMBLK

	pushad
	mov si, word ptr pb.dwOffs1+2
	mov di, word ptr pb.dwOffs1+0
	mov ax, 502h
	@DpmiCall
	jnc @F
	@errorout ERR_FROM_DPMI
@@:
	popad
	ret
_freemem endp

;*** selector allokieren ***

_allocsel proc c public pb:PARMBLK

	mov al, pb.p1.bType
	cmp al, __CONST__
	@mov ecx, 1
	jnz @F
	mov ecx, pb.dwOffs1
@@:
	mov ax, 0000
	@DpmiCall
	jnc @F
	@errorout ERR_FROM_DPMI
	jmp exit
@@:
	movzx ebx, ax
	push ebx
	@errorout MSG_SELECTOR_ALLOCATED
	pop ebx
	cmp word ptr pb.wArgc,2
	jb exit
nextitem:
	push ecx
	mov edx,[pb.dwOffs2]
	push edx
	pop dx
	pop cx
	mov ax, 7
	int 31h
	cmp word ptr pb.wArgc,3
	jb @F
	mov edx,pb.p3.dwOffs
	push edx
	pop dx
	pop cx
	mov ax,8
	int 31h
@@:
	pop ecx
	add ebx,8
	dec cx
	jnz nextitem
exit:
	ret
_allocsel endp

;*** alloc special selector ***

_allocssel proc c public pb:PARMBLK

	mov ebx, pb.dwOffs1
	mov ax, 000Dh
	@DpmiCall
	jnc @F
	@errorout ERR_FROM_DPMI
@@:
	ret
_allocssel endp

;*** selector freigeben ***

_freesel proc c public pb:PARMBLK

	mov bx, word ptr pb.dwOffs1
	mov al, pb.p2.bType
	mov ecx, 1
	cmp al, __CONST__
	jnz @F
	mov ecx, pb.dwOffs2
	jecxz freeselex
@@:
freesel2:
	push ecx
	call freesel1
	pop ecx
	add bx, 8
	loop freesel2
freeselex:
	ret
freesel1:
	mov ax,0001
	@DpmiCall
	jnc @F
	movzx ebx,bx
	push ebx
	@errorout ERR_FREE_SELECTOR
@@:
	retn
_freesel endp

;*** real mode selector holen ***

_rmsel proc c public pb:PARMBLK

	mov bx, word ptr pb.dwOffs1
	mov ax, 0002
	@DpmiCall
	jnc @F
	@errorout ERR_FROM_DPMI
	jmp exit
@@:
	movzx eax, ax
	push eax
	@errorout MSG_SELECTOR_ALLOCATED
	pop eax
exit:
	ret
_rmsel endp

;*** lock memory ***

	@cmdproc

_unlockmem proc c public pb:PARMBLK
	mov ax, 601h
	jmp _lockunlockmem
_unlockmem endp

_unlockmemrm proc c public pb:PARMBLK
	mov ax, 602h
	jmp _lockunlockmem
_unlockmemrm endp

_lockmemrm proc c public pb:PARMBLK
	mov ax, 603h
	jmp _lockunlockmem
_lockmemrm endp

_markpage proc c public pb:PARMBLK
	mov ax, 702h
	jmp _lockunlockmem
_markpage endp

_discardpages proc c public pb:PARMBLK
	mov ax, 703h
	jmp _lockunlockmem
_discardpages endp

_lockmem proc c  public pb:PARMBLK
	mov ax, 600h
;	jmp _lockunlockmem
_lockmem endp

	@cmdprocend
        
_lockunlockmem proc c uses esi edi pb:PARMBLK

	mov bx, word ptr pb.dwOffs1+2
	mov cx, word ptr pb.dwOffs1+0
	mov si, word ptr pb.dwOffs2+2
	mov di, word ptr pb.dwOffs2+0
	@DpmiCall
	jnc @F
	@errorout ERR_FROM_DPMI
@@:
	ret
_lockunlockmem endp

;*** selectoren aus GDT und LDT ausgeben ***

_gdtout proc c public pb:PARMBLK

	mov esi, offset gdtitem
	mov dl, 0
	jmp ldtout1
_gdtout endp

_ldtout proc c public pb:PARMBLK

	mov esi, offset ldtitem
	mov dl, 4
ldtout1::
	cmp cl, __VOID__
	jnz @F
	pushad
	call regsout_2
	popad
@@:
	mov ebx, pb.dwOffs1
	cmp cl, __VOID__
	jnz @F
	mov ebx, dlarg
@@:
	and bl, 0FBh
	or bl, dl
	mov ecx, 10h
	cmp byte ptr [a2.dwType],__CONST__
	jnz selector2
	mov ecx, [a2.dwOfs]
	jecxz selectorex
selector2:
	push ebx
	push ecx
	invoke selectorout, ebx
	pop ecx
	pop ebx
	add ebx, 8
	loop selector2
	mov dlarg, ebx
selectorex:
	ret
_ldtout endp

;*** gate ausgeben ***
;*** bereich: lineare adresse gdt/ldt ***
;*** maxlen:  limit von gdt/ldt ***

gateout proc stdcall public ngate:dword,bereich:dword,maxlen:dword

local   tempvar1:dword
local   tempvar2:dword
local   selattr:dword
local   gateoffs:dword
local   gatesel:dword

        movzx   ebx,word ptr ngate
        mov     tempvar1,ebx
        mov     dword ptr selattr,0
        cmp     ebx,maxlen
        jnc     novalgate
        and     bl,0F8h
        add     ebx,bereich                ;basisadresse der IDT,GDT,LDT
        mov     ax,@flat:[ebx+4]
        mov     word ptr selattr,ax        ;selector attribute
        shr     ax,13
        and     ax,03
        or      word ptr tempvar1,ax
        mov     ax,@flat:[ebx+6]
        shl     eax,16
        mov     ax,@flat:[ebx+0]
        mov     gateoffs,eax
        movzx   eax,word ptr @flat:[ebx+2]
        mov     gatesel,eax
novalgate:
        mov     eax,tempvar1
        shr     eax,3
        mov     tempvar2,eax
        invoke printf, CStr("Gate=%04X(%02X) "), tempvar1, tempvar2
        cmp     word ptr selattr,0
        jnz     @F
        @strout offset tQuestions
        invoke  _crout
        jmp     exit
@@:
        invoke printf, CStr("Selector=%04X Offset=%08X Attr=%04X "), gatesel, gateoffs, selattr
        mov     ax,word ptr selattr
        shr     ax,8
        call    getstype
        invoke  _crout
exit:
        ret
gateout endp

selectorout proc stdcall public sel:dword

local   tempvar1:dword
local   selattr:dword
local   sellimit:dword

	movzx ebx, word ptr sel
	mov tempvar1, ebx
	mov dword ptr selattr,0
	call getaccr
	jc nogatesel1
	shr eax,8
	mov selattr,eax 	 ;ACCRIGHTS
	test al,10h			 ;normale selectoren?
	jnz nogatesel
	test al,4
	jz nogatesel
	mov eax,tempvar1
	mov ebx,dword ptr [rGDTR+2]
	mov cx,word ptr [rGDTR+0]
	test ax,4
	jz @F
	mov bx,[rLDT]
	call getlimitr
	push eax
	movzx eax,[rLDT]
	invoke getbaser,eax
	pop ecx
	mov ebx,eax
	mov eax,tempvar1
@@:
	invoke gateout, eax, ebx, ecx
	jmp exit
nogatesel1:
	mov ax, 0
	stc
nogatesel:
	pushfd
	shr ax, 5
	and ax, 3
	or word ptr tempvar1, ax
	invoke printf, CStr("Sel=%04X "), tempvar1
	popfd
	jnc @F
	@strout offset tQuestions
	call _crout
	jmp exit
@@:
	@stroutc "Base="
	invoke getbaser, tempvar1
	jnc @F
	call dispunknown
	jmp selectorout_1
@@:
	call dispeax
selectorout_1:
	@stroutc " Limit="
	mov ebx,tempvar1
	call getlimitr
	jnc @F
	call dispunknown
	jmp selectorout_2
@@:
	call dispeax
selectorout_2:
	@stroutc " Attr="
	@wordout selattr
	@putchr ' '
	mov eax,selattr
	call getstype
	call _crout
exit:
	ret
dispeax:
    invoke printf, CStr("%X"), eax
    retn
dispunknown:
    invoke printf, CStr("????????")
    retn
selectorout endp

mbit7   proc c
        test    al,80h
        jnz     @F
        @stroutc ",NP"
        ret
@@:
        @stroutc ",P"
        ret
mbit7   endp

mbit3   proc c
        test    al,8
        jnz     @F
        @stroutc "Data"
        ret
@@:
        @stroutc "Code"
        ret
mbit3   endp

mbit14  proc c
        test    ah,40h
        jnz     @F
        @stroutc ",16Bit"
        ret
@@:
        @stroutc ",32Bit"
        ret
mbit14  endp

mbit2   proc c
        test    al,4
        jnz     @F
        ret
@@:
        test    al,8
        jnz     @F
        @stroutc ",DN"
        ret
@@:
        @stroutc ",CF"
        ret
mbit2   endp

mbit1   proc c
        test    al,2
        jnz     mbit11
        test    al,8
        jnz     @F
        @stroutc ",R/O"
        ret
@@:
        @stroutc ",E/O"
        ret
mbit11:
        test    al,8
        jnz     @F
        @stroutc ",R/W"
        ret
@@:
        @stroutc ",E/R"
        ret
mbit1   endp

mbit0   proc c
        test    al,1
        jnz     @F
        ret
@@:
        @stroutc ",Acc"
        ret
mbit0   endp

getstype proc stdcall public
	test	ax,10h		  ;Memory oder System Segment?
	jz		getstype1
	push	ax
	call	mbit3		  ;Code/Data
	pop 	ax
	push	ax
	call	mbit14		  ;16Bit/32Bit
	pop 	ax
	push	ax
	call	mbit7		  ;Present/Not Present
	pop 	ax
	push	ax
	call	mbit1		  ;RO/RW bzw. EO/ER
	pop 	ax
	push	ax
	call	mbit2		  ;Conforming/Expand Down
	pop 	ax
	push	ax
	call	mbit0		  ;Accessed
	pop 	ax
	ret
getstype1:
	mov		ecx, CStr("valid ")
	test	al,80h
	jnz		@F
	mov		ecx, CStr("invalid ")
@@:
	push ebx
	and al, 0Fh
	shl al, 2
	movzx ebx, al
	jz @F
	invoke printf, ecx
@@:
	mov ebx, [ebx][segtypes]
	@strout ebx
	pop ebx
	ret
getstype endp

XMSHDLTABLE struct
bVersion	BYTE ?
bSizeDesc	BYTE ?
wNumItems	WORD ?
dwHandleArray	DWORD ?
XMSHDLTABLE ends

XMSHDLDESC struct
bFlags		BYTE ?
bLocks		BYTE ?
dwAddress	DWORD ?  ; in kB
dwLength	DWORD ?  ; in kB
XMSHDLDESC ends

;--- .XMS command: display XMS state

_xms proc c public pb:PARMBLK

local	dwSum:dword
local	_rmcs:RMCS

	xor ecx, ecx
	mov _rmcs.rSSSP, ecx
	mov _rmcs.rFlags, 202h
	mov _rmcs.rAX, 04300h
	mov bx, 2Fh
	mov ax, 300h
	lea edi, _rmcs
	@DpmiCall
	cmp byte ptr _rmcs.rAX, 80h
	jnz error1

	mov _rmcs.rAX, 4310h
	mov ax, 300h
	@DpmiCall
	movzx eax,_rmcs.rES
	movzx ecx,_rmcs.rBX
	invoke printf, CStr("XMS driver address: %X:%X",lf), eax, ecx

	xor ecx, ecx
	mov _rmcs.rBX, cx
	mov _rmcs.rES, cx
	mov _rmcs.rAX, 4309h
	mov bx, 2Fh
	mov ax, 300h
	@DpmiCall
	cmp byte ptr _rmcs.rAX, 43h
	jnz error2
	movzx eax, _rmcs.rES
	shl eax, 4
	movzx esi, _rmcs.rBX
	add esi, eax		; esi -> xms handle table
	movzx ebx, word ptr @flat:[esi].XMSHDLTABLE.dwHandleArray+2
	shl ebx, 4
	movzx ecx, word ptr @flat:[esi].XMSHDLTABLE.dwHandleArray+0
	add ebx, ecx		; ebx -> xms handle array
	invoke printf, CStr("  Address Size(kB) Lock Flgs",lf)
	push eax
	invoke printchars, '-', eax, 1
	movzx ecx, @flat:[esi].XMSHDLTABLE.wNumItems
	mov dwSum,0
	.while ecx
		movzx eax, @flat:[ebx].XMSHDLDESC.bFlags
		movzx edx, @flat:[ebx].XMSHDLDESC.bLocks
		mov esi, @flat:[ebx].XMSHDLDESC.dwAddress
		mov edi, @flat:[ebx].XMSHDLDESC.dwLength
		.if (esi && edi && (al & 3))
			add dwSum, edi
			push ecx
			xor ecx, ecx
			shld ecx, esi, 10
			shl esi, 10
			.if ecx
				invoke printf, CStr("%1X%08X %8u %4X %4X",lf), ecx, esi, edi, edx, eax
			.else
				invoke printf, CStr(" %8X %8u %4X %4X",lf), esi, edi, edx, eax
			.endif
			pop ecx
		.endif
		add ebx, sizeof XMSHDLDESC
		dec ecx
	.endw
	pop eax
	invoke printchars, '-', eax, 1
	invoke printf, CStr("%18u",lf), dwSum
	ret
error1:
	invoke printf, CStr("XMS not installed",lf)
	ret
error2:
	invoke printf, CStr("int 2Fh, ax=4309h not supported",lf)
	ret
_xms endp

;--- .VCPI command: display/disable/reenable vcpi

_vcpi proc c public pb:PARMBLK

local	_vcpihlp:PF32
local	_rmcs:RMCS

		mov bl, 67h
		mov ax, 200h
		@DpmiCall
		mov ax, cx
		or ax, dx
		jnz @F
		invoke printf, CStr("int 67h real mode vector is 0",lf)
		jmp exit
@@: 	   
		.if (pb.p1.bType == __VOID__)
			xor ecx, ecx
			mov _rmcs.rAX, 0DE00h
			mov _rmcs.rSSSP, ecx
			mov _rmcs.rFlags, 202h
			mov bx, 67h
			lea edi, _rmcs
			mov ax, 300h
			@DpmiCall
			movzx eax,word ptr _rmcs.rEAX
			.if (ah != 00h)
				mov ecx, CStr("off/not installed")
			.else
				mov ecx, CStr("on")
			.endif
			invoke printf, CStr("vcpi is %s",10,"int 67h, ax=DE00h returned %X",lf), ecx, eax
		.else
			mov edx, [__RmCS]
			mov fs, edx
			movzx edx,fs:[DEBRMVAR.wVCPI]
			mov eax, pb.dwOffs1
			push ax
			mov word ptr [_vcpihlp+4], fs
			mov dword ptr [_vcpihlp+0], edx
			call _vcpihlp
		.endif
exit:
		ret
_vcpi endp

if 0
;--- .DPMI command: disable/reenable dpmi host

_dpmi proc c public pb:PARMBLK

local	_dpmihlp:PF32
local	_rmcs:RMCS

	mov		bl,2Fh
	mov		ax,0200h
	@DpmiCall
	mov		edx, [__RmCS]
	mov		fs, edx
	movzx	edx,fs:[DEBRMVAR.wDPMI]
	mov		eax, pb.dwOffs1
	push	ax
	mov		dword ptr [_dpmihlp+0], edx
	mov		word ptr [_dpmihlp+4], fs
	call	_dpmihlp
exit:
	ret
_dpmi endp

endif

	END
