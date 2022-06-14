
@GetMode MACRO
	mov ah, 0Fh
	int 10h
ENDM

@SetMode MACRO mode
	mov al, mode
	xor ah, ah
	int 10h
ENDM

@SetPage MACRO page
	mov al, page
	mov ah, 05h
	int 10h
ENDM

@Scroll MACRO distance,atrib,upcol,uprow,downcol,downrow
IFDEF upcol
	mov cl,upcol
ENDIF
IFDEF uprow
	mov ch,uprow
ENDIF
IFDEF upcol
	mov dl,upcol
ENDIF
IFDEF updownrow
	mov dh, downrow
ENDIF
IFDEF atrib
	mov bh, atrib
ELSE
	mov bh, 07h
ENDIF
IF	distance LE 0
	mov ax, 0600h + (-(distance) AND 0FFh)
ELSE
	mov ax, 700h + (distance AND 0FFh)
ENDIF
	int 10h
ENDM
