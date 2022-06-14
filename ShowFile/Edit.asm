

	.286
	.model small
	option casemap:none
	option proc:private
	.386

?NOCONST  = 1
?NONULL   = 1
?SAVE     = 1

MAXCOL    equ 9+3*16+16+1
BEGTXT    equ 9+3*16+1

	include bios.inc
	include showfile.inc

setcsrpos proto

@SetCurPos macro x,y,pag
	mov ch, x
	mov cl, y
	mov bl,pag
	call setcsrpos
endm

	.data

editstathelp label byte
	DB "ESC=Quit "
	db "Move:    "
	db 26
	db " PgUp PgDn Tab Home End Ctrl-Home Ctrl-End"
ife ?SAVE
	db " F8=Save"
endif
	db 13

mycurpos  dw 0900h

; Extended key codes
exkeys    DB      47h,48h,4Bh,4Dh,50h,4Fh,73,81,119,117
ife ?SAVE
          db 42h
endif
lexkeys   EQU     $-exkeys              ; Table of keys
extable   DW      homek
          DW      upk
          DW      leftk
          DW      rightk
          DW      downk
          DW      endk
          dw      pgupk
          dw      pgdnk
          dw      chomek
          dw      cendk
ife ?SAVE
          dw      mysavefile
endif
normkeys  DB      "0123456789abcdef"
lnormkeys EQU     $-normkeys

	.code

; Procedure Pager
; Purpose   Displays status and text lines
; Input     Stack variable: lines to scroll (negative up, positive down)
;           Global variables: "sbuffer", "pbuffer", "linenum"
; Output    To screen

_edit     PROC public

          push    bp
          mov     bp,sp
          xor     ax,ax
          push    ax

          push    es
          pushad
;          mov     byte ptr f8cmd,13
;          mov     eax,fname
;          and     eax,eax
;          jz      @F
;          mov     byte ptr f8cmd,"F"
@@:
          mov     bx,sbuffer
          lar     cx,bx
          test    ch,08h
          jz      @F
          mov     ax,000Ah
          int     31h
          jc      @F
          xchg    ax,sbuffer
          mov     [bp-2],ax
@@:
          mov     eax,0
          call    mypager

          call    csr2bufpos
          jnc     @F
          call    chomek
@@:
          call    writehelp
edit1:
          push    ds
          pop     es
          call    setcursor
          call    getkey
          call    checkkey
          jnc     edit1

          @SetCurPos 133,133,pag

          mov     ax,[bp-2]
          and     ax,ax
          jz      @F
          xchg    ax,sbuffer
          mov     bx,ax
          mov     ax,0001
          int     31h
@@:
          popad
          pop     es
          mov     sp,bp
          pop     bp
          ret
_edit     ENDP

setcursor:
          mov     cx,mycurpos
          inc     cl
          @SetCurPos ch,cl,pag
          ret
checkkey:
          cmp     al,0E0h
          jz      @F
          cmp     al,0                  ; Is it a null?
          jne     checkkey_2
@@:
          mov     al,ah
          push    ds                    ; Load DS into ES
          pop     es
          mov     di,OFFSET exkeys      ; Load address and length of key list
          mov     cx,lexkeys
          repne   scasb                 ; Find position
          jnz     @F
          sub     di,(OFFSET exkeys)+1  ; Point to key
          shl     di,1                  ; Adjust pointer for word addresses
          call    extable[di]           ; Call procedure
@@:
          clc
          ret
checkkey_2:
          cmp     al,1Bh
          jnz     @F
          stc
          retn
@@:
          cmp     al,9
          jnz     @F
          call    rightk
          call    checkcolx
          cmp     cl,0
          jz      tab_1
          call    rightk
tab_1:
          clc
          retn
@@:
          cmp     byte ptr mycurpos+1,BEGTXT
          jnc     checkkey_3
          or      al,20h
          mov     di,offset normkeys
          mov     cx,lnormkeys
          repne   scasb
          jnz     @F
          call    csr2bufpos
          call    char2buf
          call    writeline
          call    rightk
          clc
          ret
@@:
          clc
          ret
checkkey_3:
          call    csr2bufpos
          stosb
          call    writeline
          call    rightk
          clc
          ret
char2buf:
          push   ax
          mov    ax,mycurpos
          call   checkcol
          pop    ax
          call   char2val
          mov    ah,es:[edi]
          cmp    cl,1          ;2. ziffer (low nibble?)
          jz     @F
          and    ah,0Fh
          shl    al,4
          or     al,ah
          mov    es:[edi],al
          retn
@@:
          and    ah,0F0h
          or     al,ah
          mov    es:[edi],al
          retn
char2val:
          sub    al,'0'
          cmp    al,10
          jb     char2val_ex
          sub    al,'a'-('0'+10)
char2val_ex:
          retn
csr2bufpos:
          mov    bx,mycurpos
csr2bufposx:
          push   ax
          mov    al,16
          mul    bl
          xchg   ax,bx
          mov    al,ah
          mov    ah,0
          cmp    al,BEGTXT
          jc     @F
          sub    al,BEGTXT
          jmp    csr2bufpos_1
@@:
          sub    al,9
          mov    cl,3
          div    cl
csr2bufpos_1:
          mov    ah,0
          add    ax,bx
          mov    di,ax
          movzx  edi,di
          add    edi,pbuffer
          cmp    edi,fsize
          cmc                     ;C wenn bufferptr zu gro·
          mov    es,sbuffer
          pop    ax
          ret
csr2scrnpos:
          push   ax
          mov    bx,mycurpos
          inc    bl
          mov    al,columns
          mul    bl
          mov    bl,bh
          mov    bh,0
          add    ax,bx
          add    ax,ax
          mov    di,ax
          add    di,[vidOfs]
          mov    es,vidadr
          pop    ax
          ret
homek:
          mov    ax,mycurpos
          mov    ah,09h
          mov    mycurpos,ax
          ret
chomek:
          mov    ax,0900h
          mov    mycurpos,ax
          ret
upk:
          mov    ax,mycurpos
          cmp    al,0
          jz     upk1
          dec    al
          mov    mycurpos,ax
          retn
upk1:
          call   lineup
          retn
leftk:
          mov    ax,mycurpos
          cmp    ah,9
          jz     leftk1
@@:
          dec    ah
          call   checkcol
          jz     @B
          mov    bx,ax
          call   csr2bufposx
          jc     @B
          mov    mycurpos,ax
          retn
leftk1:
          call   upk
          call   endk
          retn
checkcolx:
          mov    ax,mycurpos
checkcol:
          cmp    ah,9+3*16
          jnb    checkcol1
          push   ax
          mov    al,ah
          sub    al,9
          mov    ah,0
          mov    cl,3
          div    cl
          cmp    ah,2
          mov    cl,ah
          pop    ax
          retn
checkcol1:
          mov    cl,0
          cmp    ah,9+3*16
          retn
rightk:
          mov    ax,mycurpos
@@:
          inc    ah
          cmp    ah,MAXCOL
          jz     rightk_1
          call   checkcol           ;cursor an gÅltiger stelle?
          jz     @B           ;nein, nochmal 1 weiter
          mov    bx,ax
          call   csr2bufposx
          jnc    @F
          cmp    ah,BEGTXT
          jnb    rightk_2
          mov    ah,BEGTXT
@@:
          mov    mycurpos,ax
rightk_2:
          retn
rightk_1:
          call   downk
          call   pos1k
          ret
pos1k:
          mov    ax,mycurpos
          mov    ah,9
          mov    mycurpos,ax
          ret
downk:
          mov    ax,mycurpos
          inc    al
          cmp    al,byte ptr rows
          jz     downk1
          mov    bx,ax
          call   csr2bufposx
          jc     @F
          mov    mycurpos,ax
@@:
          retn
downk1:
          call   linedn
          retn
endk:
          mov    ax,mycurpos
          mov    ah,MAXCOL-1
          mov    mycurpos,ax
@@:
          call   csr2bufpos
          jnc    @F
          dec    byte ptr mycurpos+1
          jnz    @B
@@:
          retn
cendk:
          mov    ax,mycurpos
          mov    al,byte ptr rows
          dec    al
          mov    ah,09h
          mov    mycurpos,ax
@@:
          call   csr2bufpos
          jnc    @F
          dec    byte ptr mycurpos
          jnz    @B
@@:
          retn

pgdnk:    mov     ax,rows
          movzx   eax,ax
          call    mypager
          call    writehelp
          retn

pgupk:    mov     ax,rows              ; PGUP - Page back
          movzx   eax,ax
          neg     eax
          call    mypager
          call    writehelp
          retn
lineup:
          mov     eax,-1                ; UP - scroll back 1 line
          call    mypager
          call    writehelp
          retn
linedn:   mov     eax,1                 ; DOWN - scroll forward 1 line
          call    mypager
          call    writehelp
          retn
writeline:
          mov     bx,mycurpos
          mov     bh,9
          call    csr2bufposx
          mov     bl,editatr            ; Load screen attribute
          mov     BYTE PTR cell[1],bl
          push    ds
          push    edi                   ; Arg 2
          mov     ax,mycurpos           ; Arg 3
          mov     ah,0
          inc     al
          push    ax
          push    cell                  ; Arg 4
          mov     ds,sbuffer
          call    HexWrt
          pop     ds
          ret
writehelp:
          mov     bl,statatr            ; Load status attribute
          mov     BYTE PTR cell[1],bl
          mov     ax, OFFSET editstathelp ; Arg 2
          movzx   eax,ax
          push    eax
          mov     ax,rows               ; Arg 3
          inc     ax
          push    ax
          push    cell                  ; Arg 4
          call    CellWrt               ; Write status line
          ret
mypager:
          mov     cl,editatr
          xchg    cl,scrnatr
          push    cx
          push    eax
          call    pager
          pop     ax
          mov     scrnatr,al
          call    csr2bufpos
          jnc     @F
          call    chomek
@@:
          ret
ife ?SAVE
mysavefile:
          call    savefile
          xor     eax,eax
          call    mypager
          ret
endif
          END

