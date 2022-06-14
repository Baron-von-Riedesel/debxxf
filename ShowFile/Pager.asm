
	.286
	.model small
	option casemap:none
	option proc:private
	.386

	include showfile.inc
        
	.code

; Procedure Pager
; Purpose   Displays status and text lines
; Input     Stack variable: lines to scroll (negative up, positive down)
;           Global variables: "sbuffer", "pbuffer", "linenum"
; Output    To screen

pager PROC public
          push    bp
          mov     bp,sp

          push    ds
          pop     fs

          mov     es,sbuffer            ; Initialize buffer position
          mov     edi,pbuffer

          mov     ecx,[bp+4]            ; Get count argument
          mov     ax,LF                 ; Search for linefeed

          or      ecx,ecx               ; Argument 0?
          jg      forward               ; If above, forward
          jl      backward              ; If below, backward

          jmp     show                  ; If equal, done

backward:
          call    GoBack                ; Adjust backward
          jmp     show                  ; Show screen
forward:  call    GoForwd               ; Adjust forward

; Write line number to status line, EDI=^puffer

show:     cld                           ; Go forward
          push    edi
          push    es

          push    ds                    ; Load DS to ES
          pop     es

; BinToStr (linenum,OFFSET statline[7])

          push    linenum               ; Arg 1
          mov     ax,OFFSET lineno
          push    ax                    ; Arg 2
          call    BinToStr              ; Convert to string

; Fill in status line

          mov     cx,7                  ; Seven spaces to fill
          sub     cx,ax                 ; Subtract those already done
          jc      @F
          mov     al," "                ; Fill with space
          rep     stosb
@@:
          pop     es

          mov     bl,statatr            ; Load status attribute
          mov     BYTE PTR cell[1],bl

; CellWrt (DS,OFFSET statline,0,cell)

          mov     ax,OFFSET statline    ; Arg 2
          movzx   eax,ax
          push    eax
          sub     ax,ax                 ; Arg 3
          push    ax
          push    cell                  ; Arg 4
          call    CellWrt               ; Write status line

; CellWrt (DS,OFFSET stathelp,0,cell)

          mov     ax,OFFSET stathelp    ; Arg 2
          test    byte ptr fHex,1
          jz      @F
          mov     ax,OFFSET sHexHelp
@@:
          movzx   eax,ax
          push    eax
          mov     ax,rows               ; Arg 3
          inc     ax
          push    ax
          push    cell                  ; Arg 4
          call    CellWrt               ; Write status line


          pop     edi
          mov     bl,scrnatr            ; Load screen attribute
          mov     BYTE PTR cell[1],bl
          mov     esi,edi               ; Update position
          mov     cx,rows               ; Lines per screen

show1:    mov     bx,rows               ; Lines of text
          inc     bx                    ; Adjust for 0
          sub     bx,cx                 ; Calculate current row
          movzx   ecx,cx
          push    ecx                   ; Save line number

          test    byte ptr fHex,1
          jz      @F
          mov     ds,sbuffer            ; puffer segment
          push    esi                   ; Arg 1 puffer offset
          push    bx                    ; Arg 2
          push    fs:cell               ; Arg 3
          call    HexWrt                ; Write line
          jmp     show2
@@:
; CellWrt (sbuffer,position,line,cell)

          mov     ds,sbuffer            ; puffer segment
          push    esi                   ; Arg 1 puffer offset
          push    bx                    ; Arg 2
          push    fs:cell               ; Arg 3
          call    CellWrt               ; Write line
show2:
          push    fs                    ; Restore DS
          pop     ds

          pop     ecx                   ; Restore line number
          mov     esi,eax               ; Get returned position

          cmp     eax,fsize             ; Beyond end of file?
          jae     fillout               ; Yes? Fill screen with spaces
          loop    show1                 ;    else next line
          jmp     pagedone              ; Get out if done

; Fill the rest with spaces

fillout:  dec     cx                    ; Adjust
          jcxz    pagedone
          mov     al,columns            ; Columns times remaining lines
          xor     ah,ah
          mul     cl

; CellFil (sbuffer,count,cell)

          push    sbuffer               ; Arg 1
          push    ax                    ; Arg 2
          push    cell                  ; Arg 3
          call    CellFil               ; Fill screen with spaces

          push    fs                    ; Restore DS
          pop     ds

pagedone: pop     bp
          ret     4
pager ENDP

HexWrt PROC public
          push    bp
          mov     bp,sp
          mov     es,fs:vidadr          ; Load screen buffer segment
          mov     cl,fs:columns         ; Cells per row
          xor     ch,ch
          mov     esi,[bp+8]            ; Buffer position
          mov     ax,[bp+6]             ; Starting row
          mov     bx,cx                 ; Bytes per row
          add     bx,bx
          mul     bx                    ; Figure columns per row
          movzx   edi,ax                ; Load as destination
          add     di,fs:[vidOfs]
          mov     ax,[bp+4]             ; Attribute
          mov     cx,fs:rows
          dec     cx
          mov     edx,esi
          push    ax
          mov     ah,fs:[prefatr]
          call    dwordout
          mov     al,' '
          stosw
          pop     ax
          mov     cx,16                 ; Cells per row
          mov     ebx,esi
hexwrt_1:
          cmp     esi,fs:fsize
          jae     hexwrt_2
          @useext
          lodsb                         ; Get character
          call    byteout
          mov     al,' '
          stosw
          loop    hexwrt_1
          jmp     hexwrt_4
hexwrt_2:
          mov     al,' '
          stosw
          stosw
          stosw
          loop    hexwrt_2
hexwrt_4:
          stosw
          mov     esi,ebx
          mov     cx,16
hexwrt_3:
          cmp     esi,fs:fsize
          jae     hexwrt_5
          @useext
          lodsb
          stosw
          loop    hexwrt_3
          jmp     hexwrt_6
hexwrt_5:
          mov     al,' '
          stosw
          loop    hexwrt_5
hexwrt_6:
          mov     cl,fs:[columns]
          mov     ch,0
          sub     cx,16*4+10
          jc      @F
          mov     al,' '
          rep     stosw
@@:
hexwrt_ex:
          mov     eax,esi               ; Return position
          pop     bp
          ret  8
HexWrt ENDP

nibout proc near
          and     al,0Fh
          add     al,30h
          cmp     al,39h
          jle     @F
          add     al,07h
@@:
          stosw
          ret
nibout endp

byteout proc
          push  ax
          shr   al,4
          call  nibout
          pop   ax
          jmp   nibout
byteout endp

dwordout proc
          push  cx
          mov   cx,4
          push  dx
          shr   edx,8
          push  dx
          shr   edx,8
          push  dx
          shr   edx,8
          push  dx
dwordout_1:
          pop   dx
          mov   al,dl
          call  byteout
          loop  dwordout_1
          pop   cx
          ret
dwordout endp

; Procedure CellWrt (segment,offset,line,cell)
; Purpose   Writes a line to screen buffer
; Input     Stack variables: 1 - DWORD: offset (buffer)
;                            2 -  WORD: line number (screen)
;                            3 -  WORD: attribute
; Output    Line to screen buffer

CellWrt PROC public
          push    bp
          mov     bp,sp
          mov     es,fs:vidadr          ; Load screen buffer segment
          mov     cl,fs:columns         ; Cells per row
          mov     ch,00
          mov     bx,cx                 ; Bytes per row
          add     bx,bx
          mov     esi,[bp+8]            ; Buffer position
          mov     ax,[bp+6]             ; Starting row
          mul     bx                    ; Figure columns per row
          movzx   edi,ax                ; Load as destination
          add     di,fs:[vidOfs]
          mov     ebx,edi               ; Save start for tab calculation
          mov     ax,ds
          mov     dx,fs
          cmp     ax,dx                 ; Anzeige von status lines
          mov     edx,-1
          jz      @F
          mov     edx,fs:fsize
@@:
          mov     ax,[bp+4]             ; Attribute
movechar:
          cmp     esi,edx
          jz      fillspc
          @useext
          lodsb                         ; Get character
          cmp     al,CR
          je      fillspc
          cmp     al,LF
          je      fillspc
          cmp     al,TAB
          jne     notab
          call    FillTab               ; Yes? fill with spaces
          jcxz    nextline              ; If beyond limit done
          jmp     movechar

notab:    stosw                         ; Write
          loop    movechar
          jmp     nextline              ; Done

fillspc:
          test    byte ptr fs:[fDispCR],1
          jz      @F
          mov     al,0E3h
          stosw
          dec     cx
          jcxz    space21
@@:
          mov     al," "                ; Fill with space
space2:   rep     stosw                 ; Write
space21:
          cmp     esi,edx
          jz      exit
          cmp     byte ptr [esi],LF
          jnz     exit
          inc     esi                   ; Adjust for LF
          jmp     exit                  ; Done

nextline:
          test    byte ptr fs:[fLineWrap],1
          jnz     exit
          mov     al,LF                 ; Search for next line feed
          mov     ecx,fs:[fsize]
          sub     ecx,esi
          push    edi
          push    es
          push    ds
          pop     es
          mov     edi,esi
          @useext
          repnz   scasb
          jnz     @F
          mov     esi,edi
@@:
          pop     es
          pop     edi

exit:     mov     eax,esi               ; Return position
          pop     bp
          ret     8
CellWrt ENDP

; Procedure CellFil (segment,count,cell)
; Purpose   Fills screen with character
; Input     Stack variables: 1 - segment of text (offset 0)
;                            2 - number of characters
;                            3 - attribute and character
; Output    Characters to screen buffer

CellFil PROC
          push    bp
          mov     bp,sp
          mov     es,vidadr             ; Load screen buffer segment
          mov     ds,[bp+8]             ; Buffer segment (position 0)
          mov     cx,[bp+6]             ; Characters to fill
          mov     ax,[bp+4]             ; Attribute
          rep     stosw                 ; Write
          pop     bp
          ret     6
CellFil ENDP

; Procedure FillTab
; Purpose   Writes spaces for tab to screen
; Input     EBX points to start of line, EDI points to current position
; Output    Spaces to screen buffer

FillTab PROC
          push    ebx
          push    ecx

          sub     ebx,edi               ; Get current position in line
          neg     ebx
          shr     ebx,1                 ; Divide by 2 bytes per character

          mov     cx,8                  ; Default count 8
          and     bx,7                  ; Get modulus
          sub     cx,bx                 ; Subtract
          mov     bx,cx                 ; Save modulus

          mov     al," "                ; Spaces
          rep     stosw                 ; Write

          pop     ecx
          sub     ecx,ebx               ; Adjust count
          jns     nomore                ; Make negative count 0
          sub     ecx,ecx
nomore:   pop     ebx
          ret
FillTab ENDP

; Procedure GoBack
; Purpose   Searches backward through buffer
; Input     ECX has number of lines; ES:EDI has buffer position
; Output    Updates "linenum" and "pbuffer"

GoBack PROC
          test    byte ptr fHex,1
          jz      @F
          neg     ecx                   ; Make count positive
          mov     edx,ecx
          shl     ecx,4                 ; *16
          sub     edi,ecx
          jc      atstart1
          cmp     edi,[StartBuffer]
          jbe     atstart1
          jmp     notend1
@@:
          std                           ; Go backward
          neg     ecx                   ; Make count positive
          mov     edx,ecx               ; Save a copy
          inc     ecx                   ; One extra to go up one
          cmp     edi,[StartBuffer]     ; Start of file?
          jbe     exback                ; If so, ignore
findb:    push    ecx                   ;   else save count
          mov     ecx,edi               ; Load maximum character count
          sub     ecx,[StartBuffer]
          @useext
          repne   scasb                 ; Find last previous LF
          jecxz   atstart               ; If not found, must be at start
          pop     ecx
          @useext
          loop    findb
          cmp     linenum,-1            ; End of file flag?
          jne     notend                ; No? Continue
          add     edi,2                 ; Adjust for cr/lf
          mov     pbuffer,edi           ; Save position
          call    EndCount              ; Count back to get line number
          ret

notend:
          add     edi,2                 ; Adjust for cr/lf
notend1:
          sub     linenum,edx           ; Calculate line number
          jg      positive
          mov     linenum,1             ; Set to 1 if negative
positive:
          mov     pbuffer,edi           ; Save position
          ret

atstart:  pop     ecx
atstart1:
          mov     edi,[StartBuffer]     ; Load start of file
          mov     linenum,1             ; Line 1
          mov     pbuffer,edi           ; Save position
exback:   ret
GoBack ENDP

; Procedure GoForwd
; Purpose   Searches forward through a buffer
; Input     ECX has number of lines; ES:EDI has buffer position
; Output    Updates "linenum" and "pbuffer"

GoForwd PROC
          test    byte ptr fHex,1
          jz      @F
          mov     edx,ecx
          shl     ecx,4                 ; *16
          add     edi,ecx
          cmp     edi,fsize
          jae     atend1
          jmp     findf1
@@:
          cld                           ; Go forward
          mov     edx,ecx               ; Copy count
findf:    push    ecx                   ; Save count
          mov     ecx,fsize             ; Load maximum character count
          sub     ecx,edi
          @useext
          repne   scasb                 ; Find next LF
          jecxz   atend           ; If not found, must be at end
          cmp     edi,fsize             ; Beyond end?
          jae     atend
          pop     ecx
          @useext
          loop    findf
findf1:
          add     linenum,edx           ; Calulate line number
          mov     pbuffer,edi           ; Save position
          ret

atend:    pop     ecx
atend1:
          mov     edi,pbuffer           ; Restore position
          ret
GoForwd ENDP

; Procedure EndCount
; Purpose   Counts backward to count lines in file
; Input     ES:EDI has buffer position
; Output    Modifies "linenum"

EndCount PROC
          push  edi

          mov   al,LF                 ; Search for CR
          mov   linenum,0             ; Initialize

findstrt: inc   linenum               ; Adjust count
          mov	ecx,edi               ;   else search only to start
          sub	ecx, [StartBuffer]
          @useext
          repne scasb                 ; Find last previous cr
          jecxz found                 ; If not found, must be at start
          jmp   findstrt

found:    pop   edi
          ret
EndCount ENDP

; Procedure isEGA
; Purpose   Determines if an EGA is active
; Input     None
; Output    0 if no; lines per screen if yes

isEGA PROC public
          push    bp
          push    es
          mov     ah,12h                ; Call EGA status function
          mov     bl,10h
          sub     cx,cx                 ; Clear status bits
          int     10h
          sub     ax,ax                 ; Segment 0 and assume no EGA
          jcxz    noega                 ; If status still clear, no EGA

          mov     ax,__0040H
          mov     es,ax                 ; ES=0040h
          test    BYTE PTR es:[87h],1000b ; Test active bit
          jnz     noega                 ; If set, not active
          mov     ax,1130h              ; Get EGA information
          int     10h
          mov     al,dl                 ; Return lines per screen
          cbw

noega:    pop     es
          pop     bp
          ret
isEGA ENDP

; Procedure BinToStr (number,address)
; Purpose   Converts integer to string
; Input     Stack arguments: 1 - Number to convert; 2 - Near address for write
; Output    AX has characters written

BinToStr PROC
          push    bp
          mov     bp,sp
          mov     eax,[bp+6]            ; Arg 1
          mov     di,[bp+4]             ; Arg 2

          sub     cx,cx                 ; Clear counter
          mov     ebx,10                ; Divide by 10

; Convert and save on stack backwards

getdigit: sub     edx,edx               ; Clear top
          div     ebx                   ; Divide to get last digit as remainder
          add     dl,"0"                ; Convert to ASCII
          push    edx                   ; Save on stack
          or      eax,eax               ; Quotient 0?
          loopnz  getdigit              ; No? Get another

; Take off the stack and store forward

          neg     cx                    ; Negate and save count
          mov     dx,cx
putdigit: pop     eax                   ; Get character
          stosb                         ; Store it
          loop    putdigit
          mov     ax,dx                 ; Return digit count

          pop     bp
          ret     6
BinToStr ENDP

	END

