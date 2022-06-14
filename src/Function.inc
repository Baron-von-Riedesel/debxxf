
;*** @flat, @prologue and @loadesp macros

if ?FLAT
  @flat   textequ <ds>
else
  @flat   textequ <gs>
endif

?CLEARHIGHEBP equ 1 - ?FLAT

if ?CLEARHIGHEBP

@prologue macro procname,flag,parmbyte,localbyte,reglist,userparms

    if flag
      if parmbyte + localbyte
        push    ebp
        movzx   ebp,sp
      endif
      if localbyte
        add     esp,-localbyte
      endif
    endif
    for r,reglist
       push r
    endm
        exitm %localbyte
endm

OPTION PROLOGUE: @prologue

else

@prologue equ <prologuedef>

endif

@loadesp macro x
if ?CLEARHIGHEBP
	movzx x, sp
else
	mov x, esp
endif
endm

