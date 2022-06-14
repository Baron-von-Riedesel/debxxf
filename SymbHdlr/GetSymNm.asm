
	.286
    .model small
	.386

	include windows.inc

GetEntryID proto far pascal :word, :dword
GetProcedureName proto FAR pascal :word, :word, :dword, :word

ifdef _DEBUG
CONST2 segment word public 'CODE'
CONST2 ends
CGROUP group _TEXT, CONST2
endif

@trace macro text:vararg
local sym
ifdef _DEBUG
CONST2 segment
sym db text, 0
CONST2 ends
	pusha
	mov si, offset sym
	mov ax, 12h
	int 41h
	popa
endif
endm

	.code

;--- 1. call GetEntryID
;--- 2. call GetProcedureName
    
GetSymbolicNameEx PROC FAR pascal uses si di lpproc:dword, lpstr:dword

	@trace <"GetSymbolicNameEx entered",13,10>
	mov	si, word ptr lpproc+2
	verr si
	jnz	error
	invoke GetExePtr, si
	or	ax,ax
	je	error
	@trace <"GetSymbolicNameEx: call GetExeptr ok",13,10>
	mov	di,ax
	mov	es,ax
	cmp	WORD PTR es:[0],"EN"	;;454eH
	je	@F
	mov	ax,WORD PTR es:[001Eh]
	mov	di,ax
@@:
	invoke	GetEntryID, di, lpproc
	or	ax,ax
	je	error
	@trace <"GetSymbolicNameEx: call GetEntryID ok",13,10>
	invoke	GetProcedureName, di, ax, lpstr, 40
	or	ax,ax
	je	error
	@trace <"GetSymbolicNameEx: call GetProcedureName ok",13,10>
	and	ah,127	;007fH
	ret
error:
	xor	ax,ax
	ret

GetSymbolicNameEx ENDP

END
