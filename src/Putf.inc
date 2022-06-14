
;*** macros: @wordout, @dwordout, @stroutc, @strout, @putchr

_SEROUT   = 00000001b
_DBGOUT   = 00000010b
_DOSOUT   = 00000100b
_VIOOUT   = 00001000b
_BIOSOUT  = 00010000b
_ALTOUT   = 00100000b
_I14OUT   = 10000000b

_SERINP   = 00000001b
_DBGINP   = 00000010b
_DOSINP   = 00000100b
_KBDINP   = 00001000b
_BIOSINP  = 00010000b
_ALTINP   = 00100000b
_I14INP   = 10000000b


_crout      proto stdcall
_hexout     proto stdcall
__wordout   proto stdcall :dword
__dwordout  proto stdcall :dword
__nibout    proto stdcall
__stroutebx proto stdcall                ;^string is in ebx
__strout    proto stdcall :dword         ;^string is argument
__WriteTTY  proto stdcall
__putchrx   proto stdcall

@wordout macro xx
	invoke __wordout, xx
endm

@dwordout macro xx
	invoke __dwordout, xx
endm

@stroutc macro text:vararg
local sym
CCONST segment
sym label byte
	db text, 0
CCONST ends
	call __strout32
	dd offset sym
endm

@strout macro xx
ifidni <xx>,<ebx>
	invoke  __stroutebx  ; returns size of string in eax
else
	invoke  __strout, xx ; preserves all registers and flags
endif
endm

@putchr  macro xx:req
if ((.TYPE xx) and 0004h)
	call __putchrx
	db xx
else
  ifidni <xx>,<al>
  else
	 mov  al,xx
  endif
	call __WriteTTY
endif
endm

