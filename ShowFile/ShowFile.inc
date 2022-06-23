
ifndef ?USEPREDEFSEGS
?USEPREDEFSEGS equ 0   ; __B000H and __B800H are NOT defined by DPMILD32
endif

@useext macro
	  db 67h
endm

CR	equ 0Dh
LF	equ 0Ah
TAB equ  9

PF32 typedef ptr far32

externdef  __0040H:abs
externdef  __B000H:abs
externdef  __B800H:abs

		.data

		externdef  cell:word
		externdef  rows:word
		externdef  columns:byte
		externdef  vidadr:word
		externdef  statatr:byte
		externdef  scrnatr:byte
		externdef  cga:byte
		externdef  fDispCR:byte
		externdef  fHex:byte
		externdef  vidOfs:word
		externdef  fLineWrap:byte
		externdef  stathelp:byte
		externdef  lineno:byte
		externdef  pag:byte
		externdef  prefatr:byte
		externdef  editatr:byte
		externdef  sHexHelp:byte
		externdef  fname:dword
		externdef  savefileexit:fword
		externdef  wCrt:word
		externdef  statline:byte
		externdef  linenum:dword
		externdef  buffer:dword
		externdef  pbuffer:dword
		externdef  sbuffer:word
		externdef  fsize:dword
		externdef  StartBuffer:dword
ife ?USEPREDEFSEGS
		externdef  mono:word
		externdef  color:word
endif
		externdef  ioproc:PF32

	.code

	externdef  savefile:proc
	externdef  DllEntry:proc
	externdef  DllEntry2:proc

	EXTERNDEF  pager:PROC
	EXTERNDEF  isEGA:PROC
	EXTERNDEF  _edit:PROC

	externdef  HexWrt:proc
	externdef  CellWrt:proc

	externdef  getkey:proc
	externdef  getkeyex:far
