
	.286
	.model small
	.386
	option casemap: none

	include windows.inc

	.code

GetSymbolicNameEx proto far pascal farproc:dword, lpString:dword

GetSymbolName proc far pascal farproc:dword, lpString:dword, nSize:word

	invoke GetSymbolicNameEx, farproc, lpString
	ret

GetSymbolName endp

	END

