
	.286
	.model small

	.code

Wep proc far pascal
	mov ax,1
	retf 2
Wep endp

LibMain proc far pascal
	mov ax,1
	ret
LibMain	endp

	end LibMain
