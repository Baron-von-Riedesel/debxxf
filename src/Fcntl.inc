
	pushcontext listing
	.xlist
	.xcref

;*** values for omode flags used in _open/_sopen calls ***

O_RDONLY equ    0
O_WRONLY equ    1
O_RDWR   equ    2

O_NDELAY equ    0004H
O_APPEND equ    0008H
O_NOINHERIT equ 0080h  ;/* child process doesn't inherit file */
O_CREAT  equ    0100H
O_TRUNC  equ    0200H
O_EXCL   equ    0400H

O_TEXT   equ    4000H
O_BINARY equ    8000H

O_RAW    equ    O_BINARY

S_IREAD  equ    200           ;00C8h
S_IWRITE equ    400           ;0190h

;*** shflag argument bei _sopen ***
SH_COMPAT equ   00h
SH_DENYRW equ   10h
SH_DENYWR equ   20h
SH_DENYRD equ   30h
SH_DENYNO equ   40h

; value for 'whence' parameter used in lseek() calls.  ANSI compatible.
SEEK_SET equ    0
SEEK_CUR equ    1
SEEK_END equ    2

	popcontext listing
