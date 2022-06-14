        pushcontext listing
        .xlist
        .xcref
;***
;fcntl.inc - defines access mode flags used for open()
;
;       Copyright (c) 1987-1992, Microsoft Corporation.  All rights reserved.
;
;Purpose:
;       This file defines the access mode flags used in calls to the
;       open() routine.
;
;*******************************************************************************

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

;/* macro to translate the C 2.0 name used to force binary mode for files */

O_RAW   equ O_BINARY

;*** pmode argument bei _open (mit O_CREAT)/_creat ***

S_IREAD  equ    200           ;00C8h
S_IWRITE equ    400           ;0190h

;*** shflag argument bei _sopen ***

SH_COMPAT equ   00h
SH_DENYRW equ   10h
SH_DENYWR equ   20h
SH_DENYRD equ   30h
SH_DENYNO equ   40h

; value for 'whence' parameter used in lseek() calls.  ANSI compatable.

SEEK_SET equ    0
SEEK_CUR equ    1
SEEK_END equ    2

ifdef _WINDOWS
ifndef _WINDLL

; version of win interface
_WINVER         equ     0

; parameter values for windows screen buffer size
_WINBUFINF      equ      0
_WINBUFDEF      equ     -1

; size/move settings
_WINSIZEMIN     equ     1
_WINSIZEMAX     equ     2
_WINSIZERESTORE equ     3
_WINSIZECHAR    equ     4

; size/move query types
_WINMAXREQ      equ     100
_WINCURRREQ     equ     101

; pseudo file handle for frame window
_WINFRAMEHAND   equ     -1

; menu items
_WINSTATBAR     equ     1
_WINTILE        equ     2
_WINCASCADE     equ     3
_WINARRANGE     equ     4

; quickwin exit options
_WINEXITPROMPT          equ     1
_WINEXITNOPERSIST       equ     2
_WINEXITPERSIST         equ     3

; window open structure

_wopeninfo struc

        _version        dw  ?
        _title          dd  ?
        _wbufsize       dd  ?

_wopeninfo ends

; size/move structure

_wsizeinfo struc

        _version        dw  ?
        _type           dw  ?
        _x              dw  ?
        _y              dw  ?
        _h              dw  ?
        _w              dw  ?

_wsizeinfo ends

endif
endif
        popcontext listing
