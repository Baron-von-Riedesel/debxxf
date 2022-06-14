
;--- LFN finddata structure

ifndef MAXPATH
MAXPATH equ 260
endif

FINDDATA struct
dwFileAttributes dd ?
ftCreationTime	 dq ?
ftLastAccessTime dq ?
ftLastWriteTime  dq ?
nFileSizeHigh	 dd ?
nFileSizeLow	 dd ?
dwReserved0 	 dd ?
dwReserved1 	 dd ?
cFileName		 db MAXPATH dup (?)
cAlternateFileName db 14 dup (?)
FINDDATA ends

FILEENTRY STRUCT
    feAddr            DD ?		;+0
    feAnzahl          DW ?		;+4
    feIndex           DW ?      ;+6 index in der filetabelle (00 - FF) */
    feLength          DW ?      ;+8 laenge eines eintrags (DOS abhaengig) */
    feOffset          DW ?      ;+10 aktueller offset
    feMode            DW ?		;+12
    feHandles         DW ?      ;+14 anzahl handles, 0 = file nicht geoeffnet */
    feOpen_mode       DW ?      ;+16
    feDevinfo         DW ?      ;+18 device info */
    feW1              DW 13 dup (?)	;+20
    feName            DB 8 dup (?)	;+46
    feExt             DB 3 dup (?)	;+54
    feShare_pref_sft  DD ?			;+57
    feShare_net_machine DW ?		;+61
    fePSP             DW ?			;+63
    feW2              DW 10 dup (?)	;+65 (size 85)
FILEENTRY ENDS

MCBENTRY STRUCT
    meAddr DD ?                  ;* 0: real mode addresse */
    meSize DD ?                  ;* 4 */
    meName DB 9 DUP (?)          ;* 8 */
    mePSP  DW ?                  ;* 17 */
    meNAddr DD ?                 ;* 19 */
    meSel  DW ?                  ;* 23: selector (nur temporaer gueltig) */
    meMode DB ?                  ;* 25: 1 = protected mode */
    meKB   DB ?                  ;* 26: Kennbyte
    meSK   DW ?                  ;* 27: Systemkennung, falls Name=SYSTEM
MCBENTRY ENDS


CDSENTRY STRUCT
   ceLastdrive DB ?
   ceDrive     DB ?
   ceEntry     DD ?
   ceDistance  DW ?
   ceRoot      DB ?
   ceFrei      DB ?
   ceFlags     DW ?
   ceDPB       DD ?
   cePath      DB 68 DUP (?);
CDSENTRY ENDS

NEHDR struct
ne_magic	dw	?		;+00: contains "NE"
ne_count	dw	?		;+02: module count
ne_enttab	dw	?		;+04: ^ Entries
ne_cbenttab	dw	?		;+06: length entry table
ne_ofsdgrp	dw	?		;+08: offset in segment table for DGROUP
			dw	?		;+0A: FileInfo (not used)
ne_pgmflgs	db	?		;+0C: program flags
ne_appflgs	db	?		;+0D: application flags
ne_autodata	dw	?		;+0E: no of DGROUP segment (needn't exist!)
ne_heap		dw	?		;+10: heap space (bytes/pages)
ne_stack	dw	?		;+12: extra stack space (bytes/pages)
ne_csip		dd	?		;+14: CS:IP
ne_sssp		dd	?		;+18: SS:SP
ne_cseg		dw	?		;+1C: number of segments
ne_cmod		dw	?		;+1E: number of referenced modules
ne_nreslen	dw	?		;+20: length of table nonres names
ne_segtab	dw	?		;+22: ^ segment table
ne_rsrctab	dw	?		;+24: ^ resource table
ne_restab	dw	?		;+26: ^ resident names table
ne_modtab	dw	?		;+28: ^ module table (=module references)
ne_imptab	dw	?		;+2A: ^ imported names table
ne_nrespos	dd	?		;+2C: file position nonresident names
ne_cmovent	dw	?		;+30: moveable entry points in entry table
ne_segshift	dw	?		;+32: shift factor
ne_cres		dw	?		;+34: number of resource table entries
ne_exetyp	db	?		;+36: operating system
ne_flagsothers	db	?	;+37: exe flags (long filenames, prop font)
ne_pretthunks	dw	?	;+38: start of gangload area?
ne_psegrefbytes	dw	?	;+3A: length of gangload area?
ne_swaparea	dw	?		;+3C: codeswap
ne_expver	dw	?		;+3E: expected windows version
NEHDR ends


