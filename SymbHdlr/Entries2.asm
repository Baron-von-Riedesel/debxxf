
;--- translated entries2.c

	.286
	.model small
	.386

if 0
MODSTRUC struct 
    WORD        id;            /* +0: "NE" */
    WORD        count;         /* +2: refcnt for this module */
    WORD        offsentries;   /* +4: offset exported functions */
    WORD        nextmod;       /* +6: next module */
    WORD        pautodata;     /* +8: ^ autodata */
    WORD        pfileinfo;     /* +a: ^ fileinfo */
    WORD        flags;         /* +c: status flags */
    WORD        autods;        /* 0e: # of dgroup segm */
    WORD        heapsize;      /* 10: heap size */
    WORD        stacksize;     /* 12: stack size */
    DWORD       initcsip;      /* 14: start CS:IP */
    DWORD       initsssp;      /* 18: start SS:SP */
    WORD        segs;          /* 1c: no of segments */
    WORD        wNumRefModule; /* 1e: no ref. modules */
    WORD        lennresname;   /* 20: length res names table */
    WORD        offssegtab;    /* 22: offs segment table */
    WORD        offsrctab;     /* 24: offs resource table */
    WORD        offsresname;   /* 26: offs table resident names */
    WORD        offsmodref;    /* 28: offs modref table */
    WORD        offsimpname;   /* 2A: offs imported names */
    DWORD       addrnresname;  /* 2C: filepos nichtresident names */
    WORD        moventrys;     /* 30: movable entries (# or offs?) */
    WORD        shiftfactor;   /* 32: factor for segment addr */
    WORD        reservedsegs;  /* 34: res. segments */
    BYTE        os;            /* 36: os version */
    BYTE        res7;          /* 37: ??? */
    WORD        res8;          /* 38: ??? */
    WORD        res9;          /* 3A: ??? */
    WORD        swaparea;      /* 3C: swaparea size */
    WORD        version;       /* 3E: version */
MODSTRUC ends

ENTRYSTRUCT struct
  WORD firstid;     /* first ordinal - 1 */
  WORD lastid;      /* last ordinal */
  WORD nextentry;   /* next entrystruct item */
ENTRYSTRUCT ends

SINGLEENTRYSTRUCT struct
  BYTE type;        /* 0xFF for movable segments */
  BYTE flags;       /* 1=exported, 2=public data */
  BYTE segNum;      /* log. segmentno */
  WORD offs;        /* offset */
SINGLEENTRYSTRUCT ends

endif


GetSegmentHandle proto far pascal hModule:word, wSegment:word

GetSegmentID proto far pascal hModule:word, wSegment:word

	PUBLIC GetEntryAddress
	PUBLIC GETENTRYID

	.code

;--- GetEntryAddress( hModule:word, id:word )

;	lpmodstruc = -4
;	lpentry = -4
;	lpsingleentry = -8
;	i = -14
;	rc = -12

GetEntryAddress PROC FAR pascal uses si di hModule:word, wId:word

local rc:dword

	mov rc, 0
	mov bx, hModule
	or bx,bx
	je $I2405
	cmp wId, 0
	je $I2405

	mov es, bx
	mov si, WORD PTR es:[4]
	cmp WORD PTR es:[si+4], 0
	je $FB2404

	mov bx, wId
$FC2403:
	cmp	WORD PTR es:[si+2], bx
	jae	$FB2404
; Line 31
	mov	di, WORD PTR es:[si+4]
	mov	si, di
	cmp	WORD PTR es:[di+4], 0
	jne	$FC2403

$FB2404:

	mov	ax, wId
	cmp	WORD PTR es:[si], ax
	jae	$I2405
	cmp	WORD PTR es:[si+2], ax
	jb	$I2405
; Line 34
	mov	ax, si
	mov	bx, si
	mov	cx, wId
	sub	cx, WORD PTR es:[bx]
	add	ax, 6
	mov	bx, cx
	shl	cx, 2
	add	cx, bx
	add	ax, cx
	sub	ax, 5
	mov	di, ax
; Line 36
	mov	ax, WORD PTR es:[di+3]
	mov cl, BYTE PTR es:[di+2]
	sub ch, ch
	mov si, ax
	invoke GetSegmentHandle, es, cx
	mov WORD PTR rc+0, si	;rc
	mov WORD PTR rc+2, ax

$I2405:
	mov ax,WORD PTR rc+0
	mov dx,WORD PTR rc+2
	ret

GetEntryAddress ENDP

;--- GetEntryID( hModule:word, Segment:word, offset:word )

GETENTRYID PROC FAR
; Line 46
	push bp
	mov bp, sp
	sub sp, 20
	push	di
	push	si

;	hModule = 10
;	Segment = 8
;	offset = 6
;	lpmodstruc = -4
;	lpentry = -14
;	lpsingleentry = -20
;	segnr = -16
;	i = -6
;	rc = -10

	mov	si,WORD PTR [bp+10]	;hModule
; Line 54
	or	si,si
	jne	$I2419
; Line 55
	xor	ax,ax
	pop	si
	pop	di
	mov sp, bp
	pop bp
	ret	6

; Line 56
$I2419:
	sub	ax,ax
	mov	WORD PTR [bp-8],ax
	mov	WORD PTR [bp-10],ax	;rc
; Line 58
	mov	ax,si
	and	al,252	;00fcH
	mov	dx,ax
	sub	cx,cx
	mov	bx,cx
	mov	es,ax
	mov	ax,WORD PTR es:[bx+4]
	mov	WORD PTR [bp-14],ax	;lpentry
	mov	WORD PTR [bp-12],dx
; Line 60
	invoke GetSegmentID, si, word ptr [bp+8]
	mov	WORD PTR [bp-16],ax	;segnr
	or	ax,ax
	jne	$JCC242
	jmp	$I2420
$JCC242:
	mov	di,WORD PTR [bp-14]	;lpentry
; Line 61
$FC2422:
; Line 63
	mov	ax,di
	mov	dx,WORD PTR [bp-12]
	add	ax,6
	sub	cx,cx
	mov	WORD PTR [bp-20],cx
	mov	WORD PTR [bp-18],dx
	mov	WORD PTR [bp-2],dx
; Line 65
	mov	es,dx
	mov	si,WORD PTR es:[di]
	cmp	si,WORD PTR es:[di+2]
	jae	$FB2426
	mov	bx,ax
	mov	WORD PTR [bp-14],di	;lpentry
	mov	di,si
	mov	cx,WORD PTR [bp-14]	;lpentry
$F2424:
; Line 66
	mov	es,WORD PTR [bp-2]
	mov	al,BYTE PTR es:[bx+2]
	sub	ah,ah
	cmp	ax,WORD PTR [bp-16]	;segnr
	jne	$FC2425
	mov	ax,WORD PTR [bp+6]	;offset
	cmp	WORD PTR es:[bx+3],ax
	je	$L2440
; Line 65
$FC2425:
	add	bx,5
	inc	di
	mov	si,cx
	mov	es,WORD PTR [bp-12]
	cmp	WORD PTR es:[si+2],di
	ja	$F2424
	jmp	SHORT $L2445
$L2440:
	mov	si,di
; Line 68
	lea	ax,WORD PTR [si+1]
	cwd	
	mov	WORD PTR [bp-10],ax	;rc
	mov	WORD PTR [bp-8],dx
$L2445:
	mov	di,WORD PTR [bp-14]	;lpentry
; Line 71
$FB2426:
	mov	es,WORD PTR [bp-12]
	cmp	WORD PTR es:[di+4],0
	je	$I2420
; Line 73
	mov	ax,WORD PTR es:[di+4]
	or	ax,WORD PTR [bp-20]
	mov	dx,WORD PTR [bp-18]
	mov	di,ax
	mov	WORD PTR [bp-12],dx
; Line 74
	mov	ax,WORD PTR [bp-8]
	or	ax,WORD PTR [bp-10]	;rc
	jne	$JCC376
	jmp	$FC2422
$JCC376:
; Line 75
$I2420:
	mov	ax,WORD PTR [bp-10]	;rc
; Line 76
	pop	si
	pop	di
	mov sp, bp
	pop bp
	ret 6

GETENTRYID ENDP

END
