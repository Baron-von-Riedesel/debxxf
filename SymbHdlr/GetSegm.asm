
	.286
	.model small
	.386

	include windows.inc

	.code

;--- get segment# of a handle(=selector)

GetSegmentID proc far pascal uses si di wModule:word, wSegment:word

		mov 	es,wModule
;		mov		di,10h			;length is 10h bytes for DPMILDxx

		xor		ax,ax		 
		mov 	cx,es:[001Ch]	;number of segments
		jcxz	exit

		mov 	si,es:[0022h]	;offset segment table
		mov		ax,es:[0024h]	;offset resource table
		sub		ax,si
		xor		dx,dx
		div		cx
		mov		di, ax			;size of one entry
		
		mov 	ax,wSegment
		and 	al,0FCh
		mov 	dx,1
@@:
		mov 	bx,es:[si+8]	;handle (= selector)
		and 	bl,0FCh
		cmp 	ax,bx
		jz		@F
		add 	si,di
		inc 	dx
		loop	@B
		xor 	dx,dx
@@:
		mov 	ax,dx
exit:
		ret
GetSegmentID endp

;--- get handle(=selector) of a segment#
;--- first segment# is 1, not 0!

GetSegmentHandle proc far pascal uses si wModule:word, wSegmentNr:word

		mov 	es,wModule
		xor		ax, ax
		mov 	cx,es:[001Ch]	;number of segments
		jcxz	exit
		cmp		ax,wSegmentNr	;segment# 0 is invalid
		jz		exit
		cmp		cx,wSegmentNr	;segment# too big?
		jc		exit

		mov 	si,es:[0022h]	;offset segment table
		mov		ax,es:[0024h]	;offset resource table
		sub		ax,si
		xor		dx,dx
		div		cx				;now ax=size of one segment entry
		mov 	cx,wSegmentNr
		dec		cx
		mul		cx
		add		si, ax
		mov 	ax,es:[si+8]
exit:
		ret
GetSegmentHandle endp

END
