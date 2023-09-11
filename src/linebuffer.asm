.include "memory.inc"
.include "zeropage.inc"

;******************************************************************************_
; SHL
; Shifts the linebuffer characters at the given index
; to the left by the given number of characters
; IN:
;  - .X: the index to start shifting down
;  - .Y: the number of character positions to shift
.export __linebuffer_shl
.proc __linebuffer_shl
@xstart=zp::tmp0
	cpy #$00
	beq @done
	stx @xstart
:	ldx @xstart
:	lda mem::linebuffer+1,x
	sta mem::linebuffer,x
	inx
	cpx #39
	bne :-
	dey
	bne :--
@done:	rts
.endproc
