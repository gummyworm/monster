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
@stop=zp::tmp0
	sty @stop
:	lda mem::linebuffer+1,x
	sta mem::linebuffer,x
	inx
	cpx @stop
	bne :-
@done:	rts
.endproc
