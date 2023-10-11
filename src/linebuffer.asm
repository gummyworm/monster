.include "memory.inc"
.include "zeropage.inc"

.CODE

;******************************************************************************_
; SHL
; Shifts the linebuffer characters at the given index
; to the left by the given number of characters
; IN:
;  - .X: the index to start shifting down
;  - .Y: the last index to shift
; OUT:
;  - .Z: clear
.export __linebuffer_shl
.proc __linebuffer_shl
@stop=zp::tmp0
	sty @stop
	cpx @stop
	beq @done
:	lda mem::linebuffer+1,x
	sta mem::linebuffer,x
	inx
	cpx @stop
	bne :-
@done:	rts
.endproc
