.include "c64.inc"
.include "../zeropage.inc"

.export __screen_init
.proc __screen_init
.endproc
	; TODO:
	rts

.export __screen_restore
.proc __screen_restore
	; TODO:
	rts
.endproc

.export __screen_save
.proc __screen_save
	; TODO:
	rts
.endproc

.export __screen_rvsline
.proc __screen_rvsline
	; TODO:
	rts
.endproc

.export __screen_rvsline_part
.proc __screen_rvsline_part
	; TODO:
	rts
.endproc

.export __screen_clr
.proc __screen_clr
	; TODO:
	rts
.endproc

.export __screen_clrcolor
.proc __screen_clrcolor
	; TODO:
	rts
.endproc

;*******************************************************************************
; CLRLINE
; Clears the given character row
; IN:
;  - .A: the row to clear
.export __screen_clrline
.proc __screen_clrline
@row=r0
	tax
	lda c64::rowslo,x
	sta @row
	lda c64::rowshi,x
	sta @row+1
	lda #' '
	ldy #40-1
@l0:	sta (@row),y
	dey
	bpl @l0
	rts
.endproc

.export __screen_clr_part
.proc __screen_clr_part
	; TODO:
	rts
.endproc

.export __scr_shl
.proc __scr_shl
	; TODO:
	rts
.endproc

.export __scr_shr
.proc __scr_shr
	; TODO:
	rts
.endproc

	rts
