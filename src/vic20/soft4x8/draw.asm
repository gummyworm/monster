.include "../../macros.inc"
.include "bitmap.inc"

;******************************************************************************
; RVS UNDERLINE
; Reverses a horizontal line at the row given in .A (EOR)
; IN:
;  - .A: the row to draw a horizontal line at
.export __draw_rvs_underline
.proc __draw_rvs_underline
@dst=r0
	jsr bm::charaddr
	stxy @dst

	ldx #20
	ldy #$07
@l0: 	lda #$ff
	eor (@dst),y
	sta (@dst),y
	lda @dst
	clc
	adc #$c0
	sta @dst
	bcc :+
	inc @dst+1
:	dex
	bne @l0

	rts
.endproc

