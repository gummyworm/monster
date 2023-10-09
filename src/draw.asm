.include "bitmap.inc"
.include "macros.inc"
.include "zeropage.inc"

.CODE

;******************************************************************************
; HLINE
; Draws a horizontal line at the row given in .A
; IN:
;  - .A: the row to draw a horizontal line at
.export __draw_hline
.proc __draw_hline
@dst=zp::tmp0
	jsr bm::charaddr
	stxy @dst

	ldx #20
@l0:	ldy #$07
@l1: 	lda #$ff
	sta (@dst),y
	dey
	bpl @l1
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
