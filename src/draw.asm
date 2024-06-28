.include "bitmap.inc"
.include "macros.inc"
.include "zeropage.inc"

.CODE

;******************************************************************************
; HLINE
; Draws a horizontal line at the row given in .A
; IN:
;  - .A: the row to draw a horizontal line at
;  - .W: the pattern to draw
.export __draw_hline
.proc __draw_hline
@dst=r0
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

;******************************************************************************
; RVS UNDERLINE
; Reverses a horizontal line at the row given in .A (EOR)
; IN:
;  - .A: the row to draw a horizontal line at
;  - .W: the pattern to draw
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
