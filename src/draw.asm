.include "macros.inc"
.include "memory.inc"
.include "text.inc"
.include "util.inc"
.include "zeropage.inc"

;******************************************************************************
; HLINE
; Draws a horizontal line at the row given in .A
; IN:
;  - .A: the row to draw a horizontal line at
.export __draw_hline
.proc __draw_hline
	pha
	lda #40
	sta zp::tmp0

	ldxy #mem::spare
	lda #132
	jsr util::memset

	pla
	ldxy #mem::spare
	jmp text::puts
.endproc
