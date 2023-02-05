.include "memory.inc"
.include "text.inc"
.include "util.inc"
.include "zeropage.inc"

;--------------------------------------
; hline draws a horizontal line at the row given in .A
.export __draw_hline
.proc __draw_hline
	pha
	lda #40
	sta zp::tmp0

	ldx #<mem::spare
	ldy #>mem::spare
	lda #132
	jsr util::memset

	pla
	ldx #<mem::spare
	ldy #>mem::spare
	jmp text::puts
.endproc
