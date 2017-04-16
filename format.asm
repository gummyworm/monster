.include "memory.inc"

;--------------------------------------
; fmtlabel formats linebuffer as a label.
.export __fmt_label
.proc __fmt_label
	ldx #$00
:	lda mem::linebuffer,x
	inx
	cmp #' '
	beq :-
	dex

	ldy #$00
:	lda mem::linebuffer,x
	sta mem::linebuffer,y
	iny
	inx
	cpx #40
	bcc :-

	rts
.endproc

;--------------------------------------
; fmtopcode formats linebuffer as an opcode.
.export __fmt_opcode
.proc __fmt_opcode
	ldy #2
@l1:	ldx #38
@l0:	lda mem::linebuffer,x
	sta mem::linebuffer+1,x
	dex
	bpl @l0
	dey
	bne @l1
	lda #' '
	sta mem::linebuffer
	sta mem::linebuffer+1
	rts
.endproc

