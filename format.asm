.include "codes.inc"
.include "memory.inc"

;--------------------------------------
; label formats linebuffer as a label.
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
; opcode formats linebuffer as an opcode.
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

;--------------------------------------
; line formats the linebuffer according to the value in .A
.export __fmt_line
.proc __fmt_line
	cmp #ASM_LABEL
	bne :+
	jsr __fmt_label
	lda #$00
	rts
:	cmp #ASM_OPCODE
	bne :+
	jsr __fmt_opcode
	lda #$00
	rts
:	lda #-1
	rts
.endproc
