.include "codes.inc"
.include "memory.inc"

;--------------------------------------
; label formats linebuffer as a label.
.export __fmt_label
.proc __fmt_label
	ldx #$00
:	lda mem::linebuffer,x
	inx
	cmp #$0d
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
; line formats the linebuffer according to the value in .A. The line length is
; returned in .A
.export __fmt_line
.proc __fmt_line
	cmp #ASM_LABEL
	bne :+
	jsr __fmt_label
	rts
:	cmp #ASM_OPCODE
	bne :+
	jsr __fmt_opcode

:	ldx #$00
@l0:	lda mem::linebuffer,x
	inx
	cmp #$0d
	bne @l0
	dex
	stx @len

	lda #' '
@l1:	sta mem::linebuffer,x
	inx
	cpx #40
	bcc @l1
@len=*+1 
	lda #$ff
	rts
.endproc
