.include "codes.inc"
.include "memory.inc"
.include "source.inc"
.CODE

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
	; add 2 spaces to the linebuffer
	lda #' '
	sta mem::linebuffer
	sta mem::linebuffer+1
	; return to start of line and add 2 spaces to the source
	jsr src::up
	lda #' '
	jsr src::insert
	lda #' '
	jsr src::insert

	; move to the next line
	jmp src::down
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
	beq :+
	inx
	cpx #40
	bcc @l0
:	stx @len

@len=*+1
	lda #$ff
	rts
.endproc
