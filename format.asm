.include "codes.inc"
.include "memory.inc"
.include "source.inc"
.CODE

;******************************************************************************
; LABEL
; Formats linebuffer as a label.
.export __fmt_label
.proc __fmt_label
	ldx #$00
:	lda mem::linebuffer,x
	inx
	cmp #$00
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

;******************************************************************************
; OPCODE
; Formats linebuffer as an opcode.
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

;******************************************************************************
; LINE
; Formats the linebuffer according to the value in .A. The line length is
; returned in .A
; IN:
;  - .A: the "type" to format see (codes.inc) e.g. ASM_OPCODE, etc.
; OUT:
;  - .A: the line length
.export __fmt_line
.proc __fmt_line
	pha

	; remove spaces from start of line
	jsr src::up
@removespaces:
	lda mem::linebuffer
	cmp #' '
	bne @left_aligned
	jsr src::delete
	ldx #$00
:	lda mem::linebuffer+1,x
	sta mem::linebuffer,x
	inx
	cpx #39
	bne :-
	beq @removespaces

@left_aligned:
	jsr src::down
	pla
	cmp #ASM_LABEL
	bne @notlabel
	jmp __fmt_label
@notlabel:
	cmp #ASM_OPCODE
	beq @indent
	cmp #ASM_MACRO
	bne @done
@indent:
	jmp __fmt_opcode
@done:
	rts
.endproc
