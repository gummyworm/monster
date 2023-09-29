.include "codes.inc"
.include "config.inc"
.include "linebuffer.inc"
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
	ldy #INDENT_LEVEL
@l1:	ldx #38
@l0:	lda mem::linebuffer,x
	sta mem::linebuffer+1,x
	dex
	bpl @l0
	dey
	bne @l1

	jsr src::up		; return to start of source

	; add 2 spaces to the linebuffer and source
	lda #' '
.repeat INDENT_LEVEL, I
	sta mem::linebuffer+I
	jsr src::insert
.endrepeat
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
	pha		; save the type to format

	; remove spaces from start of line
	jsr src::up
@removespaces:
	lda mem::linebuffer
	cmp #' '
	bne @left_aligned
	jsr src::delete
	ldx #$00
	ldy #39
	jsr linebuff::shl
	beq @removespaces

@left_aligned:
	jsr src::down
	pla		; get the type of line we're formatting
	beq @done	; if ASM_NONE, don't format
	cmp #ASM_LABEL
	bne @notlabel
	jmp __fmt_label
@notlabel:
	cmp #ASM_COMMENT ; if comment, don't format at all
	beq @done
@ident: jmp __fmt_opcode ; anything else- indent
@done:  rts
.endproc
