.include "codes.inc"
.include "config.inc"
.include "linebuffer.inc"
.include "memory.inc"
.include "source.inc"
.include "zeropage.inc"

.CODE

;******************************************************************************
; LABEL
; Formats linebuffer as a label.
.export __fmt_label
.proc __fmt_label
@curr=zp::tmp6
@cnt=zp::tmp6
	jsr src::up

	; read past the label
	ldy #$00
	sty @curr
@l0:	jsr src::next
	inc @curr
	cmp #' '
	beq @l1
	ldy @curr
	cpy #8		; max label length
	bne @l0

	; read until the opcode/macro/etc.
@l1:	jsr src::next
	inc @curr
	cmp #' '
	beq @l1
	jsr src::prev

	lda @curr
	cmp #INDENT_LEVEL	; is opcode already in column 10?
	beq @cont		; continue if so
	bcs @shl		; if >10, delete extra padding

@shr:	lda #' '
	jsr src::insert
	inc @curr
	lda @curr
	cmp #INDENT_LEVEL
	bcc @shr
	bcs @cont

@shl:	jsr src::backspace
	dec @curr
	lda @curr
	cmp #INDENT_LEVEL
	bcs @shl

@cont:	jsr src::up
	jsr src::get
	jsr src::down
@done:	rts
.endproc

;******************************************************************************
; OPCODE
; Formats linebuffer as an opcode.
.export __fmt_opcode
.proc __fmt_opcode
@cnt=zp::tmp6
	ldy #INDENT_LEVEL-1
@l0:	ldx #39-1
@l1:	lda mem::linebuffer,x
	sta mem::linebuffer+1,x
	dex
	bpl @l1
	dey
	bne @l0

	jsr src::up		; return to start of source

	; indent the linebuffer and source
	lda #INDENT_LEVEL-2
	sta @cnt
	lda #' '
@l2:	ldx @cnt
	sta mem::linebuffer,x
	jsr src::insert
	dec @cnt
	bpl @l2
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
@linecontent=zp::tmp6
	sta @linecontent	; save the types to format

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
	lda @linecontent 	; get the type of line we're formatting
	beq @done		; if ASM_NONE, don't format
	and #ASM_LABEL
	beq @notlabel
	jmp __fmt_label
@notlabel:
	lda @linecontent
	and #ASM_COMMENT ; if comment, don't format at all
	bne @done
@ident: jmp __fmt_opcode ; anything else- indent
@done:  rts
.endproc
