.include "codes.inc"
.include "config.inc"
.include "linebuffer.inc"
.include "memory.inc"
.include "source.inc"
.include "zeropage.inc"

.CODE

;******************************************************************************
; END_OF_LINE
; Checks if the source is at the end or end of the line
; OUT:
;  - .Z: set if the source pointer is at a newline or at the end of the buffer
.proc end_of_line
	jsr src::end
	beq @done
	jsr src::before_newl
@done:	rts
.endproc

;******************************************************************************
; LABEL
; Formats linebuffer as a label.
.export __fmt_label
.proc __fmt_label
@curr=zp::tmp6
@cnt=zp::tmp6
	; read past the label
	ldy #$00
	sty @curr
@l0:	jsr end_of_line
	beq @done	; if EOL, no more formatting needed
	jsr src::next
	inc @curr
	cmp #' '
	beq @l1
	ldy @curr
	cpy #8		; max label length
	bne @l0

	; read until the opcode/macro/etc.
@l1:	jsr end_of_line		; if we hit EOL before finding anything- done
	beq @done
	jsr src::next
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
	cmp #INDENT_LEVEL+1
	bcc @shr
	bcs @cont

@shl:	jsr src::backspace
	dec @curr
	lda @curr
	cmp #INDENT_LEVEL+2
	bcs @shl

@cont:	jsr src::up
	jsr src::get
@done:	jmp src::down
.endproc

;******************************************************************************
; OPCODE
; Formats linebuffer as an opcode.
.export __fmt_opcode
.proc __fmt_opcode
@cnt=zp::tmp8
	ldx #39-INDENT_LEVEL-1
@l0:	lda mem::linebuffer,x
	sta mem::linebuffer+1,x
	dex
	bpl @l0

	lda #$09
	sta mem::linebuffer

	; indent the linebuffer and source
	lda #$09
	jmp src::insert
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
	cmp #$00
	beq @done
	sta @linecontent	; save the types to format

	; remove spaces from start of line
	jsr src::up
@removespaces:
	lda mem::linebuffer
	cmp #' '
	beq @del
	cmp #$09
	bne @left_aligned
@del:	jsr src::delete
	ldx #$00
	ldy #39
	jsr linebuff::shl
	beq @removespaces	; branch always

@left_aligned:
	lda @linecontent 	; get the type of line we're formatting
	and #ASM_LABEL		; if formatting includes label, do __fmt_label
	beq @notlabel
	jmp __fmt_label
@notlabel:
	lda @linecontent
	and #ASM_COMMENT 	; if comment, don't format at all
	bne @done
@ident: jsr __fmt_opcode 	; anything else- indent
	jmp src::down
@done:  rts
.endproc
