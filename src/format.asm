.include "codes.inc"
.include "config.inc"
.include "linebuffer.inc"
.include "memory.inc"
.include "source.inc"
.include "zeropage.inc"

.CODE

;******************************************************************************
.proc end_of_line
	jsr src::end
	beq @done
	jsr src::after_cursor
	cmp #$0d
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
@done:	jmp src::down
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

	; indent the linebuffer and source
	lda #INDENT_LEVEL-2
	sta @cnt
	lda #' '
@l2:	ldx @cnt
	sta mem::linebuffer,x
	jsr src::insert
	dec @cnt
	bpl @l2
	rts
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
@spacecnt=zp::tmp7
	sta @linecontent	; save the types to format

	; remove spaces from start of line
	jsr src::up
	lda #$00
	sta @spacecnt
@removespaces:
	ldx @spacecnt
	cpx #40
	bcs @shiftbuff
	lda mem::linebuffer,x
	cmp #' '
	bne @shiftbuff
	jsr src::delete
	inc @spacecnt
	bne @removespaces

@shiftbuff:
	ldx @spacecnt
	beq @left_aligned
	ldy #$00
:	lda mem::linebuffer,x
	sta mem::linebuffer,y
	inx
	iny
	cpy #39
	bne :-

@left_aligned:
	lda @linecontent 	; get the type of line we're formatting
	beq @done		; if ASM_NONE, don't format
	and #ASM_LABEL		; if formatting includes labe, do __fmt_label
	beq @notlabel
	jmp __fmt_label
@notlabel:
	lda @linecontent
	and #ASM_COMMENT 	; if comment, don't format at all
	bne @done
@ident: jsr __fmt_opcode 	; anything else- indent
@done:  jmp src::down
.endproc
