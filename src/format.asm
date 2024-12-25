.include "codes.inc"
.include "config.inc"
.include "linebuffer.inc"
.include "memory.inc"
.include "source.inc"
.include "text.inc"
.include "util.inc"
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
	jmp src::before_newl
@done:	rts
.endproc

;******************************************************************************
; LABEL
; Formats linebuffer as a label.
.export __fmt_label
.proc __fmt_label
@curr=r6
@cnt=r6
	; read past the label
	ldy #$00
	sty @curr
@l0:	jsr end_of_line
	beq @done	; if EOL, no more formatting needed
	jsr src::next
	inc @curr
	jsr util::is_whitespace
	bne @l0

	; delete all spaces until the opcode/macro/etc.
@l1:	jsr src::after_cursor
	jsr util::is_whitespace
	bne @tab
	jsr src::delete
	bcc @l1
	rts		; done

@tab:	; now insert a TAB to separate label and opcode
	lda #$09
	jsr src::insert
@done:	rts
.endproc

;******************************************************************************
; OPCODE
; Formats linebuffer as an opcode.
.export __fmt_opcode
.proc __fmt_opcode
@cnt=r8
	; indent the linebuffer and source
	lda #$09
	jmp src::insert
.endproc

;******************************************************************************
; LINE
; Formats the linebuffer according to the given content type.
; IN:
;  - .A: the "type" to format see (codes.inc) e.g. ASM_OPCODE, etc.
.export __fmt_line
.proc __fmt_line
@linecontent=r6
@tmp=r4
	cmp #$00
	beq @done
	sta @linecontent	; save the types to format

	; remove spaces from start of line
	jsr src::up
	lda #$00
	sta @tmp

@removespaces:
	jsr src::after_cursor
	jsr util::is_whitespace
	bne @left_aligned

@del:	jsr src::delete
	ldx @tmp
	ldy #39
	jsr linebuff::shl
	beq @removespaces	; branch always

@left_aligned:
	lda @linecontent 	; get the type of line we're formatting
	and #ASM_LABEL		; if formatting includes label, do __fmt_label
	beq @notlabel
	jsr __fmt_label
	jmp @refresh

@notlabel:
	lda @linecontent
	and #ASM_COMMENT 	; if comment, don't format at all
	bne @done
@ident: jsr __fmt_opcode 	; anything else- indent

@refresh:
	jsr src::up	; back to start of line
	jsr src::get	; refresh linebuffer
	jmp src::down	; and go to end of line

@done:  rts
.endproc
