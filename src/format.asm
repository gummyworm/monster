;*******************************************************************************
; FORMAT.ASM
;
; This file contains the code to format a line of the user's program based
; on its contents.
; The main procedure, fmt::line, looks at the given "type" value and indents
; or unindents depending on what the line contains.
; Labels are unindented, instructions are indented.
;*******************************************************************************

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
; LABEL
; Formats linebuffer as a label.
.proc label
	; read past the label
@l0:	jsr src::right_rep
	bcs @done			; nothing on the line after the label
	jsr util::is_whitespace
	bne @l0

	; delete all spaces until the opcode/macro/etc.
@l1:	jsr src::after_cursor
	bcs @done
	cmp #$0d
	beq @done
	jsr util::is_whitespace
	bne @tab
	jsr src::delete
	bcc @l1
@done:	rts

@tab:	; now insert a TAB to separate label and opcode
	lda #$09
	jmp src::insert
.endproc

;******************************************************************************
; OPCODE
; Formats linebuffer as an opcode.
.proc opcode
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
	cmp #$00
	beq @done		; no format
	sta @linecontent	; save the types to format

	; remove spaces from start of line
	jsr src::up
@removespaces:
	jsr src::after_cursor
	jsr util::is_whitespace
	bne @left_aligned

	jsr src::delete
	ldx #$00
	ldy #39
	jsr linebuff::shl
	beq @removespaces	; branch always

@left_aligned:
	lda @linecontent 	; get the type of line we're formatting
	and #ASM_LABEL		; if formatting includes label, do __fmt_label
	beq @notlabel
	jsr label
	jmp @refresh

@notlabel:
	lda @linecontent
	and #ASM_COMMENT 	; if comment, don't format at all
	bne @done
@ident: jsr opcode		; anything else- indent

@refresh:
	jsr src::home	; back to start of line
	jsr src::get	; refresh linebuffer
	jmp src::down	; and go to end of line

@done:  rts
.endproc
