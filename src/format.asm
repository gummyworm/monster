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

.BSS

.export __fmt_enable
__fmt_enable: .byte 0	; flag to enable (!0) or disable (0) formatting

.CODE

;******************************************************************************
; LABEL
; Formats linebuffer as a label.
.proc label
	; read past the label
@l0:	jsr src::right_rep
	bcs @done			; nothing on the line after the label
	; TODO: check invalid label characters

	jsr util::is_whitespace
	bne @l0

	; delete all whitespace until the opcode/macro/etc.
@l1:	jsr src::after_cursor
	bcs @done		; no chars left -> done
	cmp #$0d
	beq @done		; newline -> done
	jsr util::is_whitespace
	bne @tab		; non-whitespace -> separate with tab
	jsr src::delete		; delete whitespaced
	bcc @l1
@done:	rts
@tab:
.endproc

;******************************************************************************
; INDENT
; Insert one indent at current source position, then refresh the
; line buffer and move to the end of it
.proc indent
	lda #$09
	jsr src::insert

@refresh:
	jsr src::pushp
	jsr src::home	; back to start of line
	jsr src::get	; refresh linebuffer
	jsr src::popgoto

	; check the size of the line now that it has a TAB
	jsr text::rendered_line_len
	bcc :+				; ok

	; line would be oversized with a TAB, use a space instead
	jsr src::backspace	; delete the TAB
	lda #' '
	jsr src::insert		; insert a space
	jmp @refresh		; and refresh the line again

:	jmp src::down	; and go to end of line
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
	beq @done
	sta @linecontent
	lda __fmt_enable
	beq @done

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
@label: beq @notlabel
	jmp label

@notlabel:
	lda @linecontent
	and #ASM_COMMENT 	; if comment, don't format at all
	bne @done

@ident: jmp indent		; anything else- indent

@done:  rts
.endproc
