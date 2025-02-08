;*******************************************************************************
; LINE.ASM
; This file contains code to advance the line (zp::line)
; These helpers ares used when parsing its contents.
;*******************************************************************************

.include "macros.inc"
.include "util.inc"
.include "zeropage.inc"

;*******************************************************************************
; INC
; Increments the line pointer, which points to the current character being read
; during assembly or other parsing (e.g. expression evaluation)
.export __line_inc
.proc __line_inc
	incw zp::line
	rts
.endproc

;*******************************************************************************
; NEXTCH
; advances the line and THEN processes whitespace
; (equivalent to jsr __line_inc : jsr process_ws).
; OUT:
;  - .A: the new character that the line points to
;  - .Z: set if line is already at the end or is at the end after moving
.export __line_nextch
.proc __line_nextch
	ldy #$00
	lda (zp::line),y
	beq :+			; -> rts

	jsr __line_inc

	; fall through
.endproc

;*******************************************************************************
; PROCESS_WS
; Reads (line) and updates it to point past ' ' chars and non-printing chars
; out:
;  .Z: set if we're at the end of the line
;  .A: the last character processed
;  .Y: 0
;  zp::line: updated to first non ' ' character
.export __line_process_ws
.proc __line_process_ws
	ldy #$00
@l0:	lda (zp::line),y
	beq :+			; if end of line, we're done
	bmi @skip		; skip non-printing chars
	jsr util::is_whitespace
	bne :+			; if not space, we're done
@skip:	jsr __line_inc
	bne @l0
:	rts
.endproc

;*******************************************************************************
; PROCESS_WORD
; Reads (line) and updates it to point to the next whitespace character
; OUT:
;  - .A: contains the last character processed
;  - .Z: set if we're at the end of the line ($00)
.export __line_process_word
.proc __line_process_word
	ldy #$00
	lda (zp::line),y
	beq :-				; to RTS
	jsr util::is_whitespace
	beq :-				; to RTS
	jsr __line_inc
	jmp __line_process_word
.endproc

