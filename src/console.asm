.include "bitmap.inc"
.include "cursor.inc"
.include "debugcmd.inc"
.include "edit.inc"
.include "key.inc"
.include "finalex.inc"
.include "macros.inc"
.include "memory.inc"
.include "strings.inc"
.include "text.inc"
.include "zeropage.inc"


;******************************************************************************
HEIGHT = 24

.segment "CONSOLE_BSS"
line: .byte 0	; the line that the console is on

.segment "CONSOLE"

;******************************************************************************
; PUTS
; Prints the given line to the console
; IN:
;   - .XY: the address of the line to print
.export __con_puts
.proc __con_puts
@msg=r0
	stxy @msg

	; check if we need to scroll
	lda line
	cmp #HEIGHT-1
	bcc @print

	; scroll everything up
	ldx #$00
	lda #HEIGHT
	CALL FINAL_BANK_MAIN, #text::scrollup
	dec line
	lda line

@print:	inc line
	ldxy @msg
	CALL FINAL_BANK_MAIN, #text::print
	rts
.endproc

;******************************************************************************
; ENTER
; Activates the console. Returns when F7 is pressed
.export __console_enter
.proc __console_enter
	CALL FINAL_BANK_MAIN, #bm::clr
	lda #$00
	sta line
@prompt:
	ldxy #mem::linebuffer
	lda line
	CALL FINAL_BANK_MAIN, #text::print
	lda #'>'
	sta mem::linebuffer
	lda #$00
	sta mem::linebuffer+1
@clrline:
	lda #$00
	sta mem::linebuffer+1

@loop:	lda line
	sta zp::cury
	lda #$01
	sta cur::minx
	sta zp::curx		; move to start of line
	ldxy #key::getch
	CALL FINAL_BANK_MAIN, #edit::gets
	bcs @clrline
	pha
	
	lda line
	cmp #HEIGHT-1
	bcc :+
	CALL FINAL_BANK_MAIN, #text::scrollup
	dec line
:	inc line		; move down a row before running command
	pla
	cmp #$02		; 2 because prompt makes min length 1
	bcc @prompt		; if command length is 0, there is no command

	; run the commanu
	ldxy #$101
	jsr dbgcmd::run
	bcc @prompt		; if it succeeded, continue

	ldxy #strings::invalid_command
	jsr __con_puts
	jmp @prompt
.endproc
