.include "bitmap.inc"
.include "cursor.inc"
.include "debugcmd.inc"
.include "edit.inc"
.include "key.inc"
.include "macros.inc"
.include "memory.inc"
.include "strings.inc"
.include "text.inc"
.include "zeropage.inc"

;******************************************************************************
HEIGHT = 24

.BSS
line: .byte 0	; the line that the console is on

.CODE

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
	jsr text::scrollup
	dec line
	lda line

@print:	inc line
	ldxy @msg
	jsr text::print
	rts
.endproc

;******************************************************************************
; ENTER
; Activates the console. Returns when F7 is pressed
.export __console_enter
.proc __console_enter
	jsr bm::clr
	lda #$00
	sta line
	
@prompt:
	jsr text::clrline
	lda #'>'
	sta mem::linebuffer
@clrline:
	lda #$00
	sta mem::linebuffer+1

@loop:	lda line
	sta zp::cury
	lda #$01
	sta cur::minx
	sta zp::curx		; move to start of line
	ldxy #key::getch
	jsr edit::gets		; get a command
	bcs @clrline
	
	inc line		; move down a row before running command
	cmp #$00
	beq @prompt		; if command length is 0, there is no command

	jsr dbgcmd::run		; run the command
	bcc @prompt		; if it succeeded, continue

	ldxy #strings::invalid_command
	jsr __con_puts
	jmp @prompt
.endproc
