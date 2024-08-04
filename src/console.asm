.include "bitmap.inc"
.include "debugcmd.inc"
.include "edit.inc"
.include "key.inc"
.include "macros.inc"
.include "strings.inc"
.include "text.inc"
.include "zeropage.inc"

.CODE

;******************************************************************************
; ENTER
; Activates the console. Returns when the "exit" command is run
.export __console_enter
.proc __console_enter
	jsr bm::clr
	lda #$00
	sta zp::cury

@loop:	lda #$00
	sta zp::curx		; move to start of line
	jsr text::clrline
	ldxy #key::getch
	jsr edit::gets		; get a command
	bcs *-3
	
	inc zp::cury		; move down a row before running command
	cmp #$00
	beq @loop		; if command length is 0, there is no command

	jsr dbgcmd::run		; run the command
	bcc @loop		; if it succeeded, continue

	ldxy #strings::invalid_command
	lda zp::cury
	jsr text::print
	inc zp::cury
	bne @loop
.endproc
