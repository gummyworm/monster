.include "bitmap.inc"
.include "debugcmd.inc"
.include "edit.inc"
.include "key.inc"
.include "macros.inc"
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
	sta zp::curx
	sta zp::cury

@loop:	ldxy #key::getch
	jsr edit::gets		; get a command
	bcs *-3
	jsr dbgcmd::run		; run the command
	jmp @loop
.endproc

;******************************************************************************
.RODATA
	.byte ">",0
