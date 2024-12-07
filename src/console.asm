.include "bitmap.inc"
.include "cursor.inc"
.include "debug.inc"
.include "debugcmd.inc"
.include "edit.inc"
.include "errors.inc"
.include "expr.inc"
.include "key.inc"
.include "keycodes.inc"
.include "finalex.inc"
.include "macros.inc"
.include "memory.inc"
.include "string.inc"
.include "strings.inc"
.include "text.inc"
.include "zeropage.inc"

;******************************************************************************
HEIGHT = 24

.segment "CONSOLE_BSS"
line: .byte 0		; the line that the console is on
repeatcmd: .byte 0	; if set, empty line repeats last command

cursave_x: .byte 0
cursave_y: .byte 0

;******************************************************************************
; SIGNALS
.export __console_quit
__console_quit: .byte 0	; if !0, console will quit when command returns to it

.CODE
;******************************************************************************
; GETCH
; Handles the key (called by the keyboard gets handler)
.proc getch
	jsr key::getch
	beq :+
	cmp #K_SWAP_USERMEM_TUI
	bne :+
	jsr dbg::swapusermem
	dec mem::coloron	; (re-disable color)
	lda #$00
:	rts
.endproc

.segment "CONSOLE"

;******************************************************************************
; PUTS
; Prints the given line to the console
; IN:
;   - .XY: the address of the line to print
.export __console_puts
.proc __console_puts
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
	JUMP FINAL_BANK_MAIN, #text::print
.endproc

;******************************************************************************
; ENTER
; Activates the console.
.export __console_enter
.proc __console_enter
	CALL FINAL_BANK_MAIN, #bm::clr
	lda #$00
	sta line

	; save cursor state of caller
	lda zp::curx
	sta cursave_x
	lda zp::cury
	sta cursave_y

	; fall through to __console_reenter
.endproc

;******************************************************************************
; REENTER
; Activates the console without clearing the screen
.export __console_reenter
.proc __console_reenter
	; initialize QUIT signal state
	lda #$00
	sta __console_quit

	; treat whitespace as separator for expressions in the console
	lda #$01
	CALL FINAL_BANK_MAIN, #expr::end_on_ws

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
	sta zp::curx		; move to start of line
	ldx #$01
	ldy #$00
	CALL FINAL_BANK_MAIN, #cur::setmin

	ldxy #getch
	CALL FINAL_BANK_MAIN, #edit::gets
	bcs @clrline
	pha

	lda line
	cmp #HEIGHT-1
	bcc :+

	; if at bottom of the screen, scroll everything up
	CALL FINAL_BANK_MAIN, #text::scrollup
	dec line
:	inc line		; move down a row before running command
	pla
	cmp #$02		; 2 because prompt makes min length 1
	bcs @run
	jmp @prompt		; if command length is 0, there is no command

@run:	; run the command
	ldxy #$101
	jsr dbgcmd::run
	bcc @ok			; if it succeeded, continue

@err:	CALL FINAL_BANK_MAIN, #err::get
	CALL FINAL_BANK_MAIN, #str::uncompress
	jsr __console_puts

@ok:	lda __console_quit	; was QUIT signal sent?
	bne @done
	jmp @prompt

@done:	lda cursave_x
	sta zp::curx
	lda cursave_y
	sta zp::cury

	lda dbg::interface
	bne :+
	; if debug interface changed back to GUI, refresh editor
	CALL FINAL_BANK_MAIN, #edit::refresh
:	rts
.endproc
