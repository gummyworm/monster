.include "bitmap.inc"
.include "cursor.inc"
.include "debug.inc"
.include "debugcmd.inc"
.include "edit.inc"
.include "expr.inc"
.include "key.inc"
.include "keycodes.inc"
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
	JUMP FINAL_BANK_MAIN, #text::print
.endproc

;******************************************************************************
; PRINT
; Prints the given text to the console without a newline
; IN:
;   - .XY: the address of the line to print
.export __con_print
.proc __con_print
@msg=r0
	stxy @msg

	ldy #$00
:	lda (@msg),y
	sta mem::linebuffer,y
	beq @done
	iny
	bne :-

	lda line
	JUMP FINAL_BANK_MAIN, #text::print
@done:	rts
.endproc

;******************************************************************************
; ENTER
; Activates the console.
.export __console_enter
.proc __console_enter
	CALL FINAL_BANK_MAIN, #bm::clr
	lda #$00
	sta line

	; fall through to __console_reenter
.endproc

;******************************************************************************
; REENTER
; Activates the console without clearing the screen
.export __console_reenter
.proc __console_reenter
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

@err:	ldxy #strings::invalid_command
	jsr __con_puts
@ok:	jmp @prompt
.endproc
