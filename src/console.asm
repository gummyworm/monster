;*******************************************************************************
; CONSOLE.ASM
; This file contains procedures for interacting with the "console".
; The console is a text-based interface that can be used for interacting with
; program state as well as debugging.
;*******************************************************************************

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

;*******************************************************************************
HEIGHT = 24

.segment "CONSOLE_BSS"
.export __console_line

__console_line:
line:      .byte 0	; the line that the console is on
repeatcmd: .byte 0	; if set, empty line repeats last command

cursave_x: .byte 0
cursave_y: .byte 0

;*******************************************************************************
; SIGNALS
.export __console_quit
__console_quit: .byte 0	; if !0, console will quit when command returns to it


;*******************************************************************************
; SCREEN
; This buffer stores the complete contents of the console.  It is used to
; restore the console to its last state when it is re-entered
screen: .res 40*24

.CODE
;******************************************************************************
; GETCH
; Handles the key (called by the keyboard gets handler)
.export __console_getch
.proc __console_getch
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

;*******************************************************************************
; NEWL
; Displays a new empty line in the console
.export __console_newl
.proc __console_newl
	ldxy #strings::null
.endproc

;*******************************************************************************
; PUTS
; Prints the given line to the console
; IN:
;   - .XY: the address of the line to print
.export __console_puts
.proc __console_puts
@msg=r0
@scr0=r2
@scr1=r4
@tmp=r6
	stxy @msg

	; check if we need to scroll
	lda line
	cmp #HEIGHT-1
	bcc @print

	; scroll everything up
	ldx #$00
	lda #HEIGHT
	CALL FINAL_BANK_MAIN, #text::scrollup

	; scroll the console screen buffer
	ldxy #screen
	stxy @scr0
	ldxy #screen+40
	stxy @scr1

	; scroll the screen buffer
	ldx #24-1
@scroll:
	ldy #40-1
:	lda (@scr1),y
	sta (@scr0),y
	dey
	bpl :-
	lda @scr0
	clc
	adc #40
	sta @scr0
	bcc :+
	inc @scr0+1
:	lda @scr0
	clc
	adc #40
	sta @scr1
	bcc :+
	inc @scr1+1
:	dex
	bne @scroll

	dec line

@print:	ldxy @msg

	CALL FINAL_BANK_MAIN, #text::render_ind
	stxy @msg

	lda #$00
	sta @scr0+1

	; copy the rendered text to the current line of the buffer
	; the buffer destination is screen + (line*40)
	lda line
	asl		; *2
	sta @tmp
	asl		; *4
	asl		; *8
	adc @tmp	; *10
	asl		; *20
	rol @scr0+1
	asl		; *40
	rol @scr0+1
	adc #<screen
	sta @scr0
	lda @scr0+1
	adc #>screen
	sta @scr0+1

	; store the text we are about to draw to the console buffer
	ldy #39
@copy:	lda (@msg),y
	sta (@scr0),y
	dey
	bpl @copy

	lda line
	inc line
	ldxy @msg
	JUMP FINAL_BANK_MAIN, #text::puts
.endproc

;******************************************************************************
; INIT
; Initializes the console
.export __console_init
.proc __console_init
	lda #$00
	sta line
	rts
.endproc

;******************************************************************************
; ENTER
; Activates the console.
.export __console_enter
.proc __console_enter
@scr=r0
@line=r2
@linebuff=mem::spare
	CALL FINAL_BANK_MAIN, #bm::clr

	lda line
	beq @cont

	; restore the contents of the console screen buffer
	lda #$00
	sta @line
	ldxy #screen
	stxy @scr

@l0:	ldy #40-1
:	lda (@scr),y
	sta @linebuff,y
	dey
	bpl :-

	lda @line
	ldxy #@linebuff
	CALL FINAL_BANK_MAIN, #text::puts
	lda @scr
	clc
	adc #40
	sta @scr
	bcc :+
	inc @scr+1
:	inc @line
	lda @line
	cmp line
	bne @l0

@cont:	; save cursor state of caller
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

	ldxy #__console_getch
	CALL FINAL_BANK_MAIN, #edit::gets
	bcs @clrline
	pha

	ldxy #mem::linebuffer
	jsr __console_puts

	lda line
	cmp #HEIGHT-1
	bcc :+

:	pla
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
