;*******************************************************************************
; CONSOLE.ASM
; This file contains procedures for interacting with the "console".
; The console is a text-based interface that can be used for interacting with
; program state as well as debugging.
;*******************************************************************************

.include "bitmap.inc"
.include "config.inc"
.include "cursor.inc"
.include "debug.inc"
.include "debugcmd.inc"
.include "edit.inc"
.include "errors.inc"
.include "expr.inc"
.include "file.inc"
.include "irq.inc"
.include "key.inc"
.include "keycodes.inc"
.include "finalex.inc"
.include "macros.inc"
.include "memory.inc"
.include "string.inc"
.include "strings.inc"
.include "text.inc"
.include "zeropage.inc"

.import is_whitespace	; from debugcmd.asm

;*******************************************************************************
HEIGHT = 24

.segment "CONSOLE_BSS"
.export __console_line

__console_line:
line:      .byte 0	; the line that the console is on
repeatcmd: .byte 0	; if set, empty line repeats last command

cursave_x: .byte 0
cursave_y: .byte 0

outfile: .byte 0	; screen (0) or file handle to output con::puts to

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
; PUTS
; Prints the given line to the console
; IN:
;   - .XY: the address of the line to print
; OUT:
;   - .C: set on error (failed to write to output file)
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
	lda #HEIGHT-1
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

	; render the message
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

	lda outfile
	beq @screen

@file:	; write the line to file
	ldy #$00
:	lda (@msg),y
	beq @write_to_file
	iny
	cpy #LINESIZE
	bcc :-

@write_to_file:
	tya
	clc
	adc @msg
	sta file::save_address_end
	lda @msg+1
	adc #$00
	sta file::save_address_end+1

	ldxy @msg
	lda outfile
	CALL FINAL_BANK_MAIN, #file::savebin

	; write a newline
	lda #$0d
	jmp $ffd2

@screen:
	lda line
	inc line
	ldxy @msg
	JUMP FINAL_BANK_MAIN, #text::print
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
	CALL FINAL_BANK_MAIN, #text::print
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

	lda #$00
	sta outfile

	ldxy #mem::linebuffer
	jsr __console_puts

	ldxy #mem::linebuffer
	jsr set_outfile

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
	php
	pha

	; close the output file (if not screen)
	lda outfile
	beq :+
	CALL FINAL_BANK_MAIN, #file::close
	CALL FINAL_BANK_MAIN, #irq::raster

:	pla
	plp
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

;*******************************************************************************
; SET OUTFILE
; Parses the line for any redirection operator ('>') and sets the output file
; for the debug command to be executed as appropriate.
; The default output "file" is the screen.
; OUT:
;   - outfile: the file ID to store to
;   - .C: set on error (failed to open file)
.proc set_outfile
	ldx #$00

@findredir:
	cpx #40-1
	beq @done
	lda mem::linebuffer+1,x	; start after prompt (+1)
	beq @done		; no redirect, return
	cmp #'>'		; redirect?
	beq @redir
	inx
	bne @findredir
	rts

@redir:	; get the filename to redirect the ouput to
	lda #$00
	sta mem::linebuffer+1,x	; terminate the line where the redirect was
@l0:	inx
	lda mem::linebuffer+1,x
	beq @err_nofile

	jsr is_whitespace
	beq @l0			; eat whitespace

	txa
	pha

	; disable IRQ for file IO
	CALL FINAL_BANK_MAIN, #irq::disable

	; found the start of the filename
	; open the output file
	pla
	clc
	adc #<(mem::linebuffer+1)
	tax
	lda #>(mem::linebuffer+1)
	adc #$00
	tay
	CALL FINAL_BANK_MAIN, #file::open_w

	bcs @err
	sta outfile
@done:	rts

@err:	; display error
	ldxy #strings::file_open_failed
	jmp __console_puts

@err_nofile:
	; display error
	ldxy #strings::nofile
	jmp __console_puts
.endproc
