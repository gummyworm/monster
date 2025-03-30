;*******************************************************************************
; CONSOLE.ASM
; This file contains procedures for interacting with the "console".
; The console is a text-based interface that can be used for interacting with
; program state as well as debugging.
;*******************************************************************************

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
.include "macros.inc"
.include "memory.inc"
.include "screen.inc"
.include "string.inc"
.include "strings.inc"
.include "text.inc"
.include "zeropage.inc"

.include "ram.inc"

.import is_whitespace	; from debugcmd.asm

NMI_HANDLER_ADDR = mem::spare+120
CMD_BUFF         = $101			; written by edit::gets

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
; OUTFILE
; Screen (0) or file handle to output con::puts to
.export __console_outfile
__console_outfile: .byte 0

;*******************************************************************************
; SIGNALS
.export __console_int
.export __console_quit
__console_quit: .byte 0	; if !0, console will quit when command returns to it
__console_int: .byte 0	; if !0, behaved commands will stop running gracefully

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
	beq @done
	cmp #K_SWAP_USERMEM_TUI
	bne @done
	jsr dbg::swapusermem
	dec mem::coloron	; (re-disable color)
	lda #$00
@done:	rts
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
	CALL FINAL_BANK_MAIN, text::scrollup

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
	CALL FINAL_BANK_MAIN, text::render_ind
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

	lda __console_outfile
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
	lda __console_outfile
	CALL FINAL_BANK_MAIN, file::savebin

	; write a newline
	lda #$0d
	jmp $ffd2

@screen:
	lda line
	inc line
	ldxy @msg
	JUMP FINAL_BANK_MAIN, text::print
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
; CLEAR
; Clears the console's contents
.export __console_clear
.proc __console_clear
@scr=r0
	; clear the screen
	CALL FINAL_BANK_MAIN, bm::clr

	; clear the console buffer
	ldxy #screen
	stxy @scr
	ldx #HEIGHT
	ldy #$00
@l0:	tya
	sta (@scr),y
	lda @scr
	clc
	adc #40
	sta @scr
	bcc :+
	inc @scr+1
:	dex
	bne @l0
	stx line	; go back to first line
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
	CALL FINAL_BANK_MAIN, scr::clr

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
	CALL FINAL_BANK_MAIN, text::print
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
	; initialize QUIT and INT signal states
	lda #$00
	sta __console_quit

	jsr install_nmi

	; treat whitespace as separator for expressions in the console
	lda #$01
	CALL FINAL_BANK_MAIN, expr::end_on_ws

@prompt:
	ldxy #mem::linebuffer
	lda line
	CALL FINAL_BANK_MAIN, text::print
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
	CALL FINAL_BANK_MAIN, cur::setmin

	CALL FINAL_BANK_MAIN, irq::on

	ldxy #__console_getch
	CALL FINAL_BANK_MAIN, edit::gets
	bcs @clrline
	pha

	lda #$7f
	sta $911d	; ack all interrupts

	lda #$00
	sta __console_outfile	; default to screen
	sta __console_int	; reset SIGINT

	ldxy #mem::linebuffer
	jsr __console_puts

	ldxy #mem::linebuffer
	jsr set___console_outfile

	lda line
	cmp #HEIGHT-1
	bcc :+

:	pla
	cmp #$02		; 2 because prompt makes min length 1
	bcs @run
	jmp @prompt		; if command length is 0, there is no command

@run:	; run the command
	ldxy #CMD_BUFF
	jsr dbgcmd::run
	php
	pha

	; close the output file (if not screen)
	lda __console_outfile
	beq :+
	CALL FINAL_BANK_MAIN, file::close
	CALL FINAL_BANK_MAIN, irq::on

:	pla
	plp
	bcc @ok			; if it succeeded, continue

@err:	CALL FINAL_BANK_MAIN, err::get
	CALL FINAL_BANK_MAIN, str::uncompress
	jsr __console_puts

@ok:	lda __console_quit	; was QUIT signal sent?
	bne @done
	jmp @prompt

@done:	lda #$7f
	sta $911e		; disable NMIs

	lda cursave_x
	sta zp::curx
	lda cursave_y
	sta zp::cury

	; if debug interface changed back to GUI, refresh editor
	JUMP FINAL_BANK_MAIN, edit::refresh
.endproc

;*******************************************************************************
; SET OUTFILE
; Parses the line for any redirection operator ('>') and sets the output file
; for the debug command to be executed as appropriate.
; The default output "file" is the screen.
; OUT:
;   - __console_outfile: the file ID to store to
;   - .C: set on error (failed to open file)
.proc set___console_outfile
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
	sta $100+1,x
@l0:	inx
	lda mem::linebuffer+1,x
	beq @err_nofile

	jsr is_whitespace
	beq @l0			; eat whitespace

	txa
	pha

	; disable IRQ for file IO
	CALL FINAL_BANK_MAIN, irq::off

	; found the start of the filename
	; open the output fil$100e
	pla
	clc
	adc #<(mem::linebuffer+1)
	tax
	lda #>(mem::linebuffer+1)
	adc #$00
	tay
	CALL FINAL_BANK_MAIN, file::open_w

	bcs @err
	sta __console_outfile
@done:	rts

@err:	; display error
	ldxy #strings::file_open_failed
	jmp __console_puts

@err_nofile:
	; display error
	ldxy #strings::nofile
	jmp __console_puts
.endproc

;*******************************************************************************
; INSTALL NMI
; Copies the NMI handler to shared RAM and enables CA1 (RESTORE key)
; interrupts to catch INT signal
.proc install_nmi
@src=r0
@dst=r2
	ldxy #@nmi_handler
	stxy @src
	ldxy #NMI_HANDLER_ADDR
	stxy @dst

	ldy #@nmi_handler_end-@nmi_handler-1
:	lda (@src),y
	sta (@dst),y
	dey
	bpl :-

	ldxy #NMI_HANDLER_ADDR
	stxy $0318
	lda #$82
	sta $911e		; enable NMIs from RESTORE key
	rts

; The NMI handler - simply sets the INT signal
@nmi_handler:
	pha
	lda $9c02
	pha			; save current bank
	lda #FINAL_BANK_CONSOLE
	sta $9c02
	lda #$01
	sta __console_int	; set INT flag
	pla
	sta $9c02		; restore bank
	pla
	rti
@nmi_handler_end:
.endproc
