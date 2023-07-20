.include "asm.inc"
.include "bitmap.inc"
.include "codes.inc"
.include "config.inc"
.include "cursor.inc"
.include "format.inc"
.include "key.inc"
.include "layout.inc"
.include "memory.inc"
.include "source.inc"
.include "text.inc"
.include "util.inc"
.include "view.inc"
.include "zeropage.inc"

.include "macros.inc"
.import help

STATUS_LINE = 23
FEATURE_VIEW = $01

;--------------------------------------
.proc draw_titlebar
	ldxy #titlebar
	lda #$00
	jsr text::puts
	lda #$00
	jmp bm::rvsline
.endproc

titlebar:
.byte "monster                      c=<h>: help"

;--------------------------------------
.export __edit_init
.proc __edit_init
        jsr bm::init
        jsr bm::clr

	jsr edit

	jsr draw_titlebar

	ldx #$00
	ldy #$01
	jmp cur::set
.endproc

;--------------------------------------
.export __edit_run
.proc __edit_run
; run is the main loop for the editor
main:
        lda #$70
        cmp $9004
        bne *-3

	jsr key::getch
	cmp #$00
	beq @done

	sei
	jsr onkey
	cli

@done:	jsr text::update
	jsr text::status
	jmp main
.endproc

;--------------------------------------
.proc save_state
	jsr bm::save
	ldx #zp::app_vars_size-1
:	lda zp::app_vars,x
	sta mem::spare,x
	dex
	bpl :-
	rts
.endproc

;--------------------------------------
.proc restore_state
	jsr bm::restore
	ldx #zp::app_vars_size-1
:	lda mem::spare,x
	sta zp::app_vars,x
	dex
	bpl :-
	rts
.endproc


;--------------------------------------
.proc command_go
	jsr asm::label_address
	cmp #$ff
	beq @not_found
	stxy @target
@target=*+1
	jsr $f00d
	jmp restore_state
@not_found:
	rts
.endproc

;--------------------------------------
; command_asm assembles the entire source into mem::program
.export command_asm
.proc command_asm
	jsr src::pushp

	jsr src::rewind
	jsr asm::reset
@doline:
	jsr src::readline
	ldxy #mem::linebuffer
	jsr asm::tokenize
	inc @ll
	jsr src::end
	bne @doline
@ll=*+1
	lda #$00

	;jsr src::popp
	;jsr src::goto

@printresult:
	ldxy zp::asmresult
	sub16 #mem::program
	txa
	pha
	tya
	pha

	ldxy #success_msg
	lda #STATUS_LINE-1
	jsr text::print
	jsr text::clrline
	rts

success_msg: .byte "done. ", $fe, " bytes", 0
.endproc

;--------------------------------------
.proc docommand
; .A contains the command
	pha
	sta @cmd

	ldx #$02
	ldy #$01
	jsr cur::setmin
	ldx #40
	ldy #STATUS_LINE+1
	jsr cur::setmax

	lda zp::curx
	pha
	lda zp::cury
	pha

	; get a line of input
	ldx #$02
	ldy #STATUS_LINE
	jsr cur::set
	jsr text::clrline

@cmd=*+1
	lda #$00
	sta mem::linebuffer
	lda #':'
	sta mem::linebuffer+1

	ldxy #mem::linebuffer
	lda #STATUS_LINE
	jsr text::drawline
	cli
@getkey:
        lda #$70
        cmp $9004
        bne *-3
	jsr text::update

	jsr key::getch
	cmp #$0d
	beq @run
	cmp #$5f	; <- (done)
	bne :+

	jsr text::clrline
	jsr edit
	pla
	tay
	pla
	tax
	pla
	jmp cur::set

:	cmp #$00
	beq @getkey
	sei
	jsr text::putch
	cli
	jmp @getkey
@run:
	jsr text::putch	; add the newline
	pla
	tay
	pla
	tax
	jsr cur::set
	jsr edit

	pla
	ldx #@num_commands-1
:	cmp @command_codes,x
	beq @found
	dex
	bpl :-
	bmi @done

@found:
	txa
	asl
	tax
	lda @command_table,x
	sta @cmd_vec
	lda @command_table+1,x
	sta @cmd_vec+1

	ldx #<(mem::linebuffer+2)
	ldy #>(mem::linebuffer+2)
@cmd_vec=*+1
	jsr $0000
@done:
	jsr text::clrline
	rts

; commands
@command_codes:
.byte 'g'
.byte 'o'
.byte 's'
@num_commands=*-@command_codes
@command_table:
.word command_go
.word load
.word save
.endproc

;--------------------------------------
; onkey is called upon the user pressing a key.
.proc onkey
	cmp #$85	; F1 (save)
	bne :+
	jmp save
:	cmp #$89	; F2 (save as)
	bne :+
	jmp saveas
:	cmp #$86	; F3 (assemble)
	bne :+
	jmp command_asm
:	cmp #$87	; F5 (Load)
	bne :+
	jmp load
:	cmp #$bc	;C=<C> (Refresh)
	bne :+
	jmp refresh
:	cmp #$b4 	; C=<H> (Help)
	bne :+
	jmp help
:	cmp #$b2	; C=<R> (Rename)
	bne :+
	jmp rename
:	cmp #$be	; C=<V> (View)
	bne :+
	jmp memview
:	cmp #$b6 	; C=<L> (Dir)
	bne :+
	jmp dir
:	cmp #$a5	; C=<G> (Go)
	bne :+
	lda #'g'
	jmp docommand
:	cmp #$b9	; C=<O> (Open)
	bne :+
	lda #'o'
	jmp docommand
:	cmp #$ae	; C=<S> (Save)
	bne :+
	lda #'s'
	jmp docommand
:	jsr insert
	jsr cur::off
	jmp cur::on
.endproc

;--------------------------------------
; refresh redraws the screen
.proc refresh
	jsr cur::off
	jsr __edit_init
	jsr src::rewind
@l0:
	jsr src::readline
	jsr drawline
	jsr src::end
	bne @l0
	rts
.endproc

;--------------------------------------
; dir lists the directory
.proc dir
	jsr bm::save
	jsr text::clrline
	jsr text::dir
	cli

	; get a selection
:	jsr key::getch
	cmp #$0d
	bne :-
	sei
	jmp bm::restore
.endproc

;--------------------------------------
; rename gets user input to rename the buffer and applies the new name.
.proc rename
	jsr text::savebuff
	jsr text::clrline
	getinput mem::statusline+23,0,23,(40-16)
	ldxy #mem::linebuffer
	jsr src::rename
	jsr text::restorebuff
	lda zp::cury
	jmp text::drawline
.endproc

;--------------------------------------
; saveas allows the user to name the current buffer- then writes it to a file
; of the same name.
.proc saveas
	jsr rename
	jmp save
.endproc

;--------------------------------------
; edit configures the cursor/screen/etc. for editing
.proc edit
	lda #$01
	sta text::insertmode
	ldx #$00
	ldy #$01
	jsr cur::setmin
	ldx #40
	ldy #STATUS_LINE
	jmp cur::setmax
.endproc

;--------------------------------------
; save writes the source buffer to a file.
.proc save
	txa
	pha
	tya
	pha

	ldxy #@savingmsg
	lda #STATUS_LINE
	jsr text::print

	pla
	tay
	pla
	tax

	sei
	jsr src::save
	cli
	cmp #$00
	bne @err
	rts	; no error
@err:
	rts
	pha
	lda #$00
	pha
	ldxy #@errmsg
	lda #STATUS_LINE
	jsr text::print
	rts
@savingmsg:
	.byte "saving...",0
@errmsg:
.byte "failed to save file; error ", $fe, 0
.endproc

;--------------------------------------
; load loads the file from disk into the source buffer
.proc load
	txa
	pha
	tya
	pha

	ldxy #@loadingmsg
	lda #STATUS_LINE
	jsr text::print

	pla
	tay
	pla
	tax

	sei
	jsr src::load
	cli
	cmp #$00
	bne @err

	jmp refresh
@err:
	pha
	lda #$00
	pha
	ldxy #@errmsg
	lda #STATUS_LINE
	jsr text::print
	rts
@loadingmsg:
	.byte "loading...",0
@errmsg:
.byte "failed to load file; error ", $fe, 0
.endproc

;--------------------------------------
; linedone attempts to compile the line entered in (mem::linebuffer)
.proc linedone
	lda zp::curx
	pha
	lda #$0d
	jsr src::insert
	jsr text::putch

	pla
	beq @nextline ; we're at column 0, scroll the screen and return

	ldx #<mem::linebuffer
	ldy #>mem::linebuffer
	jsr asm::tokenize
	tax
	bmi @err

@noerr: ; compilation was successful, format line
	bne :+
	lda #ASM_LABEL
	skw
:	lda #ASM_OPCODE
	jsr fmt::line

@nextline:
	jsr drawline

	; reset flags
	lda #$01
	sta text::insertmode

	; redraw the cleared status line
	jsr text::update

	; redraw everything from <cursor> to EOL on next line
	jsr src::get
	ldxy #mem::linebuffer
	lda zp::cury
	jmp text::print

@err:	lda #$ff
	jsr fmt::line

	; move cursor back to start of the line
@l0:	ldx #$ff
	ldy #0
	jsr cur::move
	jsr src::prev
	ldx zp::curx
	bne @l0
	jsr src::prev	; retreat one more character

	; set current mode to REPLACE and highlight the error line
	ldx #ERROR_COLOR
	lda zp::cury
	jsr text::hiline
	lda #$00
	sta text::insertmode
	rts
.endproc

;--------------------------------------
; drawline draws the line in mem::linebuffer at the current cursor position.
; The cursor is then updated and the screen scrolled.
.proc drawline
	lda zp::cury
	jsr text::drawline
	ldx #<mem::linebuffer
	ldy #>mem::linebuffer
	jsr text::hioff
@nextline:
	; scroll lines below cursor position
	ldy zp::cury
	iny
	cpy #ERROR_ROW-1
	bcc :+
	; if we're at the bottom, scroll whole screen up
	ldx #1
	lda #STATUS_LINE-2
	jsr text::scrollup
	ldy zp::cury
	ldx #$00
	jmp cur::set

:	tya
	ldx #ERROR_ROW-1
	jsr text::scrolldown

@done:
	jsr clrerror
	; move the cursor to the next line
	ldy zp::cury
	iny
	ldx #$00
	jmp cur::set
.endproc

;--------------------------------------
; memview displays the memory view (if enabled)
.proc memview
	ldx #<src::buffer
	ldy #>src::buffer
	jsr view::edit
	jmp edit
.endproc

;-------------------------------------
.proc clrerror
	; clear any error message
	jsr text::clrline
	ldxy #mem::linebuffer
	lda #ERROR_ROW
	jsr text::putz
	jmp text::hioff
.endproc

;--------------------------------------
; insert adds a character at the cursor position.
.proc insert
	cmp #$80
	bcs @controlcodes
	cmp #' '
	bcs @printable

@controlcodes:
	ldx #numccodes-1
:	cmp controlcodes,x
	beq @cc
	dex
	bpl :-
	jmp @put

@cc:	txa
	asl
	tax
	lda ccvectors,x
	sta @j
	lda ccvectors+1,x
	sta @j+1
@j=*+1
	jmp $0000

@printable:
	ldx text::insertmode
	bne @put
@replace:
	jsr src::replace
	jmp text::putch
@put:	jsr src::insert
	jmp text::putch

;--------------------------------------
.proc ccup
	ldxy src::line
	cmpw #0
	bne :+		; at line 0, don't scroll
	jsr src::up
	ldx #$00
	ldy zp::cury
	jmp cur::set

:	jsr src::up
	lda zp::cury
	pha
	jsr cur::up
	pla
	cmp zp::cury
	beq @scroll

@noscroll:
@cnt=zp::tmp6
	lda zp::curx	; leftmost column
	beq @redraw

	jsr src::up
	jsr src::get	; for rendering get source from start of line

	; go til lesser of curx or newline ($0d)
	lda #$ff
	sta @cnt
:	inc @cnt
	lda @cnt
	cmp zp::curx
	bcs @redraw2
	jsr src::next
	cmp #$0d
	bne :-
	jsr src::prev
	ldx @cnt
	ldy zp::cury
	jsr cur::set
	jmp @redraw2

@scroll:
	lda #$01
	ldx #STATUS_LINE-1
	jsr text::scrolldown	; cursor wasn't moved, scroll

@redraw:
	jsr src::get
@redraw2:
	lda zp::cury
	ldxy #mem::linebuffer
	jmp text::drawline
.endproc

;--------------------------------------
.proc ccleft
	lda zp::curx
	beq :+
	pha
	jsr cur::left
	pla
	cmp zp::curx
	beq :+
	jmp src::prev
:	rts
.endproc

;--------------------------------------
.proc ccright
	jsr src::right
	bcc :+
	jmp cur::right
:	rts
.endproc

;--------------------------------------
.proc ccdown
@cnt=zp::tmp6
@newy=zp::tmp7
@xend=zp::tmp8
	jsr src::end
	bne :+
	rts

:	lda #$00
	sta @cnt
	lda zp::cury
	sta @newy

	jsr src::down
	bcs :+
	jsr src::up
	jsr src::get
	lda #$ff
	sta @xend
	jmp @movex

:	inc @newy
	lda zp::curx
	sta @xend
	jsr src::get

	; if the cursor is on a newline, we're done
	;jsr src::next
	jsr src::end
	beq @movecur
	jsr src::next
	jsr src::atcursor
	cmp #$0d
	php
	jsr src::prev
	plp
	beq @movecur

@movex:
	jsr src::next
	cmp #$0d
	beq @movecur
	inc @cnt
	lda @cnt
	cmp @xend
	bcs @movecur
	jsr src::end
	bne @movex

@movecur:
	ldx @cnt
	lda @newy
	tay
	pha
	jsr cur::set
	pla
	cmp zp::cury
	beq @redraw

	ldx #1
	lda #STATUS_LINE-2
	jsr text::scrollup	; cursor wasn't moved, scroll

@redraw:
	ldxy #mem::linebuffer
	lda zp::cury
	jmp text::drawline
.endproc

;--------------------------------------
.proc ccdel
@cnt=zp::tmp6
	jsr src::start
	bne :+
	rts

:	jsr src::backspace
	lda #$14
	jsr text::putch
	bcs @prevline
@deldone:
	rts

@prevline:
	; move the cursor
	ldy #$ff
	ldx #0
	jsr cur::move

	; scroll everything up from below the line we deleted
	ldx zp::cury
	lda #STATUS_LINE-2
	jsr text::scrollup
	jsr draw_titlebar

	jsr text::clrline
	; get the length of the line we're moving up
	jsr src::get

	; if the current char is a newline, we're done
	jsr src::atcursor
	cmp #$0d
	beq @redraw

	ldxy #mem::linebuffer
	jsr util::strlen
	sta @line2len

	; get the new cursor position
	; new_line_len - (old_line2_len)
	jsr src::up
	;jsr src::next	; 'up' ends on a \n, advance 1 more char for drawing
	jsr src::get
	ldxy #mem::linebuffer
	jsr util::strlen
	sec
@line2len=*+1
	sbc #$00
	sta @cnt
	dec zp::curx
@endofline:
	inc zp::curx
	jsr src::next
	dec @cnt
	bpl @endofline

@redraw:
	lda zp::cury
	ldx #<mem::linebuffer
	ldy #>mem::linebuffer
	jsr text::drawline
	jmp src::end
.endproc

;--------------------------------------
controlcodes:
.byte $9d	; left
.byte $1d	; right
.byte $91	; up arrow
.byte $11	; down
.byte $14	; delete
.byte $0d	; RETURN
numccodes=*-controlcodes

;--------------------------------------
ccvectors:
.word ccleft    ; left
.word ccright	; right
.word ccup      ; up
.word ccdown	; down
.word ccdel 	; delete
.word linedone	; RETURN
.endproc

;--------------------------------------
features: .byte 0
prog_ptr: .word mem::program
