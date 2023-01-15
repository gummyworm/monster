.include "asm.inc"
.include "bitmap.inc"
.include "codes.inc"
.include "cursor.inc"
.include "format.inc"
.include "irq.inc"
.include "key.inc"
.include "layout.inc"
.include "memory.inc"
.include "screen.inc"
.include "source.inc"
.include "text.inc"
.include "util.inc"
.include "view.inc"
.include "zeropage.inc"
.include "macros.inc"

.import help

STATUS_LINE = 23
FEATURE_VIEW = $01

.import test

;--------------------------------------
.segment "SETUP"
.word @head
@head: .word @next
.word .version
.byte $9e
.asciiz "4621"
@next: .word 0

;--------------------------------------
start:
        ldx #<irq_handler
        ldy #>irq_handler
        lda #$20
        jsr irq::raster
	jsr src::new
	lda #<brkhandler
	sta $0316
	lda #>brkhandler
	sta $0317
        jmp enter

;--------------------------------------
.CODE
titlebar:
.byte "monster                      c=<h>: help"

;--------------------------------------
.proc brkhandler
	jmp reenter
.endproc

;--------------------------------------
irq_handler:
	jmp $eabf
enter:
	; set address to assemble to
	ldxy #mem::program
	stxy zp::asmresult

	;jsr test
	jsr src::new

reenter:
	ldx #$ff
	txs
	jsr initscr
	jsr text::clrline

;--------------------------------------
; main is the main loop for editing a line.
.proc main
        lda #$70
        cmp $9004
        bne *-3

	jsr key::getch
	cmp #$00
	beq @done

	sei
	jsr onkey
	cli

@done:	jsr scr::update
	jmp main
.endproc

;--------------------------------------
.proc initscr
        jsr bm::init
        jsr bm::clr

	jsr edit

	ldxy #titlebar
	lda #$00
	jsr text::puts
	lda #$00
	jsr bm::rvsline

	ldx #$00
	ldy #$01
	jsr cur::set

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
@not_found:
	jmp reenter
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
	beq @done
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
@done:
	jsr text::putch	; add the newline
	pla
	tay
	pla
	tax
	jsr cur::set
	jsr edit

	pla
	ldx #<(mem::linebuffer+2)
	ldy #>(mem::linebuffer+2)
	cmp #'g'
	bne :+
	jsr command_go
:	jsr text::clrline
	rts
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
	jsr toggle_memview
:	cmp #$b6 	; C=<L> (Dir)
	bne :+
	jmp dir
:	cmp #$a5	; C=<G> (GO)
	bne :+
	lda #'g'
	jmp docommand
:	jsr insert
	jsr cur::off
	jmp cur::on
.endproc

;--------------------------------------
.proc toggle_memview
	lda features
	eor #FEATURE_VIEW
	sta features
	beq :+
	jmp bm::restore
:	jmp bm::save
.endproc

;--------------------------------------
; refresh redraws the screen
.proc refresh
	jsr cur::off
	jsr initscr
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
	jsr text::savebuff
	jsr text::clrline
	jsr bm::save
	jsr text::dir
:	jsr key::getch
	cmp #$0d
	bne :-
	jsr text::restorebuff
	jsr bm::restore
	rts
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
	jsr text::drawline
	rts
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
	ldxy #savingmsg
	lda #STATUS_LINE
	jsr text::print
	jsr src::save
	rts
savingmsg:
	.byte "saving...",0
.endproc

;--------------------------------------
; load loads the file from disk into the source buffer
.proc load
	ldxy #loadingmsg
	lda #STATUS_LINE
	jsr text::print
	sei
	jsr src::load
	cli
	ldx #0
	ldy #1
	jsr cur::set
@redraw:
	jsr src::readline
	pha
	lda zp::cury
	jsr text::drawline
	pla
	cmp #$0d
	bne @done
	ldy #1
	ldx #0
	jsr cur::move
	lda zp::cury
	cmp #23
	bne @redraw
@done:	rts

loadingmsg:
	.byte "loading...",0
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

@noerr: ; compilation was successful, update memory and copy line to source buffer
	; format line
	bne :+
	lda #ASM_LABEL
	skw
:	lda #ASM_OPCODE
	jsr fmt::line

@nextline:
	jsr drawline

	; update memory display (if view is enabled)
	jsr memview

	; reset flags
	lda #$01
	sta text::insertmode

	;jsr clrerror

	; redraw the cleared status line
	jsr text::update
	jsr text::status

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

	; set current mode to REPLACE and highlight the error line
	ldx #$2a
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
	lda features
	and #FEATURE_VIEW
	beq :+
	ldx #<src::buffer
	ldy #>src::buffer
	jmp view::mem
:	rts
.endproc

;-------------------------------------
.proc clrerror
	; clear any error message
	jsr text::clrline
	ldxy #mem::linebuffer
	lda #ERROR_ROW
	jsr text::print
	ldx #$08
	lda zp::cury
	jmp text::hiline
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
	bne :+
@replace:
	jsr src::replace
	jsr text::putch
	rts

:	pha
	jsr text::linelen
@l0:	lda mem::linebuffer,x
	sta mem::linebuffer+1,x
	cpx zp::curx
	beq :+
	dex
	bpl @l0

:	pla
@put:	jsr src::insert
	jsr text::putch
@done:	rts

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
	jsr src::start
	bne :+
	rts
:	jsr src::backspace
	lda #$14
	jsr text::putch
	bcs @prevline
	rts

@prevline:
	; scroll everything from up from below the line we deleted
	ldx zp::cury
	dex
	lda #STATUS_LINE-2
	jsr text::scrollup

	ldy #-1
	ldx #0
	jsr cur::move

	jsr text::clrline
	jsr src::atcursor
	cmp #$0d
	beq :+
	jsr src::up
	jsr src::get
	dec zp::curx
@endofline:
	inc zp::curx
	jsr src::next
	cmp #$0d
	beq :+
	jsr src::end
	bne @endofline

:	lda zp::cury
	ldx #<mem::linebuffer
	ldy #>mem::linebuffer
	jmp text::drawline
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

ccvectors:
.word ccleft    ; left
.word ccright	; right
.word ccup      ; up
.word ccdown	; down
.word ccdel 	; delete
.word linedone	; RETURN
.endproc

features:
.byte 0

prog_ptr:
.word mem::program
