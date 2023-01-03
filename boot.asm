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
        jmp enter

;--------------------------------------
.CODE
titlebar:
.byte "monster                      c=<h>: help"

irq_handler:
	jmp $eabf
enter:
	; set address to assemble to
	ldxy #mem::program
	stxy zp::asmresult

	;jsr test
	jsr src::new

reenter:
	jsr initscr
	ldx #$ff
	txs

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
	stxy @target
@target=*+1
	jmp $f00d
	jmp reenter
.endproc

;--------------------------------------
; command_asm assembles the entire source into mem::program
.export command_asm
.proc command_asm
	lda src::line	; save line #
	pha
	lda src::line+1
	pha

	jsr src::rewind
	jsr asm::reset
@doline:
	jsr src::readline
	ldxy #mem::linebuffer
	jsr asm::tokenize
	jsr src::end
	bne @doline

	pla
	tax
	pla
	tay
	; jsr src::goto
	rts
.endproc

;--------------------------------------
.proc docommand
; .A contains the command
	lda zp::curx
	pha
	lda zp::cury
	pha

	; get a line of input
	ldx #$00
	ldy #STATUS_LINE
	jsr cur::set
	cli
@getkey:
	jsr key::getch
	cmp #$0d
	beq @done
	cmp #$00
	beq @getkey
	sei
	jsr text::putch
	cli
	jmp @getkey
@done:
	jsr text::putch	; add the newline
	pla
	tax
	pla
	tay
	jsr cur::set
	pla
	ldx #<mem::linebuffer
	ldy #>mem::linebuffer
	cmp #'g'
	bne :+
	jsr command_go
:	rts
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
	inc $900f
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

	jsr text::scrollup	; cursor wasn't moved, scroll

@redraw:
	ldxy #mem::linebuffer
	lda zp::cury
	jmp text::drawline
.endproc

;--------------------------------------
.proc ccdel
	lda #$14
	jsr text::putch
	bcs @done
	jsr src::backspace
@done:	rts
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
