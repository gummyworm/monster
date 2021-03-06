.include "asm.inc"
.include "bitmap.inc"
.include "codes.inc"
.include "cursor.inc"
.include "format.inc"
.include "irq.inc"
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
        jsr bm::init
        jsr bm::clr

	jsr edit

	ldx #$00
	ldy #$01
	jsr cur::set

	;jsr test
	jsr src::new

	ldxy #titlebar
	lda #$00
	jsr text::puts
	lda #$00
	jsr bm::rvsline

;--------------------------------------
; main is the main loop for editing a line.
.proc main
        lda #$05
        cmp $9004
        bne *-3

	jsr key::getch
	cmp #$00
	beq @done
	jsr onkey

@done:	jsr text::update
	jsr text::status
	lda #23
	jsr bm::rvsline
	jmp main
.endproc

;--------------------------------------
; onkey is called upon the user pressing a key.
.proc onkey
	cmp #$85	; F1
	bne :+
	jmp saveas
:	cmp #$86	; F3
	bne :+
	jmp save
:	cmp #$87	; F5
	bne :+
	jmp load
:	cmp #$b4 	; C=<H>
	bne :+
	jmp help
:	cmp #$b2	; C=<R>
	bne :+
	jmp rename
:	cmp #$be	; C=<V>
	bne :+
	lda features
	eor #FEATURE_VIEW
	sta features
	jmp memview
:	cmp #$b6 	; C=<L>
	bne :+
	jmp text::dir

:	jsr insert
	jsr cur::off
	jsr cur::on
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
	ldx #20
	ldy #23
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
	lda #$0d
	jsr src::insert
	jsr text::putch
	ldx #<mem::linebuffer
	ldy #>mem::linebuffer
	jsr asm::tokenize
	tax
	bmi @err

@noerr: ; compilation was successful, update memory and copy line to source
	; buffer
	pha
	jsr asm::advancepc

	; format line
	pla
	bne :+
	lda #ASM_LABEL
	skw
:	lda #ASM_OPCODE
	;jsr fmt::line

	; copy line to source buffer
	pha
	lda zp::cury
	jsr text::drawline
	ldx #<mem::linebuffer
	ldy #>mem::linebuffer
	pla
	jsr text::hioff

	; update memory display (if view is enabled)
	jsr memview

	; redraw the current (formatted) line
	lda zp::cury
	jsr text::drawline
	lda #$01
	sta text::insertmode

	; scroll lines below cursor position
	ldx zp::cury
	inx
	txa
	jsr text::scroll

	; clear any error message
	jsr text::clrline
	ldxy #mem::linebuffer
	lda #ERROR_ROW
	jsr text::print
	ldx #$08
	lda zp::cury
	jsr text::hiline

	; move the cursor to the next line
	ldy zp::cury
	iny
	ldx #$00
	jsr cur::set

	; redraw the cleared status line
	jsr text::update
	jsr text::status
	jsr refresh

	; redraw everything from <cursor> to EOL on next line
	jsr src::get
	ldxy #mem::linebuffer
	lda zp::cury
	jsr text::print
	rts

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

;--------------------------------------
; refresh redraws all the visible lines.
.proc refresh
	rts
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
	jsr cur::up
	jsr src::up
	jsr src::get
	rts
.endproc

;--------------------------------------
.proc ccleft
	jsr cur::left
	jsr src::prev
	rts
.endproc

;--------------------------------------
.proc ccright
	jsr cur::right
	jsr src::next
	rts
.endproc

;--------------------------------------
.proc ccdown
	jsr src::down
	bcc :+
	jsr cur::down
:	jsr src::get
	rts
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
