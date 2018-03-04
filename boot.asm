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

.import test

;------------------------------------------------------------------------------
.segment "SETUP"
.word head
head: .word @Next
.word .version
.byte $9e
.asciiz "4621"
@Next: .word 0
;------------------------------------------------------------------------------
start:
        ldx #<irq_handler
        ldy #>irq_handler
        lda #$20
        jsr irq::raster
	lda #<src::buffer
	sta zp::gap
	lda #>src::buffer
	sta zp::gap+1

	lda #GAPSIZE
	sta zp::gapsize

        jmp enter

;------------------------------------------------------------------------------
.CODE
irq_handler:
	jmp $eabf
enter:
        jsr bm::init
        jsr bm::clr

	ldx #$00
	ldy #$00
	jsr cur::set

	jsr test
	jsr startline

;------------------------------------------------------------------------------
; main is the main loop for editing a line.
.proc main
        lda #$05
        cmp $9004
        bne *-3

	jsr key::getch
	cmp #$00
	beq @done
	jsr insert
	jsr cur::on

@done:	jsr text::update
	jsr text::status
	jmp main
.endproc

;--------------------------------------------------------------------------------
.proc controlcode_del
	lda zp::curx
	beq @done	; cannot delete (cursor is at left side of screen)
	lda text::insertmode
	beq @deldone
	jsr linelen
	stx zp::tmp0
	ldx zp::curx
	dex
@l0:	lda mem::linebuffer+1,x
	sta mem::linebuffer,x
	inx
	cpx zp::tmp0
	bcc @l0
	lda zp::cury
	jsr text::drawline
@deldone:
	ldx #$ff
	ldy #0
	jsr cur::move
@done:	rts
.endproc

;--------------------------------------------------------------------------------
; linedone attempts to compile the line entered in (mem::linebuffer)
.proc linedone
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
	jsr fmt::line

	; copy line to source buffer
	pha
	lda zp::cury
	jsr text::drawline
	ldx #<mem::linebuffer
	ldy #>mem::linebuffer
	pla
	jsr src::puts
	jsr text::hioff

	; update memory display (if view is enabled)
	ldx #<src::buffer
	ldy #>src::buffer
	jsr view::mem

	; redraw the current (formatted) line
	lda zp::cury
	jsr text::drawline
	lda #$01
	sta text::insertmode

	; clear any error message
	jsr text::clrline
	lda #ERROR_ROW
	ldx #<mem::linebuffer
	ldy #>mem::linebuffer
	jsr text::puts
	ldx #$08
	lda zp::cury
	jsr text::hiline

	; move the curosr to the next line
	ldy zp::cury
	iny
	ldx #$00
	jsr cur::set

	; clear (zero out) the line buffer
	lda #39
	sta zp::tmp0
	ldx #<mem::linebuffer
	ldy #>mem::linebuffer
	lda #$00
	jsr util::memset

	; redraw the cleared status line
	jsr text::update
	jsr text::status
	jsr startline
	jsr refresh
	rts

@err:	lda #$ff
	jsr fmt::line

	; move cursor back to start of the line
	ldx #$00
	ldy zp::cury
	jsr cur::set

	; set current mode to REPLACE and highlight the error line
	ldx #$2a
	jsr text::hiline
	lda #$00
	sta text::insertmode
	rts
.endproc

;------------------------------------------------------------------------------
.proc uparrow
	jsr cur::off
	lda zp::curx
	lda #$00	; TODO: delete
	jsr src::lineup ; move up a line in the buffer

	; check if cursor is at the start of the text buffer, don't update the
	; cursor's y position if it is.
	ldx #<src::buffer
	ldy #>src::buffer
	cpx zp::gap
	bne @updatey
	cpy zp::gap+1
	bne @updatey
@updatex:
	ldx #$00
	ldy zp::cury
	jsr cur::set
	rts

@updatey:
	lda zp::cury
	bne @noscroll

@scroll:
	jsr refresh
	skw
@noscroll:
	dec zp::cury

@getline:
	; read the line that was moved to's contents into the linebuffer
	ldx zp::cury
	ldy #$00
	jsr src::getrow
	stx zp::tmp0
	sty zp::tmp0+1

	ldy #$00
@l0:	lda (zp::tmp0),y
	sta mem::linebuffer,y
	iny
	cpy #40
	bcs @done
	cmp #$0d
	bne @l0

@done:	rts
.endproc

;------------------------------------------------------------------------------
; refresh redraws all the visible lines.
.proc refresh
@src=zp::tmp4
@row=zp::tmp6
	lda #$00
	sta @row

@l0:	ldx @row
	ldy #$00
	jsr src::getrow
	lda zp::err
	bne @done
	stx @src
	sty @src+1

	lda #40
	sta zp::tmp0
	lda #' '
	ldx #<mem::linebuffer
	ldy #>mem::linebuffer

	ldy #$00
@l1:	lda (@src),y
	sta mem::linebuffer,y
	cmp #$0d
	beq @next
	iny
	cpy #40
	bcc @l1	; if past col 40, we can't draw the rest of the line

@next:	ldx #<mem::linebuffer
	ldy #>mem::linebuffer
	lda @row
	jsr text::drawline

	inc @row
	lda @row
	cmp #23
	bcc @l0

@done:	rts
.endproc

;------------------------------------------------------------------------------
.proc startline
@cur=zp::tmp0
	ldx zp::gap
	ldy zp::gap+1
	stx @cur
	sty @cur+1

	; copy the contents of the line to the linebuffer
	ldy #$00
@l1:	lda (@cur),y
	beq @done
	sta mem::linebuffer,y
	iny
	cmp #$0d
	bne @l1
@done:	lda #$00
	sta mem::linebuffer,y
	rts
.endproc

;------------------------------------------------------------------------------
; linelen returns the length of mem::linebuffer in .X
.proc linelen
	ldx #$ff
@l0:	inx
	lda mem::linebuffer,x
	beq @done
	cpx #40
	bcs @done
	bne @l0
@done:	rts
.endproc

;------------------------------------------------------------------------------
; insert adds a character at the cursor position.
.proc insert
	cmp #$80
	bcs @controlcodes
	cmp #' '
	bcs @printable

@controlcodes:
@left:	cmp #$9d
	bne @right
	ldx zp::curx
	beq :+
	dex
	stx zp::curx
	jsr src::prev
:	jmp @done

@right: cmp #$1d
	bne @up
	ldx zp::curx
	cpx #40
	bcs :+
	inx
	stx zp::curx
	jsr src::next
:	jmp @done

@up:	cmp #$91	; up arrow?
	bne @del
	jsr uparrow
	rts

@del:	cmp #$14	; delete?
	bne :+
	jsr controlcode_del
	rts

:	cmp #$0d	; RETURN?
	bne :+
	jsr text::putch	; add the newline character for linedone
	jsr linedone
	rts
:	jmp @put

@printable:
	ldx text::insertmode
	beq @put ; 0 = replace, skip bumping the buffer
	pha
	jsr linelen
@l0:	lda mem::linebuffer,x
	sta mem::linebuffer+1,x
	cpx zp::curx
	beq @ins
	dex
	bpl @l0

@ins:	pla
@put:	jsr text::putch
@done:	rts
.endproc

;------------------------------------------------------------------------------
; printline prints the line at the cursor position.
.proc printline
.endproc
