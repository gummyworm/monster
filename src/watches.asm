.include "bitmap.inc"
.include "debug.inc"
.include "draw.inc"
.include "edit.inc"
.include "key.inc"
.include "labels.inc"
.include "layout.inc"
.include "macros.inc"
.include "source.inc"
.include "text.inc"
.include "util.inc"
.include "zeropage.inc"

;******************************************************************************
; CONSTANTS
HEIGHT             = WATCHVIEW_STOP-WATCHVIEW_START-1

.BSS
;******************************************************************************
scroll: .byte 0
row:	.byte 0

.CODE
;******************************************************************************
; INIT
; Initializes the watch editor
.export __watches_init
.proc __watches_init
.endproc

;******************************************************************************
; VIEW
; Displays the watches
.export __watches_view
.proc __watches_view
@cnt=zp::tmp13
	; get the first visible watch's offset
	lda scroll
	sta @cnt

@l0:	ldx @cnt
	cpx dbg::numwatches
	bcs @done

	; push the value of the watch
	lda dbg::watch_vals,x
	pha

	; push the address of the watch
	lda @cnt
	asl
	tay
	lda dbg::watches,y
	pha
	tax
	lda dbg::watches+1,y
	pha
	tay

	; print the watch info
	ldxy #@watchline
	lda @cnt
	sec
	sbc scroll
	adc #WATCHVIEW_START	; +1 (carry set)
	jsr text::print

	lda @cnt
	sec
	sbc scroll
	cmp #HEIGHT-1
	bcs @done
	inc @cnt
	jmp @l0		; next watch

@done:	rts

; <"address>: <val>
@watchline:
.byte "$", ESCAPE_VALUE, ": ", ESCAPE_BYTE, 0
.endproc

;******************************************************************************
; EDIT
; Enters the watch editor until the user exits it
.export __watches_edit
.proc __watches_edit
	; display the title
	ldxy #@title
	lda #MEMVIEW_START
	jsr text::print
	lda #MEMVIEW_START
	jsr bm::rvsline

	; if there are no watches, just wait for user to quit
	lda dbg::numwatches
	bne :++
:	jsr key::getch
	cmp #$5f	; <-
	bne :-
	rts

:	lda #$00
	sta row
	sta scroll
	jmp @redraw	; highlight the first row

@loop:	jsr key::getch
	beq @loop
	cmp #$5f	; <-
	bne @up
	rts

@up:	cmp #$91	; up
	bne @down
	dec row
	bpl @redraw
	inc row
	dec scroll
	bpl @redraw
	inc scroll
	jmp @redraw

@down:	cmp #$11	; down
	bne @edit
	inc row
	lda row
	cmp dbg::numwatches
	bcs :+
	cmp #HEIGHT
	bcc @redraw
:	dec row
	inc scroll
	lda scroll
	clc
	adc row
	cmp dbg::numwatches
	bcc @redraw
	dec scroll
	jmp @redraw

@edit:	cmp #$0d
	bne @loop

@editloop:
	jsr key::getch
	beq @editloop
	cmp #$5f		; <- (quit)
	beq @loop
	cmp #$0d		; RETURN
	beq @loop

	jsr key::ishex
	bne @editloop
	jsr util::chtohex	; get the MSB
	bne @editloop

@redraw:
	jsr __watches_view
	lda row			; highlight the row selected
	clc
	adc #WATCHVIEW_START+1
	jsr bm::rvsline
	jmp @loop

@title: .byte ESCAPE_SPACING,16, "watches",0
.endproc
