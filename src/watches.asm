.include "bitmap.inc"
.include "debug.inc"
.include "draw.inc"
.include "edit.inc"
.include "errors.inc"
.include "finalex.inc"
.include "flags.inc"
.include "key.inc"
.include "labels.inc"
.include "layout.inc"
.include "macros.inc"
.include "source.inc"
.include "text.inc"
.include "util.inc"
.include "vmem.inc"
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
	lda #$00
	sta row
	sta scroll
	rts
.endproc

;******************************************************************************
; VIEW
; Displays the watches
.export __watches_view
.proc __watches_view
@cnt=zp::tmp13
	; display the title
	ldxy #@title
	lda #MEMVIEW_START
	jsr text::print
	lda #MEMVIEW_START
	jsr bm::rvsline

	; get the first visible watch's offset
	lda scroll
	sta @cnt

@l0:	ldx @cnt
	cpx dbg::numwatches
	bcs @done

	; push current value of the watch
	lda dbg::watch_vals,x
	pha

	lda #$00
	sta @watchline_end		; restore string if it was changed

	; push the value of the watch
	lda dbg::watch_flags,x
	and #WATCH_DIRTY		; dirty?
	beq :+
	lda dbg::watch_prevs,x
	pha				; push previous value if dirty
	lda #CH_R_ARROW
:	sta @watchline_end		; insert a > between old/new values

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
@watchline_end=*-1
.byte ESCAPE_BYTE,0	; if line is DIRTY, this part is appended to @watchline
@title: .byte ESCAPE_SPACING,16, "watches",0
.endproc

;******************************************************************************
; EDIT
; Enters the watch editor until the user exits it
.export __watches_edit
.proc __watches_edit
	jsr __watches_view
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
.endproc

;******************************************************************************
; UPDATE
; Reads the new values of all locations being watched and stores them
.export __watches_update
.proc __watches_update
@cnt=zp::tmp0
@addr=zp::tmp1
	ldx dbg::numwatches
	beq @done
	dex
	stx @cnt
@l0:	ldx @cnt
	lda dbg::watch_vals,x
	sta dbg::watch_prevs,x	; store curr value as prev

	lda @cnt
	asl
	tax
	lda dbg::watches,x
	ldy dbg::watches+1,x

	tax
	tya
	ldy #$00

	jsr vmem::load
	ldx @cnt
	cmp dbg::watch_prevs,x	; same as old value?
	beq :+
	pha
	lda #WATCH_DIRTY
	ora dbg::watch_flags,x
	sta dbg::watch_flags,x	; mark value as DIRTY
	pla
:	sta dbg::watch_vals,x	; store new value
	dec @cnt
	bpl @l0
@done:	rts
.endproc

;******************************************************************************
; MARK
; Marks the watch for the given address (if there is one) as DIRTY. Even if
; its value has not changed.
; IN:
;  - .XY: the address to mark dirty (if a watch exists for it)
; OUT:
;  - .C: clear if the given address was being watched
.export __watches_mark
.proc __watches_mark
@cnt=zp::tmp0
@addr=zp::tmp1
	stxy @addr
	ldx dbg::numwatches
	beq @done
	dex
	stx @cnt
@l0:	lda @cnt
	asl
	tax
	lda @addr
	cmp dbg::watches,x
	bne @next
	lda @addr+1
	cmp dbg::watches+1,x
	beq @found
@next:	dec @cnt
	bpl @l0
@done:	sec
	rts

@found:	ldx @cnt
	lda #WATCH_DIRTY
	sta dbg::watch_flags,x	; mark this watchpoint as DIRTY
	RETURN_OK
.endproc
