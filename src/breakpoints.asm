.include "bitmap.inc"
.include "debug.inc"
.include "draw.inc"
.include "key.inc"
.include "labels.inc"
.include "layout.inc"
.include "macros.inc"
.include "text.inc"
.include "zeropage.inc"

;******************************************************************************
HEIGHT = BRKVIEW_STOP - BRKVIEW_START
BREAKPOINT_ENABLED = 1

.BSS
;******************************************************************************
scroll: .byte 0
row:	.byte 0

.CODE
;******************************************************************************
; INIT
; Initializes the breakpoint editor
.export __breakpoint_init
.proc __breakpoint_init
	lda #$00
	sta row
	sta scroll
	rts
.endproc

;******************************************************************************
; EDIT
; Begins the breakpoint editor loop.
.export __breakpoint_edit
.proc __breakpoint_edit
@row=zp::tmp10
@scroll=zp::tmp11
	lda row
	sta @row
	lda scroll
	sta @scroll

	jsr __breakpoint_view

@loop:	jsr key::getch
	beq @loop
	cmp #$5f	; <-
	bne @up
	rts

@up:	cmp #$91	; up arrow
	bne @down
	dec @row
	bpl @redraw
	inc @row
	dec @scroll
	bpl @redraw
	inc @scroll
	jmp @redraw

@down:	cmp #$11	; down arrow
	bne @enter
	inc @row
	lda @row
	cmp #HEIGHT
	bcc @redraw
	dec @row
	inc @scroll
	lda @scroll
	clc
	adc @row
	cmp dbg::numbreakpoints
	bcc @redraw
	dec @scroll

@enter:	cmp #$0d
	bne @loop
	; toggle the breakpoint's active status
	lda dbg::breakpoint_flags
	eor #BREAKPOINT_ENABLED
	sta dbg::breakpoint_flags

@redraw:
	jsr __breakpoint_view
	lda @row
	clc
	adc #BRKVIEW_START
	jsr bm::rvsline
	jmp @loop
.endproc

;******************************************************************************
; VIEW
; Displays the breakpoints
.export __breakpoint_view
.proc __breakpoint_view
@cnt=zp::tmp0
@addr=zp::tmp1
@file=zp::tmp3
	lda #BRKVIEW_START
	jsr draw::hline

	; get the last breakpoint
	lda scroll
	cmp dbg::numbreakpoints
	bcs @done			; no visible breakpoints
	sta @cnt
@l0:	lda @cnt
	asl


	; push the address of the breakpoint
	tay
	lda dbg::breakpoints,y
	pha
	tax
	lda dbg::breakpoints+1,y
	pha
	tay

	; get/push the symbol name for this address (if there is one)
	stxy @addr
	jsr lbl::by_addr
	bcc @getname
@noname:
	lda #>@unknown_symbol
	pha
	lda #<@unknown_symbol
	pha
	jmp @lineno

@getname:
	jsr lbl::name_by_id
	tya
	pha
	txa
	pha

@lineno:
	; get the line number and file
	ldxy @addr
	jsr dbg::addr2line
	bcc @getlineno

; an error should not occur (there must be debug info to set a breakpoint,
; but we handle this scenario by just pushing 0 for the line number and ???
; for the filename
@err:	lda #$00
	pha
	pha

	lda #>@unknown_symbol
	pha
	lda #<@unknown_symbol
	pha
	jmp @print

@getlineno:
	sta @file
	; push the line # and filename
	txa
	pha
	tya
	pha
	lda @file
	jsr dbg::get_filename
	tya
	pha
	txa
	pha

@print:; display a '*' if the breakpoint is active
	ldx @cnt
	ldy #' '
	lda dbg::breakpoint_flags,x
	beq :+
	ldy #133
:	sty @brkline

	; print the breakpoint info
	ldxy #@brkline
	lda @cnt
	sec
	sbc scroll
	clc
	adc #BRKVIEW_START+1
	jsr text::print

	; next breakpoint
	inc @cnt
	lda @cnt
	cmp dbg::numbreakpoints
	bcc @l0
@done:	rts

; <filename> l: <line no.> <symbol> : <addr>
; <addr>:<sybmol> L:<line no.> in <filename
@brkline:
.byte " ", ESCAPE_STRING, " l:", ESCAPE_VALUE_DEC, " [", ESCAPE_STRING, "] $", ESCAPE_VALUE,0
@unknown_symbol: .byte "???",0
.endproc
