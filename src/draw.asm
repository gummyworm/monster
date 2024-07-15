.include "bitmap.inc"
.include "config.inc"
.include "macros.inc"
.include "memory.inc"
.include "zeropage.inc"

.CODE

;******************************************************************************
; BLANK
; Disables coloring in the IRQ
.export __draw_blank
.proc __draw_blank
	sei
	lda #DEFAULT_900F
	sta $900f
	lda #$00
	sta mem::coloron
	cli
	rts
.endproc

;******************************************************************************
; UNBLANK
; Enables coloring in the IRQ
.export __draw_unblank
.proc __draw_unblank
	lda #$01
	sta mem::coloron
	rts
.endproc

;******************************************************************************
; HLINE
; Draws a horizontal line at the row given in .A
; IN:
;  - .A: the color to highlight with
;  - .X: the row to highlight
.export __draw_hline
.proc __draw_hline
@dst=r0
	sta mem::rowcolors,x

	; check if we need to color in the IRQ
	ldx #24
	lda #DEFAULT_900F
:	cmp mem::rowcolors-1,x
	bne @done
	dex
	bne :-
@done:	stx mem::coloron	; (en/dis)able color
	rts
.endproc

;******************************************************************************
; RVS UNDERLINE
; Reverses a horizontal line at the row given in .A (EOR)
; IN:
;  - .A: the row to draw a horizontal line at
;  - .W: the pattern to draw
.export __draw_rvs_underline
.proc __draw_rvs_underline
@dst=r0
	jsr bm::charaddr
	stxy @dst

	ldx #20
	ldy #$07
@l0: 	lda #$ff
	eor (@dst),y
	sta (@dst),y
	lda @dst
	clc
	adc #$c0
	sta @dst
	bcc :+
	inc @dst+1
:	dex
	bne @l0

	rts
.endproc

;******************************************************************************
; SCROLLCOLORSU
; Scrolls all colors from the given start row to the given stop row up by the
; given amount
; IN:
;  - .X: the first row to scroll
;  - .Y: the last row to scroll
;  - .A: the amount to scroll
.export __draw_scrollcolorsu
.proc __draw_scrollcolorsu
@n=r0
@last=r1
	sty @last
	sta @n

	; get start row + scroll amount
	txa
	clc
	adc @n
	tay
@l0:	lda mem::rowcolors,y	; start+.X
	sta mem::rowcolors,x	; start+.A+.X
	inx
	iny
	cpx @last
	bne @l0
	lda #DEFAULT_900F
	sta mem::rowcolors,x	; clear last row
	rts
.endproc

;******************************************************************************
; SCROLLCOLORSD
; Scrolls all colors from the given start row to the given stop row down by the
; given amount
;  - .X: the first row to scroll
;  - .Y: the last row to scroll
;  - .A: the amount to scroll
.export __draw_scrollcolorsd
.proc __draw_scrollcolorsd
@n=r0
@last=r1
@start=r2
	stx @start
	sty @last
	sta @n

	; get start row + scroll amount
	tya
	sec
	sbc @n
	tax
@l0:	lda mem::rowcolors,x	; start+.X
	sta mem::rowcolors,y	; start+.A+.X
	dey
	dex
	bmi :+
	cpx @start
	bcs @l0
:	lda #DEFAULT_900F
	sta mem::rowcolors	; clear top row
	rts
.endproc
