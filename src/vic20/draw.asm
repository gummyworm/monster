.include "settings.inc"
.include "../macros.inc"
.include "../memory.inc"
.include "../zeropage.inc"

.CODE

;******************************************************************************
; HLINE
; Sets the color for the row given in .A
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
	cpx @last
	bcs @done
	sta @n

	; get start row + scroll amount
	txa
	clc
	adc @n
	tay
	cmp @last
	bcs @done		; if first row + n >= last row, don't scroll
@l0:	lda mem::rowcolors,y	; start+.X
	sta mem::rowcolors,x	; start+.A+.X
	inx
	iny
	cpx @last
	bne @l0
	lda #DEFAULT_900F
	sta mem::rowcolors,x	; clear last row
@done:	rts
.endproc

;******************************************************************************
; SCROLLCOLORSD1
; Scrolls all colors in the given range down by 1. See __draw_scrollcolorsd1
; IN:
;  - .X: the first row to scroll
;  - .Y: the last row to scroll
.export __draw_scrollcolorsd1
.proc __draw_scrollcolorsd1
	lda #$01

	; fall through to __draw_scrollcolorsd
.endproc

;******************************************************************************
; SCROLLCOLORSD
; Scrolls all colors from the given start row to the given stop row down by the
; given amount
; IN:
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

;******************************************************************************
; COLOROFF
; Disables color in the interrupt and sets the background to its default color
.export __draw_coloroff
.proc __draw_coloroff
	sei
	lda #$00
	sta mem::coloron
	lda #DEFAULT_900F
	sta $900f
	cli
	rts
.endproc
