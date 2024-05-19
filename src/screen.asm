;******************************************************************************
; SCREEN.ASM
;
; This file contains routines to manipulate the "screen".
; The screen is the character layout that defines where in memory the bitmap
; characters reside.
; By default, the screen is configured so that the bitmap begins at $1100
; at the origin (top-left), with each successive address being referring to
; the next y-coordinate. e.g. $1101 is (0,1), $1102 is (0,2), etc.
;
; When the screen is shifted, each column is offset by the shift amount.
; If the default screen is shifted left, the pixel in $11c0 is now at the top
; left of the screen.
; The bitmap still resides entirely in the $1100-$2000 range, so the addresses
; "roll over" upon crossing $2000.
; When the default layout is shifted left, this means that the address $1100
; will now refer to 8 pixels at the top right of the bitmap display.
;******************************************************************************

.include "source.inc"
.include "macros.inc"
.include "zeropage.inc"

SCREEN_ADDR = $1000
BITMAP_ADDR = $1100
NUM_COLUMNS = 20	; number of 8-pixel columns
NUM_ROWS    = 11	; number of 16-pixel rows

VSCREEN_WIDTH = 80	; virtual screen size (in 8-pixel characters)

;******************************************************************************
; SHR
; Shifts the CHARACTER data of the screen to the right.  This means that the
; bitmap addresses for each column will shift by $c0
; So the default bitmap address arrangement:
; | $1100 | $11c0 |  ...  |
; will now be:
; | $1f40 | $1100 | $11c0 |  ...  |
; after the shift
; The bottom 2 character rows are NOT shifted
.export __scr_shr
.proc __scr_shr
@src=r0
@dst=r2
	ldxy #SCREEN_ADDR+($c0*NUM_COLUMNS)
	stxy @dst
	dex
	stxy @src

	ldx #NUM_ROWS-1
@l0:	ldy #NUM_COLUMNS-1
@l1:	lda (@src),y
	sta (@dst),y
	dey
	bpl @l1
	dex
	bne @l0

	rts
.endproc

;******************************************************************************
; SHL
; Shifts the CHARACTER data of the screen to the left.
; See SHR for documentation
; The bottom 2 rows are NOT shifted
.export __scr_shl
.proc __scr_shl
@src=r0
@dst=r2
	ldxy #SCREEN_ADDR
	stxy @dst
	inx
	stxy @src

	ldx #NUM_ROWS
@l0:	ldy #$00
@l1:	lda (@src),y
	sta (@dst),y
	iny
	cpy #NUM_COLUMNS
	bne @l1
	dex
	bne @l0

	rts
.endproc

.segment "SAVESCR"
;******************************************************************************
; PUSH_COL
; Shifts the screen right by one character, clears the new rightmost column,
; pushes the leftmost bitmap column
.export __scr_pushcol
.proc __scr_pushcol
@stack=r0
	ldy #192
:	lda BITMAP_ADDR-1,y	; save the leftmost column's bm data
	sta (@stack),y
	lda #$00
	sta BITMAP_ADDR-1,y	; clear the bitmap data
	lda #$00
	dex
	bne :-

	jsr __scr_shl		; shift the screen left a character

	; update the stack pointer
	lda @stack
	clc
	adc #$c0
	sta @stack
	bcc :+
	inc @stack+1
:	rts
.endproc

;******************************************************************************
; POP_COL
; Unshifts the screen by 1 character
; Pops the bitmap column from the stack and blits it to the rightmost bitmap
; column
.export __scr_popcol
.proc __scr_popcol
@stack=r0
	ldxy #stack
	stxy @stack
	ldy #192
:	lda (@stack),y
	sta BITMAP_ADDR+(19*$c0)-1,y	; restore the rightmost column's bm data
	lda #$00
	dex
	bne :-

	jsr __scr_shr			; shift the screen right a character

	lda @stack
	clc
	adc #$c0
	sta @stack
	bcc :+
	inc @stack+1
:	rts
.endproc

.segment "SAVESCR_BSS"
stack: .res (NUM_ROWS*16)*(VSCREEN_WIDTH)-(40/2)	; $4a40
