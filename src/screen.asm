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
NUM_COLS    = 20	; number of 8-pixel columns
NUM_ROWS    = 11	; number of 16-pixel rows

SCREEN_ROWS = 12	; number of physical rows per column

PIXELS_PER_COL = 11*16	; number of pixels per column

VSCREEN_WIDTH = 80	; virtual screen size (in 8-pixel characters)

.segment "SAVESCR"

;******************************************************************************
; PUSH_COL
; Shifts the screen right by one character, clears the new rightmost column,
; pushes the leftmost bitmap column
.export __scr_pushcol
.proc __scr_pushcol
@stack=r0
	ldxy stackptr
	stxy r0

	ldy #PIXELS_PER_COL
:	lda BITMAP_ADDR-1,y	; save the leftmost column's bm data
	sta (@stack),y
	dey
	bne :-

	; update stack pointer
	lda stackptr
	clc
	adc #PIXELS_PER_COL
	sta stackptr
	bcc @done
	inc stackptr+1
@done:	; fall through to SHL
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
@l0:
	ldy #$00
	lda (@src),y
	pha

@l1:	lda (@src),y
	sta (@dst),y
	iny
	cpy #NUM_COLS-1
	bne @l1

	; wrap character at column 0 to last column
	pla
	sta (@dst),y

	lda @dst
	adc #NUM_COLS-1	; .C always set
	sta @dst
	sta @src
	inc @src

	dex
	bne @l0
	rts
.endproc

;******************************************************************************
; POP_COL
; Unshifts the screen by 1 character
; Pops the bitmap column from the stack and blits it to the rightmost bitmap
; column
.export __scr_popcol
.proc __scr_popcol
@stack=r0
	ldxy stackptr
	stxy @stack

	ldy #PIXELS_PER_COL
:	lda (@stack),y
	sta BITMAP_ADDR-1,y	; restore the leftmost column's bm data
	dey
	bne :-

	; fallthrough to shift screen
.endproc

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
	ldxy #SCREEN_ADDR+(SCREEN_ROWS*NUM_COLS)
	stxy @dst
	dex
	stxy @src

	ldx #NUM_ROWS
@l0:	ldy #NUM_COLS-2
@l1:	lda (@src),y
	sta (@dst),y
	dey
	bpl @l1

	; last character: wrap around
	ldy #NUM_COLS-1
	lda (@src),y
	ldy #$00
	sta (@dst),y

	dex
	bne @l0

	; fall through to update stack pointer
.endproc

;******************************************************************************
; DROP_COL
; Drops the top column from the screen stack
.export __scr_dropcol
.proc __scr_dropcol
@stack=r0
	lda stackptr
	sec
	sbc #PIXELS_PER_COL
	sta stackptr
	bcs @done
	dec stackptr
@done:	rts
.endproc

stackptr: 	.word stack

.segment "SAVESCR_BSS"
stack: 		.res (NUM_ROWS*16)*(VSCREEN_WIDTH)-(40/2)	; $4a40
