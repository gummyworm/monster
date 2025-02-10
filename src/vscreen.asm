;*******************************************************************************
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
;*******************************************************************************

.include "config.inc"
.include "macros.inc"
.include "memory.inc"
.include "ram.inc"
.include "settings.inc"
.include "source.inc"
.include "zeropage.inc"

.include "vic20/bitmap.inc"

.import __scr_init

SCREEN_ADDR = $1000
NUM_COLS    = 20	; number of 8-pixel columns
NUM_ROWS    = 11	; number of 16-pixel rows

SCREEN_ROWS = 12	; number of physical rows per column

PIXELS_PER_COL = 11*16	; number of pixels per column

VSCREEN_WIDTH = 80	; virtual screen size (in 8-pixel characters)

MAX_SHIFT = NUM_COLS

.segment "VSCREEN"

;*******************************************************************************
; RESTORE
; Initializes the screen using shiftamount to determine the layout
; IN:
;  - .A: the number of columns to shift the screen
.proc restore
@scr=r0
@row=r2
	lda #$00
	sta @row
	ldxy #SCREEN_ADDR
	stxy @scr

@l0:	lda shiftamount
	beq @done
	lda #NUM_COLS
	sec
	sbc shiftamount
	tay

	lda #$10
	clc
	adc @row

	ldx #NUM_COLS
@l1:	sta (@scr),y
	iny
	cpy #NUM_COLS
	bcc :+
	ldy #$00
:	clc
	adc #SCREEN_ROWS
	dex
	bne @l1

	; move to next screen row
	lda @scr
	clc
	adc #NUM_COLS
	sta @scr

	inc @row
	lda @row
	cmp #NUM_ROWS
	bne @l0
@done:	rts
.endproc

;*******************************************************************************
; SHL
; Shifts the CHARACTER data of the screen to the left.
; See SHR for documentation
; The bottom 2 rows are NOT shifted
.export __scr_shl
.proc __scr_shl
@src=r0
@dst=r2
	lda shiftamount
	cmp #MAX_SHIFT
	bcs @done

	ldxy #SCREEN_ADDR
	stxy @dst
	inx
	stxy @src

	ldx #NUM_ROWS

@l0:	ldy #$00
	lda (@dst),y
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
	inc @src	; src = dst+1

	dex
	bne @l0

	inc shiftamount
@done:	rts
.endproc

;*******************************************************************************
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
	lda shiftamount
	cmp #$01
	beq @done

	ldxy #SCREEN_ADDR
	stxy @src
	inx
	stxy @dst

	ldx #NUM_ROWS

@l0:	ldy #NUM_COLS-2
	lda (@dst),y	; save rightmost char
	pha

@l1:	lda (@src),y	; get char to shift
	sta (@dst),y	; shift it right
	dey
	bpl @l1

	; last character: wrap around
	iny
	pla		; get rightmost char
	sta (@src),y	; store it in leftmost position

	; move to the next row
	lda @src
	clc
	adc #NUM_COLS
	sta @src
	sta @dst
	inc @dst

	dex
	bne @l0

;--------------------------------------
; now copy the bitmap data in
	; get column of bitmap data to copy's address
	ldx shiftamount
	lda vcolumnslo,x
	sta @src
	lda vcolumnshi,x
	sta @src+1

	; get address to copy bitmap column to
	dec shiftamount
	ldx shiftamount
	lda bm_columnslo,x
	sta @dst
	lda bm_columnshi,x
	sta @dst+1

	; copy the column in
	ldy #NUM_ROWS*16-1
:	lda #$ff ;(@src),y
	sta (@dst),y
	dey
	bne :-

	; copy the last byte
	lda #$ff ;(@src),y
	sta (@dst),y

@done:	rts
.endproc

shiftamount: .byte NUM_COLS	; number of columns the screen is shifted

; these pointers are one less than the real addresses they reference
bmptr:	.word BITMAP_ADDR-1

vcolumnslo:
.repeat (VSCREEN_WIDTH/2), i
	.byte <(screen + ($c0*i))
.endrepeat

vcolumnshi:
.repeat VSCREEN_WIDTH/2, i
	.byte >(screen + ($c0*i))
.endrepeat

.linecont +
.define cols $1100, $11c0, $1280, $1340, $1400, $14c0, $1580, $1640, $1700, \
  $17c0, $1880, $1940, $1a00, $1ac0, $1b80, $1c40, $1d00, $1dc0, $1e80, $1f40, \
$1100, $11c0, $1280, $1340, $1400, $14c0, $1580, $1640, $1700, \
  $17c0, $1880, $1940, $1a00, $1ac0, $1b80, $1c40, $1d00, $1dc0, $1e80, $1f40
.linecont -

bm_columnslo: .lobytes cols
bm_columnshi: .hibytes cols

.segment "VSCREEN_BSS"
;*******************************************************************************
; SCREEN
; The "virtual" screen.  This is a continuation of the bitmap at address $1000
screen: .res $2000	; bitmap for 200 columns
