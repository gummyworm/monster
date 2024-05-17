.include "source.inc"
.include "macros.inc"
.include "zeropage.inc"

SCREEN_ADDR = $1000
BITMAP_ADDR = $1100
NUM_COLUMNS = 20	; number of 8-pixel columns
NUM_ROWS    = 11	; number of 16-pixel rows

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

;******************************************************************************
; PUSH_COL
; Shifts the screen by one character and
; pushes the leftmost bitmap column
.export __scr_pushcol
.proc __scr_pushcol
@stack=r0
	ldy #192
:	lda BITMAP_ADDR-1,y	; save the leftmost column's bm data
	sta (@stack),y
	lda #$00
	dex
	bne :-

	; jsr shiftleft		; shift the bitmap left a character

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
	ldy #192
:	lda (@stack),y
	sta BITMAP_ADDR+(19*$c0)-1,y	; restore the rightmost column's bm data
	lda #$00
	dex
	bne :-

	; jsr shiftleft			; shift the bitmap left a character

	lda @stack
	clc
	adc #$c0
	sta @stack
	bcc :+
	inc @stack+1
:	rts
.endproc
