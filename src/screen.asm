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

.include "bitmap.inc"
.include "finalex.inc"
.include "macros.inc"
.include "source.inc"
.include "zeropage.inc"

SCREEN_ADDR = $1000
NUM_COLS    = 20	; number of 8-pixel columns
NUM_ROWS    = 11	; number of 16-pixel rows

SCREEN_ROWS = 12	; number of physical rows per column

PIXELS_PER_COL = 11*16	; number of pixels per column

VSCREEN_WIDTH = 80	; virtual screen size (in 8-pixel characters)

.CODE

;******************************************************************************
; RESET
; Saves the source bitmap (we will want that later) and reinitializes the
; bitmap
.export __scr_reset
.proc __scr_reset
	jsr bm::save
	jmp bm::init
.endproc

.export __scr_restore
__scr_restore:
	jsr bm::restore
	JUMP FINAL_BANK_SAVESCR, #restore

.segment "SAVESCR"

;******************************************************************************
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

;******************************************************************************
; SAVE
; Saves the current screen shift amount. To restore the screen to this
; layout, call scr::restore
.export __scr_save
.proc __scr_save
	; get the stack depth
	ldxy stackptr
	sub16 #stack-1
.endproc

;******************************************************************************
; PUSH_COL
; Shifts the screen right by one character, clears the new rightmost column,
; pushes the leftmost bitmap column
.export __scr_pushcol
.proc __scr_pushcol
@stack=r0
@bm=r2
	ldxy stackptr		; get stack address - 1
	stxy @stack
	ldxy bmptr		; get bmp address - 1
	stxy @bm

	; copy (@bm) to (@stack) and clear the bitmap area that is copied
	ldy #PIXELS_PER_COL
@l0:	lda (@bm),y		; save the leftmost column's bm data
	sta (@stack),y
	lda #$00
	sta (@bm),y		; clear the bitmap data
	dey
	bne @l0

	; update stack pointer and bitmap pointer
	lda stackptr
	clc
	adc #PIXELS_PER_COL
	sta stackptr
	bcc :+
	inc stackptr+1
	clc

:	; move bmptr forward a column
	lda @bm
	adc #SCREEN_ROWS*16
	sta bmptr
	bcc @done
	inc bmptr+1

@done:	inc shiftamount

	; fall through to SHL
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
@dst=r2
	jsr __scr_dropcol
	bcc :+
	rts

:	ldxy stackptr
	stxy @stack

	ldxy bmptr
	stxy @dst

	; restore the bitmap data from the stack
	ldy #PIXELS_PER_COL
:	lda (@stack),y
	sta (@dst),y	; restore the leftmost column's bm data
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
	ldxy #SCREEN_ADDR
	stxy @src
	inx
	stxy @dst

	ldx #NUM_ROWS

@l0:	ldy #NUM_COLS-2
	lda (@dst),y
	pha

@l1:	lda (@src),y
	sta (@dst),y
	dey
	bpl @l1

	; last character: wrap around
	iny
	pla
	sta (@src),y

	lda @src
	clc
	adc #NUM_COLS
	sta @src
	sta @dst
	inc @dst

	dex
	bne @l0

	; fall through to update stack pointer
	rts
.endproc

;******************************************************************************
; DROP_COL
; Drops the top column from the screen stack
; OUT:
;  - .C: set if the stack is already empty
.export __scr_dropcol
.proc __scr_dropcol
@stack=r0
	ldxy stackptr
	cmpw #stack-1
	bne :+
	sec
	rts		; stack is empty

:	; move bmptr back a column
	lda bmptr
	sec
	sbc #SCREEN_ROWS*16
	sta bmptr
	bcs :+
	dec bmptr+1

:	; move stackptr back a column
	lda stackptr
	sec
	sbc #PIXELS_PER_COL
	sta stackptr
	bcs @done
	dec stackptr+1

@done:	dec shiftamount
	clc
	rts
.endproc

shiftamount: .byte 0	; number of columns the screen is shifted

; these pointers are one less than the real addresses they reference
stackptr: 	.word stack-1
bmptr:		.word BITMAP_ADDR-1

.segment "SAVESCR_BSS"
stack: 		.res (NUM_ROWS*16)*(VSCREEN_WIDTH)-(40/2)	; $4a40
