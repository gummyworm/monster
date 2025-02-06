;*******************************************************************************
; BITMAP.ASM
; This file contains procedures for initializing and writing to the the bitmap
; display.  The bitmap is configured as 20 columns of 12 rows of double-height
; user-defined characters for a total of 40 columns and 24 rows of 4x8
; characters.
; This configuration is popularly known as MINIGRAFIK, created by Mike
;*******************************************************************************

.include "finalex.inc"
.include "../fastcopy.inc"
.include "../macros.inc"
.include "../memory.inc"
.include "../settings.inc"
.include "../util.inc"
.include "../zeropage.inc"

;*******************************************************************************
; CONSTANTS
BITMAP_ADDR = $1100
COLMEM_ADDR = $9400

SCREEN_ADDR = $1000
NUM_COLS    = 20	; number of 8-pixel columns
NUM_ROWS    = 11	; number of 16-pixel rows

SCREEN_ROWS = 12	; number of physical rows per column

PIXELS_PER_COL = 11*16	; number of pixels per column

VSCREEN_WIDTH = 80	; virtual screen size (in 8-pixel characters)

.CODE
;*******************************************************************************
; INIT
; MINIGRAFIK VIC/memory initialization
; code by Mike
.export __screen_init
.proc __screen_init
	clc
	lda #$10
	tay
@0:	sta $0ff0,y
	adc #$0c
	bcc @1
	sbc #$ef
@1:	iny
	bne @0
	ldy #$05
@2:     clc
	lda $ede4,y
	adc inittab,y
	sta $9000,y
	dey
	bpl @2

	lda #(BG_COLOR<<4 | BORDER_COLOR)
	sta $900f
	rts
.endproc

;*******************************************************************************
; CLR
; Clears the screen
.export __screen_clr
.proc __screen_clr
	CALL FINAL_BANK_FAST, #fcpy::clr
	lda #TEXT_COLOR
	; fall through
.endproc

;*******************************************************************************
; CLRCOLOR
; Reverts all color memory to the given color
; IN:
;  - .A: the color to fill the screen with
.export __screen_clrcolor
.proc __screen_clrcolor
	ldy #$00
	lda #TEXT_COLOR
@l0:    sta COLMEM_ADDR,y
        dey
        bne @l0
        rts
.endproc

;*******************************************************************************
; CLR_PART
; Clears all rows below the given offset in every column of the bitmap
; IN:
;  - .A: the character row to start clearing at
.export __screen_clr_part
.proc __screen_clr_part
@screen=r0
@offset=r2
	asl
	asl
	asl
	sta @offset
	clc
	adc #<BITMAP_ADDR
	sta @screen
	lda #>BITMAP_ADDR
	sta @screen+1

	; get # of pixels to clear (192 - offset)
	lda #192
	sec
	sbc @offset
	sta @offset

	ldx #20			; number of columns

@l0:	ldy @offset
	dey
	lda #$00
;clear the character memory (bitmap)
@l1:    sta (r0),y
        dey
        bne @l1

	lda @screen
	clc
	adc #$c0
	sta @screen
	bcc :+
	inc @screen+1
:	dex
	bne @l0

        rts
.endproc

;*******************************************************************************
; BM CLRLINE
; Clears the given character row
; IN:
;  - .A: the row to clear
.export __screen_clrline
.proc __screen_clrline
@dst=r0
	jsr __screen_char_addr
	stx @dst
	sty @dst+1

	ldx #20
@l0:	ldy #$07
	lda #$00
@l1:	sta (@dst),y
	dey
	bpl @l1
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

;*******************************************************************************
; RVSLINE
; Reverses 1 row of characters (8 pixels high) at the given row character row
; IN:
;  - .A: the text row to reverse (pixel number / 8)
.export __screen_rvsline
.proc __screen_rvsline
@dst=r0
	jsr __screen_char_addr
	stxy @dst

	ldx #20
@l0:	ldy #$07
@l1: 	lda (@dst),y
	eor #$ff
	sta (@dst),y
	dey
	bpl @l1
	lda @dst
	clc
	adc #$c0
	sta @dst
	bcc :+
	inc @dst+1
:	dex
	bne @l0
@done:	rts
.endproc

;*******************************************************************************
; RVSLINE PART
; Reverses the given number of character (8 pixels high) in the given row
; IN:
;  - .A: the text row to reverse (pixel number / 8)
;  - .Y: the first column to reverse
;  - .X: the last column to reverse
.export __screen_rvsline_part
.proc __screen_rvsline_part
@dst=r0
@odd=r2		; !0 if the character to end at is odd
@start=r3
@stop2=r4
@start2=r5
	asl
	asl
	asl

	; swap Y and X if (X < Y)
	sty @start
	cpx @start
	pha		; save character row

	bcs :+		; if stop > start, swap
	txa
	ldx @start
	tay

:	sty @start2
	stx @stop2
	lda #$00
	sta @odd

	; check whether the start column is even/odd
	tya
	lsr
	sta @start
	ror @odd

	; get the first column to reverse
	tay
	pla
	adc __screen_columnslo,y
	sta @dst
	lda __screen_columnshi,y
	adc #$00
	sta @dst+1

	lda @odd
	beq @cont

@odd0:	; reverse right half of the first column
	ldy #$07
@col0:	lda (@dst),y
	eor #$0f
	sta (@dst),y
	dey
	bpl @col0

	; move to next column
	lda @dst
	clc
	adc #$c0
	sta @dst
	bcc :+
	inc @dst+1
:	inc @start	; first column is done
	inc @start2

	lda @start2
	cmp @stop2
	beq @done

@cont:	; divide character # by 2 to get bitmap column
	txa
	lsr
	tax

	; check if end column is even or odd
	lda #$00
	rol
	sta @odd

	cpx @start
	beq @lastcol
	bcc @done

@l0:	ldy #$07
@l1: 	lda (@dst),y
	eor #$ff
	sta (@dst),y
	dey
	bpl @l1
	lda @dst
	clc
	adc #$c0
	sta @dst
	lda @dst+1
	adc #$00
	sta @dst+1
	dex
	cpx @start
	bne @l0

	; check if we need to do the odd column
	lda @odd
	beq @done

@lastcol:
	; reverse half of the last column
	ldy #$07
@l2:	lda (@dst),y
	eor #$f0
	sta (@dst),y
	dey
	bpl @l2

@done:	rts
.endproc

;*******************************************************************************
; SAVE
; Saves the bitmap to the backup buffer. It may then be restored with a call
; to bm::restore
.export __screen_save
.proc __screen_save
	CALL FINAL_BANK_FASTCOPY2, #fcpy::save

	ldx #NUM_ROWS*2-1
:	lda mem::rowcolors,x
	sta mem::rowcolors_save,x
	lda #DEFAULT_900F
	sta mem::rowcolors,x
	dex
	bpl :-
	jmp __screen_init
.endproc

;*******************************************************************************
; RESTORE
; Restores the bitmap from the backup buffer.
; You should call bm::save first with the buffer you want to restore
.export __screen_restore
.proc __screen_restore
	CALL FINAL_BANK_FASTCOPY2, #fcpy::restore
	; restore the per-row colors
	ldx #NUM_ROWS*2-1
:	lda mem::rowcolors_save,x
	sta mem::rowcolors,x
	dex
	bpl :-
	rts
	; JUMP FINAL_BANK_VSCREEN, #restore
.endproc

;*******************************************************************************
; CHAR ADDR
; Returns the bitmap address for the "character row" of the given row.
; Characters are 8 pixels tall, so this is BITMAP_ADDR+(8*row) where row is
; the provided row.
; IN:
;  - .A: the character row to get the bitmap address of
; OUT:
;  - .XY: the bitmap address
.export __screen_char_addr
.proc __screen_char_addr
	asl
	asl
	asl
	adc #<BITMAP_ADDR
	tax
	ldy #>BITMAP_ADDR
	rts
.endproc

.RODATA
;*******************************************************************************
.linecont +
.define cols $1100, $11c0, $1280, $1340, $1400, $14c0, $1580, $1640, $1700, \
  $17c0, $1880, $1940, $1a00, $1ac0, $1b80, $1c40, $1d00, $1dc0, $1e80, $1f40
.linecont -

.export __screen_columnslo
.export __screen_columnshi
__screen_columnslo: .lobytes cols
__screen_columnshi: .hibytes cols

inittab: .byte $02,$fe,$fe,$eb,$00,$0c
