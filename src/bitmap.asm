.include "fastcopy.inc"
.include "finalex.inc"
.include "config.inc"
.include "macros.inc"
.include "memory.inc"
.include "util.inc"
.include "zeropage.inc"

;******************************************************************************
; CONSTANTS
BITMAP_ADDR = $1100
COLMEM_ADDR = $9400

.CODE
;******************************************************************************
; INIT
; MINIGRAFIK VIC/memory initialization
; code by Mike
.export __bm_init
.proc __bm_init
        clc
        lda #$10
        tay
@0:     sta $0ff0,y
        adc #$0c
        bcc @1
        sbc #$ef
@1:     iny
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

;******************************************************************************
; CLR
; Clears the bitmap and sets the color memory to TEXT_COLOR
.export __bm_clr
.proc __bm_clr
	CALL FINAL_BANK_FAST, #fcpy::clr
	lda #TEXT_COLOR
	; fall through
.endproc

;******************************************************************************
; CLRCOLOR
; Reverts all color memory to its initial values (TEXT_COLOR)
.export __bm_clrcolor
.proc __bm_clrcolor
	ldy #$00
@l0:    sta COLMEM_ADDR,y
        dey
        bne @l0
        rts
.endproc

;******************************************************************************
; CLR_PART
; Clears all pixels below the given offset in every column of the bitmap
; IN:
;  - .A: the pixel offset to start clearing at
.export __bm_clr_part
.proc __bm_clr_part
@screen=zp::tmp0
@offset=zp::tmp2
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
@l1:    sta (zp::tmp0),y
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

;******************************************************************************
; BM CLRLINE
; Clears the given character row
; IN:
;  - .A: the row to clear
.export __bm_clrline
.proc __bm_clrline
@dst=zp::tmp0
	jsr __bm_char_addr
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

;******************************************************************************
; RVSLINE
; Reverses 1 row of characters (8 pixels high) at the given row character row
; IN:
;  - .A: the text row to reverse (pixel number / 8)
.export __bm_rvsline
.proc __bm_rvsline
@dst=zp::tmp0
	jsr __bm_char_addr
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

;******************************************************************************
; RVSLINE PART
; Reverses the given number of character (8 pixels high) in the given row
; IN:
;  - .A: the text row to reverse (pixel number / 8)
;  - .Y: the first column to reverse
;  - .X: the last column to reverse
.export __bm_rvsline_part
.proc __bm_rvsline_part
@dst=zp::tmp0
@odd=zp::tmp2	; !0 if the character to end at is odd
@start=zp::tmp3
	asl
	asl
	asl

	; swap Y and X if (X < Y)
	sty @start
	cpx @start
	pha		; save character row

	bcs :+
	txa
	ldx @start
	tay

:	lda #$00
	sta @odd

	; check whether the start column is even/odd
	tya
	lsr
	sta @start
	ror @odd

	; get the first column to reverse
	tay
	pla
	adc __bm_columnslo,y
	sta @dst
	lda __bm_columnshi,y
	adc #$00
	sta @dst+1

	lda @odd
	beq @cont

	; reverse half of the first column
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

@cont:	; divide character # by 2 to get bitmap column
	txa
	lsr
	tax

	; check if end column is even or odd
	lda #$00
	adc #$00
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

;******************************************************************************
; SHR
; Shifts the bitmap right 8 pixels (one "char") at the given row
; IN:
;  - .A: the row to shift
;  - .Y: the column to start shifting at
.export __bm_shr
.proc __bm_shr
@dst=zp::tmp0
@jumpaddr=@dst
@end=zp::tmp2
@odd=zp::tmp3
@ystart=zp::tmp4
	; get the start row
	asl
	asl
	asl
	pha
	tax

	adc #$08
	sta @end

	; get the offset to jump into (how many columns to skip)
	tya
	and #$01
	sta @odd

	; calculate jump address based on # of columns we need to skip
	tya
	lsr
	sta @dst
	sta @ystart
	adc @dst		; *3 (sizeof ror abs,x)
	adc #<@target
	sta @jumpaddr
	lda #>@target
	adc #$00
	sta @jumpaddr+1

; jump into the unrolled loop below at the calculated offset
; ROR BITMAP_ADDR+$c0
; ROR BITMAP_ADDR+$c0*2
; ROR BITMAP_ADDR+$c0*3
; ...
@l0:	ldy #$03		; 1 char width
@l1:	clc
	jmp (@jumpaddr)
@target=*
.repeat 20,i
	ror BITMAP_ADDR+($c0*(i)),x
.endrepeat
	dey
	bpl @l1
	inx
	cpx @end
	bne @l0

	pla			; restore character row
	tax			; transfer to .X (pixel row index)

	lda @odd		; is this an odd character?
	beq @done		; if not, we're done

	; if odd, shift the first column back
	ldy @ystart
	lda __bm_columnslo,y
	sta @first
	lda __bm_columnshi,y
	sta @first+1

@l2:	ldy #$03
@l3:
@first=*+1
	asl $f00d,x
	dey
	bpl @l3
	inx
	cpx @end
	bne @l2

@done:	rts
.endproc

;******************************************************************************
; SAVE
; Saves the bitmap to the backup buffer. It may then be restored with a call
; to bm::restore
.export __bm_save
.proc __bm_save
	copy #mem::backbuff, #BITMAP_ADDR, #(20*192)
	rts
.endproc

;******************************************************************************
; RESTORE
; Restores the bitmap from the backup buffer.
; You should call bm::save first with the buffer you want to restore
.export __bm_restore
.proc __bm_restore
	copy #BITMAP_ADDR, #mem::backbuff, #(20*192)
	rts
.endproc

;******************************************************************************
; CHAR ADDR
; Returns the bitmap address for the "character row" of the given row.
; Characters are 8 pixels tall, so this is BITMAP_ADDR+(8*row) where row is
; the provided row.
; IN:
;  - .A: the character row to get the bitmap address of
; OUT:
;  - .XY: the bitmap address
.export __bm_char_addr
.proc __bm_char_addr
	asl
	asl
	asl
	adc #<BITMAP_ADDR
	tax
	ldy #>BITMAP_ADDR
	rts
.endproc

.RODATA
;******************************************************************************
.export __bm_columns
__bm_columns:
.word $1100
.word $11c0
.word $1280
.word $1340
.word $1400
.word $14c0
.word $1580
.word $1640
.word $1700
.word $17c0
.word $1880
.word $1940
.word $1a00
.word $1ac0
.word $1b80
.word $1c40
.word $1d00
.word $1dc0
.word $1e80
.word $1f40

.linecont +
.define cols $1100, $11c0, $1280, $1340, $1400, $14c0, $1580, $1640, $1700, \
  $17c0, $1880, $1940, $1a00, $1ac0, $1b80, $1c40, $1d00, $1dc0, $1e80, $1f40
.linecont -

.export __bm_columnslo
.export __bm_columnshi
__bm_columnslo: .lobytes cols
__bm_columnshi: .hibytes cols

inittab: .byte $02,$fe,$fe,$eb,$00,$0c
