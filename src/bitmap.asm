.include "config.inc"
.include "macros.inc"
.include "memory.inc"
.include "util.inc"
.include "zeropage.inc"

;******************************************************************************
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
        adc @inittab,y
        sta $9000,y
        dey
        bpl @2

	lda #(BG_COLOR<<4 | BORDER_COLOR)
	sta $900f
        rts
@inittab: .byte $02,$fe,$fe,$eb,$00,$0c
.endproc

;******************************************************************************
; CLR
; Clears the bitmap and sets the color memory to TEXT_COLOR
.export __bm_clr
.proc __bm_clr
        lda #>BITMAP_ADDR
        sta zp::tmp0+1
        lda #$00
        sta zp::tmp0
        ldx #$0f
        tay

;clear the character memory (bitmap)
@l0:    sta (zp::tmp0),y
        dey
        bne @l0
        inc zp::tmp0+1
        dex
        bne @l0
	lda #TEXT_COLOR

;clear the color memory
@l1:    sta COLMEM_ADDR,y
        dey
        bne @l1
        rts
.endproc

;******************************************************************************
; RVSLINE
; Reverses 1 character (8 pixels high) in the given row
; IN:
;  - .A: the text row to reverse (pixel number / 8)
.export __bm_rvsline
.proc __bm_rvsline
@dst=zp::tmp0
	asl
	asl
	asl
	adc #<BITMAP_ADDR
	sta @dst
	lda #>BITMAP_ADDR
	sta @dst+1
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
	lda @dst+1
	adc #$00
	sta @dst+1
	dex
	bne @l0
	rts
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

.DATA
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
