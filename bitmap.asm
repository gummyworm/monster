.include "zeropage.inc"
BITMAP_ADDR = $1100
COLMEM_ADDR = $9400

.CODE

;--------------------------------------
;init
;MINIGRAFIK VIC/memory initialization
;code by Mike
;
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

	lda #$08
	sta $900f
        rts
@inittab: .byte $02,$fe,$fe,$eb,$00,$0c

.endproc

;--------------------------------------
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
	lda #$07
;clear the color memory
@l1:    sta COLMEM_ADDR,y
        dey
        bne @l1

        rts
.endproc

;--------------------------------------
.export __bm_clrpixel 
.proc __bm_clrpixel
        txa
        and #$07
        sta @xtab_off
        txa
        lsr
        tax
        lda __bm_columns,x
        sta zp::tmp0
        lda __bm_columns+1,x
        sta zp::tmp0+1
@xtab_off=*+1
        lda pixel_table
        and (zp::tmp0),y
        sta (zp::tmp0),y
        rts
.endproc

;--------------------------------------
.export __bm_setpixel 
.proc __bm_setpixel
        txa
        and #$07
        sta @xtab_off
        txa
        lsr
        lsr
        lsr
        tax
        lda __bm_columns,x
        sta zp::tmp0+1
        lda __bm_columns+1,x
        sta zp::tmp0
@xtab_off=*+1
        lda pixel_table
        ora (zp::tmp0),y
        sta (zp::tmp0),y
        rts
.endproc

;--------------------------------------
.export __bm_togglepixel
.proc __bm_togglepixel
        txa
        and #$07
        sta @xtab_off
        lda pixel_table,x
        pha
        txa
        lsr
        lda __bm_columns,x
        sta zp::tmp0
        lda __bm_columns+1,x
        sta zp::tmp0+1
@xtab_off=*+1
        lda pixel_table
        eor (zp::tmp0),y
        sta (zp::tmp0),y
        rts
.endproc

;--------------------------------------
.export __bm_line
.proc __bm_line
@x0 = zp::arg0
@y0 = zp::arg1
@x1 = zp::arg2
@y1 = zp::arg3
@dx = zp::tmp0
@dy = zp::tmp1
;-
@d   = zp::tmp0
        lda @x1
        sec
        sbc @x0
        sta @dx
        lda @y1
        sec 
        sbc @y0
        sta @dy
@2dy = *+1
        adc #$00
.endproc

;--------------------------------------
;these tables are aligned so that it can be effeciently accessed
;TODO: lots of free room between them
.align 256
pixel_table:
.byte $80,$40,$20,$10,$08,$04,$02,$01
.align 256
pixel_table_inverted:
.byte $7f,$bf,$df,$ef,$f7,$fb,$fd,$fe

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
