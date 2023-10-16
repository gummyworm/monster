.include "bitmap.inc"
.include "macros.inc"
.include "zeropage.inc"

.segment "FASTTEXT"

;******************************************************************************
; FAST PUTCH
; Puts the character given at the current cursor position
; IN:
;  - .A: the character to plot
.export __ftxt_putch
.proc __ftxt_putch
@src=zp::text
@mask=zp::text+2
@dst=zp::text+4
@clrmask=zp::text+6
@dsttmp=zp::text+7
@char=zp::text+7
	lda @char
	jsr get_char_addr	; zp::text contains char address

	; get destination
	lda zp::cury
	asl
	asl
	asl
	sta @dst

	lda zp::curx
	lsr
	asl
	tax
	lda bmcolumns,x
	clc
	adc @dst
	sta @dst
	lda bmcolumns+1,x
	adc #$00
	sta @dst+1

	lda zp::curx
	and #$01
	bne @right

@left:	lda #$f0
	skw
@right:	lda #$0f
	sta @mask
	eor #$ff
	sta @clrmask
	ldy #$07

@blit:	; clear the pixels we're going to overwrite with the new half-char
	lda @clrmask
	and (@dst),y
	sta @dsttmp

	; write the new half-char
	lda @mask
	and (@src),y
	ora @dsttmp
	sta (@dst),y
	dey
	bpl @blit

@updatecur:
	inc zp::curx

@done:	clc	; "put" was successful
	rts
.endproc

;******************************************************************************
; PUTS
; Displays the given string at the given row.  Regardless of the contents of
; the string, text::len characters are displayed (including 0's etc.)
; IN:
;  - .XY: the string to display
;  - .A: the text to display
.export __ftxt_puts
.proc __ftxt_puts
@txtbyte  = zp::text
@txtleft  = zp::text+1
@txtright = zp::text+3
@txtdst   = zp::text+5
@txtsrc   = zp::text+7
@ysave	  = zp::text+9
        lda #<BITMAP_ADDR
	clc
        adc @txtdst
        sta @txtdst
	lda #>BITMAP_ADDR
        sta @txtdst+1

	ldy #$00
@l0:    lda (@txtsrc),y
	iny
	tax
	lda charaddrlo-32,x
	sta @txtleft
	lda charaddrhi-32,x
	sta @txtleft+1

@right:	lda (@txtsrc),y
        iny
	tax
	lda charaddrlo-32,x
	sta @txtright
	lda charaddrhi-32,x
	sta @txtright+1

	sty @ysave

        ldy #8-1
@l1:    lda (@txtleft),y
        and #$f0
        sta @txtbyte
        lda (@txtright),y
        and #$0f
        ora @txtbyte
        sta (@txtdst),y
	dey
	bpl @l1

	ldy @ysave
        lda @txtdst
        clc
        adc #192
        sta @txtdst
	bcc @nextch
	inc @txtdst+1
@nextch:
	cpy #40
	bcc @l0
        rts
.endproc

;******************************************************************************
; GET_CHAR_ADDR
; Returns the address of the character in .A
; IN:
;  - .A: the character to get the address of
; OUT:
;  zp::text: the address of the character
.proc get_char_addr
@ch=zp::text
	tax
	lda charaddrlo-32,x
	sta @ch
	lda charaddrhi-32,x
	sta @ch+1
	rts
.endproc

;******************************************************************************
; CHARMAP
; This is the 40 column character set for the text routines
charmap:
.byte   0,   0,   0,   0,   0,   0,   0,   0
.byte   0,  34,  34,  34,  34,   0,  34,   0
.byte   0,  85,  85,   0,   0,   0,   0,   0
.byte   0,  85, 119,  85,  85, 119,  85,   0
.byte   0,  34,  51, 102,  51, 102,  34,   0
.byte   0,  85,  17,  34,  34,  68,  85,   0
.byte   0, 102, 102,  51, 102, 102,  51,   0
.byte  34,  34,   0,   0,   0,   0,   0,   0
.byte   0,  17,  34,  34,  34,  34,  17,   0
.byte   0,  68,  34,  34,  34,  34,  68,   0
.byte   0,   0,   0,  85,  34,  85,   0,   0
.byte   0,   0,   0,  34, 119,  34,   0,   0
.byte   0,   0,   0,   0,   0,   0,  34,  68
.byte   0,   0,   0,   0, 119,   0,   0,   0
.byte   0,   0,   0,   0,   0,   0,  34,   0
.byte   0,  17,  17,  34,  34,  68,  68,   0
.byte   0, 119,  85,  85,  85,  85, 119,   0
.byte   0,  17,  17,  17,  17,  17,  17,   0
.byte   0, 119,  17, 119,  68,  68, 119,   0
.byte   0, 119,  17, 119,  17,  17, 119,   0
.byte   0,  85,  85, 119,  17,  17,  17,   0
.byte   0, 119,  68, 119,  17,  17, 119,   0
.byte   0, 119,  68, 119,  85,  85, 119,   0
.byte   0, 119,  17,  17,  17,  17,  17,   0
.byte   0, 119,  85, 119,  85,  85, 119,   0
.byte   0, 119,  85, 119,  17,  17, 119,   0
.byte   0,   0,   0,  34,   0,   0,  34,   0
.byte   0,   0,   0,  34,   0,   0,  34,  68
.byte   0,   0,  17,  34,  68,  34,  17,   0
.byte   0,   0,   0, 119,   0, 119,   0,   0
.byte   0,   0,  68,  34,  17,  34,  68,   0
.byte   0,  34,  85,  17,  34,   0,  34,   0
.byte   0,  51,  85,  85,  85,  68,  51,   0
.byte   0,  34,  85,  85, 119,  85,  85,   0
.byte   0, 102,  85, 102,  85,  85, 102,   0
.byte   0,  51,  68,  68,  68,  68,  51,   0
.byte   0, 102,  85,  85,  85,  85, 102,   0
.byte   0, 119,  68, 102,  68,  68, 119,   0
.byte   0, 119,  68, 102,  68,  68,  68,   0
.byte   0,  51,  68,  68,  85,  85,  51,   0
.byte   0,  85,  85, 119,  85,  85,  85,   0
.byte   0, 119,  34,  34,  34,  34, 119,   0
.byte   0,  51,  17,  17,  17,  85,  34,   0
.byte   0,  85,  85, 102,  85,  85,  85,   0
.byte   0,  68,  68,  68,  68,  68, 119,   0
.byte   0,  85, 119,  85,  85,  85,  85,   0
.byte   0, 102,  85,  85,  85,  85,  85,   0
.byte   0,  34,  85,  85,  85,  85,  34,   0
.byte   0, 102,  85,  85, 102,  68,  68,   0
.byte   0,  34,  85,  85,  85, 102,  51,   0
.byte   0, 102,  85,  85, 102,  85,  85,   0
.byte   0,  51,  68,  34,  17,  17, 102,   0
.byte   0, 119,  34,  34,  34,  34,  34,   0
.byte   0,  85,  85,  85,  85,  85,  51,   0
.byte   0,  85,  85,  85,  85,  34,  34,   0
.byte   0,  85,  85,  85,  85, 119,  85,   0
.byte   0,  85,  85,  34,  85,  85,  85,   0
.byte   0,  85,  85,  85,  34,  34,  34,   0
.byte   0, 119,  17,  34,  34,  68, 119,   0
.byte   0,  51,  34,  34,  34,  34,  51,   0
.byte   0,  68,  68,  34,  34,  17,  17,   0
.byte   0, 102,  34,  34,  34,  34, 102,   0
.byte  34,  85,   0,   0,   0,   0,   0,   0
.byte   0,   0,   0,   0,   0,   0,   0, 255
.byte  34,  17,   0,   0,   0,   0,   0,   0
.byte   0,   0,   0,  51,  85,  85,  51,   0
.byte   0,  68,  68, 102,  85,  85, 102,   0
.byte   0,   0,   0,  51,  68,  68,  51,   0
.byte   0,  17,  17,  51,  85,  85,  51,   0
.byte   0,   0,   0,  34,  85, 102,  51,   0
.byte   0,  17,  34, 119,  34,  34,  34,   0
.byte   0,   0,   0,  51,  85,  51,  17, 102
.byte   0,  68,  68, 102,  85,  85,  85,   0
.byte   0,  34,   0,  34,  34,  34,  34,   0
.byte   0,  34,   0,  34,  34,  34,  34,  68
.byte   0,  68,  68,  85, 102,  85,  85,   0
.byte   0,  34,  34,  34,  34,  34,  34,   0
.byte   0,   0,   0,  85, 119,  85,  85,   0
.byte   0,   0,   0, 102,  85,  85,  85,   0
.byte   0,   0,   0,  34,  85,  85,  34,   0
.byte   0,   0,   0, 102,  85, 102,  68,  68
.byte   0,   0,   0,  51,  85,  51,  17,  17
.byte   0,   0,   0, 102,  85,  68,  68,   0
.byte   0,   0,   0,  51, 102,  51, 102,   0
.byte   0,  34,  34, 119,  34,  34,  51,   0
.byte   0,   0,   0,  85,  85,  85,  51,   0
.byte   0,   0,   0,  85,  85,  34,  34,   0
.byte   0,   0,   0,  85,  85, 119,  85,   0
.byte   0,   0,   0,  85,  34,  34,  85,   0
.byte   0,   0,   0,  85,  85,  51,  17, 102
.byte   0,   0,   0, 119,  17,  34, 119,   0
.byte   0,  51,  34,  68,  34,  34,  51,   0
.byte   0,  34,  34,   0,  34,  34,  34,   0
.byte   0, 102,  34,  17,  34,  34, 102,   0
.byte   0,   0,   0,  85, 170,   0,   0,   0
.byte   0,   0,   0,   0,   0,   0,   0,   0

;CUSTOM CHARS. starting @ 128
.byte   $44,$44,$44,$44,$44,$44,$44,$44   ; 128  |
.byte   $ff,$00,$00,$00,$00,$00,$00,$ff	  ; 129
.byte   $88,$88,$88,$88,$88,$88,$88,$88	  ; 130
.byte   $ff,$00,$00,$00,$00,$00,$00,$00	  ; 131
.byte   $ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff	  ; 132 cursor
.byte   $00,$00,$22,$77,$77,$22,$00,$00	  ; 133 bullet/breakpoint symbol
.byte   $00,$00,$22,$55,$55,$22,$00,$00	  ; 134 unfilled bullet/breakpoint unset
.byte   $00,$44,$66,$77,$77,$66,$44,$00	  ; 135 arrow pointing right
num_chars = (*-charmap)/8

;******************************************************************************
charaddrlo:
.repeat  num_chars, i
	.byte <((charmap)+(i*8))
.endrepeat

charaddrhi:
.repeat num_chars, i
	.byte >((charmap)+(i*8))
.endrepeat

;******************************************************************************
bmcolumns:
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
