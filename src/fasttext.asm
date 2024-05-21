.include "bitmap.inc"
.include "macros.inc"
.include "zeropage.inc"

ESCAPE_8x8UDG    = $f9

.segment "FASTTEXT"

;******************************************************************************
txtbyte = zp::text
txtsrc  = zp::text+7
txtdst  = zp::text+5	; bitmap address to render graphics at

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
	lda bmcolumnslo,x
	clc
	adc @dst
	sta @dst
	lda bmcolumnshi,x
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

@done:	clc	; "put" was successful
	rts
.endproc


;******************************************************************************
; PUTS
; Displays the given string at the given row.  Regardless of the contents of
; the string, text::len characters are displayed (including 0's etc.)
; IN:
;  - zp::text+5: dest - the offset from the bitmap base to display the string at
;  - zp::text+7: source - the address of the text string to display
.export __ftxt_puts
.proc __ftxt_puts
        lda #<BITMAP_ADDR
	clc
        adc txtdst
        sta txtdst
	lda #>BITMAP_ADDR
        sta txtdst+1

	ldy #$ff
@l0:	iny
	lda (txtsrc),y
	bpl :+
	; if negative, this is a special character, handle it
	jsr drawspecial

:	tax
	; get the left (even) character's bitmap data for this bitmap column
	lda charaddrlo-32,x
	sta txtleft
	lda charaddrhi-32,x
	sta txtleft+1

	; get the right (odd) character's bitmap data
@right:	iny
	lda (txtsrc),y
	bpl :+
	jsr drawspecial

:	tax
	lda charaddrlo-32,x
	sta txtright
	lda charaddrhi-32,x
	sta txtright+1

	; draw the character
	jsr drawcell

	; move to next bitmap column
        lda txtdst
        clc
        adc #192
        sta txtdst
	bcc @nextch
	inc txtdst+1
@nextch:
	cpy #40		; have we drawn the full line?
	bcc @l0		; continue until we have
        rts
.endproc

;******************************************************************************
; DRAWCELL
; Draws the graphics in (txtleft) | (textright) at (txtdst)
; IN:
;  - txtleft:  the address of the graphics to draw on the left half of the cell
;  - txtright: the address of the graphics to draw to the right half of the cell
;  - txtdst:   the bitmap address to render the cell at
drawcell:
ysave = zp::text+9
	sty ysave
	; OR the even character's data with the odd's and draw the full cell
        ldy #8-1
txtleft=*+1
	lda $f00d,y
        and #$f0
        sta txtbyte
txtright=*+1
	lda $f00d,y
        and #$0f
        ora txtbyte
        sta (txtdst),y
	dey
	bpl txtleft-1
	ldy ysave	; restore .Y
	rts

;******************************************************************************
; DRAWSPECIAL
; Handles special drawing character codes and draws them.
; If the code is not recognized, does nothing
.proc drawspecial
	cmp #ESCAPE_8x8UDG
	bne @done
	;jmp draw_8x8

@done:	rts
.endproc

;******************************************************************************
; DRAW a literal 8x8 graphic
; The format of this is:
;  $fe $xx, $xx, $xx, $xx, $xx, $xx, $xx, $xx
; OUT:
;  - txtleft: the 4
.proc draw_8x8
@odd=zp::text+9
@left=r0	; 8 bytes
@right=r8	; 8 bytes
	; if .Y is odd, we're halfway through an 8x8 cell
	tya
	pha
	and #$01
	sta @odd

@l0:	lda (txtsrc),y
	ldx @odd
	bne @do_odd

	; if even, draw the entire cell
	lda txtsrc
	sta txtleft
	lda txtsrc+1
	sta txtleft+1
	lda #<charmap		; all 0's
	sta txtright
	lda #>charmap		; all 0's
	sta txtright+1
	bne @render		; branch always

@do_odd:
	; if odd, populate txtright and setup txtleft for the next iteration
	; of the draw loop
	ldy #$07
	ldx #$07
:	lda (txtsrc),y
	lsr
	ror @left,x
	lsr
	ror @left,x
	lsr
	ror @left,x
	lsr
	ror @left,x
	sta @right,x
	dey
	dex
	bpl :-

	lda #$00
	sta txtright+1
	sta txtleft+1
	lda #@right
	sta txtright
	lda #@left
	sta txtleft
	jsr @render

@render:
	jsr drawcell		; draw (txtleft) | (txtright)

	; move source pointer beyond the 8 bytes of graphic data
@done:	lda txtsrc
	clc
	adc #$08
	sta txtsrc
	bcc :+
	inc txtsrc+1

:	pla			; restore current char offset to .Y
	tay
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
;.byte   0, 102, 102,  51, 102, 102,  51,   0
.byte  $00,$22,$33,$44,$77,$44,$33,$00		; &
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

.segment "SETUP"
;******************************************************************************
; GEN_CHAR_ADDRS
; Generates the charaddrlo and charaddrhi tables
.export __ftxt_init
.proc __ftxt_init
@addr=r0
	ldxy #charmap
	stxy @addr

	ldx #$00
@l0:	lda @addr
	sta charaddrlo,x
	lda @addr+1
	sta charaddrhi,x

	lda @addr
	clc
	adc #$08
	sta @addr
	bcc :+
	inc @addr+1

:	inx
	cpx #num_chars
	bne @l0

	rts
.endproc

;******************************************************************************
.linecont +
.define cols $1100, $11c0, $1280, $1340, $1400, $14c0, $1580, $1640, $1700, \
  $17c0, $1880, $1940, $1a00, $1ac0, $1b80, $1c40, $1d00, $1dc0, $1e80, $1f40
.linecont -
bmcolumnslo: .lobytes cols
bmcolumnshi: .hibytes cols

;******************************************************************************
.segment "FASTTEXT_BSS"
charaddrlo: .res num_chars
;.repeat  num_chars, i
;	.byte <((charmap)+(i*8))
;.endrepeat

charaddrhi: .res num_chars
;.repeat num_chars, i
;	.byte >((charmap)+(i*8))
;.endrepeat
