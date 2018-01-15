.include "asm.inc"
.include "bitmap.inc"
.include "memory.inc"
.include "irq.inc"
.include "util.inc"
.include "zeropage.inc"

ESCAPE_CHARACTER = $ff
ESCAPE_RVS_ON = $01
ESCAPE_RVS_OFF = $02
STATUS_LINE = 23
STATUS_COL  = 0

.CODE

;--------------------------------------
.export __text_refresh
.proc __text_refresh

.endproc

;--------------------------------------
.export __text_status
.proc __text_status
	ldx #<mem::statusline
	ldy #>mem::statusline
	lda #STATUS_LINE
	jsr __text_puts
	rts
.endproc

;--------------------------------------
; update updates the statusline according to the current cursor position.
.export __text_update
.proc __text_update
	lda zp::curx
	and #$f0
	lsr
	lsr
	lsr
	lsr
	tax
	cmp #1
	adc #'0'
	sta mem::statusline+STATUS_COL+1

	lda zp::curx
	and #$0f
	cmp #10
	adc mem::statusline+STATUS_COL+1
	sta mem::statusline+STATUS_COL+1

	; add current PC - "*=$XXXX"
	lda #'*'
	sta mem::statusline+STATUS_COL+3
	lda #'='
	sta mem::statusline+STATUS_COL+4
	lda #'$'
	sta mem::statusline+STATUS_COL+5

	lda asm::pc+1
	jsr util::hextostr
	sty mem::statusline+STATUS_COL+6
	stx mem::statusline+STATUS_COL+7
	lda asm::pc
	jsr util::hextostr
	sty mem::statusline+STATUS_COL+8
	stx mem::statusline+STATUS_COL+9

@blink: dec curtmr
curtmr=*+1
	lda #40
	bne @done
	jsr blinkcur

	lda #40
	sta curtmr
@done:	rts
.endproc

;--------------------------------------
blinkcur:
	lda zp::curx
	lsr
	lda #$f0
	bcc :+
	lda #$0f
:	sta @mask

	lda zp::cury
	asl
	asl
	asl
	sta @add

	lda zp::curx
	and #$fe
	tax
	lda bm::columns,x
@add=*+1
	adc #$00
	sta @dst1
	sta @dst2
	lda bm::columns+1,x
	adc #$00
	sta @dst1+1
	sta @dst2+1

	ldy #$07
@l0:
@dst1=*+1
	lda $ffff,y
@mask=*+1
	eor #$f0
@dst2=*+1
	sta $ffff,y
	dey
	bpl @l0
	rts

;--------------------------------------
; clrline clears the text linebuffer.
.export __text_clrline
.proc __text_clrline
	lda #' '
	ldx #39
:	sta mem::linebuffer,x
	dex
	bpl :-
	rts
.endproc

;--------------------------------------
; print displays the format string in (<X,>Y) at the row in .A.
.export __text_print
.proc __text_print
@str = zp::tmp0
@row = zp::tmp2
@savex = zp::tmp3
        stx @str
        sty @str+1
	sta @row

        ldx #$00
        ldy #$00
;copy the string (substituting escaped characters)
@l0:    lda (@str),y
        beq @disp
	cmp #$18	; TAB
	bne :+
	lda #' '
	sta mem::spare,x
	sta mem::spare+1,x
	sta mem::spare+2,x
	sta mem::spare+3,x
	inx
	inx
	inx
	inx
	jmp @cont

:	cmp #ESCAPE_CHARACTER
        bne @ch

;substitute escape character with string from stack
@esc:	stx @savex
	tsx
	lda $103,x
	sta @sub
	lda $104,x
	sta @sub+1
	ldx @savex

@sub=*+1
@l1:	lda $ffff
	beq @cont
	cmp #$20
	beq @cont
	sta mem::spare,x
	inc @sub
	bne :+
	inc @sub+1
:	inx
	bne @l1

@ch:    sta mem::spare,x
	inx

@cont:	iny
        jmp @l0

@disp:  lda #' '
:	sta mem::spare,x
	inx
	cpx #40
	bcc :-

	ldx #<mem::spare
	ldy #>mem::spare
	lda @row
        jsr __text_puts

        rts
.endproc

;--------------------------------------
; putch adds the character in .A to the current cursor position in the
; text linebuffer.
.export __text_putch
.proc __text_putch
	cmp #$0d
	bne @left
@return:
	ldx zp::curx
	lda #' '
	sta mem::linebuffer,x
	lda #0
	sta __text_colstart
	lda #40
	sta __text_len
	ldx #<mem::linebuffer
	ldy #>mem::linebuffer
	lda zp::cury
	jsr __text_puts

	lda #$00
	sta zp::curx
	inc zp::cury
	jsr __text_clrline
	ldx #<mem::linebuffer
	ldy #>mem::linebuffer
	lda zp::cury
	jsr __text_puts
	jmp @done

@left:	cmp #$9d
	bne @right
	ldx zp::curx
	beq :+
	dex
	stx zp::curx
:	jmp @redraw

@right: cmp #$1d
	bne @delete
	ldx zp::curx
	cpx #40
	bcs :+
	inx
	stx zp::curx
:	jmp @redraw

@delete:
	cmp #$14
	bne @printable
	dec zp::curx
	bpl :+
	inc zp::curx
	rts
:	lda #' '
	ldx zp::curx
	sta mem::linebuffer+1,x
	ldx #<mem::linebuffer
	ldy #>mem::linebuffer
	lda zp::cury
	jsr __text_puts
	jmp @done

@printable:
	ldx zp::curx
	sta mem::linebuffer,x
	lda #0
	sta __text_colstart
	lda #40
	sta __text_len
	lda zp::curx
	cmp #40
	bcs @redraw
	inc zp::curx

@redraw: 
	ldx #<mem::linebuffer
	ldy #>mem::linebuffer
	lda zp::cury
        jsr __text_puts

@done:	rts
.endproc

;--------------------------------------
.export __text_colstart
.export __text_len
.export __text_puts
__text_puts:
txtbyte  = $24
txtleft  = $25
txtright = $27
txtdst   = $29
txtsrc   = $4e
flags = $50
        stx txtsrc
        sty txtsrc+1
        asl
        asl
        asl
        sta txtdst
__text_colstart=*+1
        lda #$00
        asl
        tax
        lda bm::columns,x
        adc txtdst
        sta txtdst
        lda #$00
        adc bm::columns+1,x
        sta txtdst+1

        ldy #$00
l0:     lda (txtsrc),y
        iny
        sta txtleft
        lda #$00
        asl txtleft
        rol
        asl txtleft
        rol
        asl txtleft
        rol
        sta txtleft+1
        clc
        lda txtleft
        adc #<((__text_charmap-256) .mod 256)
        sta txtleft
        lda txtleft+1
        adc #<((__text_charmap-256) / 256)
        sta txtleft+1
        lda (txtsrc),y
        iny
        sta txtright
        lda #$00
        asl txtright
        rol
        asl txtright
        rol
        asl txtright
        rol
        sta txtright+1
        clc
        lda txtright
        adc #<((__text_charmap-256) .mod 256)
        sta txtright
        lda txtright+1
        adc #<((__text_charmap-256) / 256)
        sta txtright+1
        tya
        pha
        ldy #$00
l1:     lda (txtleft),y
        and #$f0
        sta txtbyte
        lda (txtright),y
        and #$0f
        ora txtbyte
        sta (txtdst),y
        iny
        cpy #8
        bne l1 
        pla
        tay
        clc
        lda txtdst
        adc #192
        sta txtdst
        lda txtdst+1
        adc #0
        sta txtdst+1
__text_len=*+1
        cpy #40
        bne l0
        rts

.export __text_charmap
__text_charmap:
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
.byte   $44,$44,$44,$44,$44,$44,$44,$44        ; |
.byte   $ff,$00,$00,$00,$00,$00,$00,$ff
.byte   $88,$88,$88,$88,$88,$88,$88,$88
.byte   $ff,$00,$00,$00,$00,$00,$00,$00
.byte   $ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff	; cursor

;--------------------------------------
; hiline highlights the row in .A with the color in .X
.export __text_hiline
.proc __text_hiline
	stx hicolor
	ldx #<hiirq
	ldy #>hiirq
	asl
	asl
	adc #13
	jsr irq::raster
	rts
.endproc

hiirq: ldx #65/5-1
	dex
	bne *-1
hicolor=*+1
	lda #$00
	sta $900f
	ldx #(65)/5*8-3
:	dex
	bne :-
	nop
	lda #$08
	sta $900f
	jmp $eabf

;--------------------------------------
; hioff clears any active line highlight
.export __text_hioff
.proc __text_hioff
	lda #$08
	sta hicolor
	rts
.endproc

curx: .byte 0
cury: .byte 0
