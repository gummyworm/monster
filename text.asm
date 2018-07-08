.include "asm.inc"
.include "bitmap.inc"
.include "cursor.inc"
.include "key.inc"
.include "irq.inc"
.include "macros.inc"
.include "memory.inc"
.include "source.inc"
.include "util.inc"
.include "zeropage.inc"

ESCAPE_CHARACTER = $ff
ESCAPE_RVS_ON = $01
ESCAPE_RVS_OFF = $02
STATUS_LINE = 23
STATUS_COL  = 0

.CODE

L_INSERT_MASK=$80
R_INSERT_MASK=$08

L_REPLACE_MASK=$f0
R_REPLACE_MASK=$0f

;--------------------------------------
; mask returnst the mask used to draw the cursor
.proc mask
	lda __text_insertmode
	beq @replace
@insert:
	lda zp::curx
	and #$01
	beq :+
	lda #R_INSERT_MASK
	rts
:	lda #L_INSERT_MASK
	rts
@replace:
	lda zp::curx
	and #$01
	beq :+
	lda #R_REPLACE_MASK
	rts
:	lda #L_REPLACE_MASK
	rts
.endproc

;--------------------------------------
; refresh the entire display
.export __text_refresh
.proc __text_refresh
	jsr src::get
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

	; current edit mode (insert or replace)
	ldx #'r'
	lda __text_insertmode
	beq :+
	ldx #'i'
:	stx mem::statusline+STATUS_COL+11

	; cursor position in file
	;jsr src::pos
	ldx #$ab
	ldy #$cd
	txa
	pha
	tya
	jsr util::hextostr
	sty mem::statusline+STATUS_COL+13
	stx mem::statusline+STATUS_COL+14
	pla
	jsr util::hextostr
	sty mem::statusline+STATUS_COL+15
	stx mem::statusline+STATUS_COL+16

	lda #'/'
	sta mem::statusline+STATUS_COL+17

	; file size
	;jsr src::size
	ldx #$ab
	ldy #$cd
	txa
	pha
	tya
	jsr util::hextostr
	sty mem::statusline+STATUS_COL+18
	stx mem::statusline+STATUS_COL+19
	pla
	jsr util::hextostr
	sty mem::statusline+STATUS_COL+20
	stx mem::statusline+STATUS_COL+21

	; filename
	ldx #39-23
:	lda src::name,x
	sta mem::statusline+23,x
	dex
	bpl :-

@blink: dec curtmr
curtmr=*+1
	lda #40
	bne @done
	jsr cur::toggle

	lda #40
	sta curtmr
@done:	rts
.endproc

;--------------------------------------
; clrline clears the text linebuffer.
.export __text_clrline
.proc __text_clrline
	lda #$00
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
	cmp #' '
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
	cmp #$14
	bne :+

	; backspace
	lda zp::curx
	beq @done	; cannot delete (cursor is at left side of screen)
	lda __text_insertmode
	beq @movex
	jsr __text_linelen
	stx zp::tmp0
	ldx zp::curx
	dex
@l0:	lda mem::linebuffer+1,x
	sta mem::linebuffer,x
	inx
	cpx zp::tmp0
	bcc @l0
@movex: ldx #-1
	ldy #0
	jsr cur::move
	jmp @redraw

:	pha
	jsr cur::off
	pla
	ldx zp::curx
	sta mem::linebuffer,x
@update:
	lda #0
	sta __text_colstart
	lda #40
	sta __text_len
	lda zp::curx
	cmp #40
	bcs @redraw
	ldx #1
	ldy #0
	jsr cur::move
@redraw:
	lda zp::cury
	jsr __text_drawline
@done:	rts
.endproc

;--------------------------------------
; drawline renders the text in mem::linebuffer at the cursor position
.export __text_drawline
.proc __text_drawline
	pha

	lda #40
	sta zp::tmp0
	ldx #<mem::spare
	ldy #>mem::spare
	lda #' '
	jsr util::memset

	ldx #$00
@l0:	lda mem::linebuffer,x
	bmi :+
	beq @done
	cmp #$0d
	beq @done
	cmp #' '
	bcc :+
	sta mem::spare,x
:	inx
	cpx #40
	bcc @l0

@done:	ldx #<mem::spare
	ldy #>mem::spare
	pla
        jsr __text_puts
	rts
.endproc

;--------------------------------------
; scroll scrolls all lines including and below row .A
.proc __text_scroll
.export __text_scroll
@rowstop=zp::tmp0
@src=zp::tmp1
@dst=zp::tmp3
	asl
	asl
	asl
	sta @rowstop

	ldxy #BITMAP_ADDR+8
	stx @dst
	sty @dst+1
	ldxy #BITMAP_ADDR
	stx @src
	sty @src+1

	ldx #19
@l0:	ldy #8*14
@l1:	lda #$00
	cpy #$08
	bcc :+
	lda (@src),y
:	sta (@dst),y
	dey
	cpy @rowstop
	bne @l1
	lda @src
	clc
	adc #$c0
	sta @src
	lda @src+1
	adc #$00
	sta @src+1
	lda @dst
	clc
	adc #$c0
	sta @dst
	lda @dst+1
	adc #$00
	sta @dst+1
	dex
	bpl @l0
	rts
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
; linelen returns the length of mem::linebuffer in .X
.proc __text_linelen
.export __text_linelen
	ldx #$ff
@l0:	inx
	lda mem::linebuffer,x
	beq @done
	cpx #40
	bcs @done
	bne @l0
@done:	rts
.endproc

;--------------------------------------
; get reads text (up to .A bytes) into (zp::tmp0)
.export __text_get
.proc __text_get
@len=zp::tmp0
	sta @len
@l0:    jsr key::getch
	cmp #$00
	beq @l0
	cmp #$0d
	bne :+
	rts
:	jsr __text_putch
	jmp @l0
.endproc

;--------------------------------------
; hioff clears any active line highlight
.export __text_hioff
.proc __text_hioff
	lda #$08
	sta hicolor
	rts
.endproc

.export __text_insertmode
__text_insertmode: .byte 1

;--------------------------------------
; savebuff stores the contents of linbuffer in spare memory.
.export __text_savebuff
.proc __text_savebuff
	ldy #39
:	lda mem::linebuffer,y
	sta mem::linebuffer2,y
	dey
	bpl :-
	rts
.endproc

;--------------------------------------
; restorebuff restores the linebuffer from the contents of spare memory.
.export __text_restorebuff
.proc __text_restorebuff
	ldy #39
:	lda mem::linebuffer2,y
	sta mem::linebuffer,y
	dey
	bpl :-
	rts
.endproc
