.include "asm.inc"
.include "bitmap.inc"
.include "config.inc"
.include "cursor.inc"
.include "draw.inc"
.include "key.inc"
.include "irq.inc"
.include "macros.inc"
.include "memory.inc"
.include "source.inc"
.include "util.inc"
.include "zeropage.inc"
.CODE

ESCAPE_STRING = $ff
ESCAPE_VALUE = $fe
ESCAPE_RVS_ON = $01
ESCAPE_RVS_OFF = $02
STATUS_LINE = 23
STATUS_COL  = 0

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
	jsr __text_putz
	lda #STATUS_LINE
	jmp bm::rvsline
.endproc

;--------------------------------------
.proc draw_statusline
COLUMN_START=STATUS_COL+3
LINE_START=STATUS_COL+6
SIZE_START=STATUS_COL+13
MODE_START=STATUS_COL
	ldy #$00
	ldx zp::curx
	jsr util::todec

	lda mem::spare+3
	sta mem::statusline+COLUMN_START
	lda mem::spare+4
	sta mem::statusline+COLUMN_START+1
	lda #','
	sta mem::statusline+COLUMN_START+2

	ldxy src::line
	jsr util::todec
	ldx #4
:	lda mem::spare,x
	sta mem::statusline+LINE_START,x
	dex
	bpl :-

	; add current PC - "*=$XXXX"
	lda #'['
	sta mem::statusline+SIZE_START
	lda #'*'
	sta mem::statusline+SIZE_START+1
	lda #'='
	sta mem::statusline+SIZE_START+2
	lda #'$'
	sta mem::statusline+SIZE_START+3

	lda zp::asmresult
	sec
	sbc #<mem::program
	php
	jsr util::hextostr
	sty mem::statusline+SIZE_START+6
	stx mem::statusline+SIZE_START+7
	lda zp::asmresult+1
	plp
	sbc #>mem::program
	jsr util::hextostr
	sty mem::statusline+SIZE_START+4
	stx mem::statusline+SIZE_START+5
	lda #']'
	sta mem::statusline+SIZE_START+8

	; current edit mode (insert or replace)
	ldx #'r'
	lda __text_insertmode
	beq :+
	ldx #'i'
:	stx mem::statusline+MODE_START

	; filename
	ldxy #src::name
	jsr util::strlen
	tay
	ldx #39
:	lda src::name,y
	sta mem::statusline,x
	dex
	dey
	bpl :-
	rts
.endproc

;--------------------------------------
; update updates the statusline according to the current cursor position.
.export __text_update
.proc __text_update
	jsr draw_statusline

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
	lda #0
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
@savey = zp::tmp4
@ret = zp::tmp5
@buff = @printbuffer
        stx @str
        sty @str+1
	sta @row

	pla
	sta @ret
	pla
	sta @ret+1

        ldx #$00
        ldy #$00
;copy the string (substituting escaped characters)
@l0:    lda (@str),y
	bne :+
	jmp @disp

:	cmp #$18	; TAB
	bne :+
	lda #' '
	sta @buff,x
	sta @buff+1,x
	sta @buff+2,x
	sta @buff+3,x
	inx
	inx
	inx
	inx
	jmp @cont

:	cmp #ESCAPE_STRING
	beq @string
	cmp #ESCAPE_VALUE
        bne @ch

;substitute escape character with value from stack
@value:
	stx @savex
	sty @savey

	pla
	jsr util::hextostr
	txa
	ldx @savex
	sta @buff+1,x
	tya
	sta @buff,x

	pla
	jsr util::hextostr
	txa
	ldx @savex
	sta @buff+3,x
	tya
	sta @buff+2,x

	inx
	inx
	inx
	inx
	ldy @savey
	jmp @cont

;substitute escape character with string from stack
@string:
	pla
	sta @sub
	pla
	sta @sub+1
@sub=*+1
@l1:	lda $ffff
	beq @cont
	cmp #' '
	beq @cont
	cmp #$0d
	beq @cont
	sta @buff,x
	inc @sub
	bne :+
	inc @sub+1
:	inx
	cpx #40
	bcs @disp
	bne @l1

@ch:
	sta @buff,x
	inx

@cont:
	iny
        jmp @l0

@disp:
	lda #' '
:	sta @buff,x
	inx
	cpx #40
	bcc :-

	lda @ret+1
	pha
	lda @ret
	pha

	ldx #<@buff
	ldy #>@buff
	lda @row
	jmp __text_puts
@printbuffer: .res 40
.endproc

;--------------------------------------
; putch adds the character in .A to the current cursor position in the
; text linebuffer.
; Returns:
;  .C: set if character was unsuccessfully put
.export __text_putch
.proc __text_putch
	cmp #$14
	bne @printing

	; backspace
	sec
	lda zp::curx
	beq @done	; cannot delete (cursor is at left side of screen)
	cmp cur::minx
	bcc @done	; cursor is limited
	beq @done
	lda __text_insertmode
	beq @moveback
@shift_left:
	jsr __text_linelen
	stx zp::tmp0
	ldx zp::curx
	dex
@l0:	lda mem::linebuffer+1,x
	sta mem::linebuffer,x
	inx
	cpx zp::tmp0
	bcc @l0
@moveback:
	ldx #$ff
	ldy #0
	jsr cur::move
	lda zp::cury
	jsr __text_drawline
	clc	; "put" was successful
@done:
	rts

@printing:
	pha
	lda __text_insertmode
	beq @fastput

@slowput:
@shift_right:
	jsr __text_linelen
	cpx zp::curx
	beq @fastput
	lda #$00
	sta mem::linebuffer+2,x
@shr:	lda mem::linebuffer,x
	sta mem::linebuffer+1,x
	cpx zp::curx
	beq :+
	dex
	bpl @shr
:	jsr cur::off
	pla
	ldx zp::curx
	sta mem::linebuffer,x
	ldxy #mem::linebuffer
	lda zp::cury
	jsr __text_print
	jmp @updatecur

@fastput:
	jsr cur::off
	ldx zp::curx
	lda #$00
	sta mem::linebuffer+1,x
	pla
	sta mem::linebuffer,x
	cmp #$0d
	bne :+
	rts

:	jsr get_char_addr	; zp::tmp0 contains char address

	; get destination
	lda zp::cury
	asl
	asl
	asl
	sta zp::tmp2

	lda zp::curx
	lsr
	asl
	tax
	lda bm::columns,x
	clc
	adc zp::tmp2
	sta zp::tmp2
	lda bm::columns+1,x
	adc #$00
	sta zp::tmp2+1

	lda zp::curx
	and #$01
	bne @right
@left:
	lda #$f0
	.byte $2c
@right:
	lda #$0f
	sta @mask
	ldy #$07
@blit:
@mask=*+1
	lda #$00
	and (zp::tmp0),y
	ora (zp::tmp2),y
	sta (zp::tmp2),y
	dey
	bpl @blit
@updatecur:
	ldx #1
	ldy #0
	jsr cur::move
	clc	; "put" was successful
	rts
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
	jmp __text_puts
.endproc

;--------------------------------------
; scroll scrolls all lines from .X to .A up
.proc __text_scrollup
.export __text_scrollup
@src=zp::tmp1
@dst=zp::tmp3
@startline=zp::tmp5
@stopline=zp::tmp6
@numrows=zp::tmp7
	stx @numrows
	pha
	sec
	sbc @numrows
	asl
	asl
	asl
	sta @numrows
	pla

	asl
	asl
	asl
	sta @stopline

	txa
	asl
	asl
	asl
	sta @startline

	lda #<BITMAP_ADDR
	adc @startline
	sta @dst
	lda #>BITMAP_ADDR
	adc #$00
	sta @dst+1

	lda @dst
	adc #8
	sta @src
	lda @dst+1
	adc #$00
	sta @src+1

	ldx #19
@l0:	ldy #$00
@l1:    lda #$00
	cpy #8
	bcc :+
	lda (@src),y
:	sta (@dst),y
	iny
	cpy @numrows
	bcc @l1

	lda @src
	clc
	adc #$c0
	sta @src
	bcc :+
	inc @src+1
:	lda @dst
	clc
	adc #$c0
	sta @dst
	bcc :+
	inc @dst+1

:	dex
	bpl @l0
	rts
.endproc

;--------------------------------------
.export __text_scrolldown
; scrolls all rows from .A to .X
.proc __text_scrolldown
@rowstart=zp::tmp0
@rowstop=zp::tmp2
@rows=zp::tmp2
@src=zp::tmp3
@dst=zp::tmp5
	sta @rowstart
	stx @rowstop

	lda @rowstop
	sec
	sbc @rowstart
	asl
	asl
	asl
	sta @rows

	lda @rowstart
	asl
	asl
	asl
	sta @src
	adc #$08
	sta @dst

	lda #$11
	sta @src+1
	sta @dst+1

@l0:
	ldy @rows
@l1:	lda (@src),y
	sta (@dst),y
	dey
	bne @l1
	lda (@src),y
	sta (@dst),y

	lda @src
	clc
	adc #$c0
	sta @src
	bcc :+
	inc @src+1

:	lda @dst
	clc
	adc #$c0
	sta @dst
	bcc :+
	inc @dst+1
:	lda @dst+1
	cmp #$20
	bcc @l0
	rts
.endproc

;--------------------------------------
.export __text_putz
.proc __text_putz
; prints a full width (40 column) string, terminated by a 0.
;
@src = $24
	pha
	stxy @src

	ldy #39
	lda #' '
:	sta mem::spare,y
	dey
	bpl :-

	iny
@l0:
	lda (@src),y
	beq @done
	sta mem::spare,y
	iny
	cpy #40
	bcc @l0
@done:
	ldxy #mem::spare
	pla
	jmp __text_puts
.endproc

;--------------------------------------
.export __text_len
.export __text_puts
__text_puts:
txtbyte  = $24
txtleft  = $25
txtright = $27
txtdst   = $29
txtsrc   = $4e
        stx txtsrc
        sty txtsrc+1
        asl
        asl
        asl
        sta txtdst

	lda #40
	sta __text_len
        lda #<BITMAP_ADDR
        adc txtdst
        sta txtdst
        lda #$00
	sta rvs
        adc #>BITMAP_ADDR
        sta txtdst+1

        ldy #$00

l0:     lda (txtsrc),y
	jsr handlecc
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

@right:
        lda (txtsrc),y
	jsr handlecc
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
	eor rvs
        and #$f0
        sta txtbyte
        lda (txtright),y
	eor rvs
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
nextch:
__text_len=*+1
        cpy #40
	bcc l0
        rts

.proc handlecc
	cmp #$12	; RVS on?
	bne :+
	lda #$ff
	bne @setrvs
:	cmp #$92	; RVS off
	bne @done
	lda #$00
@setrvs:
	sta rvs
	iny
	pla
	pla
	inc __text_len
	jmp nextch
@done:  rts
.endproc
rvs: .byte 0

;--------------------------------------
; get_char_addr returns the address of the character in .A
.proc get_char_addr
@ch=zp::tmp0
	sta @ch
        lda #$00
	asl @ch
        rol
	asl @ch
        rol
	asl @ch
        rol
	sta @ch+1
        clc
	lda @ch
        adc #<((__text_charmap-256) .mod 256)
	sta @ch
	lda @ch+1
        adc #<((__text_charmap-256) / 256)
	sta @ch+1
	rts
.endproc

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
	lda #$08 | (BG_COLOR<<4) | BORDER_COLOR
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
	lda #$00
	jsr __text_putch
	rts
:	jsr __text_putch
	jmp @l0
.endproc

;--------------------------------------
; hioff clears any active line highlight
.export __text_hioff
.proc __text_hioff
	lda #$08 | (BG_COLOR<<4) | BORDER_COLOR
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

;--------------------------------------
; dir lists the directory of the attached disk
.export __text_dir
.proc __text_dir
@line=zp::tmp8
	jsr src::loaddir

	ldxy #mem::spare+2
	stxy @line

	pushcur
	ldy #1
	ldx #0
	jsr cur::set

@l0:
	jsr __text_clrline


	incw @line	; skip line #
	incw @line
	ldx #$00

@fname: ; print filename
@l2:	ldy #$00
	lda (@line),y
	incw @line
	tay
	beq @next
	sta mem::linebuffer,x
	inx
	cpx #39
	bcc @l2

@next:
	; read line link
	ldy #$00
	lda (@line),y
	bne :+
	iny
	lda (@line),y
	beq @done
:	incw @line
	incw @line
	beq @done

	ldxy #mem::linebuffer
	lda zp::cury
	jsr __text_print

	ldy zp::cury
	iny
	ldx #0
	jsr cur::set
	jmp @l0

@done:
	lda zp::cury
	jsr draw::hline
	popcur
	jmp __text_clrline
.endproc

