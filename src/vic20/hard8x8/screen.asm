;*******************************************************************************
; SCREEN23.ASM
;*******************************************************************************

.linecont +

.include "../fastcopy.inc"
.include "../finalex.inc"
.include "../../macros.inc"
.include "../../memory.inc"
.include "../../settings.inc"
.include "../../util.inc"
.include "../../zeropage.inc"

;*******************************************************************************
; CONSTANTS
COLMEM_ADDR = $9400

SCREEN_ADDR = $1000
NUM_COLS    = 22	; number of 8-pixel columns
NUM_ROWS    = 23	; number of 8-pixel rows

SCREEN_ROWS = 12	; number of physical rows per column

.segment "VSCREEN"
.segment "VSCREEN_BSS"
.segment "FASTTEXT"
.segment "FASTTEXT_BSS"

.segment "SETUP"
;*******************************************************************************
.export __text_init
.proc __text_init
	jsr $e5c3
	lda #(BG_COLOR<<4 | BORDER_COLOR)
	sta $900f
	rts
.endproc

.CODE
;*******************************************************************************
.export __screen_init
.proc __screen_init
	jsr $e5c3

	lda #$c2
	sta $9005

	lda #(BG_COLOR<<4 | BORDER_COLOR)
	sta $900f
	rts
.endproc

.CODE
;*******************************************************************************
; CLR
; Clears the screen
.export __screen_clr
.proc __screen_clr
@bm=r0
	ldxy #$1100
	stxy @bm

	lda #$00
	tay
:	sta (@bm),y
	iny
	bne :-
	inc @bm+1
	ldx @bm+1
	cpx #>$2000
	bne :-

	; fall through to clrcolor
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
; Clears all rows below the given offset in every column
; IN:
;  - .A: the character row to start clearing at
.export __screen_clr_part
.proc __screen_clr_part
@cnt=r2
	sta @cnt

@l0:	lda @cnt
	jsr __screen_clrline
	inc @cnt
	lda @cnt
	cmp #NUM_ROWS
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

	lda #$20
	ldy #NUM_COLS-1
@l0:	sta (@dst),y
	dey
	bpl @l0
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

	ldy #NUM_COLS-1
@l0: 	lda (@dst),y
	eor #$80
	sta (@dst),y
	dey
	bpl @l0
	rts
.endproc

;*******************************************************************************
; RVSLINE PART
; Reverses the given number of characters (8 pixels high) in the given row
; IN:
;  - .A: the text row to reverse (in characters)
;  - .Y: the first column to reverse
;  - .X: the last column to reverse
.export __screen_rvsline_part
.proc __screen_rvsline_part
@dst=r0
@start=r2
	; swap Y and X if (X < Y)
	sty @start

	; get the row address to reverse
	tay
	adc __screen_rowslo,y
	sta @dst
	lda __screen_rowshi,y
	adc #$00
	sta @dst+1

	ldy @start
	cpx @start
	bcs @l0
	txa
	ldx @start
	tay

@l0:	lda (@dst),y
	eor #$80
	sta (@dst),y
	dey
	cpy @start
	bpl @l0

@done:	rts
.endproc

;*******************************************************************************
; SAVE
; Saves the screen to the backup buffer. It may then be restored with a call
; to scr::restore
.export __screen_save
.proc __screen_save
	; TODO:
	;CALL FINAL_BANK_VSCREEN, save

	; save colors
	ldx #SCREEN_ROWS*2-1
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
; Restores screen from the the backup buffer.
; You should call bm::save first with the buffer you want to restore
.export __screen_restore
.proc __screen_restore
@buff=r0
@bm=r2
	; TODO
	;CALL FINAL_BANK_VSCREEN, restore

	; restore the per-row colors
	ldx #SCREEN_ROWS*2-1
:	lda mem::rowcolors_save,x
	sta mem::rowcolors,x
	dex
	bpl :-

	rts
.endproc

;*******************************************************************************
; CHAR ADDR
; Returns the address for the "character row" of the given row.
; IN:
;  - .A: the character row to get the address of
; OUT:
;  - .XY: the address
.export __screen_char_addr
.proc __screen_char_addr
	tax
	ldy __screen_rowshi,x
	lda __screen_rowslo,x
	tax
	rts
.endproc

;*******************************************************************************
; SCROLLUP
; Scrolls all lines from .X to .A up
; IN:
;  - .X: the top line that characters are scrolled to
;  - .A: the bottom line that is scrolled
.proc __text_scrollup
.export __text_scrollup
@src=zp::text
@dst=zp::text+2
@numrows=zp::text+4
	stx @numrows
	cmp @numrows
	bcs :+
	rts

:	sec
	sbc @numrows
	sta @numrows

	lda __screen_rowslo,x
	sta @dst
	lda __screen_rowshi,x
	sta @dst+1
	tax
	lda __screen_rowslo,x
	sta @src
	lda __screen_rowshi,x
	sta @src+1

	ldx @numrows
@l0:	ldy #NUM_COLS-1
@l1:	lda (@src),y
	sta (@dst),y
	dey
	bpl @l1
	lda @src
	clc
	adc #NUM_COLS
	sta @src
	bcc :+
	inc @src+1
:	lda @dst
	clc
	adc #NUM_COLS
	sta @dst
	bcc :+
	inc @dst+1
:	dex
	bne @l0
	rts
.endproc

;*******************************************************************************
; SCROLLDOWN
; Scrolls all rows from .A to .X
; IN:
;  - .A: the first column to scroll down
;  - .X: the last column to scroll down to
.export __text_scrolldown
.proc __text_scrolldown
	ldy #$01

	; fallthrough
.endproc

;*******************************************************************************
; SCROLLDOWNN
; Scrolls all rows in the given range down by the given number of rows
; IN:
;  - .A: the first row to scroll down
;  - .X: the last row to scroll down
;  - .Y: the number of characters to scroll each row by
.export __text_scrolldownn
.proc __text_scrolldownn
@src=r0
@dst=r2
@rowstart=r4
@offset=r5
	sta @rowstart
	sty @offset
	dec @offset

	cpx @rowstart
	beq @done	; if first and last rows are equal, no scroll

	; calculate number of rows: (last_row - first_row - offset)
	txa
	sec
	sbc @rowstart
	sbc @offset
	tax

	ldy @rowstart
	lda __screen_rowslo,y
	sta @src
	lda __screen_rowshi,y
	sta @src+1

	lda @rowstart
	clc
	adc @offset
	tay
	lda __screen_rowslo,y
	sta @dst
	lda __screen_rowshi,y
	sta @dst+1

@l0:	ldy #NUM_COLS-1
@l1:	lda (@src),y
	sta (@dst),y
	dey
	bpl @l1

	lda @src
	clc
	adc #NUM_COLS
	sta @src
	bcc :+
	inc @src+1

:	lda @dst
	clc
	adc #NUM_COLS
	sta @dst
	bcc :+
	inc @dst+1
:	dex		; decrement row counter
	bne @l0

@done:	rts
.endproc

;******************************************************************************
; PUTCH
; Puts the character given at the current cursor position
; IN:
;  - .A: the character to plot
.export putch
.proc putch
@dst=zp::text
	pha
	ldx zp::cury
	lda __screen_rowslo,x
	sta @dst
	lda __screen_rowshi,x
	sta @dst+1
	ldy zp::curx
	pla
	jsr asc2scr
	sta (@dst),y
	rts
.endproc

;******************************************************************************
; PUTS
; Displays the given string at the given row.  Regardless of the contents of
; the string, NUM_COLS characters are displayed (including 0's etc.)
; IN:
;  - .XY: the string to display
;  - .A:  the row to display the text at
.export __text_puts
__text_puts:
.export puts
.proc puts
@src = zp::text
@dst = zp::text+2
	stxy @src

	tax
	lda __screen_rowslo,x
	sta @dst
	lda __screen_rowshi,x
	sta @dst+1

	ldy #$00
@l0:	lda (@src),y
	jsr asc2scr
	sta (@dst),y
	iny
	cpy #NUM_COLS
	bne @l0

	rts
.endproc

;******************************************************************************
; SHL
.export __scr_shl
.proc __scr_shl
.endproc

;******************************************************************************
; SHR
.export __scr_shr
.proc __scr_shr
.endproc

;*******************************************************************************
; ASC2SCR
; Returns the screen code for the given ASCII character
; IN:
;   - .A: the ASCII code to convert
; OUT:
;   - .A: the screen code that corresponds to the given char
.proc asc2scr
	stx @savex
	cmp #$ff
	beq @done

	ldx #$ff
:	inx
	cmp @convtab,x
	bcs :-
	clc
	adc @offset,x

@savex=*+1
	ldx #$00
@done:	rts
.PUSHSEG
.RODATA
;|  Code   | Conversion Offset |
;| ----------------------------|
;| $00-$1F | $80               |
;| $20-$3F | $00               |
;| $40-$5F | $C0               |
;| $60-$7F | $E0               |
;| $80-$9F | $40               |
;| $A0-$BF | $C0               |
;| $C0-$DF | $80               |
;| $E0-$FE | $80               |
;| $FF     | $00               |
@convtab:
.byte $20,$40,$60,$80,$a0,$c0,$e0,$ff
@offset:
.byte $80,$00,$00,$a0,$40,$c0,$80,$80
.POPSEG

.endproc

.RODATA
;*******************************************************************************
.define rows \
	SCREEN_ADDR+$00, \
	SCREEN_ADDR+$16, \
	SCREEN_ADDR+$2c, \
	SCREEN_ADDR+$42, \
	SCREEN_ADDR+$58, \
	SCREEN_ADDR+$6e, \
	SCREEN_ADDR+$84, \
	SCREEN_ADDR+$9a, \
	SCREEN_ADDR+$b0, \
	SCREEN_ADDR+$c6, \
	SCREEN_ADDR+$dc, \
	SCREEN_ADDR+$f2, \
	SCREEN_ADDR+$108,\
	SCREEN_ADDR+$11e,\
	SCREEN_ADDR+$134,\
	SCREEN_ADDR+$14a,\
	SCREEN_ADDR+$160,\
	SCREEN_ADDR+$176,\
	SCREEN_ADDR+$18c,\
	SCREEN_ADDR+$1a2,\
	SCREEN_ADDR+$1b8,\
	SCREEN_ADDR+$1ce,\
	SCREEN_ADDR+$1e4

.export __screen_rowslo
.export __screen_rowshi
__screen_rowslo: .lobytes rows
__screen_rowshi: .hibytes rows

.linecont -
