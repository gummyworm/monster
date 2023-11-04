.include "asm.inc"
.include "bitmap.inc"
.include "config.inc"
.include "cursor.inc"
.include "draw.inc"
.include "edit.inc"
.include "fasttext.inc"
.include "file.inc"
.include "finalex.inc"
.include "irq.inc"
.include "key.inc"
.include "linebuffer.inc"
.include "macros.inc"
.include "memory.inc"
.include "source.inc"
.include "string.inc"
.include "util.inc"
.include "zeropage.inc"

ESCAPE_STRING    = $ff
ESCAPE_VALUE     = $fe
ESCAPE_VALUE_DEC = $fd
ESCAPE_SPACING   = $fc
ESCAPE_BYTE      = $fb
ESCAPE_CHAR      = $fa
ESCAPE_RVS_ON    = $12
ESCAPE_RVS_OFF   = $92
STATUS_LINE      = 23
STATUS_COL       = 0

.BSS
;******************************************************************************
.export __text_len
__text_len: .byte 0

.export  __text_buffer
__text_buffer: .byte 0	; if 0, putch immediately draws to the screen

.export __text_rvs
__text_rvs: .byte 0	; reverse text state ($ff = rvs on, $00 = rvs off)

.export __text_insertmode
__text_insertmode: .byte 0	; the insert mode (1 = insert, 0 = replace)

.export __text_status_mode
__text_status_mode: .byte 0	; the mode to display on the status line

.CODE
;******************************************************************************
.export __text_status
.proc __text_status
	ldx #<mem::statusline
	ldy #>mem::statusline
	lda #STATUS_LINE
	jsr __text_puts
	lda #STATUS_LINE
	jmp bm::rvsline
.endproc

;******************************************************************************
; UPDATE_STATUSLINE
; Updates mem::statusline with new information (cursor pos, etc.)
; SIDE-EFFECTS:
;  - mem::statusline: contains the new status info
.proc update_statusline
@filename=zp::tmp0
@tmp=zp::tmp0

@columnstart=STATUS_COL+3
@linestart=STATUS_COL+6
@sizestart=STATUS_COL+13
@modestart=STATUS_COL
	lda #' '
	ldx #39
@clr:	sta mem::statusline,x
	dex
	bpl @clr

	lda zp::curx
	jsr util::todec8

	; display the column
	ldy #$00
	cpx #'0'
	beq :+
	stx mem::statusline+@columnstart
	iny
:	sta mem::statusline+@columnstart,y

	lda #','
	sta mem::statusline+@columnstart+1,y

	; display current line
	ldxy src::line
	jsr util::todec
	ldx #$00
@l0:	lda mem::spare,x
	beq :+
	sta mem::statusline+@linestart,x
	inx
	bne @l0

:	lda #'/'
	sta mem::statusline+@linestart,x

	stx @tmp

	; display total lines
	ldxy src::lines
	jsr util::todec

	ldx @tmp
	ldy #$00
@l1:	lda mem::spare,y
	beq :+
	sta mem::statusline+@linestart+1,x
	iny
	inx
	bne @l1

:	; add the editor mode
	lda __text_status_mode
	sta mem::statusline+@modestart

@copy_filename:
	; filename
	lda src::activebuff
	jsr src::filename
	stxy @filename
	jsr str::len
	tay
	dey
	beq @drive
	ldx #39
:	lda (@filename),y
	sta mem::statusline,x
	dex
	dey
	bpl :-

; display a '*' if the file is dirty
	jsr src::isdirty
	beq @drive
	lda #'*'
	sta mem::statusline,x

; display the drive name followed by a colon to the left of the filename
@drive:	lda #':'
	sta mem::statusline-1,x

	stx @tmp
	lda zp::device
	jsr util::todec8
	ldy @tmp
	sta mem::statusline-2,y
	cpx #'0'
	beq :+
	txa
	dey
	sta mem::statusline-2,y
:	lda #'#'
	sta mem::statusline-3,y
@done:	rts
.endproc

;******************************************************************************
; UPDATE
; updates the statusline according to the current cursor position
; and blinks the cursor if it's time
.export __text_update
.proc __text_update
	jsr update_statusline
@blink: lda cur::mode
	bne @done			; if not NORMAL (SELECT), don't blink
	dec zp::curtmr
	bne @done

	jsr cur::toggle
	lda #CUR_BLINK_SPEED
	sta zp::curtmr
@done:	rts
.endproc

;******************************************************************************
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

;******************************************************************************
; PRINT
; displays the format string in (<X,>Y) at the row in .A.
; NOTE: you MUST call it with JSR (not JMP) because it manipulates the stack to
; get operands
; IN:
;  - .XY: the address of the message to print
;  - .A:  the row to print the string at
.export __text_print
.proc __text_print
@sub = zp::text	; address of string to replace escape char with
@str = zp::text+2
@row = zp::text+4
@savex = zp::text+5
@savey = zp::text+6
@ret = zp::text+8
@buff = mem::linebuffer2
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

:	cmp #$18		; TAB
	bne :+

	sty @savey
	ldy #TAB_WIDTH
	lda #' '
@tab:	sta @buff,x
	inx
	dey
	bne @tab
	ldy @savey
	jmp @cont

:	cmp #ESCAPE_CHAR
	bne :+
	pla			; get the character
	jmp @ch			; and continue to printing it

:	cmp #ESCAPE_STRING
	bne :+
	jmp @string
:	cmp #ESCAPE_BYTE
	beq @value_byte
	cmp #ESCAPE_VALUE
	beq @value
	cmp #ESCAPE_VALUE_DEC
	beq @value_dec
	cmp #ESCAPE_SPACING
	beq @spacing
	jmp @ch

@spacing:
;substitute escape character with number of spaces from stack
	iny
	sty @savey
	lda (@str),y
	tay
	bne :+
	jmp @cont
:	lda #' '
	sta @buff,x
	inx
	dey
	bne :-
	ldy @savey
	jmp @cont

@value_byte:
	stx @savex
	sty @savey

	pla
	jsr util::hextostr
	txa
	ldx @savex
	sta @buff+1,x
	tya
	sta @buff,x
	jmp @value_1byte_done

;substitute escape character with value from stack formatted in base-10
@value_dec:
	stx @savex
	sty @savey

	pla
	tay
	pla
	tax
	jsr util::todec
	ldy #0
	ldx @savex
:	lda mem::spare,y
	beq @decdone
	sta @buff,x
	inx
	iny
	bne :-
@decdone:
	ldy @savey
	jmp @cont

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

@value_2byte_done:
	inx
	inx
@value_1byte_done:
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
	sty @savey
	ldy #$00
@l1:	lda (@sub),y
	bne :+
	ldy @savey
	bpl @cont	; branch always
:	sta @buff,x
	iny
	inx
	cpx #40
	bcs @disp
	bne @l1

@ch:	sta @buff,x
	inx

@cont:	iny
        jmp @l0

@disp:	lda #' '
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
	jsr __text_puts

	; if __text_rvs is set, reverse the line after drawing it
	lda __text_rvs
	bne :+
	rts
:	lda @row
	jmp bm::rvsline
.endproc

;******************************************************************************
; PRINT_COMPRESSED
; Print the 5-bit-per-character compressed text string. This is equivalent to
; calling str::uncompress and text::print with the resulting string.
; IN:
;  - .A:  the row to print at
;  - .XY: the address of the encoded string
.export __text_print_compressed
.proc __text_print_compressed
	pha
	jsr str::uncompress
	pla
	jmp __text_putz
.endproc

;******************************************************************************
; PLOT
; Puts given character onto the screen at the given (X,Y) corrdinates
; IN:
;  - .A: the character to plot
;  - .X: the x corrdinate to plot at
;  - .Y: the y coordinate to plot at
.export __text_plot
.proc __text_plot
@ch=zp::text
	sta @ch
	lda zp::curx
	pha
	lda zp::cury
	pha

	stx zp::curx
	sty zp::cury

	lda @ch
	jsr __text_putch

	pla
	sta zp::cury
	pla
	sta zp::curx
	rts
.endproc

;******************************************************************************
; PUTCH
; Adds the character in .A to the current cursor position in the
; text linebuffer. The cursor is then updated to the next position.
; IN:
;  - .A: the character to put
; OUT:
;  - .C: set if character was unsuccessfully put
.export __text_putch
.proc __text_putch
@src=zp::text
@mask=zp::text+2
@dst=zp::text+4
@clrmask=zp::text+6
@char=zp::text+7
@len=zp::text+7
@dx=zp::text+8
@curi=zp::text+9
	cmp #$14
	bne @printing

	; backspace
	lda zp::curx
	beq @err	; cannot delete (cursor is at left side of screen)
	cmp cur::minx
	bcc @err	; cursor is limited
	beq @err

	; get the amount to move the cursor (1 if not tab)
	jsr __text_char_index
	sty @curi
	ldx mem::linebuffer-1,y
	lda #$ff
	cpx #$18		; TAB?
	bne :+
	lda #(TAB_WIDTH^$ff)+1	; (2's complement)
:	pha

	lda __text_insertmode
	beq @moveback	; if REPLACE, just move cursor

@shift_left:
	jsr __text_linelen
	lda #$00
	sta mem::linebuffer,x
	txa
	tay
	ldx @curi
	dex
	jsr linebuff::shl
@moveback:
	pla
	clc
	adc zp::curx
	sta zp::curx
	clc		; "put" was successful
	rts
@err:	sec		; couldn't perform action
	rts

@printing:
	ldx zp::curx
	cpx cur::maxx
	bcs @err	; cursor is limited

	pha
	lda __text_insertmode
	beq @fastput	; if REPLACE, no need to shift, do fast put

@slowput:
@shift_right:
	jsr __text_char_index
	sty @curi
	; insert a new char and redraw the line
	jsr __text_linelen
	stx @len
	ldy @curi
	cpy @len
	beq @fastputi
	lda #$00
	sta mem::linebuffer+2,y
@shr:	lda mem::linebuffer,y
	sta mem::linebuffer+1,y
	cpy @curi
	beq :+
	dey
	bpl @shr

:	; shift the bitmap (if buffering is disabled)
	lda __text_buffer
	bne :+
	ldy @len
	lda zp::cury
	jsr bm::shr

:	jsr __text_linelen
	jmp @cont

@fastputi:
	lda #$00
	sta mem::linebuffer+1,x	; keep the line 0-terminated
@fastput:
	; replace the underlying character
@cont:	pla
	sta mem::linebuffer,x
	bne :+
	rts			; terminating 0, we're done

:	cmp #$18		; TAB
	bne :+
@tab:	lda zp::curx
	clc
	adc #TAB_WIDTH
	sta zp::curx
	lda zp::cury
	inc zp::curi
	jmp __text_drawline	; re-render whole line

:	sta @char
	ldx __text_buffer
	bne @done		; if BUFFER is enabled, don't blit
	CALL FINAL_BANK_FASTTEXT, #ftxt::putch
@done:	inc zp::curx
	inc zp::curi
	clc	; "put" was successful
	rts
.endproc

;******************************************************************************
; DRAWLINE
; Renders the text in mem::linebuffer at the given row
; IN:
;  - .A: the row to draw the line at
;  - mem::linebuffer: the text to draw
.export __text_drawline
.proc __text_drawline
	ldx #40
	stx __text_len
	ldxy #mem::linebuffer
	jmp __text_print
.endproc

;******************************************************************************
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
	asl
	asl
	asl
	sta @numrows

	txa
	asl
	asl
	asl
	sta @dst
	adc #$08
	sta @src

	lda #>BITMAP_ADDR
	sta @dst+1
	sta @src+1

@l0:	ldy #$00
@l1:	lda (@src),y
	sta (@dst),y
	iny
	cpy @numrows
	bne @l1

@updatesrc:
	lda @src
	clc
	adc #$c0
	sta @src
	bcc @updatedst
	inc @src+1

@updatedst:
	lda @dst
	clc
	adc #$c0
	sta @dst

	lda @dst+1
	adc #$00
	sta @dst+1
	cmp #$20
	bne @l0
	rts
.endproc

;******************************************************************************
; SCROLLDOWN
; Scrolls all rows from .A to .X
; IN:
;  - .A: the first column to scroll down
;  - .X: the last column to scroll down to
.export __text_scrolldown
.proc __text_scrolldown
@rowstart=zp::text
@rows=zp::text+1
@src=zp::text+2
@dst=zp::text+4
	sta @rowstart

	cpx @rowstart
	beq @noscroll
	bcs :+
@noscroll:
	rts		; nothing to scroll

:	txa
	sec
	sbc @rowstart
	asl
	asl
	asl
	sta @rows
	dec @rows	; -1 because we will do the last row separately

	lda @rowstart
	asl
	asl
	asl
	sta @src
	adc #$08
	sta @dst

	lda #>BITMAP_ADDR
	sta @src+1
	sta @dst+1

@l0:	ldy @rows
@l1:
	lda (@src),y
	sta (@dst),y
	dey
	bne @l1
	lda (@src),y	; do last row
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
	lda @dst+1
	adc #$00
	sta @dst+1
	cmp #$20
	bcc @l0
	rts
.endproc

;******************************************************************************
; PUTZ
; Prints a full width (40 column) string, terminated by a 0 at the given row
; IN:
;  - .XY: the string to display
;  - .A: the row to display the string at
.export __text_putz
.proc __text_putz
;
@src = zp::text
	pha
	stxy @src

	ldy #39
	lda #' '
:	sta mem::spare,y
	dey
	bpl :-

	iny
@l0:	lda (@src),y
	beq @done
	sta mem::spare,y
	iny
	cpy #40
	bcc @l0
@done:	ldxy #mem::spare
	pla
	jmp __text_puts
.endproc

;******************************************************************************
; PUTS
; Displays the given string at the given row.  Regardless of the contents of
; the string, text::len characters are displayed (including 0's etc.)
; IN:
;  - .XY: the string to display
;  - .A: the text to display
.export __text_puts
.proc __text_puts
@txtdst   = zp::text+5
@txtsrc = zp::text+7
	stxy @txtsrc
	asl
        asl
        asl
        sta @txtdst
	CALL FINAL_BANK_FASTTEXT, #ftxt::puts
	rts
.endproc

;******************************************************************************
; LINELEN
; Returns the length of mem::linebuffer
; OUT:
;  - .X: the length of mem::linebuffer
.proc __text_linelen
.export __text_linelen
	ldx #$ff
@l0:	inx
	lda mem::linebuffer,x
	beq @done
	cpx #40
	bne @l0
@done:	rts
.endproc

;******************************************************************************
; RENDERED LINE LEN
; Returns the length of mem::linebuffer in number of rendered characters
; OUT:
;  - .X: the length of mem::linebuffer as it appears on screen
.proc __text_rendered_line_len
.export __text_rendered_line_len
	ldx #$ff
	ldy #$ff
@l0:	iny
	inx
	lda mem::linebuffer,y
	beq @done
	cmp #$18
	bne :+
	txa
	adc #TAB_WIDTH-2	; -2 because .C is set and we've already INX'd
	tax
:	cpx #40
	bcc @l0
@done:	rts
.endproc

;******************************************************************************
; LINE INDEX
; Returns the character index of the current cursor position
; OUT:
;  X: the x column position of the cursor
;  Y: the character index of the cursor
.export __text_char_index
.proc __text_char_index
	ldx #$ff
	ldy #$ff
@l0:	iny
	inx
	lda mem::linebuffer,y
	beq @done
	cmp #$18
	bne :+
	txa
	adc #TAB_WIDTH-2
	tax
:	cpx zp::curx
	bcc @l0
@done:	rts
.endproc

;******************************************************************************
; SAVEBUFF
; Stores the contents of linebuffer to spare memory. The contents of the
; most recent call to this routine will be restored when text::restorebuff is
; called
.export __text_savebuff
.proc __text_savebuff
	ldy #39
:	lda mem::linebuffer,y
	sta mem::linesave,y
	dey
	bpl :-
	rts
.endproc

;******************************************************************************
; RESTOREBUFF
; Restores the linebuffer from the contents of spare memory (saved by the most
; recent call to text::savebuff)
.export __text_restorebuff
.proc __text_restorebuff
	ldy #39
:	lda mem::linesave,y
	sta mem::linebuffer,y
	dey
	bpl :-
	rts
.endproc

