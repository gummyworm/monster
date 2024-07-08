.include "asm.inc"
.include "bitmap.inc"
.include "config.inc"
.include "cursor.inc"
.include "draw.inc"
.include "edit.inc"
.include "errors.inc"
.include "fasttext.inc"
.include "file.inc"
.include "finalex.inc"
.include "irq.inc"
.include "key.inc"
.include "layout.inc"
.include "linebuffer.inc"
.include "macros.inc"
.include "memory.inc"
.include "source.inc"
.include "string.inc"
.include "strings.inc"
.include "util.inc"
.include "zeropage.inc"

ESCAPE_STRING    = $ff
ESCAPE_VALUE     = $fe
ESCAPE_VALUE_DEC = $fd
ESCAPE_SPACING   = $fc
ESCAPE_BYTE      = $fb
ESCAPE_CHAR      = $fa
ESCAPE_8x8UDG    = $f9
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
; BUFFERON
; enables buffering for text (text will only be drawn when lines are
; drawn, inserts will not redraw screen)
.export __text_bufferon
.proc __text_bufferon
	lda #$01
	skw

	; fallthrough
.endproc

;******************************************************************************
; BUFFEROFF
; Disables buffering. All characters will be drawn to screen
.export __text_bufferoff
.proc __text_bufferoff
	lda #$00
	sta __text_buffer
	rts
.endproc

;******************************************************************************
; STATUS
; Draws the text status (row, column, etc.) at the given row
; IN:
;  - .A: the row to display the status details on
.export __text_status
.proc __text_status
	ldx #<mem::statusline
	ldy #>mem::statusline
	jmp __text_puts
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
; CLRLINE
; Clears the linebuffer by setting its first byte to 0.
.export __text_clrline
.proc __text_clrline
	lda #0
	sta mem::linebuffer
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

	; save the return address (variadic args are stored on stack)
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

:	cmp #$09		; TAB
	bne :+

	sty @savey
	txa
	jsr __text_tabr_dist_a
	tay
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

	; restore the return address
	lda @ret+1
	pha
	lda @ret
	pha

	; print the rendered string
	ldxy #@buff
	lda @row
	jsr __text_puts

	; if __text_rvs is set, reverse the line after drawing it
	lda __text_rvs
	bne @rvs
	rts
@rvs:	lda @row
	jmp bm::rvsline
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
@curi=zp::text
	cmp #$14
	bne @printing

	; backspace
	lda zp::curx
	beq @err	; cannot delete (cursor is at left side of screen)
	cmp cur::minx
	bcc @err	; cursor is limited
	beq @err

	jsr __text_char_index
	sty @curi
	; get the new x position
	lda mem::linebuffer-1,y
	pha
	lda #$00
	sta mem::linebuffer-1,y	; temporarily 0-terminate line
	jsr __text_rendered_line_len
	stx zp::curx
	pla
	ldy @curi
	sta mem::linebuffer-1,y	; restore

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
	clc		; "put" was successful
	rts
@err:	sec		; couldn't perform action
	rts

@printing:
	ldx zp::curx
	cpx cur::maxx
	bcs @err	; cursor is limited
	pha
	jsr __text_char_index
	sty @curi
	lda __text_insertmode
	beq @fastput	; if REPLACE, no need to shift, do fast put

@slowput:
@shift_right:
	; insert a new char and redraw the line
	jsr __text_linelen
	cpx @curi
	beq @fastputi
	lda #$00
	sta mem::linebuffer+2,x
@shr:	lda mem::linebuffer,x
	sta mem::linebuffer+1,x
	dex
	cpx @curi
	bmi @cont		; if we've shifted all columns, continue
	bcc @cont
	bcs @shr		; if more to shift, repeat

@fastputi:
	lda #$00
	sta mem::linebuffer+1,x	; keep the line 0-terminated

@fastput:
	; replace the underlying character
@cont:	ldx @curi
	pla
	sta mem::linebuffer,x
	bne :+
	RETURN_OK		; terminating 0, we're done

:	cmp #$09		; TAB
	bne @redraw
@tab:	jsr __text_tabr_dist
	clc
	adc zp::curx
	sta zp::curx
	lda zp::cury
	jmp @redrawline		; re-render the line

@redraw:
	lda __text_buffer
	bne @done
	lda zp::cury
	inc zp::curx
@redrawline:
	jsr __text_drawline	; re-render whole line
	RETURN_OK

@done:	inc zp::curx
	RETURN_OK		; "put" was successful
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
	ldy #$01

	; fallthrough
.endproc

;******************************************************************************
; SCROLLDOWNN
; Scrolls all rows in the given range down by the given number of rows
; IN:
;  - .A: the first row to scroll down
;  - .X: the last row to scroll down
;  - .Y: the number of pixels to scroll each row by
.export __text_scrolldownn
.proc __text_scrolldownn
@rowstart=zp::text
@rows=zp::text+1
@src=zp::text+2
@dst=zp::text+4
@offset=r0
	sta @rowstart
	sty @offset
	dec @offset

	cpx @rowstart
	beq @noscroll
	bcs :+
@noscroll:
	rts		; nothing to scroll

:	txa
	sec
	sbc @rowstart
	sbc @offset
	asl
	asl
	asl
	sta @rows
	dec @rows	; -1 because we will do the last row separately

	tya
	asl
	asl
	asl
	sta @offset

	lda @rowstart
	asl
	asl
	asl
	sta @src
	adc @offset
	sta @dst

	lda #>BITMAP_ADDR
	sta @src+1
	sta @dst+1

@l0:	ldy @rows
@l1:	lda (@src),y
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
; PUTS
; Displays the given string at the given row.  Regardless of the contents of
; the string, text::len characters are displayed (including 0's etc.)
; IN:
;  - .XY: the string to display
;  - .A: the text to display
.export __text_puts
.proc __text_puts
	JUMP FINAL_BANK_FASTTEXT, #ftxt::puts
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
@tabsz=zp::tmp0
@savey=zp::tmp1
	ldx #$ff
	ldy #$ff
@l0:	iny
	inx
	lda mem::linebuffer,y
	beq @done
	cmp #$09
	bne :+
	txa
	sty @savey
	jsr __text_tabr_dist_a
	ldy @savey
	sta @tabsz
	txa
	clc
	adc @tabsz
	tax
	dex			; undo the INX
:	cpx #40
	bcc @l0
@done:	rts
.endproc

;******************************************************************************
; CHAR INDEX
; Returns the character index of the current cursor position
; If the cursor is within a TAB character's rendered range, returns the
; index of the start of the TAB
; OUT:
;  .A: the character under the cursor
;  .X: the x column position of the cursor
;  .Y: the character index of the cursor
.export __text_char_index
.proc __text_char_index
@tabsz=r0
@savey=r1
	ldx #$ff
	ldy #$ff
@l0:	iny
	inx
	lda mem::linebuffer,y
	beq @done
	pha
	cmp #$09
	bne :+
	txa
	sty @savey
	jsr __text_tabr_dist_a
	ldy @savey
	sta @tabsz
	txa
	clc
	adc @tabsz
	tax
	dex			; undo the INX
:	cpx zp::curx
	pla
	bcc @l0
@done:	lda mem::linebuffer,y
	rts
.endproc

;******************************************************************************
; INDEX2CURSOR
; Returns the x cursor position for the given offset in linebuffer
; IN:
;  - .A: the index in linebuffer to get the cursor position for
; OUT:
;  - .X: the corresponding cursor position
.export __text_index2cursor
.proc __text_index2cursor
@i=zp::text
@x=zp::text+1
@seek=zp::text+2
	sta @seek
	lda #$00
	sta @i
	sta @x
	lda mem::linebuffer
	beq @done

@l0:	ldx @i
	lda mem::linebuffer,x
	beq @end	; end of buffer
	cpx @seek
	beq @done
	inc @i
	cmp #$09	; TAB
	bne :+

	lda @x
	jsr __text_tabr_dist_a
	clc
	adc @x
	sta @x
	bne @l0

:	inc @x
	bne @l0

@end:	dec @x
@done:	ldx @x
	rts
.endproc

;******************************************************************************
; TABR_DIST
; Returns the # of columns to the next tab
; OUT:
;  - .A: the number of characters to the next tab
.export __text_tabr_dist
__text_tabr_dist_a=*+2
.proc __text_tabr_dist
	lda zp::curx
	sta zp::util
	ldy #$00
:	iny
	cmp tabs,y
	bcs :-
	lda tabs,y
	sec
	sbc zp::util
	rts
.endproc

;******************************************************************************
; TABL_DIST
; Returns the number of columns left to the previous tab column
; OUT:
;  - .A: the number of characters to the previous tab
.export __text_tabl_dist
.proc __text_tabl_dist
	lda zp::curx
	ldy #tabs_end
:	dey
	cmp tabs,y
	bcc :-
	beq :-
	lda tabs,y
	sta zp::util
	lda zp::curx
	sec
	sbc zp::util
	rts
.endproc

;******************************************************************************
; TABS
; This table stores the offsets to each TAB column
tabs:
.repeat TAB_WIDTH, i
	.byte i*TAB_WIDTH
.endrepeat
tabs_end=*-tabs

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

;******************************************************************************
; INFO
; Prints the given string (text::print) at the info row (STATUS_ROW-1)
; ; IN:
;  - .XY: the string to print
.export __text_info
.proc __text_info
	lda #STATUS_ROW-1
	jmp __text_print
.endproc
