;*******************************************************************************
; UDGEDIT.ASM
;
; This file contains the source for the UDG Editor module, which allows a user
; to define "user defined graphics", or UDG's, for insertion into their code.
; The UDG Editor displays an 8x8 grid that can be navigated using the normal
; vi-like navigation keys. Plotting pixels is done with the '1' key.
; The 'M' key toggles between multicolor and hires mode
; Pressing ENTER confirms the creation of a graphic, while the QUIT key
; exits the editor without creating the graphic.
;
; It is up to the caller (the editor) to update the source with the data for
; the created graphic
;*******************************************************************************

.include "bitmap.inc"
.include "cursor.inc"
.include "keycodes.inc"
.include "macros.inc"
.include "zeropage.inc"

;*******************************************************************************
PIXEL_SIZE = 4		; size of each pixel in the editor
CUR_DELAY  = $2000	; ticks before cursor is toggled

CANVAS_Y      = 64		; start row (in pixels)
CANVAS_X      = 8*8		; start column (in pixels)
CANVAS_HEIGHT = 8*PIXEL_SIZE
CANVAS_WIDTH  = 8*PIXEL_SIZE

color      = zp::editortmp
cur_on     = zp::editortmp+1	; cursor on flag
cur_tmr    = zp::editortmp+2	; cursor blink timer (2 bytes)
multicolor = zp::editortmp+4	; flag for multi-color enabled (!0)

udg = r8	; buffer where character data is stored (8 bytes)

linebuffer = $0400

.segment "UDGEDIT"
;*******************************************************************************
; EDIT
; Activates the UDG editor
; OUT:
;  - r8-rf: the character that the user created
;  - .A:    0: no graphic created or updated
;           1: new graphic created
;           2: graphic updated
.export __udg_edit
.proc __udg_edit
@result=r4
	cli
	jsr clrcanvas

	lda #$0e
	sta $900a		; set aux color to light blue
	lda #$00
	sta multicolor		; multicolor off

	jsr init_mc		; init multicolor mode

	; parse linebuffer, populate udg (r8) if line contains a .db directive
	jsr parse_bytes
	lda #$01
	sta @result		; flag that we are creating new graphic
	bcs @cont		; line doesn't contain a UDG definition

	inc @result		; flag that we are updating graphic

	jsr draw_udg

@cont:
	; move cursor back to (0,0)
	lda #$00
	sta zp::curx
	sta zp::cury

	ldxy #CUR_DELAY
	stxy cur_tmr

@main:	decw cur_tmr
	bne :+
	lda cur_tmr+1
	bne :+
	jsr curtoggle
	ldxy #CUR_DELAY
	stxy cur_tmr

:	jsr $f1f9		; get key
	cmp #$00
	beq @main
	cmp #K_RETURN
	beq @ok
	cmp #K_QUIT
	beq @ret
	jsr handlekey
	jmp @main

@ok:	lda @result
	rts

@ret:	lda #$00	; no graphic created
	rts
.endproc

;*******************************************************************************
; HANDLEKEY
.proc handlekey
	ldx #@numkeys-1
:	cmp @keys,x
	beq @handle
	dex
	bpl :-
	rts	; nothing to do for key

@handle:
	lda @handlerslo,x
	sta zp::jmpvec
	lda @handlershi,x
	sta zp::jmpvec+1
	jmp zp::jmpaddr

@keys:
	; k, j, h, l, 1, 2, m
	.byte $4b, $4a, $48, $4c, '1', '2', '3', '4', K_UDG_TOGGLE_MODE
@numkeys=*-@keys

.define handlers up, down, left, right, plot0, plot1, plot2, plot3, toggle_mode
@handlerslo: .lobytes handlers
@handlershi: .hibytes handlers
.endproc

;*******************************************************************************
; CLRCANVAS
; Clears the 8x8 UDG canvas and the underlying character data
.proc clrcanvas
@dst=r0
	; clear the character buffer
	lda #$00
	sta cur_on	; clear cursor status

	ldx #$07
:	sta udg,x
	dex
	bpl :-

	; clear the bitmap area of the canvas
	ldxy #BITMAP_ADDR+($c0*(CANVAS_X/8))+CANVAS_Y-1-$c0
	stxy @dst
	ldx #CANVAS_WIDTH/8+1	; +1 for border

; draw left border
	lda #$01
	ldy #CANVAS_HEIGHT+1
:	sta (@dst),y
	dey
	bpl :-
	bmi @nextcol

@l0:	lda #$00
	ldy #CANVAS_HEIGHT+1
	lda #$ff
	sta (@dst),y	; bottom border
	dey
:	lda #$00
	sta (@dst),y
	dey
	bne :-

	lda #$ff
	sta (@dst),y	; top border

@nextcol:
	lda @dst
	clc
	adc #$c0	; next col
	sta @dst
	bcc :+
	inc @dst+1
:	dex
	bne @l0

@rborder:
	; draw rightborder
	lda #$80
	ldy #CANVAS_HEIGHT+1
:	sta (@dst),y
	dey
	bpl :-

@done:	rts
.endproc

;*******************************************************************************
; CUROFF
; Turns off the cursor
.proc curoff
	lda cur_on
	beq @done	; already off
	jmp curtoggle
@done:	rts
.endproc

;*******************************************************************************
; CURON
; Turns on the cursor
.proc curon
	lda cur_on
	bne @done	; already on
	jmp curtoggle
@done:	rts
.endproc

;*******************************************************************************
; CURTOGGLE
; Toggles the cursor
.proc curtoggle
@dst=r0
@y=r2
	lda cur_on
	eor #$01
	sta cur_on

	lda zp::curx
	lsr
	tax

	lda colslo+(CANVAS_X/8)-1,x
	clc
	adc #CANVAS_Y
	sta @dst
	lda colshi+(CANVAS_X/8)-1,x
	adc #$00
	sta @dst+1

	lda zp::cury
	asl
	asl
	tay
	sty @y
	lda zp::curx
	and #$01
	bne @oddcol

	lda multicolor
	bne @evencol_mc

@evencol_hires:
	; left border
	ldx #PIXEL_SIZE
:	lda #$02
	eor (@dst),y
	sta (@dst),y
	iny
	dex
	bne :-

	lda @dst
	clc
	adc #BITMAP_HEIGHT
	sta @dst
	bcc :+
	inc @dst+1

:	ldy @y
	; top row
	lda #$a8
	eor (@dst),y
	sta (@dst),y

	; middle rows
	iny
	lda #$08
	eor (@dst),y
	sta (@dst),y
	iny
	lda #$08
	eor (@dst),y
	sta (@dst),y

	; bottom row
	iny
	lda #$a8
	eor (@dst),y
	sta (@dst),y
	rts

@oddcol:
	lda @dst
	clc
	adc #BITMAP_HEIGHT
	sta @dst
	bcc :+
	inc @dst+1

:	; top row
	lda #$2a
	eor (@dst),y
	sta (@dst),y

	; middle rows
	iny
	lda #$20
	eor (@dst),y
	sta (@dst),y
	iny
	lda #$20
	eor (@dst),y
	sta (@dst),y

	; bottom row
	iny
	lda #$2a
	eor (@dst),y
	sta (@dst),y

	lda @dst
	clc
	adc #BITMAP_HEIGHT
	sta @dst
	bcc :+
	inc @dst+1

:	ldy @y
	; right border
	ldx #PIXEL_SIZE
:	lda #$80
	eor (@dst),y
	sta (@dst),y
	iny
	dex
	bne :-
	rts

; multicolor is always even
@evencol_mc:
	; left border
	ldx #PIXEL_SIZE
:	lda #$02
	eor (@dst),y
	sta (@dst),y
	iny
	dex
	bne :-

	lda @dst
	clc
	adc #BITMAP_HEIGHT
	sta @dst
	bcc :+
	inc @dst+1

:	ldy @y
	lda #$aa		; top border
	eor (@dst),y
	sta (@dst),y
	iny
	iny
	iny
	lda #$aa		; bottom border
	eor (@dst),y
	sta (@dst),y

	lda @dst
	clc
	adc #BITMAP_HEIGHT
	sta @dst
	bcc :+
	inc @dst+1

:	ldy @y
	; right border
	ldx #PIXEL_SIZE
:	lda #$80
	eor (@dst),y
	sta (@dst),y
	iny
	dex
	bne :-

	rts
.endproc

;*******************************************************************************
; DRAW UDG
; Draws the UDG stored in "udg"
; If the multicolor flag is set, will be drawn in multicolor else hires
.proc draw_udg
@row=r6
@mask=r7
	lda #7
	sta zp::cury	; start at bottom

	lda #$01
	sta @mask	; for hires, set bitmask to %1
	lda multicolor
	beq @l0
	sec
	rol		; set bit mask to %11 for multicolor

@l0:	lda #7
	sta zp::curx
	lda multicolor
	beq :+
	dec zp::curx
:	ldy zp::cury
	lda udg,y
	sta @row	; get a row of data to render

@l1:	lda @row
	and @mask
	jsr plot

@next:	lda multicolor
	beq :+
	dec zp::curx	; if multicolor, move x in increments of 2
	lsr @row

:	lsr @row
	dec zp::curx
	bpl @l1

	dec zp::cury
	bpl @l0
	rts
.endproc

;*******************************************************************************
; GETDSTMASK
; Gets the bitmap address of the cursor and its mask within the cell
; OUT:
;  - r0: the bitmap address of the cursor
;  - r2: the bitmask of the cursor
;  - r3: the inverse bitmask of the cursor
.proc getdstmask
@dst=r0
@mask=r2
@clrmask=r3
	jsr curoff
	lda zp::curx
	lsr
	tax

	lda colslo+(CANVAS_X/8),x
	clc
	adc #CANVAS_Y
	sta @dst
	lda colshi+(CANVAS_X/8),x
	adc #$00
	sta @dst+1

	lda zp::cury
	asl
	asl
	tay

	lda multicolor
	beq :+
	lda #$ff	; multicolor takes up full 8-pixels
	sta @mask
	lda #$00
	sta @clrmask	; clear whole row
	rts

:	lda zp::curx
	and #$01
	bne :+
	lda #$f0	; even mask
	skw
:	lda #$0f	; odd mask
	sta @mask
	eor #$ff
	sta @clrmask	; set clear mask
	rts
.endproc

;*******************************************************************************
; TOGGLE MODE
; Toggles between multicolor and hires mode
.proc toggle_mode
	jsr curoff

	lda #$08
	eor multicolor
	sta multicolor
	beq @done

	; move cursor to even position if on odd one
	lda zp::curx
	and #$fe
	sta zp::curx

@done:	pushcur
	jsr draw_udg	; redraw the contents of the UDG
	popcur
	rts
.endproc

;*******************************************************************************
; PLOT
; The following plot routines set the respective bit patterns to the current
; cursor position:
;  hires:      0, 1
;  multicolor: 00, 01, 10, 11
plot0:	lda #$00
	skw
plot1:	lda #(1<<6)|(1<<4)|(1<<2)|1
	skw
plot2:	lda #(2<<6)|(2<<4)|(2<<2)|2
	skw
plot3:	lda #(3<<6)|(3<<4)|(3<<2)|3
plot:
@dst=r0
@mask=r2
@clrmask=r3
@pattern=r4
	sta @pattern

	; replicate the given bit pattern across a whole byte
	ldx #7
@genpatt:
	asl
	ldy multicolor
	beq :+
	asl			; for multicolor, move in increments of 2
	dex
:	ora @pattern
	dex
	bpl @genpatt

	jsr getdstmask

	; draw the pixel
	ldx #PIXEL_SIZE		; rows to draw
@l0:	lda @clrmask
	and (@dst),y		; clear the underlying pixels
	ora @pattern
	and @mask
	sta (@dst),y		; set the new pattern
	iny
	dex
	bne @l0

	; fall through to setpixel in the UDG buffer

;*******************************************************************************
; SETPIXEL
; Sets the pixel at the cursor to the given pattern
; IN:
;   - .A: the 1 or 2 (multicolor) bit pattern to set at the cursor's position
.proc setpixel
@patt=r4
@clrpatt=r0
	; move the pattern into position based on the cursor's position
	lda #$fe
	sta @clrpatt
	lda #$07
	sec
	sbc zp::curx
	tax
	beq @set
@l0:	asl @patt
	asl @clrpatt
	dex
	bne @l0

@set:	lda udg,y
	and @clrpatt
	ora @patt
	sta udg,y
	rts
.endproc

;*******************************************************************************
; RIGHT
; Handle the "move right" behavior
.proc right
	jsr curoff
	lda zp::curx
	ldx multicolor
	bne @mc

@hires:	cmp #7
	bcs @done
	inc zp::curx
	bne @done	; branch always

@mc:	cmp #6
	bcs @done
	inc zp::curx
	inc zp::curx	; in multicolor mode, move in incremenets of 2

@done:	jmp curon
.endproc

;*******************************************************************************
; LEFT
; Handle the "move left" behavior
.proc left
	jsr curoff
	lda multicolor
	bne @mc

@hires:	dec zp::curx
	bpl @done
	inc zp::curx
	bpl @done	; branch always

@mc:	lda zp::curx
	beq @done
	dec zp::curx
	dec zp::curx	; in multicolor mode, move in incremenets of 2
@done:	jmp curon
.endproc

;*******************************************************************************
; DOWN
; Handle the "move down" behavior
.proc down
	jsr curoff
	lda zp::cury
	cmp #7
	bcs :+
	inc zp::cury
:	jmp curon
.endproc

;*******************************************************************************
; UP
; Handle the "move up" behavior
.proc up
	jsr curoff
	dec zp::cury
	bpl :+
	inc zp::cury
:	jmp curon
.endproc

;*******************************************************************************
; PARSEHEX
; Parses the given characters and returns the binary data they represent
; IN:
;  - .X: the LSB of the 2 character string hex value
;  - .Y: the MSB of the 2 character string hex value
; OUT:
;  - .A: the binary value
;  - .C: set if no value could be parsed (.X/.Y contains invalid hex digit)
.proc parsehex
@byte=r4
	tya
	jsr @tohex
	asl
	asl
	asl
	asl
	sta @byte
	txa
	jsr @tohex
	ora @byte

@ok:	clc
	rts

@err:	sec
	rts

@tohex:
	cmp #'f'+1
	bcs @err
	cmp #'a'
	bcc :+
	sbc #'a'-$a
	rts

:	cmp #'F'+1
	bcs @err
	cmp #'A'
	bcc @numeric
	sbc #'A'-$a
	rts

@numeric:
	cmp #'9'+1
	bcs @err
	cmp #'0'
	bcc @err
	sbc #'0'
	rts
.endproc

;*******************************************************************************
; PARSE_BYTES
; Parses the line for graphic data
; Graphic data lines are .DB directives. If more than 8 bytes are defined
; in the line, the first 8 are used for the character.
; If less than 8 are defined, the remaining characters are padded with zeroes.
; NOTE: only hex values are supported
; IN:
;  - linebuffer: contains the line to parse for UDG data (.DB $xx,...)
; OUT:
;  - .C:  set if line could not be parsed
;  - UDG: contains the parsed data of the existing UDG (on success)
.proc parse_bytes
@buff=r0
@udg=r2
	ldxy #linebuffer
	stxy @buff
	ldxy #udg
	stxy @udg

	ldy #$00
@finddb:
	lda (@buff),y
	beq @err		; no .DB on this line
	cmp #$0d
	beq @err		; no .DB
	cmp #';'
	beq @err		; no .DB
	cmp #$09		; TAB
	beq @nextch
	cmp #' '
	beq @nextch
	cmp #'.'
	bne @err		; not a .DB
	iny
	lda (@buff),y
	cmp #'D'
	beq :+
	cmp #'d'
	bne @err		; not .DB
:	iny
	lda (@buff),y
	cmp #'B'
	beq @getbytes
	cmp #'b'
	beq @getbytes

@err:	sec
	rts

@nextch:
	iny
	bne @finddb

; .DB was found, parse the data
@getbytes:
	tya
	adc @buff	; +1 (.C is set)
	sta @buff

@parsebyte:
	ldy #$00
	lda (@buff),y
	beq @ok
	cmp #$0d
	beq @ok
	cmp #';'
	beq @ok

	cmp #' '
	beq @next
	cmp #$09
	beq @next

	cmp #'$'
	bne @err	; unexpected char
@hex:	incw @buff
	ldy #$01
	lda (@buff),y	; least significant hex digit
	tax
	dey
	lda (@buff),y	; most significant hex digit
	tay
	jsr parsehex
	bcs @err	; unparseable
	ldy #$00
	sta (@udg),y	; save the result
	incw @udg

	incw @buff
	incw @buff

	ldy #$00
@findcomma:
	lda (@buff),y
	beq @ok
	cmp #$0d
	beq @ok
	cmp #' '
	beq :+
	cmp #$09	; TAB
	beq :+
	cmp #','
	beq @next
	bne @err	; unexpected char
:	incw @buff
	jmp @findcomma

@next:	incw @buff
	jmp @parsebyte

@ok:	clc
	rts
.endproc

;*******************************************************************************
; INIT MC
; Initializes multicolor mode for all cells used by the UDG editor
.proc init_mc
@color=r0
	; enable multicolor for all cells
	ldxy #(COLMEM_ADDR + ((CANVAS_Y/16)*(BITMAP_WIDTH/8) + (CANVAS_X/8)))
	stxy @color

	ldx #(CANVAS_HEIGHT/16)
@l0:	ldy #(CANVAS_WIDTH/8)-1
@l1:	lda (@color),y
	eor #$08
	sta (@color),y
	dey
	bpl @l1
	lda @color
	clc
	adc #(BITMAP_WIDTH/8)
	sta @color
	bcc :+
	inc @color+1
:	dex
	bne @l0
	rts
.endproc

;*******************************************************************************
.linecont +
.define cols $1100, $11c0, $1280, $1340, $1400, $14c0, $1580, $1640, $1700, \
  $17c0, $1880, $1940, $1a00, $1ac0, $1b80, $1c40, $1d00, $1dc0, $1e80, $1f40
.linecont -
colslo: .lobytes cols
colshi: .hibytes cols
