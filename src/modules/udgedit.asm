.include "../keycodes.inc"
.include "../macros.inc"
.include "../zeropage.inc"

;******************************************************************************
BITMAP_ADDR = $1100
PIXEL_SIZE = 4		; size of each pixel in the editor

CANVAS_Y      = 40		; start row (in pixels)
CANVAS_X      = 24		; start column (in pixels)
CANVAS_HEIGHT = 8*PIXEL_SIZE
CANVAS_WIDTH  = 8*8*4*PIXEL_SIZE

BORDER_SIZE = 4		; border around editor (in pixels)

color   = zp::editortmp
cur_on  = zp::editortmp+1	; cursor on flag
cur_tmr = zp::editortmp+2	; cursor blink timer
udg     = zp::editortmp+3

.CODE
.word @header
@header:

;******************************************************************************
; ENTER
; Activates the UDG editor
; OUT:
;  - r0-r7: the character that the user created
;  - .Z:    clear if the user quit the editor without creating a character
;        set if the user did create a new UDG
.export __udgedit_enter
.proc __udgedit_enter
	cli
	jsr clrcanvas
	lda #$00
	sta zp::curx
	sta zp::cury

@main:	dec cur_tmr
	bne :+
	jsr curtoggle

:	jsr $f1f9		; get key
	cmp #$00
	beq @main
	cmp #K_RETURN
	beq @ok
	cmp #K_QUIT
	beq @ret
	jsr handlekey
	jmp @main

@ok:	jsr curoff
	clc
	rts

@ret:	jsr curoff
	sec			; no graphic created
	rts
.endproc

;******************************************************************************
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
	.byte $4b, $4a, $48, $4c, '1'	; k, j, h, l
@numkeys=*-@keys

.define handlers up, down, left, right, plot
@handlerslo: .lobytes handlers
@handlershi: .hibytes handlers
.endproc

;******************************************************************************
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
	ldxy #BITMAP_ADDR+($c0*(CANVAS_X/8))+CANVAS_Y-1
	stxy @dst
	ldx #CANVAS_WIDTH/8

@l0:	lda #$00
	ldy #CANVAS_HEIGHT
:	sta (@dst),y
	dey
	bne :-

	dex
	beq @done
	lda @dst
	clc
	adc #$c0	; next col
	sta @dst

	bcc @l0
	inc @dst+1
	bne @l0

@done:	rts
.endproc

;******************************************************************************
; CUROFF
; Turns off the cursor
.proc curoff
	lda cur_on
	beq @done	; already off
	jsr curtoggle
@done:	rts
.endproc

;******************************************************************************
; CURON
; Turns on the cursor
.proc curon
	lda cur_on
	bne @done	; already on
	jsr curtoggle
@done:	rts
.endproc

;******************************************************************************
; CURTOGGLE
; Toggles the cursor
.proc curtoggle
@dst=r0
	lda cur_on
	eor #$01
	sta cur_on

	lda zp::curx
	lsr
	tax

	lda colslo+(CANVAS_X/8),x
	clc
	adc #CANVAS_Y
	sta @dst
	lda colshi+(CANVAS_X/8),x
	sta @dst+1

	lda zp::cury
	asl
	asl
	tay
	lda zp::curx
	and #$01
	bne @oddcol

@evencol:
	lda #$f0
	eor (@dst),y
	sta (@dst),y
	iny
	lda #$90
	eor (@dst),y
	sta (@dst),y
	iny
	lda #$90
	eor (@dst),y
	sta (@dst),y
	iny
	lda #$f0
	eor (@dst),y
	sta (@dst),y
	rts

@oddcol:
	lda #$0f
	eor (@dst),y
	sta (@dst),y
	iny
	lda #$09
	eor (@dst),y
	sta (@dst),y
	iny
	lda #$09
	eor (@dst),y
	sta (@dst),y
	iny
	lda #$0f
	eor (@dst),y
	sta (@dst),y
	rts
.endproc

;******************************************************************************
; PLOT
; Plots the given (x,y) coordinate on the canvas
.proc plot
@dst=r0
@mask=r2
	jsr curoff
	lda zp::curx
	lsr
	tax

	lda colslo+(CANVAS_X/8),x
	clc
	adc #CANVAS_Y
	sta @dst
	lda colshi+(CANVAS_X/8),x
	sta @dst+1

	lda zp::cury
	asl
	asl
	tay

	lda zp::curx
	and #$01
	bne :+
	lda #$f0	; even mask
	skw
:	lda #$0f	; odd mask
	sta @mask

; draw the pixel
	ldx #4
:	lda @mask
	ora (@dst),y
	sta (@dst),y
	iny
	dex
	bne :-
	rts
.endproc

;******************************************************************************
; SETPIXEL
; Sets the pixel at the cursor to the active color
.proc setpixel
	; render the pixel on the canvas

	; update the UDG pixel data
	ldx zp::curx
	ldy zp::cury
	lda udg,y
	ora $8314,x	; charrom '/' (mask associated with pixel)
	sta udg,y
	rts
.endproc

;******************************************************************************
; RIGHT
; Handle the "move right" behavior
.proc right
	jsr curoff
	lda zp::curx
	cmp #7
	bcs :+
	inc zp::curx
:	rts
.endproc

;******************************************************************************
; LEFT
; Handle the "move left" behavior
.proc left
	jsr curoff
	dec zp::curx
	bpl :+
	inc zp::curx
:	rts
.endproc

;******************************************************************************
; DOWN
; Handle the "move down" behavior
.proc down
	jsr curoff
	lda zp::cury
	cmp #7
	bcs :+
	inc zp::cury
:	rts
.endproc

;******************************************************************************
; UP
; Handle the "move up" behavior
.proc up
	jsr curoff
	dec zp::cury
	bpl :+
	inc zp::cury
:	rts
.endproc

;******************************************************************************
.linecont +
.define cols $1100, $11c0, $1280, $1340, $1400, $14c0, $1580, $1640, $1700, \
  $17c0, $1880, $1940, $1a00, $1ac0, $1b80, $1c40, $1d00, $1dc0, $1e80, $1f40
.linecont -
colslo: .lobytes cols
colshi: .hibytes cols
