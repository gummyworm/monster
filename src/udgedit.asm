.include "bitmap.inc"
.include "cursor.inc"
.include "key.inc"
.include "keycodes.inc"
.include "macros.inc"
.include "source.inc"
.include "util.inc"
.include "zeropage.inc"

;******************************************************************************
PIXEL_SIZE = 4		; size of each pixel in the editor

CANVAS_Y      = 40		; start row (in pixels)
CANVAS_X      = 24		; start column (in pixels)
CANVAS_HEIGHT = 8*8*PIXEL_SIZE
CANVAS_WIDTH  = 8*8*4*PIXEL_SIZE

BORDER_SIZE = 4		; border around editor (in pixels)

color = zp::editortmp
udg = zp::editortmp+1

.CODE
;******************************************************************************
; ENTER
; Activates the UDG editor
; OUT:
;  - .XY: the address of the newly created graphic
;  - .Z: clear if the user quit the editor without creating a character
;        set if the user did create a new UDG
.export __udgedit_enter
.proc __udgedit_enter
	jsr bm::save
	pushcur

	jsr clrcanvas

@main:	jsr key::getch
	jsr handlekey
	jmp @main

@done:	pha			; save the quit key (QUIT or RETURN)
	jsr bm::restore
	popcur
	pla
	cmp #K_RETURN		; did we confirm UDG creation?
	bne @ret		; if not, we're done

@ret:	sec			; no graphic created
	rts

.endproc

;******************************************************************************
; WRITEOUT
; writes the UDG as .DB bytes to the source code
@writeresult:
@cnt=zp::tmp4
	lda #4
	sta @cnt

	; write the .DB directive
:	ldx @cnt
	lda @db,x
	jsr src::insert
	dec @cnt
	bpl :-

	; now write the data $xx,$xx,$xx
	inc @cnt
:	lda #'$'
	jsr src::insert
	ldx @cnt
	lda udg,x
	jsr util::hextostr
	txa
	pha
	tya
	jsr src::insert
	pla
	jsr src::insert
	inc @cnt
	lda @cnt
	cmp #$08
	beq @done
	lda #','
	jsr src::insert
	jmp :-

@done:	rts
@db: .byte " bd."

;******************************************************************************
; HANDLEKEY
.proc handlekey
	ldx #@numkeys-1
:	cmp @keys,x
	beq @handle
	dex
	bpl :-
	rts
@handle:
	lda @handlerslo,x
	sta zp::jmpvec
	lda @handlershi,x
	sta zp::jmpvec+1
	jmp zp::jmpvec
@keys:
.byte K_UP, K_DOWN, K_LEFT, K_RIGHT, '1'
@numkeys=*-@keys
@handlerslo:
.lobytes up, down, left, right, plot
@handlershi:
.hibytes up, down, left, right, plot
.endproc

;******************************************************************************
; CLRCANVAS
; Clears the 8x8 UDG canvas and the underlying character data
.proc clrcanvas
@dst=r0
	; clear the character buffer
	lda #$00
	ldx #$07
:	sta udg,x
	dex
	bpl :-

	; clear the bitmap area of the canvas
	ldxy #(BITMAP_ADDR+(192*(CANVAS_X/8))-1)
	stxy @dst
	ldx #CANVAS_WIDTH/8
@l0:	lda #$00
	ldy #CANVAS_HEIGHT-CANVAS_Y
:	sta (@dst),y
	dey
	bne :-
	dex
	beq @done
	lda @dst
	clc
	adc #192
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
.endproc

;******************************************************************************
; CURON
; Turns on the cursor
.proc curon
.endproc


;******************************************************************************
; CURTOGGLE
; Toggles the cursor
.proc curtoggle
@dst=r0
	lda zp::curx
	and #$01
	bne @oddcol
@evencol:
	lda #$f0
	eor (@dst),y
	lda #$90
	eor (@dst),y
	iny
	eor (@dst),y
	iny
	lda #$f0
	eor (@dst),y
	rts
@oddcol:
	lda #$0f
	eor (@dst),y
	lda #$09
	eor (@dst),y
	iny
	eor (@dst),y
	iny
	lda #$0f
	eor (@dst),y
	rts
.endproc

;******************************************************************************
; PLOT
; Plots the given (x,y) coordinate on the canvas
.proc plot
@dst=r0
	lda zp::curx
	asl
	asl
	lda bm::columns+(CANVAS_X/8),x
	adc #CANVAS_Y
	sta @dst
	lda bm::columns+1,x
	sta @dst+1

	lda zp::curx
	and #$01
	bne :+
	lda #$f0	; even mask
	skw
:	lda #$0f	; odd mask

; draw the pixel
	ldy #3
:	ora (@dst),y
	dey
	bpl :-
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
