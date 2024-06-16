.include "bitmap.inc"
.include "config.inc"
.include "edit.inc"
.include "macros.inc"
.include "memory.inc"
.include "text.inc"
.include "zeropage.inc"

;******************************************************************************
; CONSTANTS
L_INSERT_MASK  = $80
R_INSERT_MASK  = $08
L_REPLACE_MASK = $f0
R_REPLACE_MASK = $0f

;******************************************************************************
.BSS
.export __cur_mode
__cur_mode: .byte 0	; 0 = NORMAL, 1 = SELECT

.export __cur_status
__cur_status: .byte 0	; 0 = OFF, 1 = ON

.export __cur_minx
__cur_minx:
minx: .byte 0

.export __cur_maxx
__cur_maxx:
maxx: .byte 0

.export __cur_miny
__cur_miny:
miny: .byte 0

.export __cur_maxy
__cur_maxy:
maxy: .byte 0

.CODE

;******************************************************************************
; MASK
; Returns the mask used to draw the cursor based on the current mode and
; cursor position.
; OUT:
;  - .A: the mask that will be EOR'd to draw the cursor
.proc mask
	lda zp::editor_mode
	cmp #MODE_COMMAND
	beq @replace		; use REPLACE mask for COMMAND mode
	lda text::insertmode
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

;******************************************************************************
; OFF
; Turns off the cursor if it is on. Has no effect if the cursor is already off.
.export __cur_off
__cur_off:
	lda __cur_status
	bne __cur_toggle
	rts
;******************************************************************************
; ON
; Turns on the cursor if it is off. Has no effect if the cursor is already on.
.export __cur_on
__cur_on:
	lda __cur_mode
	bne __cur_toggle	; always turn ON in SELECT mode
	lda __cur_status
	beq __cur_toggle
	rts
;******************************************************************************
; TOGGLE
; Toggles the cursor (turns it off if its on or vise-versa)
.export __cur_toggle
__cur_toggle:
@dst=zp::tmp0
	ldx zp::curx
	ldy zp::cury

	txa
	lsr
	tax

	tya
	asl
	asl
	asl
	adc bm::columnslo,x
	sta @dst
	lda #$00
	adc bm::columnshi,x
	sta @dst+1

	jsr mask
	sta @mask
	ldy #7
@mask=*+1
@l0:	lda #$ff
	eor (@dst),y
	sta (@dst),y
	dey
	bpl @l0

	lda #1
	eor __cur_status
	sta __cur_status

@done:  ldx zp::curx
	ldy zp::cury
	rts

;******************************************************************************
; UP
; Moves the cursor up a row.
; If moving up would move the cursor outside its defined limits, has no effect
.export __cur_up
.proc __cur_up
	lda zp::cury
	cmp miny
	bcs :+
	ldy miny
	ldx #$00
	jmp __cur_set
:	ldy #$ff
	ldx #$00
	jmp __cur_move
.endproc

;******************************************************************************
; UP
; Moves the cursor down a row.
; If moving down would move the cursor outside its defined limits, has no effect
.export __cur_down
.proc __cur_down
	lda zp::cury
	cmp maxy
	bcs @done
	ldy #1
	ldx #$00
	jmp __cur_move
@done:	rts
.endproc

;******************************************************************************
; RIGHT
; Moves the cursor right a column
; If moving right would move the cursor outside its limits, has no effect
.export __cur_right
.proc __cur_right
	lda zp::curx
	cmp #39
	bcs @done

	jsr text::char_index
	lda mem::linebuffer,y
	cmp #$09		; TAB
	bne :+
	jsr text::tabr_dist
	clc
	adc zp::curx
	sta zp::curx
	rts
:	inc zp::curx
@done:	rts
.endproc

;******************************************************************************
; LEFT
; Moves the cursor left a column
; If moving left would move the cursor outside its defined limits, has no effect
; OUT:
;  - .C: set if the cursor wasn't moved
.export __cur_left
.proc __cur_left
	jsr text::char_index
	cpy #$00
	beq @nomove
	lda mem::linebuffer,y
	cmp #$09		; TAB
	bne :+
	lda zp::curx
	sbc #TAB_WIDTH
	sta zp::curx
:	dec zp::curx
	bpl @done
	lda #$00
	sta zp::curx
@done:	clc
@nomove:
	rts
.endproc

;******************************************************************************
; MOVE
; Updates the cursor's (x,y) position by the offsets given
; IN:
;  - .X: the signed number of columns to move
;  - .Y: the signed number of rows to move
.export __cur_move
.proc __cur_move
@xdst=zp::tmp2
@ydst=zp::tmp3
	stx @xdst
	sty @ydst
	lda __cur_mode
	bne :+
	jsr __cur_off
:	lda @xdst
	clc
	adc zp::curx
	bmi @movey
	cmp maxx
	bcs @movey
	cmp minx
	bcc @movey
	sta zp::curx

@movey: lda @ydst
	clc
	adc zp::cury
	bmi @done
	cmp maxy
	bcs @done
	cmp miny
	bcc @done
	sta zp::cury
@done:	rts
.endproc

;******************************************************************************
; SET
; Sets the cursor position (x,y) to the values given
; IN:
;  .X: the column to set the cursor to
;  .Y: the row to set the cursor to
.export __cur_set
.proc __cur_set
	cpx maxx
	bcc :+
	ldx maxx
	dex
:	cpx minx
	bcs :+
	ldx minx
:	stx zp::tmp2

	cpy maxy
	bcc :+
	ldy maxy
	dey
:	cpy miny
	bcs :+
	ldy miny
:	sty zp::tmp3

	jsr __cur_off

	ldx zp::tmp2
	ldy zp::tmp3
	stx zp::curx
	sty zp::cury
	rts
.endproc

;******************************************************************************
; FORCESET
; Sets the cursor X and Y without respecting limits
; IN:
;  - .X: the column to set the cursor to
;  - .Y: the row to set the cursor to
.export __cur_forceset
.proc __cur_forceset
	stx zp::curx
	sty zp::cury
	rts
.endproc

;******************************************************************************
; SETMAX
; Sets the maximum values for the cursor's X and Y values
; IN:
;  - .X: the column limit to set for the cursor
;  - .Y: the row limit to set for the cursor
.export __cur_setmax
.proc __cur_setmax
	stx maxx
	sty maxy
	rts
.endproc

;******************************************************************************
; SETMIN
; Sets the minimum values for the cursor's X and Y values
; IN:
;  - .X: the column limit to set for the cursor
;  - .Y: the row limit to set for the cursor
.export __cur_setmin
.proc __cur_setmin
	stx minx
	sty miny
	rts
.endproc

;******************************************************************************
; UNLIMIT
.export __cur_unlimit
.proc __cur_unlimit
	ldxy #$00
	jsr __cur_setmin
	ldx #40
	ldy #23
	jmp __cur_setmax
.endproc

