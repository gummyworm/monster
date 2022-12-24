.include "bitmap.inc"
.include "zeropage.inc"
.include "text.inc"
.CODE

L_INSERT_MASK=$80
R_INSERT_MASK=$08
L_REPLACE_MASK=$f0
R_REPLACE_MASK=$0f

;--------------------------------------
; mask returns the mask used to draw the cursor based on the current mode and
; cursor position.
.proc mask
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

;--------------------------------------
.export __cur_off
__cur_off:
	lda curstatus
	bne __cur_toggle
	rts
.export __cur_on
__cur_on:
	lda curstatus
	beq __cur_toggle
	rts
.export __cur_toggle
__cur_toggle:
@dst=zp::tmp0
	lda zp::curx
	and #$fe
	tax
	lda zp::cury
	asl
	asl
	asl
	adc bm::columns,x
	sta @dst
	lda #$00
	adc bm::columns+1,x
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
	eor curstatus
	sta curstatus
	rts

;--------------------------------------
.export __cur_up
.proc __cur_up
	lda zp::cury
	bne :+
	tay
	ldx #$00
	jsr __cur_set
	rts
:	ldy #$ff
	ldx #$00
	jsr __cur_move
	rts
.endproc

;--------------------------------------
.export __cur_down
.proc __cur_down
	lda zp::cury
	cmp #19
	bcs @done
	ldy #1
	ldx #$00
	jsr __cur_move
@done:	rts
.endproc

;--------------------------------------
.export __cur_right
.proc __cur_right
	lda zp::curx
	cmp #39
	bcs @done
	ldy #0
	ldx #1
	jsr __cur_move
@done:	rts
.endproc

;--------------------------------------
.export __cur_left
.proc __cur_left
	lda zp::curx
	beq @done
	ldy #0
	ldx #$ff
	jsr __cur_move
@done:	rts
.endproc


;--------------------------------------
; move updates the cursor's (x,y) position by the offsets given in (.X,.Y).
.export __cur_move
.proc __cur_move
	stx zp::tmp2
	sty zp::tmp3
	jsr __cur_off

	lda zp::tmp2
	clc
	adc zp::curx
	bmi @movey
	cmp maxx
	bcs @movey
	cmp minx
	bcc @movey
	sta zp::curx

@movey: lda zp::tmp3
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

;--------------------------------------
; set sets the cursor position (x,y) to the values given in (.X,.Y)
.export __cur_set
.proc __cur_set
	stx zp::tmp2
	sty zp::tmp3
	jsr __cur_off

	ldx zp::tmp2
	ldy zp::tmp3
	stx zp::curx
	sty zp::cury
	rts
.endproc

;--------------------------------------
; setmax sets the maximum values for the cursor's X and Y values
.export __cur_setmax
.proc __cur_setmax
	stx maxx
	sty maxy
	rts
.endproc

;--------------------------------------
; setmin sets the minimum values for the cursor's X and Y values
.export __cur_setmin
.proc __cur_setmin
	stx minx
	sty miny
	rts
	rts
.endproc

;--------------------------------------
; unlimit allows the cursor to move anywhwere on the display
.export __cur_unlimit
.proc __cur_unlimit
	lda #$00
	sta minx
	sta miny
	lda #40
	sta maxx
	lda #23
	sta maxy
	rts
.endproc

curstatus: .byte 0
minx: .byte 0
maxx: .byte 40
miny: .byte 0
maxy: .byte 24
