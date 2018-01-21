.include "bitmap.inc"
.include "zeropage.inc"
.include "text.inc"

L_INSERT_MASK=$80
R_INSERT_MASK=$08

L_REPLACE_MASK=$f0
R_REPLACE_MASK=$0f

;--------------------------------------
; mask returnst the mask used to draw the cursor
.proc mask
	lda text::insertmode
	beq @replace
@insert:
	lda zp::curx
	beq :+
	lda #R_INSERT_MASK
	rts
:	lda #L_INSERT_MASK
	rts
@replace:
	lda zp::curx
	beq :+
	lda #R_REPLACE_MASK
	rts
:	lda #L_REPLACE_MASK
	rts
.endproc

;--------------------------------------
.export __cur_on
.proc __cur_on
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

@done:	rts
.endproc

;--------------------------------------
.export __cur_off
.proc __cur_off
	lda curstatus
	beq @done
	inc curstatus
	jsr __cur_on
	lda #$00
	sta curstatus
@done:	rts
.endproc

;--------------------------------------
.export __cur_move
.proc __cur_move
	stx zp::tmp2
	sty zp::tmp3
	jsr __cur_off

	lda zp::tmp2
	clc
	adc zp::curx
	sta zp::curx
	lda zp::tmp3
	clc
	adc zp::cury
	sta zp::cury
	jsr __cur_on
	rts
.endproc

;--------------------------------------
.export __cur_set
.proc __cur_set
	stx zp::tmp2
	sty zp::tmp3
	;jsr __cur_off
	ldx zp::tmp2
	ldy zp::tmp3
	stx zp::curx
	sty zp::cury
	jsr __cur_on
	rts
.endproc

curstatus: .byte 0
