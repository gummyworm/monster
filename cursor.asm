.include "bitmap.inc"
.include "zeropage.inc"

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

	lda zp::curx
	and #$01
	beq :+
	lda #$ff
:	eor #$f0
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
