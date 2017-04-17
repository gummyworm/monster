.include "bitmap.inc"
.include "zeropage.inc"

;--------------------------------------
.export __cur_on
.proc __cur_on
@dst=zp::tmp0
	lda curstatus
	bne @done

	lda curx
	asl
	tax
	lda bm::coluns,x
	sta @dst
	lda bm::coluns+1,x
	sta @dst+1

	ldy #7
:	lda #$ff
	eor (@dst),y
	sta (@dst),y
	dey
	bpl :-
	sta curstatus
@done:	rts
.endproc

;--------------------------------------
.export __cur_off
.proc __cur_off
	lda curstatus
	beq @done
	inc curstatus
	jsr __text_curon
	inc curstatus
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
	jsr __cur_off
	ldx zp::tmp2
	ldy zp::tmp3
	stx zp::curx
	sty zp::cury
	jsr __cur_on
	rts
.endproc

.byte curstatus 0
