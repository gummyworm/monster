.include "zeropage.inc"

;--------------------------------------
; memset sets zp::tmp0 bytes of the memory at (YX) to .A.
.export __util_memset
.proc __util_memset
	stx zp::tmp1
	sty zp::tmp1+1
	ldy zp::tmp0
@l0:	sta (zp::tmp1),y
	dey
	bpl @l0

	rts
.endproc

;--------------------------------------
; memmove moves zp::tmp0 bytes from (zp::tmp2) to (zp::tmp4).
.export __util_memcpy
.proc __util_memcpy
	ldy #$00

	lda zp::tmp4
	clc
	adc zp::tmp0
	sta zp::tmp4
	lda zp::tmp4+1
	adc zp::tmp0+1
	sta zp::tmp4+1
	
	lda zp::tmp2
	clc
	adc zp::tmp0
	sta zp::tmp2
	lda zp::tmp2+1
	adc zp::tmp0+1
	sta zp::tmp2+1

	ldy #$00
@l0:	lda (zp::tmp2),y
	sta (zp::tmp4),y

	lda zp::tmp2
	bne :+
	dec zp::tmp2+1
:	dec zp::tmp2

	lda zp::tmp4
	bne :+
	dec zp::tmp4+1
:	dec zp::tmp4

	lda zp::tmp0
	bne :+
	dec zp::tmp0+1
	bpl :+
	rts
:	dec zp::tmp0
	jmp @l0
.endproc
