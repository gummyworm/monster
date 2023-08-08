.include "zeropage.inc"

.CODE
;--------------------------------------
;16-bit multiply with 32-bit product
;from 6502.org
.export __math_mul16
.proc __math_mul16
@multiplier	= zp::tmp0
@multiplicand	= zp::tmp2
@product	= zp::tmpa
	lda	#$00
	sta	@product+2	; clear upper bits of product
	sta	@product+3
	ldx	#$10		; set binary count to 16
@shift_r:
	lsr	@multiplier+1	; divide multiplier by 2
	ror	@multiplier
	bcc	@rotate_r
	lda	@product+2	; get upper half of product and add multiplicand
	clc
	adc	@multiplicand
	sta	@product+2
	lda	@product+3
	adc	@multiplicand+1
@rotate_r:
	ror			; rotate partial product
	sta	@product+3
	ror	@product+2
	ror	@product+1
	ror	@product
	dex
	bne	@shift_r
	ldx @product
	ldy @product+1
	rts
.endproc


;--------------------------------------
.export __math_div16
.proc __math_div16
@divisor = zp::tmp2
@dividend = zp::tmp0
@remainder = zp::tmpa
@result = @dividend ;save memory by reusing divident to store the result

	lda #0	        ;preset remainder to 0
	sta @remainder
	sta @remainder+1
	ldx #16	        ;repeat for each bit: ...

@divloop:
	asl @dividend	;dividend lb & hb*2, msb -> Carry
	rol @dividend+1
	rol @remainder	;remainder lb & hb * 2 + msb from carry
	rol @remainder+1
	lda @remainder
	sec
	sbc @divisor	;substract divisor to see if it fits in
	tay	        ;lb result -> Y, for we may need it later
	lda @remainder+1
	sbc @divisor+1
	bcc @skip	;if carry=0 then divisor didn't fit in yet

	sta @remainder+1	;else save substraction result as new remainder,
	sty @remainder
	inc @result	;and INCrement result cause divisor fit in 1 times

@skip:
	dex
	bne @divloop
	rts
.endproc
