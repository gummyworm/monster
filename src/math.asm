.include "zeropage.inc"

.CODE

;******************************************************************************
; MUL8
; 16*8-bit multiply with 16-bit product
; by WhiteFlame
; https://codebase64.org/doku.php?id=base:8bit_multiplication_16bit_product
; IN:
;  - .AY: multiplicand
;  - .X: multiplier
; OUT:
;  - .AY: the product
.export __math_mul8
.proc __math_mul8
@num1=zp::math
@num2=zp::math+2
	sta @num1
	sty @num1+1
	stx @num2

	lda #$00
	tay
	beq @enterLoop

@doAdd:	clc
	adc @num1
	tax

	tya
	adc @num1+1
	tay
	txa

@loop:  asl @num1
	rol @num1+1
@enterLoop:
	lsr @num2
	bcs @doAdd
	bne @loop
	rts
.endproc

;******************************************************************************
; MUL16
;16-bit multiply with 32-bit product
;from 6502.org
; IN:
;  - r0: the multiplier
;  - r2: the multiplicand
; OUT:
;  - ra: the product
.export __math_mul16
.proc __math_mul16
@multiplier	= r0
@multiplicand	= r2
@product	= ra
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

;******************************************************************************
; DIV16
; Divides the given divisor by the given dividend
; IN:
;  - r2: the divisor
;  - r0: the dividend
; OUT:
;  - ra: the remainder
;  - r0: the quotient
.export __math_div16
.proc __math_div16
@divisor = r2
@dividend = r0
@remainder = r4
@result = @dividend	;return quotient in dividend's place
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

@skip:	dex
	bne @divloop
	rts
.endproc
