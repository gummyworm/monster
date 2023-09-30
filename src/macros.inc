; incw increments the 16-bit value at the address w.
.macro incw w
.local @skip
	inc w
	bne @skip
	inc w+1
@skip:
.endmacro

; decw decrements the 16-bit value at the address w.
; clobbers .A
.macro decw w
.local @skip
	lda w
	bne @skip
	dec w+1
@skip:	dec w
.endmacro

; cmpw compares (YX) to the 16-bit value. The Z flag is set if the two
; are equal
.macro cmpw arg
.local @skip
	.if .match( .left( 1, {arg} ), # )
		cpy #>(.right( .tcount( {arg} )-1, {arg} ))
		bne @skip
		cpx #<(.right( .tcount( {arg} )-1, {arg} ))
	.else
		cpy arg+1
		bne @skip
		cpx arg
	.endif
@skip:
.endmacro

; ldxy loads .X with the LSB of arg and .Y with the MSB.
.macro ldxy arg
	.if .match( .left( 1, {arg} ), # )
		ldx #<(.right( .tcount( {arg} )-1, {arg} ))
		ldy #>(.right( .tcount( {arg} )-1, {arg} ))
	.else
		ldx arg
		ldy arg+1
	.endif
.endmacro

.macro stxy arg
	stx arg
	sty arg+1
.endmacro

.macro add16 arg
	txa
	clc
	.if .match( .left( 1, {arg} ), # )
		adc #<(.right( .tcount( {arg} )-1, {arg} ))
	.else
		adc arg
	.endif
	tax
	tya
	.if .match( .left( 1, {arg} ), # )
		adc #>(.right( .tcount( {arg} )-1, {arg} ))
	.else
		adc arg+1
	.endif
	tay
.endmacro

.macro sub16 arg
	txa
	sec
	.if .match( .left( 1, {arg} ), # )
		sbc #<(.right( .tcount( {arg} )-1, {arg} ))
	.else
		sbc arg
	.endif
	tax
	tya
	.if .match( .left( 1, {arg} ), # )
		sbc #>(.right( .tcount( {arg} )-1, {arg} ))
	.else
		sbc arg+1
	.endif
	tay
.endmacro

.macro skb
	.byte $24
.endmacro

; skw skips execution of the next word (equivalent to .byte $2C (bit ABS))
.macro skw
	.byte $2c
.endmacro

; iszero sets the .Z flag if the operand is 0
; .A is destroyed
.macro iszero arg
	lda arg
	ora arg+1
.endmacro

.macro pushregs
	pha
	txa
	pha
	tya
	pha
.endmacro

.macro popregs
	pla
	tay
	pla
	tax
	pla
.endmacro
