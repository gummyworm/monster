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
		cpx #<(.right( .tcount( {arg} )-1, {arg} ))
		bne @skip
		cpy #>(.right( .tcount( {arg} )-1, {arg} ))
	.else
		cpx arg
		bne @skip
		cpy arg+1
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

; skw skips execution of the next word (equivalent to .byte $2C (bit ABS))
.macro skw
	.byte $2c
.endmacro
