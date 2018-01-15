; incw increments the 16-bit value at the address w.
.macro incw w
.local @skip
	inc w
	bne @skip
	inc w+1
@skip:
.endmacro

; decw decrements the 16-bit value at the address w.
; clobers .A
.macro decw w
.local @skip
	lda w
	bne @skip
	dec w+1
@skip:	dec w
.endmacro

; cmpw compares (YX) to the 16-bit value. The Z flag is set if the two
; are equal
.macro cmpw w
	cpx w
	bne @skip
	cpy w+1
@skip:
.endmacro

; ldxy loads .X with the LSB of arg and .Y with the MSB.
.macro ldxy arg
	.if .match( .left( 1, {arg} ), # )
		ldy #<(.right( .tcount( {arg} )-1, {arg} ))
		ldx #>(.right( .tcount( {arg} )-1, {arg} ))
	.else
		ldy (arg)
		ldx (arg)+1
	.endif
.endmacro

