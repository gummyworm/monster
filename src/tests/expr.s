.org $7a00
	; addition
	lda #1
	lda #1+1
	lda #1+1+1
	lda #1-1
	;lda #1-1-1

	; subtraction
	lda #9-(3+3)

	; multiplication
	lda #2*3
	lda #2*2*2

	; division
	lda #100/5
	
	; bitwise ops
	lda #$a0.$07
	lda #$77&$33
	lda #$f0^$f3
	
	; order of operations
	lda #1+2*4
	lda #(1+2)*4

	; misc
	lda *+3

	jmp *
