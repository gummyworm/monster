.org $7a00
start
	; addition
	lda #1
	lda #1+1
	lda #1+1+1
	lda #1-1
	;lda #1-1-1

	lda #>$100+<$102
	lda #>($1e00+2+5)

	lda #*-start
	lda #(*-start)/2

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

	lda #>$10f0
	lda #<$f010

	lda #>$110+$210
	lda #<($08*$100)

	lda <($08*$100-10*5)

	jmp *
