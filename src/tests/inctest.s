.inc "kernal.inc"
.org $7a00
	lda #$31
	jsr chrout
	jmp *
