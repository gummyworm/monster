.org $1000
	ldx #$30
	txs
	lda #$10
	pha
	lda #$ff
	pha
	plp
	pla
	nop
	jmp *

