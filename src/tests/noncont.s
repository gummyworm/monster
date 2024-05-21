.org $1200
	lda #$cf
	sta $9005
	lda #$00
	sta $1000
	jmp *
.org $1c00
.db $ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff

