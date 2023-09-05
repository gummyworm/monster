.org $7600
.IFDEF NTSC
	.eq val 100
.ELSE
	.eq val 120
.ENDIF
	lda #val
	sta $1000
	jmp *
