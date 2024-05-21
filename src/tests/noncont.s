.org $1200
	lda #$cf
	sta $9005
	lda #$00
	sta $1000
	jmp *
; non-contiguous memory
.org $1c00
.DB $FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF

