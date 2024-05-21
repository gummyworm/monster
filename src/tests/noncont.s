.org $1200
	jsr $e5b5	; INIT VIC
	jsr $e55f	; CLRSCR
	lda #$cf
	sta $9005
	lda #$00
	sta $1000
	lda #$00
	sta $9400
	jmp *
; non-contiguous memory
.org $1c00
.DB $FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF

