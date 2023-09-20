.org $3000
global1:
	lda #$00
@local:
	sta $1000
	bne @local
global2:
	lda #$20
@local:
	sta $1001
	beq @local
