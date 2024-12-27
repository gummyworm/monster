.org $2000
start:
	lda #$01
	jsr myproc
	jsr myproc2
	jmp *

myproc:
	asl
	asl
	asl
	rts

myproc2:
	asl
	asl
	asl
	rts
