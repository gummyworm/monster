.org $1400
main:
	jsr $e5b5	; INIT VIC
	jsr $e55f	; CLRSCR
	ldx #0
loop:
	lda msg,x
	beq done
	jsr $ffd2
	inx
	bne loop
done:
	jmp *
msg:
	.db "hello world!",0
