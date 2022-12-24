.import test_all

;--------------------------------------
.segment "SETUP"
.word @head
@head: .word @next
.word .version
.byte $9e
.asciiz "4621"
@next: .word 0

;--------------------------------------
start:
	jsr test_all

	; ok
	jsr $E55F ; clear screen
	lda #'o'
	jsr $ffd2
	lda #'k'
	jmp $ffd2
