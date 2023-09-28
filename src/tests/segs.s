.org $3000
a:
 lda #$00
 sta $1000
 jmp b
.org $3010
b:
 lda #$10
 sta $1001
 jmp c
.org $3020
c:
 lda #$20
 sta $1002
 jmp *
