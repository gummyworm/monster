.org $7700
c:
    adc #$00
    adc $10
    adc $20,x
    adc $3030
    adc $4040,x
    adc $5050,y
    adc ($60,x)
    adc ($70),y
    and #$80
    and $90
    and $10,x
    and $1111
    and $1212,x
    and $1313,y
    and ($14,x)
    and ($15),y
    asl
    asl $17
    asl $18,x
    asl $1919
    asl $2020,x
    ;bcc *+2
    ;bcs *+2
    ;beq *+2
    ;bit $24
    ;bit $2525
    ;bmi *+2
    ;bne *+2
    ;bpl *+2
    ;brk
    ;bvc *+2
    ;bvs *+2
    clc
    cld
    cli
    clv
    cmp #$36
    cmp $37
    cmp $38,x
    cmp $3939
    cmp $4040,x
    cmp $4141,y
    cmp ($42,x)
    cmp ($43),y
    cpx #$44
    cpx $45
    cpx $4646
    cpy #$47
    cpy $48
    cpy $4949
    dec $50
    dec $51,x
    dec $5252
    dec $5353,x
    dex
    dey
    eor #$56
    eor $57
    eor $58,x
    eor $5959
    eor $6060,x
    eor $6161,y
    eor ($62,x)
    eor ($63),y
    inc $64
    inc $65,x
    inc $6666
    inc $6767,x
    inx
    iny
    jmp *+3
    jmp (*+3)
.dw next
next:
    jsr *+3
    lda #$73
    lda $74
    lda $75,x
    lda $7676
    lda $7777,x
    lda $7878,y
    lda ($79,x)
    lda ($80),y
    ldx #$81
    ldx $82
    ldx $83,y
    ldx $8484
    ldx $8585,y
    ldy #$86
    ldy $87
    ldy $88,x
    ldy $8989
    ldy $9090,x
    lsr
    lsr $92
    lsr $93,x
    lsr $9494
    lsr $9595,x
    nop
    ora #$97
    ora $98
    ora $99,x
    ora $1000
    ora $1010,x
    ora $2020,y
    ora ($30,x)
    ora ($40),y
    pha
    php
    pla
    plp
    rol
    rol $10
    rol $11,x
    rol $1212
    rol $1313,x
    ror
    ror $15
    ror $16,x
    ror $1717
    ror $1818,x
    rti
    rts
    sbc #$21
    sbc $22
    sbc $23,x
    sbc $2424
    sbc $2525,x
    sbc $2626,y
    sbc ($27,x)
    sbc ($28),y
    sec
    sed
    sei
    sta $32
    sta $33,x
    sta $3434
    sta $3535,x
    sta $3636,y
    sta ($37,x)
    sta ($38),y
    stx $39
    stx $40,y
    stx $4141
    sty $42
    sty $43,x
