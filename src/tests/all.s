; ALL.S
; test for all opcodes
; cycle counts (and total cycle count)
; in comments
.org $7700
c:
    adc #$00	; 2
    adc $10	; 3 (5)
    adc $20,x	; 4 (9)
    adc $3030	; 4 (13)
    adc $4040,x	; 4* (17)
    adc $5050,y	; 4* (21)
    adc ($60,x)	; 6 (27)
    adc ($70),y	; 5* (32)
    and #$80	; 2 (34)
    and $90	; 3 (37)
    and $10,x	; 4 (41)
    and $1111	; 4 (45)
    and $1212,x	; 4* (49)
    and $1313,y ; 4* (53)
    and ($14,x)	; 6 (59)
    and ($15),y	; 5* (64)
    asl		; 2 (66)
    asl $17	; 5 (71)
    asl $18,x	; 6 (77)
    asl $1919	; 6 (83)
    asl $2020,x	; 7 (90)
    bcc *+2	; 2* (92)
    bcs *+2	; 2+1 (95)
    beq *+2	; 2* (97)
    bit $24	; 3 (100)
    bit $2525	; 4 (104)
    bmi *+2	; 2+1 (107)
    bne *+2	; 2* (109)
    bpl *+2	; 2* (111)
    ;brk
    bvc *+2	; 2* (113)
    bvs *+2	; 2+1 (116)
    clc		; 2 (118)
    cld		; 2 (120)
    cli		; 2 (122)
    clv		; 2 (124)
    cmp #$36	; 2 (126)
    cmp $37	; 3 (129)
    cmp $38,x	; 4 (133)
    cmp $3939	; 4 (137)
    cmp $4040,x	; 4* (141)
    cmp $4141,y	; 4* (145)
    cmp ($42,x)	; 6 (151)
    cmp ($43),y	; 5* (156)
    cpx #$44	; 2 (158)
    cpx $45	; 3 (161)
    cpx $4646	; 4 (165)
    cpy #$47	; 2 (167)
    cpy $48	; 3 (170)
    cpy $4949	; 4 (174)
    dec $50	; 5 (179)
    dec $51,x	; 6 (185)
    dec $5252	; 6 (191)
    dec $5353,x	; 7 (198)
    dex		; 2 (200)
    dey		; 2 (202)
    eor #$56	; 2 (204)
    eor $57	; 3 (207)
    eor $58,x	; 4 (211)
    eor $5959	; 4 (215)
    eor $6060,x	; 4* (219)
    eor $6161,y	; 4* (223)
    eor ($62,x)	; 6 (229)
    eor ($63),y	; 5* (234)
    inc $64	; 5 (239)
    inc $65,x	; 6 (245)
    inc $6666	; 6 (251)
    inc $6767,x	; 7 (258)
    inx		; 2 (260)
    iny		; 2 (262)
    jmp *+3	; 3 (265)
    jmp (*+3)	; 5 (270)
.dw next
next:
    jsr *+3	; 6 (276)
    lda #$73	; 2 (278)
    lda $74	; 3 (281)
    lda $75,x	; 4 (285)
    lda $7676	; 4 (289)
    lda $7777,x	; 4* (293)
    lda $7878,y	; 4* (297)
    lda ($79,x)	; 6 (303)
    lda ($80),y	; 5* (308)
    ldx #$81	; 2 (310)
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
    ;rti
    ;rts
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
