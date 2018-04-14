.include "asm.inc"
.include "codes.inc"
.include "zeropage.inc"
.include "source.inc"
.include "test_macros.inc"

TEST = 1

.import getaddrmode
.import getvalue
.import getlabel

.export test
.proc test
	jsr src::test
	rts
	jsr test_labels
	jsr test_tokenize
	jsr test_getaddrmode
	jsr test_getvalue
	rts
.endproc

;--------------------------------------
.proc cmpstr
; returns .A=0 if (zp::tmp0) == (zp::tmp2) for .A bytes
	tay
@l0:	dey
	bpl :+
	lda #$00	; ==
	rts

:	lda (zp::tmp0),y
	cmp (zp::tmp2),y
	beq @l0

	lda #$ff	; !=
	rts
.endproc

;--------------------------------------
.export test_tokenize
test_tokenize:
	; test LDA #$00
	ldx #<@testop
	ldy #>@testop
	jsr asm::tokenize
	lda asm::result
	cmp #$a9
	assertz
	lda asm::result+1
	cmp #$00
	assertz

	; test RTI
	ldx #<@testop2
	ldy #>@testop2
	jsr asm::tokenize
	lda asm::result
	cmp #$40
	assertz

	; test STA ($a2,x)
	ldx #<@testop3
	ldy #>@testop3
	jsr asm::tokenize
	lda asm::result
	cmp #$81
	assertz
	lda asm::result+1
	cmp #$a2
	assertz

	; test ORA ($f0),y
	ldx #<@testop4
	ldy #>@testop4
	jsr asm::tokenize
	lda asm::result
	cmp #$11
	assertz
	lda asm::result+1
	cmp #$f0
	assertz

	; test AND $f193,y
	ldx #<@testop5
	ldy #>@testop5
	jsr asm::tokenize
	lda asm::result
	cmp #$39
	assertz
	lda asm::result+1
	cmp #$93
	assertz
	lda asm::result+2
	cmp #$f1
	assertz

	; test JMP ($0120)
	ldx #<@testop6
	ldy #>@testop6
	jsr asm::tokenize
	lda asm::result
	cmp #$6c
	assertz
	lda asm::result+1
	cmp #$20
	assertz
	lda asm::result+2
	cmp #$01
	assertz

	; test JSR $1234
	ldx #<@testop7
	ldy #>@testop7
	jsr asm::tokenize
	lda asm::result
	cmp #$20
	assertz
	lda asm::result+1
	cmp #$34
	assertz
	lda asm::result+2
	cmp #$12
	assertz

	; instruction + comment
	ldx #<$9000
	ldy #>$9000
	jsr asm::setpc

	; LDA #$12 ;comment
	ldx #<@testop8
	ldy #>@testop8
	jsr asm::tokenize
	lda asm::result
	cmp #$a9
	assertz
	lda asm::result+1
	cmp #$12
	assertz

	jsr asm::advancepc
	lda asm::pc
	cmp #$02
	assertz

	; comment should exist at $1234
	ldx #<$9000
	ldy #>$9000
	jsr asm::labelat
	cmp #$00
	assertz

	; label
	ldx #<@testlabel
	ldy #>@testlabel
	jsr asm::tokenize
	cmp #$00
	assertz

	rts

@testop: .byte "lda #$00",$0d
@testop2: .byte "rti",$0d
@testop3: .byte "sta ($a2,x)",$0d
@testop4: .byte "ora ($f0),y",$0d
@testop5: .byte "and $f193,y",$0d
@testop6: .byte "jmp ($0120)",$0d
@testop7: .byte "jsr $1234",$0d
@testop8: .byte "lda #$12 ;comment",$0d
@testlabel: .byte "label:",$0d

;--------------------------------------
.export test_getvalue
test_getvalue:
@line=zp::tmp0
	ldx #<@test1
	ldy #>@test1
	stx @line
	sty @line+1
	jsr getvalue
	cmp #1
	assertz
	cpx #$a9
	assertz

	ldx #<@test2
	ldy #>@test2
	stx @line
	sty @line+1
	jsr getvalue
	cmp #2
	assertz
	cpx #$24
	assertz
	cpy #$42
	assertz
	rts

@test1: .byte "$a9 "
@test2: .byte "$4224 "

;--------------------------------------
.export test_getaddrmode
test_getaddrmode:
@indirect=zp::tmp2  ; 1=indirect, 0=absolute
@indexed=zp::tmp3   ; 1=x-indexed, 2=y-indexed, 0=not indexed
@immediate=zp::tmp4 ; 1=immediate, 0=not immediate
@operandsz=zp::tmp5
	lda #$00
	sta @indirect
	sta @indexed
	sta @immediate
	sta @operandsz

	; implied/accumulator
	jsr getaddrmode
	cmp #$00
	assertz

	; immediate
	lda #1
	sta @operandsz
	sta @immediate
	jsr getaddrmode
	ldx #1
	cmp #1
	assertz

	; zp
	lda #0
	sta @immediate
	jsr getaddrmode
	ldx #2
	cmp #2
	assertz

	; zp,x
	lda #1
	sta @indexed
	jsr getaddrmode
	ldx #3
	cmp #3
	assertz

	; (zp,x)
	lda #1
	sta @indirect
	sta @indexed
	jsr getaddrmode
	ldx #4
	cmp #4
	assertz

	; (zp),y
	lda #2
	sta @indexed
	jsr getaddrmode
	ldx #5
	cmp #5
	assertz

	; abs
	lda #0
	sta @indexed
	sta @indirect
	lda #2
	sta @operandsz
	jsr getaddrmode
	ldx #6
	cmp #6
	assertz

	; abs,x
	lda #1
	sta @indexed
	jsr getaddrmode
	ldx #7
	cmp #7
	assertz

	; abs,y
	lda #2
	sta @indexed
	jsr getaddrmode
	ldx #8
	cmp #8
	assertz

	; (abs)
	lda #1
	sta @indirect
	lda #0
	sta @indexed
	jsr getaddrmode
	ldx #9
	cmp #9
	assertz
	rts

;--------------------------------------
.export test_labels
test_labels:
@test_addr=$1234
@test_addr2=$5678
@line=zp::tmp0
	ldx #<@test_addr
	ldy #>@test_addr
	stx @line
	sty @line+1
	ldx #<@testlabel
	ldy #>@testlabel
	lda #@testlabel_len
	jsr asm::addlabel

	ldx #<@test_addr2
	ldy #>@test_addr2
	stx @line
	sty @line+1
	ldx #<@testlabel2
	ldy #>@testlabel2
	lda #@testlabel2_len
	jsr asm::addlabel

	ldx #<@test
	ldy #>@test
	stx @line
	sty @line+1
	jsr getlabel

	; assert address matches expected address
	cpx #<@test_addr
	assertz
	cpy #>@test_addr
	assertz

	; assert second address matches expected address
	ldx #<@test2
	ldy #>@test2
	stx @line
	sty @line+1
	jsr getlabel
	cpx #<@test_addr2
	assertz
	cpy #>@test_addr2
	assertz

	; assert that the labels can be found from their addresses
	ldx #<@test_addr
	ldy #>@test_addr
	jsr asm::labelat

	ldx #<@test_addr2
	ldy #>@test_addr2
	jsr asm::labelat

	rts

@test: .byte "test "
@test2: .byte "another "
@testlabel: .byte "test"
@testlabel_len=*-@testlabel
@testlabel2: .byte "another"
@testlabel2_len=*-@testlabel2
