.include "memory.inc"
.include "text.inc"
.include "zeropage.inc"

.include "macros.inc"
.CODE

;--------------------------------------
; memset sets zp::tmp0 bytes of the memory at (YX) to .A.
.export __util_memset
.proc __util_memset
	stx zp::tmp1
	sty zp::tmp1+1
	ldy zp::tmp0
@l0:	sta (zp::tmp1),y
	dey
	bpl @l0

	rts
.endproc

;--------------------------------------
; memcpy moves zp::tmp0 bytes from (zp::tmp2) to (zp::tmp4).
.export __util_memcpy
.proc __util_memcpy
@src=zp::tmp2
@dst=zp::tmp4
@len=zp::tmp0
	sei
	ldxy @len
	cmpw #$0000
	beq @done

@l0:	ldy #$00
	lda (@src),y
	sta (@dst),y
	incw @src
	incw @dst
	decw @len
	ldxy @len
	cmpw #$0000
	bne @l0
@done:  cli
	rts
.endproc

;--------------------------------------
; chtohex returns the binary representation of the character given in .A
.export __util_chtohex
.proc __util_chtohex
	cmp #'f'+1
	bcs @done
	cmp #'a'
	bcc @numeric
	sbc #'a'-$a
	rts
@numeric:
	cmp #'9'+1
	bcs @done
	cmp #'0'
	bcc @done
	sbc #'0'
@done:
	rts
.endproc


;--------------------------------------
; hextostr returns the string representation of the hex value in .A
; .X contains the low nybble and Y contains the high nybble
.export __util_hextostr
.proc __util_hextostr
	pha
	and #$f0
	lsr
	lsr
	lsr
	lsr
	cmp #$0a
	bcs :+
	adc #'0'
	bcc :++
:	adc #'a'-$a-1
:	tay

	pla
	and #$0f
	cmp #$0a
	bcs :+
	adc #'0'
	bcc :++
:	adc #'a'-$a-1
:	tax
	rts
.endproc

;--------------------------------------
; todec returns a ptr to a decimal representation of the value given in .XY
; in mem::spare
.export __util_todec
.proc __util_todec
result=mem::spare
	lda #'0'
	sta result
	sta result+1
	sta result+2
	sta result+3
	sta result+4
@l1000s:
	sub16 #1000
	bcc @do100s
	inc result+1
	lda result+1
	cmp #'9'+1
	bcc @l1000s
	lda #'0'
	sta result+1
	inc result
	bne @l1000s

@do100s:
	add16 #1000
@l100s:
	sub16 #100
	bcc @do10s
	inc result+2
	bne @l100s

@do10s:
	add16 #100
@l10s:
	sub16 #10
	bcc @do1s
	inc result+3
	bne @l10s
@do1s:
	txa
	clc
	adc #10+'0'
	sta result+4
	rts
.endproc

;--------------------------------------
; strncmp compares the strings in (tmp0) and (tmp2) up to a length of .A
; If the strings are equal, 0 is returned in .A. and the zero flag is set.
.export __util_strncmp
.proc __util_strncmp
	tay
	dey
	bmi @match
@l0:	lda (zp::tmp0),y
	cmp (zp::tmp2),y
	beq :+
	rts
:	dey
	bpl @l0
@match:	lda #$00
	rts
.endproc

;--------------------------------------
; strlen returns the length of the string in .YX in .A
.export __util_strlen
.proc __util_strlen
@str=zp::tmp0
	stx @str
	sty @str+1
	ldy #$00
@l0:	lda (@str),y
	beq @done
	iny
	bne @l0
@done:	tya
	rts
.endproc

