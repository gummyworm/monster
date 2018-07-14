.include "memory.inc"
.include "text.inc"
.include "zeropage.inc"

.include "macros.inc"

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
	ldxy @len
	cmpw #$0000
	beq @done

	ldxy @dst
	add16 @len
	stxy @dst

	ldxy @src
	add16 @len
	stxy @src

	ldy #$00
@l0:	lda (zp::tmp2),y
	sta (zp::tmp4),y
	decw @src
	decw @dst
	decw @len
	ldxy @len
	cmpw #$ffff
	bne @l0
@done:  rts
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
; hline draws a horizontal line at the row given in .A
.export __util_hline
.proc __util_hline
	pha
	lda #$00
	sta text::colstart
	lda #40
	sta text::len
	sta zp::tmp0

	ldx #<mem::spare
	ldy #>mem::spare
	lda #132
	jsr __util_memset

	pla
	ldx #<mem::spare
	ldy #>mem::spare
	jsr text::puts
	rts
.endproc
