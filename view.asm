.include "macros.inc"
.include "memory.inc"
.include "text.inc"
.include "util.inc"
.include "zeropage.inc"
.CODE

ROW_START=15
ROW_STOP=23
COL_STOP=8

;--------------------------------------
; mem displays the contents of memory in a large block beginning with the
; address in (YX).
.export __view_mem
.proc __view_mem
@src=zp::tmp7
@col=zp::tmp9
@row=zp::tmpa
	stx @src
	sty @src+1

	lda #ROW_START
	jsr util::hline

	lda #40
	sta zp::tmp0
	lda #' '
	ldx #<mem::spare
	ldy #>mem::spare
	jsr util::memset

	lda #ROW_START+1
	sta @row
@l0:	; draw the address of this line
	lda @src+1
	jsr util::hextostr
	sty mem::spare
	stx mem::spare+1
	lda @src
	jsr util::hextostr
	sty mem::spare+2
	stx mem::spare+3
	lda #':'
	sta mem::spare+4

	ldx #$00
@l1:	stx @col
	; get a byte to display
	ldy #$00
	lda (@src),y
	incw @src

	sta mem::spare+31,x
	jsr util::hextostr
	txa
	pha
	lda @col
	asl
	adc @col
	tax
	pla
	sta mem::spare+6,x
	tya
	sta mem::spare+5,x
	ldx @col
	inx
	cpx #COL_STOP
	bcc @l1

	ldx #<mem::spare
	ldy #>mem::spare
	lda @row
	jsr text::puts
	inc @row
	lda @row
	cmp #ROW_STOP
	bcc @l0
	rts
.endproc
