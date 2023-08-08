.include "bitmap.inc"
.include "key.inc"
.include "macros.inc"
.include "memory.inc"
.include "text.inc"
.include "zeropage.inc"

.DATA
;--------------------------------------
save_sp: .word mem::spare+40 ; stack ptr for save/restore stack
stack_depth: .byte 0

.CODE
;--------------------------------------
; saverow
; saverow saves the row .A of text to the UI's save stack
.proc saverow
@src=zp::tmp0
@dst=zp::tmp2
@cnt=zp::tmp4
	asl
	asl
	asl
	sta @src
	lda #>BITMAP_ADDR
	sta @src+1

	ldxy save_sp
	stxy @dst

	lda #20
	sta @cnt
@l0:	ldy #$07
@l1:	lda (@src),y
	sta (@dst),y
	dey
	bpl @l1
	ldxy @dst
	add16 #8
	stxy @dst
	ldxy @src
	add16 #$c0
	stxy @src
	dec @cnt
	bne @l0

	ldxy save_sp
	add16 #8*20
	stxy save_sp
	inc stack_depth
	rts
.endproc

;--------------------------------------
; restorerow
; restorerow restores the row .A of text from the top of te UI's save stack
.proc restorerow
@src=zp::tmp0
@dst=zp::tmp2
@cnt=zp::tmp4
	asl
	asl
	asl
	sta @dst
	lda #>BITMAP_ADDR
	sta @dst+1

	ldxy save_sp
	sub16 #8*20
	stxy save_sp
	stxy @src

	lda #20
	sta @cnt
@l0:	ldy #$07
@l1:	lda (@src),y
	sta (@dst),y
	dey
	bpl @l1
	ldxy @src
	add16 #8
	stxy @src
	ldxy @dst
	add16 #$c0
	stxy @dst
	dec @cnt
	bne @l0

	dec stack_depth
	rts
.endproc

;--------------------------------------
; msgbox displays .A rows of text given in (zp::tmp0) at the position
; in (zp::tmp0, zp::tmp1) and waits for the user to press <RETURN>
.export __ui_msgbox
.proc __ui_msgbox
@lines=zp::tmp0
@line=zp::tmpa
@cnt=zp::tmp8
	sta @cnt
	sty @row
	ldxy @lines
	stxy @line

@l0:	; back up area behind row
	lda @row
	jsr saverow

	ldy #$00
	lda (@line),y
	tax
	iny
	lda (@line),y
	tay
@row=*+1
	lda #$00
	jsr text::print
	lda @row
	jsr bm::rvsline
	incw @line
	incw @line
	inc @row
	dec @cnt
	bne @l0

@l1:	jsr key::getch
	cmp #$00
	beq @l1

	; restore screen
:	dec @row
	lda @row
	jsr restorerow
	lda stack_depth
	bne :-

	rts
.endproc
