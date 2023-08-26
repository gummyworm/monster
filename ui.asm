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
;******************************************************************************
; SAVEROW
; Saves the row .A of text to the UI's save stack
; IN:
;  .A: the character row of the bitmap to backup
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

;******************************************************************************
; RESTOREROW
; Restorerow restores the row .A of text from the top of te UI's save stack
; IN:
;  .A: the row of text to restore
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

;******************************************************************************
; MSGBOX
; Displays .A rows of text given in (zp::tmp0) at the position
; in (zp::tmpa, zp::tmp8) and waits for the user to press <RETURN>
; IN:
;  .Y: the start row of the message area
;  .A: the number of rows to display
;  zp::tmp0: list of pointers to the rows to display
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
