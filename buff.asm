.include "zeropage.inc"
.include "memory.inc"
.include "util.inc"

GAP_GROW = 64

cursor: .word 0
buffend: .word 0
buffer: .res $100

gapstart: .word 0
gapend: .word 0
gapsize: .byte 0

;--------------------------------------
; newgap makes a new gap, .A bytes in size, at the cursor position.
.proc newgap
src=zp::tmp0
	ldx gapsize
	ldy gapsize
	stx zp::tmp0
	sty zp::tmp0

	lda gapend
	ldy gapend+1
	sta zp::tmp2
	sty zp::tmp2+1
	
	clc
	adc #GAP_GROW
	sta zp::tmp4
	bcc :+
	iny
:	sta zp::tmp4
	sty zp::tmp4+1

	jsr util::memcpy

	rts
.endproc

;--------------------------------------
; insertch inserts the character in .A at the current cursor position.
.export __buff_insertch
.proc __buff_insertch
@ins=zp::tmp0
	pha
	lda cursor
	clc
	adc #<mem::filebuffer1
	sta @ins
	lda cursor+1
	adc #>mem::filebuffer1
	sta @ins+1

	pla
	ldy #$00
	sta (@ins),y
	
	rts
.endproc

;--------------------------------------
; delch deletes the character at the cursor position.
.export __buff_delch
.proc __buff_delch
	lda cursor
	bne :+
	dec cursor+1
:	dec cursor
	rts

.endproc
