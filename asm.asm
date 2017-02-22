.include "text.inc"
.include "zeropage.inc"

ERROR_ROW = 22

line = zp::tmp0

.CODE

;--------------------------------------
; errors
ERR_UNALIGNED_LABEL=1
err_unaligned_label:
	.byte "label not in leftmost column",0
ERR_ILLEGAL_OPCODE=2
err_illegal_opcode:
	.byte "unknown opcode: A",0
ERR_ILLEGAL_ADDRMODE=3
err_illegal_addrmode:
	.byte "invalid addressing mode: A",0
ERR_ILLEGAL_DIRECTIVE=4
err_illegal_directive:
	.byte "unknown directive: A",0

;--------------------------------------
NUM_OPCODES = 2
opcodes:
.byte  "LDA", "STA"

;--------------------------------------
.proc report_error
	lda #ERROR_ROW
	jsr text::print
.endproc

;--------------------------------------
.proc islabel
	lda (line),y
	cmp #$18	; if not TAB, this is not a label
	beq @done
@chkerr:
	lda #ERR_ILLEGAL_OPCODE
	rts
@done:	lda #$00
	rts
.endproc

;--------------------------------------
.proc isopcode
@optab = zp::tmp2
@op = zp::tmp4
	lda #$00
	sta @op

	ldx #<opcodes-1
	ldy #>opcodes-1
	stx @optab
	sty @optab+1

	ldx #3
	ldy #0
@l0:	iny
@l1:	lda (line),y
	cmp (@optab),y
	bne @next
	dex
	bne @l0

@done:	lda #$00
	rts

@next:	lda @optab
	adc #$03
	sta @optab
	bcc :+
	inc @optab+1
:	inc @op
	lda @op
	cmp #NUM_OPCODES
	bcc @l0

@err:	lda #ERR_ILLEGAL_OPCODE
	rts
.endproc

;--------------------------------------
.export __asm_compile
.proc __asm_compile
	stx line
	sty line+1

	ldy #$00
	ldx #$00
@next:
	jsr islabel
	cmp #0
	bne @err
	jsr isopcode
	cmp #0
	bne @err

@label: jsr __text_puts
@err:	rts
.endproc
