.include "layout.inc"
.include "text.inc"

;--------------------------------------
err_unaligned_label:
	.byte "label is not left-aligned",0
err_illegal_opcode:
	.byte "invalid opcode:",ESCAPE_STRING,0
err_illegal_addrmode:
	.byte "invalid addressing mode: ",ESCAPE_STRING,0
err_illegal_directive:
	.byte "invalid directive: ",ESCAPE_STRING,0
err_oversized_operand:
	.byte "oversized operand", 0
err_illegal_label:
	.byte "invalid label ",ESCAPE_STRING,0

;--------------------------------------
errors: .word 0	 ; no error
	.word err_unaligned_label
	.word err_illegal_opcode
	.word err_illegal_addrmode
	.word err_illegal_directive

;--------------------------------------
.export __err_print_with_arg
.proc __err_print_with_arg
	cmp #$00
	bne :+
	pla
	pla
	rts

:	tya
	pha
	txa
	pha

	jsr geterr
	lda #ERROR_ROW
	jsr text::print
	pla
	pla
	rts
.endproc

;--------------------------------------
; print displays the error id given in .A to the status line
; .X/.Y contain the parameter for the error (if any).
.export __err_print
.proc __err_print
	cmp #$00
	bne @err
	rts
@err:	jsr geterr
	lda #ERROR_ROW
	jmp text::print
.endproc

;--------------------------------------
; geterr returns the address of the error id in .A in .XY
.proc geterr
	asl
	tax
	lda errors+1,x
	tay
	lda errors,x
	tax
.endproc
