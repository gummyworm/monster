.include "layout.inc"
.include "text.inc"

.DATA
;--------------------------------------
err_no_err:
	.byte 0
err_stack_underflow:
	.byte "stack underflow",0
err_stack_overflow:
	.byte "stack overflow",0
err_line_too_long:
	.byte "oversized line",0
err_invalid_expression:
	.byte "invalid expression",0
err_invalid_args_for_macro:
	.byte "invalid macro arguments",0
err_syntax:
	.byte "syntax error",0
err_invalid_directive:
	.byte "invalid directive",0
err_undefined_label:
	.byte "label undefined",0

err_unmatched_endif:
	.byte ".endif with no .if",0

;------------------
err_unaligned_label:
	.byte "label is not left-aligned",0
err_illegal_opcode:
	.byte "invalid opcode",0
err_illegal_addrmode:
	.byte "invalid addressing mode",0
err_oversized_operand:
	.byte "oversized operand", 0
err_illegal_label:
	.byte "invalid label",0
err_unexpected_char:
	.byte "unexpected character", 0
err_io:
	.byte "i/o error",0
err_no_macro_name:
	.byte "no macro name given",0
err_unresolvable_label:
	.byte "label is unresolvable",0
err_cyclic_include:
	.byte "cyclic include",0
err_overlapping_segments:
	.byte "overlapping segments",0
err_max_files_exceeded:
	.byte "too many files",0
err_param_name_too_long:
	.byte "param name too long",0
err_line_not_found:
	.byte "line not found for address",0


;--------------------------------------
errors: .word err_no_err	 ; no error
	.word err_stack_underflow
	.word err_stack_overflow
	.word err_line_too_long
	.word err_invalid_expression
	.word err_invalid_args_for_macro
	.word err_syntax
	.word err_invalid_directive
	.word err_undefined_label

	.word err_unmatched_endif
	.word err_unaligned_label
	.word err_illegal_opcode
	.word err_illegal_addrmode
	.word err_oversized_operand
	.word err_illegal_label
	.word err_unexpected_char
	.word err_io
	.word err_no_macro_name
	.word err_unresolvable_label
	.word err_cyclic_include
	.word err_overlapping_segments
	.word err_max_files_exceeded
	.word err_param_name_too_long

.CODE
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

	jsr __err_get
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
@err:	jsr __err_get
	lda #ERROR_ROW
	jmp text::print
.endproc

;--------------------------------------
; geterr returns the address of the error id in .A in .XY
.export __err_get
.proc __err_get
	asl
	tax
	lda errors+1,x
	tay
	lda errors,x
	tax
	rts
.endproc
