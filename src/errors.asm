.include "layout.inc"
.include "text.inc"

.DATA
;******************************************************************************
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
	.byte "invalid macro args",0
err_syntax:
	.byte "syntax error",0
err_invalid_directive:
	.byte "invalid directive",0
err_undefined_label:
	.byte "label undefined",0

err_unmatched_endif:
	.byte ".endif with no .if",0

;******************************************************************************
err_unaligned_label:
	.byte "label not left-aligned",0
err_illegal_opcode:
	.byte "invalid opcode",0
err_illegal_addrmode:
	.byte "invalid addr mode",0
err_oversized_operand:
	.byte "oversized operand", 0
err_illegal_label:
	.byte "invalid label",0
err_unexpected_char:
	.byte "unexpected char", 0
err_io:
	.byte "i/o error",0
err_no_macro_name:
	.byte "no macro name",0
err_unresolvable_label:
	.byte "label unresolvable",0
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
err_no_origin:
	.byte "origin unset",0
err_branch_out_of_range:
	.byte "range error",0
err_file_not_found:
	.byte "file not found",0
err_too_many_open_files:
	.byte "too many open files",0
err_logical_file_in_use:
	.byte "logical file in use",0
err_drive_did_not_respond:
	.byte "no response from drive",0

;******************************************************************************
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
	.word err_no_origin
	.word err_branch_out_of_range
	.word err_file_not_found
	.word err_too_many_open_files
	.word err_logical_file_in_use
	.word err_drive_did_not_respond

.CODE
;******************************************************************************
; GET
; Returns the address of the given error id's error message
; IN:
;  - .A: the ID of the error to get the message for
; OUT:
;  -.XY: the address of the error message
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
