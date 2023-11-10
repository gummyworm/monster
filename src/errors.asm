.include "layout.inc"
.include "text.inc"

.DATA
;******************************************************************************
err_no_err:
	.byte 0

err_stack_underflow:
	;.byte "stack underflow",0
	.byte $9d,$1, $1a,$db, $ab,$84, $2c,$86, $63,$d7, $0

err_stack_overflow:
	;.byte "stack overflow",0
	.byte $9d,$1,$1a,$db,$7d,$85,$91,$8c,$7d,$c0

err_line_too_long:
	;.byte "oversized line",0
	.byte $7d,$85,$94,$c9,$d1,$44,$db,$9,$71,$40

err_invalid_expression:
	;.byte "invalid expression",0
	.byte $4b,$96,$b,$9,$26,$c5,$c4,$12,$2c,$d3,$4b,$ce,$0

err_invalid_args_for_macro:
	;.byte "invalid macro args",0
	.byte $4b,$96,$b,$9,$26,$cd,$8,$d2,$7e,$c1,$91,$d3,$0

err_syntax:
	;.byte "syntax error",0
	.byte $9e,$4e,$a0,$58,$d9,$52,$93,$d2,$0

err_invalid_directive:
	;.byte "invalid directive",0
	.byte $4b,$96,$b,$9,$26,$c4,$4c,$85,$1d,$9,$b1,$40

err_undefined_label:
	;.byte "label undefined",0
	.byte $60,$42,$2b,$1b,$ab,$84,$29,$89,$71,$44,$0

err_unmatched_endif:
	;.byte "endif with no if",0
	.byte $2b,$84,$49,$9b,$ba,$54,$46,$ce,$7e,$c9,$30,$0

;******************************************************************************
err_unaligned_label:
	;.byte "label not left aligned",0
	.byte $60,$42,$2b,$1b,$73,$d4,$db,$5,$35,$1b,$b,$9,$3b,$85,$20,$0

err_illegal_opcode:
	;.byte "invalid opcode",0
	.byte $4b,$96,$b,$9,$26,$cf,$80,$cf,$21,$40

err_illegal_addrmode:
	;.byte "invalid addr mode",0
	.byte $4b,$96,$b,$9,$26,$c1,$21,$12,$db,$4f,$21,$40

err_oversized_operand:
	;.byte "oversized operand", 0
	.byte $7d,$85,$94,$c9,$d1,$44,$db,$d0,$2c,$81,$71,$0

err_illegal_label:
	;.byte "invalid label",0
	.byte $4b,$96,$b,$9,$26,$cc,$8,$85,$60,$0

err_unexpected_char:
	;.byte "unexpected char", 0
	.byte $ab,$85,$c4,$5,$1d,$5,$26,$c3,$40,$52,$0

err_io:
	;.byte "io error"
	.byte $4b,$db,$2c,$92,$7c,$80

err_no_macro_name:
	;.byte "no macro name",0
	.byte $73,$db,$68,$43,$93,$db,$70,$4d,$28,$0

err_unresolvable_label:
	;.byte "label unresolvable",0
	.byte $60,$42,$2b,$1b,$ab,$92,$2c,$cf,$65,$81,$13,$5,$0

err_cyclic_include:
	;.byte "cyclic include",0
	.byte $1e,$43,$62,$43,$da,$4e,$1b,$15,$21,$40

err_overlapping_segments:
	;.byte "segment overlap",0
	.byte $99,$47,$69,$4e,$a6,$cf,$b1,$52,$60,$50,$0

err_max_files_exceeded:
	;.byte "too many files",0
	.byte $a3,$cf,$db,$41,$76,$5b,$32,$4c,$2c,$c0

err_param_name_too_long:
	;.byte "param name too long",0
	.byte $80,$52,$b,$5b,$70,$4d,$2e,$d4,$7b,$db,$63,$ce,$38,$0

err_line_not_found:
	;.byte "line not found for address",0
	.byte $62,$4e,$2e,$ce,$7d,$1b,$33,$d5,$71,$1b,$33,$d2,$d8,$44,$24,$85,$9c,$c0

err_no_origin:
	;.byte "origin unset",0
	.byte $7c,$89,$3a,$4e,$dd,$4e,$99,$54,$0

err_branch_out_of_range:
	;.byte "range error",0
	.byte $90,$4e,$39,$5b,$2c,$92,$7c,$80

err_file_not_found:
	;.byte "file not found",0
	.byte $32,$4c,$2e,$ce,$7d,$1b,$33,$d5,$71,$0

err_unknown_segment:
	;.byte "unknown segment",0
	.byte $ab,$8b,$73,$d7,$76,$d3,$29,$cd,$2b,$94,$0

err_too_many_open_files:
	;.byte "too many open files",0
	.byte $a3,$cf,$db,$41,$76,$5b,$7c,$5,$76,$c6,$4b,$5,$98,$0

err_logical_file_in_use:
	;.byte "logical file in use",0
	.byte $63,$c7,$48,$c1,$66,$c6,$4b,$5,$da,$4e,$dd,$53,$28,$0

err_drive_did_not_respond:
	;.byte "no drive response",0
	.byte $73,$db,$24,$89,$b1,$5b,$91,$53,$83,$ce,$99,$40

err_unnamed_buffer:
	;.byte "unnamed buffer",0
	.byte $ab,$8e,$b,$45,$26,$c2,$a9,$86,$2c,$80

err_no_open_scope:
	; .byte "no open scope",0
	.byte $73,$db,$7c,$5,$76,$d3,$1b,$d0,$28,$0

err_label_already_defined:
	.byte $60,$42,$2b,$1b,$b,$12,$28,$44,$ce,$c4,$29,$89,$71,$44,$0

;******************************************************************************
.linecont +
.define errors \
	err_no_err, \
	err_stack_underflow, \
	err_stack_overflow, \
	err_line_too_long, \
	err_invalid_expression, \
	err_invalid_args_for_macro, \
	err_syntax, \
	err_invalid_directive, \
	err_undefined_label, \
	\
	err_unmatched_endif, \
	err_unaligned_label, \
	err_illegal_opcode, \
	err_illegal_addrmode, \
	err_oversized_operand,\
	err_illegal_label, \
	err_unexpected_char, \
	err_io, \
	err_no_macro_name, \
	err_unresolvable_label, \
	err_cyclic_include, \
	err_overlapping_segments, \
	err_max_files_exceeded, \
	err_param_name_too_long, \
	err_line_not_found, \
	err_no_origin, \
	err_branch_out_of_range, \
	err_file_not_found, \
	err_unknown_segment, \
	err_too_many_open_files, \
	err_logical_file_in_use, \
	err_drive_did_not_respond, \
	err_unnamed_buffer, \
	err_no_open_scope, \
	err_label_already_defined
.linecont -
errorslo: .lobytes errors
errorshi: .hibytes errors

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
	tax
	ldy errorshi,x
	lda errorslo,x
	tax
	rts
.endproc
