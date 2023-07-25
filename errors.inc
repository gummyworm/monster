ERR_UNALIGNED_LABEL=$ff
ERR_ILLEGAL_OPCODE=$ff-2
ERR_ILLEGAL_ADDRMODE=$ff-3
ERR_ILLEGAL_DIRECTIVE=$ff-4
ERR_OVERSIZED_OPERAND=$ff-5
ERR_ILLEGAL_LABEL=$ff-6

.import __err_print
.import __err_print_with_arg

;--------------------------------------
; err is a macro to display an error.
; if the corresponding error message (see errors.asm) contains an escape character,
; param should contain the value you wish to print for that character.
; If the the corresponding error does not contain an escape code, you may omit
; param. 
; examples:
;  - err ERR_INVALID_OPCODE, my_opcode
;  - err ERR_UNALIGNED_LABEL
; 
.macro err id, param
	lda #id
	.ifnblank param
		ldx #<param
		ldy #>param
		jsr __err_print_with_arg
	.else
		jsr __err_print
	.endif
.endmacro
