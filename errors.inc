;--------------------------------------
; stack errors
ERR_STACK_UNDERFLOW=1
ERR_STACK_OVERFLOW=2

;--------------------------------------
; string errors
ERR_LINE_TOO_LONG=3

;--------------------------------------
; expression errors
ERR_INVALID_EXPRESSION = 4

;--------------------------------------
; macro errors
ERR_INVALID_MACRO_ARGS = 5
ERR_SYNTAX_ERROR=6
ERR_INVALID_DIRECTIVE=7
ERR_LABEL_UNDEFINED=8
ERR_UNMATCHED_ENDIF = 9

;--------------------------------------
; assembly errors
ERR_UNALIGNED_LABEL=10
ERR_ILLEGAL_OPCODE=11
ERR_ILLEGAL_ADDRMODE=12
ERR_OVERSIZED_OPERAND=13
ERR_ILLEGAL_LABEL=14
ERR_UNEXPECTED_CHAR=15
ERR_IO_ERROR=16
ERR_NO_MACRO_NAME=17
ERR_UNRESOLVABLE_LABEL=17

.import __err_print
.import __err_get
.import __err_print_with_arg

.scope err
	print = __err_print
	get = __err_get
.endscope

;--------------------------------------
; RETURN_ERR
; Executes an RTS after loading .A with the given code and setting the carry flag
; - .C - set
; - .A - the error code
;
.macro RETURN_ERR code
	lda #code
	sec
	rts
.endmacro

;--------------------------------------
; RETURN_OK
; Clears the carry and returns (RTS)
.macro RETURN_OK
	clc
	rts
.endmacro
