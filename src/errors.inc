;******************************************************************************
; stack errors
ERR_STACK_UNDERFLOW = 1
ERR_STACK_OVERFLOW  = 2

;******************************************************************************
; string errors
ERR_LINE_TOO_LONG = 3

;******************************************************************************
; expression errors
ERR_INVALID_EXPRESSION = 4

;******************************************************************************
; macro errors
ERR_INVALID_MACRO_ARGS = 5
ERR_SYNTAX_ERROR       = 6
ERR_INVALID_DIRECTIVE  = 7
ERR_LABEL_UNDEFINED    = 8
ERR_UNMATCHED_ENDIF    = 9

;******************************************************************************
; assembly errors
ERR_UNALIGNED_LABEL      = 10
ERR_ILLEGAL_OPCODE       = 11
ERR_ILLEGAL_ADDRMODE     = 12
ERR_OVERSIZED_OPERAND    = 13
ERR_ILLEGAL_LABEL        = 14
ERR_LABEL_TOO_LONG       = 15
ERR_UNEXPECTED_CHAR      = 16
ERR_IO_ERROR             = 17
ERR_NO_MACRO_NAME        = 18
ERR_UNRESOLVABLE_LABEL   = 19
ERR_CYCLIC_INCLUDE       = 20
ERR_FAILED_OPEN_INCLUDE  = 21
ERR_OVERLAPPING_SEGMENTS = 22
ERR_MAX_FILES_EXCEEDED   = 23
ERR_PARAM_NAME_TOO_LONG  = 24
ERR_LINE_NOT_FOUND       = 25
ERR_NO_ORIGIN            = 26
ERR_BRANCH_OUT_OF_RANGE  = 27
ERR_FILE_NOT_FOUND       = 28
ERR_UNKNOWN_SEGMENT      = 29
ERR_PC_TARGET_UNWRITABLE = 30

;******************************************************************************
; file i/o errors
ERR_TOO_MANY_OPEN_FILES   = 31
ERR_LOGICAL_FILE_IN_USE   = 32
ERR_DRIVE_DID_NOT_RESPOND = 33

;******************************************************************************
ERR_UNNAMED_BUFFER = 34
ERR_NO_FILENAME    = 35
ERR_NO_OPEN_SCOPE  = 36

ERR_LABEL_ALREADY_DEFINED = 37
ERR_TOO_MANY_LABELS       = 38

ERR_INVALID_COMMAND = 39

ERR_COPY_TOO_BIG = 40

ERR_BUFFER_NAME_EXISTS = 41
ERR_BUFFER_FULL = 42
ERR_BUFFER_MUST_BE_NAMED = 43

;******************************************************************************
; linker errors
ERR_SECTION_TOO_SMALL = 44
ERR_NO_SECTIONS = 45

.import __err_get

.scope err
	get = __err_get
.endscope

;******************************************************************************
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

;******************************************************************************
; RETURN_OK
; Clears the carry and returns (RTS)
.macro RETURN_OK
	clc
	rts
.endmacro
