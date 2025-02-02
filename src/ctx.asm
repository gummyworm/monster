;*******************************************************************************
; CTX.ASM
; This file contains the code for interacting with the assembly "context"
; The "context" is a special buffer used by the .MAC and .REP directives to
; store lines of data, which is required to complete the assembly of these
; directives when their corresponding .ENDMAC or .ENDREP directive is found.
;*******************************************************************************

.include "errors.inc"
.include "macros.inc"
.include "memory.inc"
.include "util.inc"
.include "zeropage.inc"

;*******************************************************************************
; CONSTANTS
CONTEXT_SIZE = $200	; size of buffer per context
PARAM_LENGTH = 16	; size of param (stored after the context data)
MAX_PARAMS   = 4	; max params for a context
MAX_CONTEXTS = 3	; $1000-$400 / $200

CTX_ITER_START   = 2
CTX_PARAMS_START = 8
CTX_LINES_START  = 11

;*******************************************************************************
; CONTEXTS
; Contexts are stored in spare mem, which is unused by the assembler during the
; assembly of a program.
; The number of contexts is limited by the size of a context (defined as
; CONTEXT_SIZE).
.export contexts
contexts = mem::spare

.BSS
;*******************************************************************************
activectx: .byte 0

ctx       = zp::ctx+0	; address of context
iter      = zp::ctx+2	; (REP) iterator's current value
iterend   = zp::ctx+4	; (REP) iterator's end value
cur       = zp::ctx+6	; cursor to current ctx data
params    = zp::ctx+8	; address of params (grows down from CONTEXT+$200-PARAM_LENGTH)
numparams = zp::ctx+10	; the number of parameters for the context

.CODE
;*******************************************************************************
; INIT
; initializes the context state by clearing the stack
.export  __ctx_init
.proc __ctx_init
	lda #$00
	sta activectx

	; fallthrough
.endproc

;*******************************************************************************
; RESET
; resets the state for the active context
.proc reset
	lda #$00
	sta numparams
	sta mem::ctxbuffer
	rts
.endproc

;*******************************************************************************
; PUSH
; Saves the current context and beings a new one
; OUT:
; - .C: set if there is no room to create a new context
.export __ctx_push
.proc __ctx_push
	lda activectx
	beq @push
	cmp #MAX_CONTEXTS+1
	bcs @err

	; save the active context's state
	ldy #CTX_LINES_START-CTX_ITER_START
:	lda ctx+CTX_ITER_START,y
	sta (ctx),y
	dey
	bpl :-

@push:  inc activectx
@ok:	jsr getctx
	jsr reset
	jmp __ctx_rewind
@err:	lda #ERR_STACK_OVERFLOW
	rts
.endproc

;*******************************************************************************
; POP
; Restores the last PUSH'ed context
; OUT:
;  -.C: set if there are no contexts to pop
.export __ctx_pop
.proc __ctx_pop
	lda activectx
	beq @err
	dec activectx
@ok:	RETURN_OK
@err:	RETURN_ERR ERR_STACK_UNDERFLOW
.endproc

;*******************************************************************************
; GETLINE
; Returns a line from the active context.
; OUT:
;  - .XY: the address of the line returned
;  - .A: the # of bytes read (0 if EOF)
;  - .C: set if there are no lines to read
;  - mem::ctxbuffer: the line read from the context
.export __ctx_getline
.proc __ctx_getline
@out=mem::ctxbuffer
	; read until a newline or EOF
	ldy #$00
@read:	lda (cur),y
	sta @out,y
	beq @done
	iny
	cpy #40
	bcc @read
	RETURN_ERR ERR_LINE_TOO_LONG

@done:
	iny
	tya
	clc
	adc cur
	sta cur
	bcc :+
	inc cur+1
:	ldxy #@out
	RETURN_OK
.endproc

;*******************************************************************************
; GETPARAMS
; returns a list of the parameters for the active context
; IN:
;  - .XY: address of buffer to store params in
; OUT:
;  - .A: the number of parameters
;  - (.XY): the updated buffer filled with 0-separated params
.export __ctx_getparams
.proc __ctx_getparams
@buff=r0
@cnt=r2
@params=r3
	stxy @buff
	ldx numparams
	beq @done
	stx @cnt

	; *16
	dex
	txa
	asl
	asl
	asl
	asl
	adc params
	sta @params
	lda params+1
	adc #$00
	sta @params+1

@l0:	ldy #$00
@l1:	lda (@params),y
	sta (@buff),y
	beq @next
	iny
	cpy #$10
	bcc @l1
	RETURN_ERR ERR_PARAM_NAME_TOO_LONG

@next:	tya
	sec		; +1
	adc @buff
	sta @buff
	bcc :+
	inc @buff+1
:	lda @params
	sec
	sbc #$10
	sta @params
	bcs :+
	dec @params+1
:	dec @cnt
	bne @l0

@done:	lda numparams
	RETURN_OK
.endproc

;*******************************************************************************
; GETDATA
; returns the address of the data for the active context.
; OUT:
;  - .XY: the address of the data for the current context
.export __ctx_getdata
.proc __ctx_getdata
	lda ctx
	clc
	adc #CTX_LINES_START
	tax
	lda ctx+1
	adc #$00
	tay
	rts
.endproc

;*******************************************************************************
; WRITE
; Writes the given line to the context at its current position
; Comments are ignored to save space in the context buffer.
; IN:
;  - .XY: the line to write to the active context
; OUT:
;  - .XY: the address of the active context.
;  - .C:  set on error
.export __ctx_write
.proc __ctx_write
@line=r0
	stxy @line
	ldy #$00
@write: lda (@line),y
	beq @done
	cmp #';'
	beq @done
	sta (cur),y
	beq @done
	iny
	cpy #40
	bne @write
@err:	RETURN_ERR ERR_LINE_TOO_LONG

@done:	lda #$00
	sta (cur),y	; terminate this line in the buffer

	; update buffer cursor
	iny
	tya
	clc
	adc cur
	sta cur
	bcc :+
	inc cur+1
:	RETURN_OK
.endproc

;*******************************************************************************
; ADDPARAM
; Adds the given parameter to the active context
; IN:
;  - .XY: the 0, ' ', or ',' terminated parameter to add to the active context
; OUT:
;  - .XY: the rest of the string after the parameter that was extracted
.export __ctx_addparam
.proc __ctx_addparam
@param=r0
	stxy @param

	; move pointer to next open param
	lda params
	sec
	sbc #PARAM_LENGTH
	sta params
	bcs :+
	dec params+1

:	ldy #$00
@copy:
	lda (@param),y
	sta (params),y
	beq @done
	cmp #','
	beq @done
	cmp #' '
	beq @done
	iny
	cpy #PARAM_LENGTH
	bne @copy
	RETURN_ERR ERR_LINE_TOO_LONG

@done:	inc numparams
	lda #$00
	sta (params),y	; 0-terminate

	; get addr of rest of string for caller
	tya
	clc
	adc @param
	tax
	lda @param+1
	adc #$00
	tay
	RETURN_OK
.endproc

;*******************************************************************************
; REWIND
; Rewinds the context so that the cursor points to the beginning of its lines
.export  __ctx_rewind
.proc __ctx_rewind
	lda ctx
	adc #CTX_LINES_START
	sta cur
	lda ctx+1
	adc #$00
	sta cur+1
	rts
.endproc

;*******************************************************************************
; NUMLINES
; Returns the length (in lines) of the context
.export __ctx_numlines
.proc __ctx_numlines
@base=r0
@len=r2
	lda ctx
	adc #CTX_LINES_START
	sta @base
	lda ctx+1
	adc #$00
	sta @base+1

	ldy #$00
	sty @len
	beq @next

@l0:	ldy #$00
	lda (@base),y
	bne :+
	inc @len
:	incw @base
@next:	ldxy @base
	cmpw cur
	bne @l0

	lda @len
	rts
.endproc

;*******************************************************************************
; GETCTX
; Updates the zeropage context variables with values from the active context
; OUT:
;  - zp::ctx-zp::ctx+8: context variables for the active ctx
.proc getctx
	; get activectx LSB (0)
	lda #<contexts
	sta ctx

	; get the MSB (ctx_id << 1)
	ldx activectx
	dex	; make id 0-based
	txa
	asl
	ora #>contexts
	sta ctx+1

	; restore the context data
	; get the ctx metadata (iter, iterend, cur, and param)
	ldy #CTX_PARAMS_START-CTX_ITER_START
@l0:	lda (ctx),y
	sta ctx+CTX_ITER_START,y
	dey
	bpl @l0

	; get address of param buffer (max address- will grow down)
	lda ctx
	adc #<(CONTEXT_SIZE)
	sta params
	lda ctx+1
	adc #>(CONTEXT_SIZE)
	sta params+1

	rts
.endproc
