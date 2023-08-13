.include "errors.inc"
.include "macros.inc"
.include "memory.inc"
.include "util.inc"
.include "zeropage.inc"

;--------------------------------------
CONTEXT_SIZE = $200	; size of buffer per context
PARAM_LENGTH = 16	; size of param (stored after the context data)
MAX_PARAMS   = 4	; max params for a context
MAX_CONTEXTS = 3	; $1000-$400 / $200

;--------------------------------------
; contexts are stored in spare mem, which is unused by the assembler during the
; assembly of a program.
; The number of contexts is limited by the size of a context (defined as
; CONTEXT_SIZE).
contexts = mem::spare

.BSS
;--------------------------------------
.export __ctx_type
__ctx_type:
type: .byte 0	; the active context
activectx: .byte 0

; TODO: support context other than REP
ctx=zp::ctx+0		; address of context
iter=zp::ctx+2		; (REP) iterator's current value
iterend=zp::ctx+4	; (REP) iterator's end value
cur=zp::ctx+6		; cursor to current ctx data
params=zp::ctx+8	; address of params (grows down from CONTEXT+$200-PARAM_LENGTH)
numparams=zp::ctx+10	; the number of parameters for the context

CTX_LINES_START = 9

.CODE
;--------------------------------------
; INIT
; initializes the context state by clearing the stack
.export  __ctx_init
.proc __ctx_init
	lda #$00
	sta activectx
	sta numparams
	sta type
	rts
.endproc

;--------------------------------------
; .C is set if there is no room to create a new context
.export __ctx_push
.proc __ctx_push
	lda activectx
	cmp #MAX_CONTEXTS
	bcs @err
	beq @push

	; save the active context's state
	ldy #CTX_LINES_START-1
:	lda ctx+2,y
	sta (ctx),y
	dey
	bpl :-

@push:  inc activectx
@ok:	jsr getctx
	jmp __ctx_rewind
@err:	lda #ERR_STACK_OVERFLOW
	rts
.endproc

;--------------------------------------
; .C is set if there are no contexts to pop
.export __ctx_pop
.proc __ctx_pop
	lda activectx
	beq @err
	dec activectx
@ok:	RETURN_OK
@err:	RETURN_ERR ERR_STACK_UNDERFLOW
.endproc

;--------------------------------------
; GETLINE
; Returns a line from the active context.
; out:
;  - .XY: the address of the line returned
;  - .A: the # of bytes read (0 if EOF)
;  - .C: set if there are no lines to read
;  - mem::ctxbuffer: the line read from the context
.export __ctx_getline
.proc __ctx_getline
@out=mem::ctxbuffer
	; TODO: support types other than REP
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

;--------------------------------------
; GETDATA
; returns the address of the data for the active context.
; out:
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

;--------------------------------------
; WRITE
; writes the line in .YX to the context at its activectxent position
; out:
;  - .XY: the address of the active context.
.export __ctx_write
.proc __ctx_write
@line=zp::tmp0
	stxy @line
	ldy #$00
@write: lda (@line),y
	sta (cur),y
	beq @done
	iny
	cpy #40
	bne @write
@err:	RETURN_ERR ERR_LINE_TOO_LONG
@done:	iny
	tya
	clc
	adc cur
	sta cur
	bcc :+
	inc cur+1
:	RETURN_OK
.endproc

;--------------------------------------
; ADDPARAM
; adds the parameter in .YX to the active context
; in:
;  - .XY: the 0 terminated parameter to add to the active context
; out:
;  - .XY: the rest of the string after the parameter that was extracted
.export __ctx_addparam
.proc __ctx_addparam
@param=zp::tmp0
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

;--------------------------------------
; REWIND
; rewinds the context so that the cursor points to the beginning of its lines
.export  __ctx_rewind
.proc __ctx_rewind
	; the contexts are aligned to $200 byte boundaries, so just clear the
	; bottom nine bits
	lda ctx
	adc #CTX_LINES_START
	sta cur
	lda ctx+1
	adc #$00
	sta cur+1
	rts
.endproc

;--------------------------------------
; GETCTX
; Updates the zeropage context variables with values from
; the active context
; out:
;  - zp::ctx-zp::ctx+8: context variables for the active ctx
.proc getctx
	; get activectx LSB (0)
	lda #<contexts
	sta ctx

	ldx activectx
	dex	; make id 0-based
	txa

	; get multiple of $200 (activectx << 2)
	asl
	ora #>contexts
	sta ctx+1
	tay

	; TODO: support types other than REP
	; get the ctx metadata (iter, iterend, cur, and param)
	ldy #CTX_LINES_START-3
@l0:	lda (ctx),y
	sta ctx+2,y
	dey
	bpl @l0

	; get address of param buffer
	lda ctx
	adc #<(CONTEXT_SIZE)
	sta params
	lda ctx+1
	adc #>(CONTEXT_SIZE)
	sta params+1

	rts
.endproc
