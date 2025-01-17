;******************************************************************************
; EXPR.ASM
; This file contains code to evaluate expressions. This is used, among other
; things, to resolve operand values during assembly
;******************************************************************************

.include "errors.inc"
.include "labels.inc"
.include "line.inc"
.include "macros.inc"
.include "math.inc"
.include "state.inc"
.include "util.inc"

;******************************************************************************
; CONSTANTS
MAX_OPERATORS = $10
MAX_OPERANDS  = MAX_OPERATORS/2

.BSS
end_on_whitespace: .byte 0


; HINT VALUE
; This value will be used if a label cannot be resolved when evaluating the
; expression
.export __expr_label_hint
.export __expr_label_size_hint
__expr_label_hint:      .word 0
__expr_label_size_hint: .byte 0

.CODE

;******************************************************************************
; END_ON_SPACE
; Configures the evaluation behavior when whitespace is encountered.
; IN:
;   - .A: if set to 0, future calls to eval will ignore whitespace
;         if set to 1, future calls to eval will treat whitespace as end of expr
.export __expr_end_on_whitespace
.proc __expr_end_on_whitespace
	sta end_on_whitespace
	rts
.endproc

;******************************************************************************
; EVAL
; Resolves the contents of the given zp::line and returns its evaluated value
; IN:
;  - zp::line: pointer to the expression to evaluate
; OUT:
;  - .A:       the size of the returned value in bytes or the error code
;  - .XY:      the result of the evaluated expression
;  - .C:       clear on success or set on failure
;  - zp::line: updated to point beyond the parsed expression
.export __expr_eval
.proc __expr_eval
@val1=zp::expr
@val2=zp::expr+2
@num_operators=zp::expr+4
@num_operands=zp::expr+5	; num operands * 2
@may_be_unary=zp::expr+6

@operators=$100
@operands=$120
@priorities=$130
	lda #$00
	sta @num_operators
	sta @num_operands
	tay

	; by default flag that operator might be unary
	lda #$01
	sta @may_be_unary

	lda (zp::line),y
	bne @l0
	sec
	rts			; no expression

@l0:	ldy #$00
	lda (zp::line),y
	jsr util::is_whitespace	; eat whitespace
	bne :+

	; check whitespace behavior, finish if configured as terminator
	lda end_on_whitespace
	bne @end
	jsr line::incptr
	bne @l0		; branch always

:	lda (zp::line),y
	jsr @isterminator
	bne @rparen
@end:	jmp @done

@rparen:
	cmp #'('
	bne @lparen
	inc @may_be_unary
	jsr @pushop
	jsr line::incptr
	bne @l0		; branch always

@lparen:
	cmp #')'
	bne @checkop
	ldx #$00
	stx @may_be_unary

@paren_eval:
	ldx @num_operators
	dex
	bmi @err	; no parentheses found
	lda @operators,x
	cmp #'('
	bne :+
	jsr @popop	; pop the parentheses

	jsr line::incptr
	jmp @l0		; and we're done evaluating this () block

:	jsr @eval	; evaluate the top 2 operands
	jmp @paren_eval

@checkop:
	ldy #$00
	lda (zp::line),y
	cmp #'*'		; '*' can be a value or operator
	bne :+
	ldx @may_be_unary	; if unary logic applies, treat as value (PC)
	bne @getoperand

:	jsr util::isoperator
	bne @getoperand
	pha			; save the operator
	jsr @priority		; get the priority of this operator
@process_ops:
	ldx @num_operators	; any operators to the left?
	beq @process_ops_done
	dex
	; if the operator to the left has >= priority, process it
	cmp @priorities,x
	beq :+
	bcs @process_ops_done
:	pha			; save priority
	jsr @eval		; evaluate top 2 elements of the operand stack
	pla			; get priority
	jmp @process_ops	; continue til op on left has lower priority

@process_ops_done:
	pla
	jsr @pushop
	jsr line::incptr
	inc @may_be_unary
	jmp @l0

@getoperand:
	jsr isval
	bcs @label		; not a val, try label
	jsr __expr_getval	; is this a value?
	bcc @pushoperand
	rts			; return err

@label: ldxy zp::line
	jsr get_label		; is it a label?
	bcs @err

@pushoperand:
	jsr @pushval
	lda #$00
	sta @may_be_unary
	jmp @l0

@err:	; check if this is parentheses (could be indirect addressing)
	ldy #$00
	lda (zp::line),y
	cmp #')'
	beq @done
	RETURN_ERR ERR_LABEL_UNDEFINED

@done:	ldx @num_operators	; if there are still ops on stack
	beq @getresult		; no operators: just get the result
	jsr @eval		; evaluate each remaining operator
	jmp @done

@getresult:
	ldx @operands
	lda #$01
	ldy @operands+1
	beq :+
	lda #$02
:	RETURN_OK

;------------------
; isterminator returns .Z set if the character in .A is
; one that should end the evaluation of the expression
@isterminator:
	cmp #$00
	beq :+
	cmp #';'
	beq :+
	cmp #','
:	rts

;------------------
@popval:
	dec @num_operands
	dec @num_operands
	ldx @num_operands
	lda @operands+1,x
	tay
	lda @operands,x
	tax
	rts

;------------------
@pushval:
	txa
	ldx @num_operands
	cpx #MAX_OPERANDS
	bcc :+
:	sta @operands,x
	tya
	sta @operands+1,x
	inc @num_operands
	inc @num_operands
	rts

;------------------
@popop:
	dec @num_operators
	ldx @num_operators
	lda @operators,x
	rts

;------------------
@pushop:
	ldx @num_operators
	cpx #MAX_OPERATORS
	bcc :+
:	sta @operators,x
	pha
	jsr @priority
	sta @priorities,x
	pla
	inc @num_operators
	rts

;------------------
@priority:
	cmp #'+'
	beq @prio1
	cmp #'-'
	beq @prio1
	cmp #'*'
	beq @prio2
	cmp #'/'
	beq @prio2
	cmp #'&'
	beq @prio3
	cmp #'^'
	beq @prio4
	cmp #'.'
	beq @prio5
	cmp #'<'
	beq @prio3
	cmp #'>'
	beq @prio3

	lda #$00	; unknown
	rts

@prio1:	lda #$01
	rts
@prio2: lda #$02
	rts
@prio3: lda #$03
	rts
@prio4:	lda #$04
	rts
@prio5:	lda #$05
	rts

;------------------
; returns the evaluation of the operator in .A on the operands @val1 and @val2
@eval:	jsr @popval
	stxy @val1

	jsr @popop
	; check if operator is unary
	cmp #'<'
	beq @unary
	cmp #'>'
	bne @binary
@unary: jmp @check_unary

@binary:
	; not unary, get a second argument
	pha
	jsr @popval
	stxy @val2

	pla
	cmp #'+'
	bne :+

@add:   lda @val1
	clc
	adc @val2
	tax
	lda @val1+1
	adc @val2+1
	tay
	jmp @pushval

:	cmp #'-'
	bne :+
@sub:	lda @val2
	sec
	sbc @val1
	tax
	lda @val2+1
	sbc @val1+1
	tay
	jmp @pushval

:	cmp #'*'	; MULTIPLY
	bne :+
	; get the product TODO: 32-bit precision expressions?
	ldxy @val1
	stxy r0
	ldxy @val2
	stxy r2
	jsr m::mul16
	ldxy ra	; get product
	jmp @pushval

:	cmp #'/'	; DIVIDE
	bne :+
	ldxy @val1
	stxy r2
	ldxy @val2
	stxy r0
	jsr m::div16
	ldxy r0
	jmp @pushval

:	cmp #'&'	; AND
	bne :+
	lda @val1
	and @val2
	tax
	lda @val1+1
	and @val2+1
	tay
	jmp @pushval

:	cmp #'.'	; OR
	bne :+
	lda @val1
	ora @val2
	tax
	lda @val1+1
	ora @val2+1
	tay
	jmp @pushval

:	cmp #'^'	; EOR
	bne @unknown_op
	lda @val1
	eor @val2
	tax
	lda @val1+1
	eor @val2+1
	tay
	jmp @pushval

@check_unary:
	ldx #$00
	stx @val2
	stx @val2+1

@lsb:	cmp #'<'
	bne @msb
	ldy #$00
	ldx @val1
	jmp @pushval

@msb:	cmp #'>'
	bne @unknown_op
	ldy #$00
	ldx @val1+1
	jmp @pushval

@unknown_op:
	rts
.endproc

;******************************************************************************
; GETLABEL
; Reads the given label and returns the address of it if there
; is one
; IN:
;  - .XY: pointer to the label to get the address of
; OUT:
;  - .C is set if no label is found
;  - .A: the size of the label's address
;  - .XY: the value of the label
.proc get_label
	jsr lbl::isvalid 	; if verifying, let this pass if label is valid
	bcs @done

	; try to get the label address
	ldxy zp::line
	jsr lbl::addr
	bcc @updateline

	; if we failed to get the address, but we're on pass 1 / verifying
	; proceed with a dummy value
	lda zp::pass
	cmp #$02
	bcs @done		; not verifying, return with error

	; default to the hint value/size
	lda #$ff
	ldxy zp::virtualpc	; TODO: assume smallest possible value

@updateline:
	pha	; save size
	tya	; save MSB
	pha

	; move the line pointer to the separator
	ldy #$00
@l0:	lda (zp::line),y
	jsr util::isseparator
	beq :+
	jsr line::incptr
	bne @l0

:	pla
	tay
	pla
	clc
@done:	rts
.endproc

;******************************************************************************
; ISVAL
; Checks if the word in zp::line is a value or not
; OUT:
;  - .C: clear if the string is a hex or decimal value
.proc isval
	; allow first char to be '$' or '*'
	ldx #$00	; 0 if decimal, 1 if hex
	lda (zp::line),y

	; check for '*' (current PC)
	cmp #'*'
	bne :+
	ldy #$01
	lda (zp::line),y
	jsr util::isseparator
	beq @done
	bne @err
:	cmp #$27	; single quote
	bne :+
	RETURN_OK	; if single quote, this is either a value or nothing

:	cmp #'$'
	bne @cont
	inx		; flag hex
	iny
@cont:
	lda (zp::line),y
	jsr util::isseparator
	beq @done

	cpx #$00
	beq @dec

	; check hex
	cmp #$66+1	; 'f'+1
	bcs @err
	cmp #$61	; 'a'
	bcs @ok
	cmp #$46+1	; 'F'+1
	bcs @err
	cmp #$41	; 'A'
	bcs @ok

@dec:	cmp #'0'
	bcc @err
	cmp #'9'+1
	bcs @err

@ok:	iny
	bne @cont

@done:	RETURN_OK
@err:	sec
	rts

.endproc

;******************************************************************************
; GETVAL
; Parses zp::line for a decimal or hexadecimal value up to 16 bits in size.
; It may also parse a character in the format 'x'.  This must be a 1 byte
; value.
; The character '*' is also parsed and results in the value of zp::virtualpc
; IN:
;  - zp::line: the text to parse a value from
; OUT:
;  -.XY: the value of the string in zp::line
;  -.C: set on error and clear if a value was extracted.
.export __expr_getval
.proc __expr_getval
@val=zp::expr+6
	ldy #$00
	sty @val
	sty @val+1
	lda (zp::line),y
	cmp #'$'
	beq @hex

	cmp #$27		; single quote
	beq @char

	cmp #'*'
	bne @decimal
	iny
	lda (zp::line),y
	jsr util::isseparator
	beq :+
	jmp @err

:	jsr line::incptr
	ldx zp::virtualpc
	ldy zp::virtualpc+1
	beq :+
	lda #$02
	skw
:	lda #$01
	RETURN_OK

;------------------
@char:
	jsr line::incptr
	lda (zp::line),y	; read the character value
	tax			; put it into .X
	jsr line::incptr
	lda (zp::line),y	; make sure there is a terminating quote
	cmp #$27		; single quote
	bne @badchar
	jsr line::incptr
	lda #$01		; result is 1 byte in size
	RETURN_OK

;------------------
@decimal:
	ldxy zp::line
	jsr atoi	; convert to binary
	bcc :+
	rts		; return err
:	adc zp::line
	sta zp::line
	bcc :+
	inc zp::line+1
:	stx @val
	sty @val+1
	jmp @success

;------------------
@hex:
	iny
@l0:	lda (zp::line),y
	jsr util::isseparator
	beq @done

	jsr util::chtohex
	bcc @next

@badchar:
	RETURN_ERR ERR_UNEXPECTED_CHAR

@next:	asl
	asl
	asl
	asl
	asl
	rol @val
	rol @val+1
	asl
	rol @val
	rol @val+1
	asl
	rol @val
	rol @val+1
	asl
	rol @val
	rol @val+1
	bcs @err	; oversized value
	iny
	bne @l0

@done: 	tya
	clc
	adc zp::line
	sta zp::line
	bcc @success
	inc zp::line+1
@success:
	ldx @val
	ldy @val+1
	beq :+
	lda #$02	; 2 bytes
	skw
:	lda #$01	; 1 byte
	RETURN_OK

@err:	RETURN_ERR ERR_OVERSIZED_OPERAND
.endproc
