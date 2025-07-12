;******************************************************************************
; EXPR.ASM
; This file contains code to evaluate expressions. This is used, among other
; things, to resolve operand values during assembly
; For the sake of generating the RELOCATION TABLE for an object file, we
; gather info to answer the following questions:
;   - is the expression resolvable? (any labels in another SECTION)
;   - does the expression require relocation? (any unreduced labels)
;     - NOTE: reducable labels are differences of 2 in same section: e.g. (a-b)
;   - if not resolvable, which symobl is used as base?
;   - does any post-processing need to be done by linker?
;******************************************************************************

.include "asm.inc"
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

TOK_SYMBOL    = 1	; symbol e.g. "label"
TOK_VALUE     = 2	; constant value e.g. 123
TOK_BINARY_OP = 3	; binary operator e.g. '+' or '-'
TOK_UNARY_OP  = 4	; unary operator e.g. '<'
TOK_END       = $ff	; end of expression marker

;******************************************************************************
; ID of the EXTERNAL symbol referenced in expression (if any)
; This is used to provide the symbol reference for expressions in object code
; Such expressions must be simplified to this external symbol + an offset
.export __expr_global_id
__expr_global_id = zp::expr+7

;******************************************************************************
; OPERATOR for the EXTERNAL symbol referenced in expression (if any)
.export __expr_global_op
__expr_global_op = zp::expr+9

.export __expr_contains_global
__expr_contains_global = zp::expr+10	; if !0, expression references global

.export __expr_global_postproc
__expr_global_postproc = zp::expr+11	; 0=none, 1=LSB, 2=MSB

.BSS

;******************************************************************************
; END ON WHITESPACE
; If !0, expr::eval will terminate parsing when whitespace is encountered.
; If 0, whitespace is ignored
end_on_whitespace: .byte 0

;******************************************************************************
; REQUIRES RELOC
; Set if the expression contains 1 or more labels.
; In the context of assembly, this tells the assembler that it must produce a
; relocation for this expression
; NOTE: if expr::requires_reloc is !0 but contains_global is also 0, this means
; we need to relocate relative to the section at link-time (not relative to
; label)
.export __expr_requires_reloc
__expr_requires_reloc: .byte 0

__expr_rpnlist: .res $20

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
:	rts
.endproc

;******************************************************************************
; EVAL
; Resolves the contents of the given zp::line and returns its evaluated value
; If evaluating an expression that contains an external (imported) symbol,
; there is some slightly stricter syntax required.
; That symbol must be the first in the expression and its operation must be
; immediately after that
; e.g. `GLOBAL + (rest of expr)`
; If post-processing is to be used, it must be the FIRST character of the
; expression. Do not use parentheses.
; e.g. `<GLOBAL+3`
;
; IN:
;  - zp::line: pointer to the expression to evaluate
; OUT:
;  - .A:       the size of the returned value in bytes or the error code
;  - .XY:      the result of the evaluated expression
;  - .C:       clear on success or set on failure
;  - zp::line: updated to point beyond the parsed expression
.export __expr_eval
.proc __expr_eval
	jsr __expr_parse	; parse the RPN list
	bcs :-			; -> rts
	jmp __expr_eval_list
.endproc

;******************************************************************************
; EVAL LIST
; Evaluates the provided RPN list of tokens and returns the result
; IN:
;  - $150: the list of tokens to evaluate (as produced by expr::parse)
; OUT:
;  - .A:       the size of the returned value in bytes or the error code
;  - .XY:      the result of the evaluated expression
;  - .C:       clear on success or set on failure
.export __expr_eval_list
.proc __expr_eval_list
@i=r0
@sp=r1
@val1=r2
@val2=r4
@operands=r5		; operand stack (grows up from here)
@eval:	ldx #$00
	stx @i
	stx @sp

@evalloop:
	ldx @i
	lda __expr_rpnlist,x
	bpl @cont
@done:	ldxy @operands		; read result (should be only value on stack)
	clc			; ok
@ret:	rts

@cont:	inx			; move past token index
	cmp #TOK_UNARY_OP
	beq @eval_unary
	cmp #TOK_BINARY_OP
	beq @eval_binary

	; not operator, get the operand
	pha			; save TOKEN type
	lda __expr_rpnlist,x
	inx
	ldy __expr_rpnlist,x
	inx
	stx @i
	tax
	pla			; restore TOKEN type
	cmp #TOK_SYMBOL
	bne @const
@sym:	; resolve symobl and push its value
	jsr lbl::addr
	bcs @ret		; unresolvable
@const:	jsr @pushval
	jmp @evalloop		; continue processing

;--------------------------------------
; handle unary operator
@eval_unary:
	lda __expr_rpnlist,x	; get the operator
	pha		; and save it
	inx		; move index past the operator
	stx @i		; update list index

	; get the operand for the unary operation
	jsr @popval
	stxy @val1

@lsb:	cmp #'<'
	bne @msb
	ldy #$00
	ldx @val1
	jmp @pushval

@msb:	cmp #'>'
	bne :+
	ldy #$00
	ldx @val1+1
	jmp @pushval
:	jmp *		; TODO: should be impossible

;--------------------------------------
; handle binary operator
@eval_binary:
	lda __expr_rpnlist,x	; get the operator
	pha		; and save it
	inx		; move index past the operator
	stx @i		; update list index

	; get the operands for the binary operation
	jsr @popval
	stxy @val1
	jsr @popval
	stxy @val2

	pla		; restore operator
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
	bne :+
	lda @val1
	eor @val2
	tax
	lda @val1+1
	eor @val2+1
	tay
	jmp @pushval
:	jmp *		; TODO: should be impossible

;--------------------------------------
@popval:
	lda @sp
	sec
	sbc #$02
	sta @sp
	ldy @operands+1,x	; get MSB
	lda @operands,x		; and LSB
	tax
	rts

;--------------------------------------
@pushval:
	txa
	ldx @sp
	sta @operands,x		; store LSB
	tya
	sta @operands+1,x	; store MSB

	; update stack pointer
	lda @sp
	clc
	adc #$02
	sta @sp
	rts
.endproc

;******************************************************************************
; PARSE
; Parses the expression into a RPN list of tokens evaluatable by
; expr::eval_token_list
; IN:
;   - zp::line: pointer to the expression to parse
; OUT:
;   - $150: a list of tokens for evaluation (in RPN format)
.export __expr_parse
.proc __expr_parse
@i=zp::expr
@num_operators=zp::expr+1
@num_operands=zp::expr+2	; num operands * 2
@may_be_unary=zp::expr+3
@operators=$128+1
@operands=@operators+MAX_OPERATORS
@priorities=@operands+(MAX_OPERANDS*3)	; 2 bytes for val + 1 byte for type
	ldy #$00
	sty @num_operators
	sty @num_operands
	sty __expr_requires_reloc
	sty __expr_contains_global

	lda (zp::line),y
	bne :+
	sec
	rts			; no expression

:	; by default flag that operator might be unary
	iny
	sty @may_be_unary

@l0:	ldy #$00
	lda (zp::line),y
	jsr util::is_whitespace	; eat whitespace
	bne :+

	; check whitespace behavior, finish if configured as terminator
	lda end_on_whitespace
	bne @done
	jsr line::incptr
	bne @l0		; branch always

:	lda (zp::line),y
	jsr @isterminator
	bne @rparen

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
	bne @l0		; branch always - done evaluating this () block

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

	; if the operator to the left has >= priority, append it to result
	cmp @priorities,x
	beq :+
	bcs @process_ops_done
:	pha			; save priority
	jsr @eval		; append operation to the stack
	pla			; get priority
	jmp @process_ops	; continue til op on left has lower priority

@process_ops_done:
	pla
	jsr @pushop
	jsr line::incptr
	inc @may_be_unary
	bne @l0			; branch always

@getoperand:
	jsr get_operand		; have we found a valid operand?
	bcs @err		; no

@pushoperand:
	jsr @pushval
	lda #$00
	sta @may_be_unary
	jmp @l0

@done:	ldx @num_operators	; if there are still ops on stack
	beq @end		; no operators: terminate the RPN list
	jsr @eval		; evaluate each remaining operator
	jmp @done

@end:	lda #TOK_END
	ldx @i
	sta __expr_rpnlist,x
	RETURN_OK

@err:	; check if this is parentheses (could be indirect addressing)
	ldy #$00
	lda (zp::line),y
	cmp #')'
	beq @done
	RETURN_ERR ERR_LABEL_UNDEFINED

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
	lda @num_operands
	sec
	sbc #$03
	tax
	stx @num_operands
	ldy @operands+2,x	; MSB
	lda @operands,x		; TOKEN type
	pha
	lda @operands+1,x	; LSB
	tax
	pla
	rts

;------------------
; .A=token type .XY=token operand
@pushval:
	pha			; save TOKEN type
	txa
	ldx @num_operands
	cpx #MAX_OPERANDS
	bcs :+
	sta @operands+1,x	; store LSB
	tya
	sta @operands+2,x	; store MSB
	pla			; restore TOKEN type
	sta @operands,x		; store TOKEN type
	lda @num_operands
	;clc
	adc #$03
	sta @num_operands
:	rts

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
	ldy #@num_prios
:	cmp @priochars-1,y
	beq @prio_found
	dex
	bne :-
	tya			; .A=0
	rts			; not found
@prio_found:
	lda @prios-1,y
	rts

.PUSHSEG
.RODATA
@priochars: .byte '+', '-', '*', '/', '&', '^', '.', '<', '>'
@prios:	    .byte  1,   1,   2,   2,   3,   4,   5,   3,   3
@num_prios=*-@prios
.POPSEG

;------------------
; appends the operands involved in the evaluation followed by the operation
; to the RPN result
; returns the evaluation of the operator in .A on the operands @val1 and @val2
@eval:	jsr @popval
	jsr @appendval

	jsr @popop

	; check if operator is unary
	cmp #'<'
	beq @unary
	cmp #'>'
	beq @unary
@binary:
	; not unary, append a second argument
	pha
	jsr @popval
	jsr @appendval		; append the 2nd arg
	pla
	lda #TOK_BINARY_OP
	skw			; skip next instruction
@unary: lda #TOK_UNARY_OP

	; append the operator
	ldx @i
	sta __expr_rpnlist+1,x	; write operator
	sta __expr_rpnlist,x		; write TOKEN type
	inx
	inx
	stx @i
	rts

;--------------------------------------
; append .XY (token type in .A) to the RPN list result
@appendval:
	pha
	txa
	ldx @i
	sta __expr_rpnlist+1,x	; LSB
	tya
	sta __expr_rpnlist+2,x	; MSB
	pla
	sta __expr_rpnlist,x		; TOKEN type
	inx
	inx
	inx
	stx @i
	rts
.endproc

;******************************************************************************
; GETLABEL
; Reads the given label and returns the address of it if there
; is one
; IN:
;  - .XY: pointer to the label to get the address of
; OUT:
;  - .C   is set if no label is found
;  - .A:  the size of the label's address
;  - .XY: the value of the label
;  - r2:  the ID of the label (if any)
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

@dummy:	; default to the hint value/size
	lda #$ff
	ldxy zp::virtualpc	; TODO: assume smallest possible value

@updateline:
	pha	; save size
	tya	; save MSB
	pha

	; labels are involved, relocation is required
	inc __expr_requires_reloc

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
; GET OPERAND
; Attempts to get an operand from an expression (constant value or label)
; and returns the token for it if one was found
; IN:
;   - zp::line: the text to parse
; OUT:
;   - .A:  the token type
;   - .XY: the rest of the operand
;   - .C:  set if no operand was able to be parsed
.proc get_operand
	jsr isval
	bcs @label		; not a val, try label
	jsr get_val		; is this a value?
	bcs @label		; if not, try label
	lda #TOK_VALUE
	rts

@label: ldxy zp::line
	jsr get_label		; is it a label?
	bcs @done		; no
	lda #TOK_SYMBOL
	;clc
@done:	rts
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
.proc get_val
@val=zp::expr
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
	bne @err

	jsr line::incptr
	ldx zp::virtualpc
	ldy zp::virtualpc+1

	; always use absolute (2-byte) addressing for '*'
	lda #$02
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
