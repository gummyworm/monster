.include "errors.inc"
.include "labels.inc"
.include "macros.inc"
.include "math.inc"
.include "state.inc"
.include "util.inc"

;--------------------------------------
MAX_OPERATORS=$10
MAX_OPERANDS=$10/2

;--------------------------------------
; EVAL
; resolves the contents of the given zp::line and returns its evaluated value
; in:
;  - .XY: pointer to the expression to evaluate
; out:
;  - .A: the size of the returned value in bytes
;  - .XY: the result of the evaluated expression
;  - .C: clear on success or set on failure
;
.export __expr_eval
.proc __expr_eval
@val1=zp::expr
@val2=zp::expr+2
@num_operators=zp::expr+4
@num_operands=zp::expr+5
@operators=$100
@operands=$120
@priorities=$130
	lda #$00
	sta @num_operators
	sta @num_operands
@l0:
	ldy #$00
	lda (zp::line),y
	jsr @isterminator
	beq @done

@rparen:
	cmp #'('
	bne @lparen
	jsr @pushop
	incw zp::line
	jmp @l0

@lparen:
	cmp #')'
	bne @checkop

@paren_eval:
	ldx @num_operators
	dex
	bmi @err	; no parentheses found
	lda @operators,x
	cmp #'('
	bne :+
	jsr @popop	; pop the parentheses
	incw zp::line
	jmp @l0		; and we're done evaluating this () block

:	jsr @eval	; evaluate the top 2 operands
	jmp @paren_eval

@checkop:
	ldy #$00
	lda (zp::line),y
	jsr util::isoperator
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
:	pha		; save priority
	jsr @eval	; evaluate the top 2 elements of the operand stack
	pla		; get priority
	jmp @process_ops	; continue until the op to the left has lower priority

@process_ops_done:
	pla
	jsr @pushop
	incw zp::line
	jmp @l0

@getoperand:
	jsr __expr_getval	; is this a value?
	bcc :+
	ldxy zp::line
	jsr get_label	; is it a label?
	bcs @err
:	jsr @pushval
	jmp @l0

@err:
	; check if this is parentheses (could be indirect addressing)
	cmp #')'
	beq @done
	RETURN_ERR ERR_LABEL_UNDEFINED

@done:
	ldx @num_operators
	beq @getresult
	jsr @eval
	jmp @done

@getresult:
	ldx @operands
	lda #$01
	ldy @operands+1
	beq :+
	;cpy #$ff	; 1 byte negative
	;beq :+
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
	lda #$00
	rts
@prio1:	lda #$01
	rts
@prio2: lda #$02
	rts

;------------------
; returns the evaluation of the operator in .A on the operands @val1 and @val2
@eval:
	jsr @popval
	stxy @val1
	jsr @popval
	stxy @val2

	jsr @popop
	cmp #'+'
	bne :+
@add:
	lda @val1
	clc
	adc @val2
	tax
	lda @val1+1
	adc @val2+1
	tay
	jmp @pushval

:	cmp #'-'
	bne :+
@sub:
	lda @val2
	sec
	sbc @val1
	tax
	lda @val2+1
	sbc @val1+1
	tay
	jmp @pushval

:	cmp #'*'
	bne :+
	; get the product TODO: 32-bit precision expressions?
	jsr m::mul16
	jmp @pushval

:	cmp #'/'
	bne @unknown_op
	jsr m::div16
	ldx @val1
	ldy @val1+1
	jmp @pushval

@unknown_op:
	rts
.endproc

;--------------------------------------
; GETLABEL
; get_label reads the given label and returns the address of it if there
; is one
; in:
;  - .XY pointer to the label to get the address of
; out:
;  - .C is set if no label is found
;  - .A: the size of the label's address
;  - .XY: the value of the label
.proc get_label
	jsr lbl::isvalid ; if we're verifying, let this pass if its a valid label
	bcs @done

	lda  state::verify
	beq :+

	lda #$ff	; flag that we don't know the size of the label
	ldxy #$00	; assume smallest possible value
	beq @updateline

:	ldxy zp::line
	jsr lbl::addr
	bcs @done

@updateline:
	pha
	tya
	pha

	ldy #$00
@l0:	lda (zp::line),y
	jsr util::isseparator
	beq :+
	incw zp::line
	bne @l0

:	pla
	tay
	pla
	clc
@done:	rts
.endproc

;--------------------------------------
; GETVAL
; parses the given string for a decimal or hexadecimal value up to 16 bits in size.
; If it succeeds, the size
; is returned in .A and the value in (<.X/>.Y) and zp::line is updated to point
; after the value.
; The character '*' is also parsed and results in the value of zp::asmresult
; .C is set on error and clear if a value was extracted.
.export __expr_getval
.proc __expr_getval
@val=zp::expr+$a
	ldy #$00
	sty @val
	sty @val+1
	lda (zp::line),y
	cmp #'$'
	beq @hex

	cmp #'*'
	bne @decimal
	iny
	lda (zp::line),y
	jsr util::isseparator
	beq :+
	jmp @err

:	incw zp::line
	ldx zp::asmresult
	ldy zp::asmresult+1
	beq *+3
	lda #$02
	skw
	lda #$01
	RETURN_OK

;------------------
@decimal:
	ldxy zp::line
	jsr atoi	; convert to binary
	bcs @err
	adc zp::line
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

	cmp #'0'
	bcc @err
	cmp #'9'+1
	bcs @convertchar
	sec
	sbc #'0'
	jmp @next

@badhex:
	RETURN_ERR ERR_UNEXPECTED_CHAR

@convertchar:
	cmp #'a'
	bcc @badhex
	cmp #'f'+1
	bcs @badhex
	sec
	sbc #'a'-$a

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

@done:
	tya
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

@err:	sec
	rts
.endproc
