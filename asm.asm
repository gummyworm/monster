.include "codes.inc"
.include "text.inc"
.include "zeropage.inc"
.include "macros.inc"
.include "memory.inc"
.include "layout.inc"

line = zp::tmp0

.CODE

;--------------------------------------
; errors
ERR_OK=0
ERR_UNALIGNED_LABEL=-1
err_unaligned_label:
	.byte "label not in leftmost column",0
ERR_ILLEGAL_OPCODE=-2
err_illegal_opcode:
	.byte "unknown opcode:",$ff,0
ERR_ILLEGAL_ADDRMODE=-3
err_illegal_addrmode:
	.byte "invalid addressing mode: ",$ff,0
ERR_ILLEGAL_DIRECTIVE=-4
err_illegal_directive:
	.byte "unknown directive: ",$ff,0

errors:
	.word 0	 ; no error
	.word err_unaligned_label
	.word err_illegal_opcode
	.word err_illegal_addrmode
	.word err_illegal_directive

.proc mkerr
	cmp #$00
	bne :+
	jsr text::clrline
	ldx #<mem::linebuffer
	ldy #>mem::linebuffer
	lda #ERROR_ROW
	jsr text::puts
	rts

:	sta zp::tmp0
	tya
	pha
	txa
	pha

	lda zp::tmp0
	asl
	tax
	lda errors+1,x
	tay
	lda errors,x
	tax

	lda #ERROR_ROW
	jsr text::print
	pla
	pla
	rts
.endproc

;--------------------------------------
NUM_OPCODES = 46
CC_00=0
CC_01=6
CC_10=14
CC_IMP=22
opcodes:
; cc = 00
.byt "bit" ; 001
.byt "jmp" ; 010/011
.byt "sty" ; 100
.byt "ldy" ; 101
.byt "cpy" ; 110
.byt "cpx" ; 111
;cc = 01
.byt "ora" ; 000
.byt "and" ; 001
.byt "eor" ; 010
.byt "adc" ; 011
.byt "sta" ; 100
.byt "lda" ; 101
.byt "cmp" ; 110
.byt "sbc" ; 111
;cc = 10
.byt "asl" ; 000
.byt "rol" ; 001
.byt "lsr" ; 010
.byt "ror" ; 011
.byt "stx" ; 100
.byt "ldx" ; 101
.byt "dec" ; 110
.byt "inc" ; 111
;implied
.byt "brk"
.byt "rti"
.byt "rts"
.byt "php"
.byt "plp"
.byt "pha"
.byt "pla"
.byt "dey"
.byt "tay"
.byt "iny"
.byt "inx"
.byt "clc"
.byt "sec"
.byt "cli"
.byt "sei"
.byt "tya"
.byt "clv"
.byt "cld"
.byt "sed"
.byt "txa"
.byt "txs"
.byt "tax"
.byt "tsx"
.byt "dex"
.byt "nop"

;--------------------------------------
; report error prints the error in .A
.export __asm_reporterr
.proc __asm_reporterr
	cmp #$00
	bne :+
	jsr text::clrline
	ldx #<mem::linebuffer
	ldy #>mem::linebuffer
	lda #ERROR_ROW
	jsr text::puts
	rts
:	asl
	tax
	lda errors+1,x
	tay
	lda errors,x
	tax
	lda #ERROR_ROW
	jsr text::print
	rts
.endproc

;--------------------------------------
; asm_opcode assembles the opcode in (line)
.proc asm_opcode
	ldy #$00
	lda (line),y
.endproc

;--------------------------------------
; line assembles the string at (<X/>Y) into into asmresult.
; The size of the assembled operation is returned in .A (negative indicates
; an error occurred).
.proc tokenize
;flags
@indirect=zp::tmp2  ; 1=indirect, 0=absolute
@indexed=zp::tmp3   ; 1=x-indexed, 2=y-indexed, 0=not indexed
@immediate=zp::tmp4 ; 1=immediate, 0=not immediate
	stx line
	sty line

	lda #$00
	sta @indirect
	sta @indexed
	sta @immediate

@getws: lda (line),y
	incw line
	cmp #' '
	bne @getws
	
	lda (line),y
	cmp #'('
	bne @immediate

@lparen:
	inc @indirect
	incw line
	bne @abslabelorvalue

@immediate:
	cmp #'#'
	bne @abslabelorvalue
	inc @immediate

@abslabelorvalue:
	tya
	pha
	jsr getvalue
	beq :+
:	jsr getlabel
	beq :+
	jmp @err

	pla
	tay
	lda @indirect
	beq @index
@rparen:
	lda (line),y
	incw line
	cmp #')'
	beq @index
	jmp @err

@index: lda (line),y 
	incw line
	cmp #','
	bne @getws2

@getindex:
	lda (line),y
	cmp #'X'
	bne :+
	inc @indexed
:	cmp #'Y'
	bne @getws2
	inc @indexed
	inc @indexed

@getws2:
	lda (line),y
	incw line
	cmp #' '
	bne @getws2

@comment:
	cmp #$0d
	beq @done
	cmp #';'

@done:  lda #$00
	rts

@err:	lda #-1
	rts
.endproc

;--------------------------------------
; getvalue parses (line) for a hexadecimal value.  If it succeeds, the size
; is returned in .A and the value in (<.X/>.Y).
.proc getvalue
@val=zp::tmp0
	ldy #$00
	lda (line),y
	cmp #'$'
	bne :+
	iny

@l0:	cmp #' '
	beq @done
	cmp #'0'
	bcc @err
	cmp #'9'
	bcs :+
	sec
	sbc #'0'
	ora @val
	sta @val+1
	jmp @next

:	cmp #'a'
	bcc @err
	cmp #'f'+1
	bcs @err
	sec
	sbc #'0'
	ora @val
	sta @val+1

@next:	asl @val
	rol @val+1
	asl @val
	rol @val+1
	asl @val
	rol @val+1
	asl @val
	rol @val+1
	jmp @l0

@done:	lda @val+1
	beq :+
	lda #$01
:	ldx @val
	ldy @val+1
	rts

@err:	lda #-1
	rts
.endproc

;--------------------------------------
; getlabel returns the address of the label in (line) in (<X,>Y).
.proc getlabel
@l=zp::tmp2
@num=zp::tmp4
	lda #$00
	sta @num

@l0:	lda @num
	cmp numlabels
	bcs @err
	inc @num
	asl
	tax

	lda label_addresses,x
	sta @l
	ldy label_addresses+1,x
	sta @l+1
	lda (@l),y
	tax
	ldy #$00

@l1:	lda (line),y
	cmp (@l),y
	bne @l0
	iny
	dex
	bne @l1
	lda (line),y
	cmp #' '
	bne @err

@done:	lda #$00
	rts

@err:	lda #-1
	rts
.endproc

;--------------------------------------
.proc islabel
	ldy #$00
:	lda (line),y
	iny
	cpy #40
	bcs @notlabel
	cmp #':'
	bne :-

@done:	lda #ASM_LABEL
	rts
@notlabel:
	lda #-1
	rts
.endproc

;--------------------------------------
; isopcode returns ASM_OPCODE if (line) contains an opcode.
; The opcode's ID is returned in .X
.proc isopcode
@optab = zp::tmp2
@op = zp::tmp4
	lda #$00
	sta @op

	ldx #<opcodes
	ldy #>opcodes
	stx @optab
	sty @optab+1

@l0:	ldy #$02
@l1:	lda (line),y
	cmp (@optab),y
	bne @next
	dey
	bpl @l1

@done:	lda #ASM_OPCODE
	ldx @op
	rts

@next:	lda @optab
	clc
	adc #$03
	sta @optab
	bcc :+
	inc @optab+1
:	inc @op
	lda @op
	cmp #NUM_OPCODES
	bcc @l0

@err:	lda #ERR_ILLEGAL_OPCODE
	rts
.endproc

;--------------------------------------
; compile analiyzes (line) and updates the affected areas.
.export __asm_compile
.proc __asm_compile
	stx line
	sty line+1

	ldy #$00
	ldx #$00
@next:  jsr islabel
	bpl @noerr
	jsr isopcode
	bpl @noerr
@label:
@err:	lda #$02
	ldx line
	ldy line+1
	lda #$02
	jsr mkerr
	lda #ERR_ILLEGAL_OPCODE
	rts

@noerr: pha
	lda #$00
	;jsr mkerr
	pla
	rts
.endproc

;--------------------------------------
; findlabel returns the address that the label in (YX) (length in .A) 
; corresponds to.
.export __asm_findlabel
.proc __asm_findlabel
@label=zp::tmp0
@tab=zp::tmp2
@len = zp::tmp4
@id=zp::tmp5
	stx @label
	sty @label+1
	sta @len

	ldy #$00
	sty @id
	sty @id+1
@l0:	lda @len
	cmp (@tab),y
	bne @next
	
	tay
@strcmp:
	lda (@tab),y 
	cmp (@label),y
	dey
	bpl @strcmp
@found:	lda @id
	asl
	rol @id+1
	adc label_addresses
	sta @label
	lda @id+1
	adc label_addresses+1
	sta @label+1
	ldy #$00
	lda (@label),y
	tax
	iny
	lda (@label),y
	tay
	rts

@next:	lda @label
	clc
	adc @label
	sta @label
	bcc @l0
	inc @label+1
	bcs @l0

	rts
.endproc

;--------------------------------------
; addlabel adds a label of .A len in (YX) to the label table.  The address of
; the lable is provided in zp::tmp0
.export __asm_addlabel
.proc __asm_addlabel
@label=zp::tmp2
@savey=zp::tmp4
	pha
	sty @savey

	lda #<__asm_labels
	sta @label
	lda #>__asm_labels
	sta @label+1

	; find the next free label location (0 byte in the label table)
	ldy #$00
@l0:	lda (@label),y
	beq @found
	lda @label
	clc
	adc @label
	sta @label
	bcc @l0
	inc @label+1
	bcs @l0

	; free label location found
@found: pla
	ldy #$00
	sta (@label),y
	iny
	txa
	sta (@label),y
	iny
	lda @savey
	sta (@label),y

	; store the address of the label in the label_addresses table
	lda numlabels
	asl
	tax
	lda zp::tmp0
	sta label_addresses,x
	lda zp::tmp0+1
	sta label_addresses+1,x

	inc numlabels

	rts
.endproc

;--------------------------------------
.export __asm_labels
__asm_labels: .res 256 * 16
numlabels: .byt 0
label_addresses: .res 256 * 2


