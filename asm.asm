.include "codes.inc"
.include "text.inc"
.include "zeropage.inc"
.include "macros.inc"
.include "memory.inc"
.include "layout.inc"
.CODE

ERR_OK=0
ERR_UNALIGNED_LABEL=$ff
err_unaligned_label:
	.byte "label not in leftmost column",0
ERR_ILLEGAL_OPCODE=$ff-2
err_illegal_opcode:
	.byte "unknown opcode:",$ff,0
ERR_ILLEGAL_ADDRMODE=$ff-3
err_illegal_addrmode:
	.byte "invalid addressing mode: ",$ff,0
ERR_ILLEGAL_DIRECTIVE=$ff-4
err_illegal_directive:
	.byte "unknown directive: ",$ff,0

;--------------------------------------
line = zp::tmp0

;--------------------------------------
errors: .word 0	 ; no error
	.word err_unaligned_label
	.word err_illegal_opcode
	.word err_illegal_addrmode
	.word err_illegal_directive

;--------------------------------------
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
NUM_OPCODES = 58
CC_00=0
CC_01=8
CC_10=16
CC_IMP=24
AAA_JMP=$02
AAA_JMP_IND=$03
opcodes:
; cc = 00
.byt $ff,$ff,$ff ; unused
.byt "bit" ; 001
.byt "jmp" ; 010
.byt "jmp" ; 011
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
; branch $10, $30, $50...
.byt "bpl"
.byt "bmi"
.byt "bvc"
.byt "bvs"
.byt "bcc"
.byt "bcs"
.byt "bne"
.byt "beq"
;implied + jsr
.byt "brk"
.byt "jsr"
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

opcodetab:
; cc=00
.byt $10, $30, $50, $70, $90, $B0, $D0, $F0 ;branches
.byt $00, $20, $40, $60
.byt $08, $28, $48, $68, $88, $A8, $C8, $E8
.byt $18, $38, $58, $78, $98, $B8, $D8, $F8
.byt $8A, $9A, $AA, $BA, $CA, $EA

; directives
directives:
.byte "db",0,"dw",0
directives_len=*-directives

directive_vectors:
.word definebyte
.word defineword

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
; tokenize assembles the string at (YX) into an instruction in (asm::result)
; if (YX) contains an instruction.  Any labels or comments encountered are
; saved at the address in (pc).
; The size of the assembled operation is returned in .A (negative indicates
; an error occurred).
.export __asm_tokenize
__asm_tokenize:
.proc tokenize
;flags
@indirect=zp::tmp2  ; 1=indirect, 0=absolute
@indexed=zp::tmp3   ; 1=x-indexed, 2=y-indexed, 0=not indexed
@immediate=zp::tmp4 ; 1=immediate, 0=not immediate
@operandsz=zp::tmp5 ; size of the operand (in bytes)
@cc=zp::tmp9
@resulttype=zp::tmpa
	stx line
	sty line+1

	lda #$00
	sta @indirect
	sta @indexed
	sta @operandsz
	sta @immediate

	ldy #$00
@getws0:
	lda (line),y
	cmp #' '
	bne @opcode
	incw line
	jmp @getws0

@opcode:
	jsr getopcode
	cmp #ASM_OPCODE
	bne @label
	sta @resulttype
	txa
	ldy #$00
	sta (zp::asmresult),y
	jmp @getopws

@label:
	jsr islabel
	bmi @directive
	sta @resulttype
	ldx line
	ldy line+1
	jsr __asm_addlabel
	lda #$00
	rts

@directive:
	jsr getdirective
	cmp #$ff
	bne :+
	jmp @err
:	lda #ASM_DIRECTIVE
	sta @resulttype
	jmp @getws2

; from here onwards we are either reading a comment or an operand
@getopws:
	ldy #$00
	lda (line),y
	cmp #$0d
	bne @getws1
	jmp @done

@getws1:
	incw line
	lda (line),y
	cmp #$0d
	bne :+
	jmp @done
:	cmp #' '
	beq @getws1

	lda (line),y
	cmp #'('
	bne @pound
@lparen:
	inc @indirect
	incw line
	jmp @abslabelorvalue

@pound: cmp #'#'
	bne @abslabelorvalue
	inc @immediate
	incw line

@abslabelorvalue:
	tya
	pha
	jsr getvalue
	bcs @notvalue
	sta @operandsz
	tya
	ldy #$02
	sta (zp::asmresult),y
	dey
	txa
	sta (zp::asmresult),y
	jmp @cont

@notvalue:
	jsr getlabel
	cmp #$ff
	bne :+
	pla
	jmp @err

:	sta @operandsz
	tya
	pha
	txa
	ldy #$01
	sta (zp::asmresult),y
	pla
	cpy @operandsz
	beq @cont 		; zeropage label
	iny
	sta (zp::asmresult),y

; TODO: use address of label

@cont:	pla
	tay
	lda @indirect
	beq @index
@rparen:
; look for closing paren or ",X"
	lda (line),y
	incw line
	cmp #','
	bne @rparen_noprex
	lda (line),y
	incw line
	cmp #'x'
	beq :+
	jmp @err
:	lda (line),y
	incw line
	cmp #')'
	beq :+
	jmp @err
:	inc @indexed
	jmp @getws2

@rparen_noprex:
	cmp #')'
	beq @index
	jmp @err

@index:
	lda (line),y
	cmp #','
	bne @getws2
	incw line
@getindex:
	lda (line),y
	cmp #'x'
	bne @getindexy
	lda @indirect
	bne :+
	inc @indexed
	jmp @getws2
:	inc @indexed
@getindexy:
	cmp #'y'
	bne @getws2
	lda @indirect
	beq :+
:	inc @indexed
	inc @indexed

@getws2:
	ldy #$00
	lda (line),y
	incw line
	cmp #$0d
	beq @done
	cmp #' '
	beq @getws2

@comment:
	lda (line),y
	incw line
	cmp #$0d
	beq @done
	cmp #';'
	; error- trailing garbage
	bne @err

	; get length of comment
:	iny
	lda (line),y
	cmp #$0d
	bne :-

	ldx line
	ldy line+1
	jsr addcomment

; done, create the assembled result based upon the opcode, operand, and addr mode
@done:
	lda @resulttype
	cmp #ASM_OPCODE
	beq :+
	rts		; if not an instruction, we're done

:	jsr getaddrmode
	cmp #$ff
	beq @err
	tax

	; JMP (xxxx) has a different opcode than JMP
	ldy #$00
	lda (zp::asmresult),y
	cmp #$40
	bne @getbbb
	lda @cc
	bne @getbbb
	lda @indirect
	beq @jmpabs

@jmpind:
	cpx #ABS_IND	; only abs-indirect is supported for JMP (XXXX)
	bne @err
	lda #$6c
	sta (zp::asmresult),y
	jmp @noerr
@jmpabs:
	cpx #ABS
	bne @err 	; only ABS supported for JMP XXXX
	lda #$4c
	sta (zp::asmresult),y
	jmp @noerr

@getbbb:
; get bbb bits based upon the address mode and @cc
	lda @cc
	cmp #$03
	bne @validate_cc

	; check if opcode was a JSR
	lda (zp::asmresult),y
	cmp #$20
	bne :+
	cpx #ABS
	bne @err	; only ABS supported for JSR
	jmp @noerr

:	; check if opcode was a branch
	and #$1f
	cmp #$10
	bne :+
	cpx #ABS	; only ABS/ZP supported for branches
	bne @err
	jmp @noerr ; TODO: get relative address for branch instruction

:	; remaining opcodes are single byte- implied/accumulator only
	cpx #IMPLIED
	beq @noerr
@err:	lda #$ff
	rts

@noerr: ldx @operandsz
	inx
	txa
	pha

	; update asm::result pointer by (1 + operand size)
	clc
	adc zp::asmresult
	sta zp::asmresult
	bcc :+
	inc zp::asmresult+1
:	pla
	rts

@validate_cc:
	ldy @cc
	bne :+
	lda bbb00,x
:	cpy #$01
	bne :+
	lda bbb01,x
:	cpy #$02
	bne :+
	lda bbb10,x

:	cmp #$ff
	beq @err

	asl
	asl
	ora @cc
	ldy #$00
	ora (zp::asmresult),y

@final_validate:
	; check for invalid instructions ("gaps" in the ISA)
	ldx #@num_illegals-1
:	cmp @illegal_opcodes,x
	beq @err
	dex
	bpl :-

	sta (zp::asmresult),y
	jmp @noerr

@illegal_opcodes:
.byte %10001001 ; STA #imm

.byte %00000010 ; ASL #imm
.byte %00100010 ; ROL #imm
.byte %01000010 ; LSR #imm
.byte %01100010 ; ROR #imm
.byte %10000010 ; STX #imm
.byte %11000010 ; DEC #imm
.byte %11100010 ; INC #imm
.byte %10001010 ; STX A
.byte %10101010 ; LDX A
.byte %11001010 ; DEC A
.byte %11101010 ; INC A
.byte %10011110 ; STX ABS,X

.byte %00100000 ; BIT #imm
.byte %00110100 ; BIT zp,x
.byte %00111100 ; BIT abs,x
.byte %10000000 ; STY #imm
.byte %10011100	; STY abs,x
.byte %11010100 ; CPY zp,x
.byte %11011100 ; CPY abs,x
.byte %11110100 ; CPX zp,x
.byte %11111100 ; CPX abs,x


@num_illegals = *-@illegal_opcodes

.endproc

;---------------------------------------
; getaddrmode returns the address mode according to the provided flags
.export getaddrmode
.proc getaddrmode
@indirect=zp::tmp2  ; 1=indirect, 0=absolute
@indexed=zp::tmp3   ; 1=x-indexed, 2=y-indexed, 0=not indexed
@immediate=zp::tmp4 ; 1=immediate, 0=not immediate
@operandsz=zp::tmp5
	; get addressing mode index for bbb tables
	lda @operandsz
	beq @impl
	cmp #2
	beq @abs
	cmp #1
	beq @zp
@err:   lda #$ff		; error- oversized operand
	rts

@zp:	lda @immediate
	bne @imm
	ldx @indexed
	lda @indirect
	beq :+
	dex
	bmi @err 	; error- indirect zeropage not a valid addressing mode
:	txa
	clc
	adc @indirect
	adc @indirect
	adc #ZEROPAGE
	rts

@abs:   lda @indirect
	beq :+
	lda @indexed
	bne @err 	; error- indirect absolute doesn't support indexing
	lda #ABS_IND
	rts
:	lda @indexed
	clc
	adc #ABS
	rts

@imm:	lda @indirect
	bne @err	; error- immediate doesn't support indirection
	lda @indexed
	bne @err	; error- immediate doesn't support indexing
	lda #IMMEDIATE
	rts

@impl:	lda #IMPLIED
@done:	rts
.endproc

IMPLIED=0
IMMEDIATE=1
ABS=6
ABS_IND=9
ZEROPAGE=2
bbb01:
	.byte $ff ; implied/accumulator
	.byte $02 ; immediate
	.byte $01 ; zp
	.byte $05 ; zp,x
	.byte $00 ; (zp,x)
	.byte $04 ; (zp),y
	.byte $03 ; abs
	.byte $07 ; abs,x
	.byte $06 ; abs,y
	.byte $ff ; (abs)

bbb10:
	.byte $02 ; implied/accumulator
	.byte $00 ; immediate
	.byte $01 ; zp
	.byte $05 ; zp,x
	.byte $ff ; (zp,x)
	.byte $ff ; (zp),y
	.byte $03 ; abs
	.byte $07 ; abs,x
	.byte $ff ; abs,y
	.byte $ff ; (abs)

bbb00:
	.byte $ff ; implied/accumulator
	.byte $00 ; immediate
	.byte $01 ; zp
	.byte $05 ; zp,x
	.byte $ff ; (zp,x)
	.byte $ff ; (zp),y
	.byte $03 ; abs
	.byte $07 ; abs,x
	.byte $ff ; abs,y
	.byte $ff ; (abs)

;--------------------------------------
; getvalue parses (line) for a hexadecimal value.  If it succeeds, the size
; is returned in .A and the value in (<.X/>.Y) and line is updated to point
; after the value.
; .C is set on error and clear if a value was extracted.
.export getvalue
.proc getvalue
@val=zp::tmp6
	ldy #$00
	sty @val
	sta @val+1
	lda (line),y
	cmp #'$'
	bne @convertchar
	iny

@l0:	lda (line),y
	cmp #' '
	beq @done
	cmp #$0d
	beq @done
	cmp #','
	beq @done
	cmp #')'
	beq @done
	cmp #'0'
	bcc @err
	cmp #'9'+1
	bcs @convertchar
	sec
	sbc #'0'
	jmp @next

@convertchar:
	cmp #'a'
	bcc @err
	cmp #'f'+1
	bcs @err
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
	iny
	bne @l0

@done:	lda #$02
	ldx @val+1
	bne :+
	lda #$01
:	pha
	tya
	clc
	adc line
	sta line
	bcc :+
	inc line+1
:	pla
	ldx @val
	ldy @val+1
	clc
	rts

@err:	sec
	rts
.endproc

;--------------------------------------
; gettext parses an enquoted text string and returns it in mem::spare
; returns the length in .A ($ff if no string was found)
.proc gettext
	ldy #$00
	lda (line),y
	cmp #'"'
	bne @err

	ldx #$00
@l0:
	incw line
	lda (line),y
	cmp #'"'
	beq @done
	cmp #$0d
	beq @err	; no closing quote
	sta mem::spare,x
	inx
	bne @l0
@done:
	incw line
	txa
	clc
	rts
@err:
	sec
	rts
.endproc

;--------------------------------------
; getlabel returns the address of the label in (line) in (<X,>Y).
; The size of the label is returned in .A (1 if zeropage, 2 if not)
; line is updated to the character after the label.
.export getlabel
.proc getlabel
@l=zp::tmp6
@num=zp::tmp8
	lda #$ff
	sta @num

	lda #<__asm_labels
	sta @l
	lda #>__asm_labels
	sta @l+1
	ldx #$00
@l0:	inc @num
	lda @num
	cmp numlabels
	bcs @err

	txa
	clc
	adc @l
	bcc :+
	inc @l+1
:	sta @l
	ldy #$00
	lda (@l),y
	tax

	inc @l
	bne @l1
	inc @l+1
@l1:	lda (line),y
	cmp (@l),y
	bne @l0
	iny
	dex
	bne @l1
	lda (line),y
	cmp #' '
	beq @done
	cmp #$0d
	bne @err

@done:	tya
	clc
	adc line
	sta line
	bcc :+
	inc line+1

:	lda @num
	asl
	tax
	lda label_addresses,x
	ldy label_addresses+1,x
	tax

	; get the size of the label
	lda #2
	cpy #0
	bne :+
	lda #1
:	rts

@err:	lda #$ff
	rts
.endproc

;--------------------------------------
; labelat returns the label at the address in (YX), if .A is $ff, no label
; was found at the given address.
.export __asm_labelat
.proc __asm_labelat
@msb=zp::tmp4
@num=zp::tmp5
	lda #<(label_addresses+1)
	sta zp::tmp0
	lda #>(label_addresses+1)
	sta zp::tmp0+1
	lda #<label_addresses
	sta zp::tmp2
	lda #>label_addresses
	sta zp::tmp2+1

	sty @msb
	ldy #$00
	sty @num

@l0:	lda @num
	cmp numlabels
	bcc :+
	lda #$ff
	rts

:	inc @num
	; compare MSB
	lda @msb
	cmp (zp::tmp0),y
	bne @next
	; compare LSB
	txa
	cmp (zp::tmp2),y
	beq @found
@next:	incw zp::tmp0
	incw zp::tmp0
	incw zp::tmp2
	incw zp::tmp2
	bne @l0

@found: ; get the label name
	lda #<__asm_labels
	sta zp::tmp0
	lda #>__asm_labels
	sta zp::tmp0+1

	ldy #$00
@l1:	dec @num
	beq @done
	lda (zp::tmp0),y
	; move ptr to the next length prefixed label
	sec
	adc zp::tmp0
	sta zp::tmp0
	bcc @l1
	inc zp::tmp0+1
	bne @l1

@done:	ldx zp::tmp0
	ldy zp::tmp0+1
	lda #$00
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
	lda #$ff
	rts
.endproc

;--------------------------------------
; getopcode returns ASM_OPCODE if (line) contains an opcode.
; The opcode's ID is returned in .X
.proc getopcode
@optab = zp::tmp6
@op = zp::tmp8
@cc = zp::tmp9
	lda #$00
	sta @op
	sta @cc

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

	; make sure there are no trailing characters
	ldy #$03
	lda (line),y
	cmp #$0d
	beq @done
	cmp #' '
	beq @done
	jmp @err

@done:	lda @op
	tax
	cmp #CC_01
	bcc @setcc
	inc @cc
	cmp #CC_10
	bcc @setcc
	inc @cc
	cmp #CC_IMP
	bcc @setcc
	inc @cc

	; look up the opcode from a table
	sbc #CC_IMP
	tax
	lda opcodetab,x
	tax
	jmp @return

@setcc:	asl
	asl
	asl
	asl
	asl
	tax

@return:
	; update line ptr and return
	lda line
	clc
	adc #$03
	sta line
	bcc :+
	inc line+1
:	lda #ASM_OPCODE
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
.proc getdirective
@cnt=zp::tmp2
	ldy #$00
	lda (line),y
	cmp #'.'
	beq :+
	lda #$ff	; not a directive
	rts

:	ldx #$00
	stx @cnt
@l0:	ldy #$00
@l1:	lda directives,x
	beq @found
	inx
	iny
	cmp (line),y
	beq @l1

	cpx #directives_len
	bne :+
	lda #$ff	; no match
	rts

:	inc @cnt
@l2:	lda directives,x ; move to next directive
	inx
	cmp #$00
	beq @l0

@found:
	tya
	sec
	adc line
	sta line
	bcc :+
	inc line+1
:	jsr processws

	lda @cnt
	asl
	tax
	lda directive_vectors,x
	sta @vec
	lda directive_vectors+1,x
	sta @vec+1
@vec=*+1
	jmp $fadd
.endproc

;--------------------------------------
.proc processws
	ldy #$00
	lda (line),y
	cmp #' '
	bne @done
@l0:
	incw line
	lda (line),y
	cmp #' '
	beq @l0
@done:
	rts
.endproc

;--------------------------------------
; defines 0 or more bytes and stores them in (asmresult)
; Returns the number of bytes written in .A
.proc definebyte
	jsr getvalue
	bcs @text
	cmp #$01
	bne @err	; over/undersized value
	; store the extracted value
	ldy #$00
	txa
	sta (zp::asmresult),y
	incw zp::asmresult
	jmp @commaorws

@text:	jsr gettext
	bcs @err
	; store the extracted text
	tay
	tax
	beq @done
	dex
	dey
:	lda mem::spare,y
	sta (zp::asmresult),y
	dey
	bpl :-
	txa
	sec
	adc zp::asmresult
	sta zp::asmresult
	bcc :+
	inc zp::asmresult+1
:	txa
	rts

@commaorws:
	ldy #$00
	lda (line),y
	cmp #$0d
	beq @done
	incw line
	cmp #','
	beq definebyte
	cmp #' '
	beq @commaorws
	; unexpected character
@err:
	lda #$ff
	rts

@done:
	rts

.endproc

;--------------------------------------
.proc defineword
	; TODO
	lda #$ff
	rts
.endproc

;--------------------------------------
; compile analyzes (YX) and updates the affected areas.
.export __asm_compile
.proc __asm_compile
	stx line
	sty line+1

	ldy #$00
	ldx #$00
@next:  jsr islabel
	bpl @noerr
	jsr getopcode
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
; addlabel adds a ':' terminated label of in (YX) to the label table.  The address of
.export __asm_addlabel
.proc __asm_addlabel
@label=zp::tmp6
@savey=zp::tmp8
@src=zp::tmpa
@len=zp::tmpc
	lda #$00
	sta @len
	stx @src
	sty @src+1

	lda #<__asm_labels
	sta @label
	lda #>__asm_labels
	sta @label+1

	; find the next free label location (0 byte in the label table)
	ldy #$00
@l0:	lda (@label),y
	beq @found
	incw @label
	bne @l0

@found:
	; get the label length
:	lda (@src),y
	iny
	cmp #':'
	bne :-
	dey

	; free label location found, write the length
	tya
	ldy #$00
	sta (@label),y
	tay
	dey
	incw @label

	; write the label
:	lda (@src),y
	sta (@label),y
	dey
	bpl :-

@done:
	; store the address of the label in the label_addresses table
	lda numlabels
	asl
	tax
	lda line
	sta label_addresses,x
	lda line+1
	sta label_addresses+1,x

	inc numlabels
	lda numlabels
	lsr
	lsr
	lsr
	tay
	lda numlabels
	and #$07
	tax
	lda #$fe
:	sec
	rol
	dex
	bpl :-
	and labelflags,y
	sta labelflags,y
	rts
.endproc

;--------------------------------------
; addcomment adds a comment of .A len in (YX) to the label table.  The address
; of the comment is provided in zp::tmp0
.proc addcomment
	jsr __asm_addlabel
	lda numlabels
	lsr
	lsr
	lsr
	tay
	lda numlabels
	and #$07
	tax
	lda #$01
:	asl
	dex
	bpl :-
	ora labelflags,y
	sta labelflags,y
	rts
.endproc


;--------------------------------------
.export __asm_labels
__asm_labels: .res 256 * 16
numlabels: .byt 0
label_addresses: .res 256 * 2

; if corresponding bit is 0- label represents a label, 1- comment
labelflags:
.res 256/8
