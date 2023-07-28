.include "codes.inc"
.include "errors.inc"
.include "layout.inc"
.include "macros.inc"
.include "memory.inc"
.include "text.inc"
.include "util.inc"
.include "zeropage.inc"
.CODE

;--------------------------------------
indirect=zp::asm ; 1=indirect, 0=absolute
indexed=zp::asm+1   ; 1=x-indexed, 2=y-indexed, 0=not indexed
immediate=zp::asm+2 ; 1=immediate, 0=not immediate
operandsz=zp::asm+3 ; size of the operand (in bytes)
cc=zp::asm+4
resulttype=zp::asm+5
label_value = zp::asm+6 ; param to addlabel
line = zp::asm+8

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
.byte "db",0
.byte "eq",0
.byte "dw",0
.byte "inc",0
directives_len=*-directives

directive_vectors:
.word definebyte
.word defineconst
.word defineword
.word includefile

;--------------------------------------
; validate verifies that the string at (YX) is a valid instrcution
; The size of the assembled operation is returned in .A (negative indicates an error occurred).
; If the instruction contains a label, this proc will check that it is a valid label, but
; it does not require that the label is defined.
.export __asm_validate
__asm_validate:
	jmp tokenize

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
	stx line
	sty line+1

	jsr process_ws

	ldy #$00
	sty indirect
	sty indexed
	sty operandsz
	sty immediate

@full_line_comment:
	lda (line),y
	cmp #';'
	bne @opcode
	; rest of the line is a comment, we're done
	lda #ASM_COMMENT
	sta resulttype
	rts

@opcode:
	jsr getopcode
	cmp #ERR_ILLEGAL_OPCODE
	beq @label
	sta resulttype
	txa
	ldy #$00
	sta (zp::asmresult),y
	jmp @getopws

@label:
	jsr islabel
	bmi @directive
	sta resulttype
	ldx line
	ldy line+1
	lda zp::asmresult
	sta label_value
	lda zp::asmresult+1
	sta label_value+1
	jsr __asm_addlabel
	lda #$00
	rts

@directive:
	jsr getdirective
	cmp #$ff
	bne :+
	jmp @err
:	lda #ASM_DIRECTIVE
	sta resulttype
	jmp @getws2

; from here onwards we are either reading a comment or an operand
@getopws:
	jsr process_ws
	bne :+
	jmp @done

:	lda (line),y
	cmp #'('
	bne @pound
@lparen:
	inc indirect
	incw line
	jmp @abslabelorvalue

@pound: cmp #'#'
	bne @abslabelorvalue
	inc immediate
	incw line

@abslabelorvalue:
	jsr getvalue
	bcs @notvalue
	sta operandsz
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
	jmp @err

:	sta operandsz
	tya
	pha
	txa
	ldy #$01
	sta (zp::asmresult),y
	pla
	cpy operandsz
	beq @cont 		; zeropage label
	iny
	sta (zp::asmresult),y

@cont:
	tay
	lda indirect
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
:	inc indexed
	jmp @getws2

@rparen_noprex:
	cmp #')'
	beq @index
	jmp @err

@index:
	ldy #$00
	lda (line),y
	cmp #','
	bne @getws2
	incw line
@getindexx:
	lda (line),y
	cmp #'x'
	bne @getindexy
	inc indexed
	incw line
@getindexy:
	cmp #'y'
	bne @getws2
	inc indexed
	inc indexed
	incw line

@getws2:
	jsr process_ws

@comment:
	lda (line),y
	beq @done
	cmp #$0d
	beq @done
	cmp #';'
	; error- trailing garbage
	beq :+
	jmp @err

	; get length of comment
:	iny
	lda (line),y
	beq :+
	cmp #$0d
	bne :-

:	ldx line
	ldy line+1
	jsr addcomment

; done, create the assembled result based upon the opcode, operand, and addr mode
@done:
	lda resulttype
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
	lda cc
	bne @getbbb
	lda indirect
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
	lda cc
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
	bne @verifyimm

	cpx #ABS	; only ABS/ZP supported for branches
	bne @err
	; convert operand to relative address
	ldy #$01
	lda (zp::asmresult),y
	sec
	sbc zp::asmresult
	iny
	tax
	lda (zp::asmresult),y
	sbc zp::asmresult+1
	beq :+
	cmp #$ff
	beq :+
	bne @err		; address out of range

	; replace 2 byte operand with 1 byte relative address
:	txa
	sec
	sbc #$02	; offset is -2 from current instruction's address
	dey
	sta (zp::asmresult),y
	lda #$01
	sta operandsz
	jmp @noerr

@verifyimm:
	; remaining opcodes are single byte- implied/accumulator only
	cpx #IMPLIED
	beq @noerr
@err:	lda #$ff
	rts

@noerr:
	; update asm::result pointer by (1 + operand size)
	lda operandsz
	sec
	adc zp::asmresult
	sta zp::asmresult
	bcc :+
	inc zp::asmresult+1
:	lda #ASM_OPCODE
	rts

@validate_cc:
	ldy cc
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
	ora cc
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
	; get addressing mode index for bbb tables
	lda operandsz
	beq @impl
	cmp #2
	beq @abs
	cmp #1
	beq @zp
@err:   lda #$ff		; error- oversized operand
	rts

@zp:	lda immediate
	bne @imm
	ldx indexed
	lda indirect
	beq :+
	dex
	bmi @err 	; error- indirect zeropage not a valid addressing mode
:	txa
	clc
	adc indirect
	adc indirect
	adc #ZEROPAGE
	rts

@abs:   lda immediate
	bne @err	; error- immediate abs illegal (operand too large)
	lda indirect
	beq :+
	lda indexed
	bne @err 	; error- indirect absolute doesn't support indexing
	lda #ABS_IND
	rts
:	lda indexed
	clc
	adc #ABS
	rts

@imm:	lda indirect
	bne @err	; error- immediate doesn't support indirection
	lda indexed
	bne @err	; error- immediate doesn't support indexing
	lda #IMMEDIATE
	rts

@impl:	lda #IMPLIED
@done:	rts
.endproc

;--------------------------------------
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
; getvalue parses (line) for a hexadecimal value up to 16 bits in size.
; If it succeeds, the size
; is returned in .A and the value in (<.X/>.Y) and line is updated to point
; after the value.
; .C is set on error and clear if a value was extracted.
.export getvalue
.proc getvalue
@val=zp::tmp6
	ldy #$00
	sty @val
	sty @val+1
	lda (line),y
	cmp #'$'
	beq @hex

@decimal:
	ldxy line
	jsr atoi	; convert to binary
	bcc :+
	sec		; error
	rts
:	adc line
	sta line
	bcc :+
	inc line+1
:	stx @val
	sty @val+1
	jmp @success

@hex:
	iny
@l0:	lda (line),y
	jsr is_null_return_space_comma_closingparen_newline
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
	bcc :+
	; oversized value
	sec
	rts
:	iny
	bne @l0

@done:
	tya
	clc
	adc line
	sta line
	bcc @success
	inc line+1
@success:
	ldx @val
	ldy @val+1
	cpy #$00
	beq :+
	lda #$02	; 2 bytes
	skw
:	lda #$01	; 1 byte
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
	beq @err	; no closing quote
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
; label_address returns the address of the label in (.YX)
; The size of the label is returned in .A (1 if zeropage, 2 if not)
; line is updated to the character after the label.
.export __asm_label_address
.proc __asm_label_address
	stx line
	sty line+1
	jmp getlabel
.endproc

;--------------------------------------
; validlabel returns with .Z set if the string at (line) contains valid
; characters for a label.
; line is updated to the character after the label.
.export validlabel
.proc validlabel
@l0:
	; first character must be a letter
	lda (line),y
	cmp #'a'
	bcc @err
	cmp #'z'+1
	bcs @err
@findend:
	incw line
	lda (line),y
	jsr is_whitespace
	beq @done
	cmp #')'
	beq @done
@err:
	lda #$ff
@done:
	rts
.endproc

;--------------------------------------
; getlabel returns the address of the label in (line) in (<X,>Y).
; The size of the label is returned in .A (1 if zeropage, 2 if not, $ff if no label found)
; line is updated to the character after the label.
.export getlabel
.proc getlabel
@l=zp::tmp6
@num=zp::tmp8
@sz=zp::tmp9
	lda #$ff
	sta @num

	lda #<__asm_labels
	sta @l
	lda #>__asm_labels
	sta @l+1

	lda #$00
	sta @sz

@l0:	inc @num
	lda @num
	cmp numlabels
	bcs @err

	; add label size to label pointer
	lda @sz
	adc @l
	sta @l
	bcc :+
	inc @l+1

:	ldy #$00
	lda (@l),y	; get new size of label
	sta @sz
	tax

	incw @l
@l1:	lda (line),y
	cmp (@l),y
	bne @l0
	iny
	dex
	bne @l1

	lda (line),y
	jsr is_null_return_space_comma_closingparen_newline
	bne @err

@done:	lda @sz
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
; returns ASM_LABEL if the contents on (line) are a valid label
.proc islabel
	jsr getopcode	; make sure string is not an opcode
	cmp #ERR_ILLEGAL_OPCODE
	beq @cont
	lda #ERR_ILLEGAL_LABEL
	rts

@cont:
	ldy #$00
	lda (line),y
	cmp #'.'	; label cannot have '.' prefix
	beq @notlabel
:	lda (line),y
	iny
	cpy #40
	bcs @notlabel
	cmp #' '
	beq @done
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
; Returns:
;  - .A contains ASM_OPCODE (on success) else error
;  - .X contains the opcode's ID
;  - tmp9 is updated with the .CC part of the opcode
.proc getopcode
@optab = zp::tmp6
@op = zp::tmp8
	lda #$00
	sta @op
	sta cc

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
	beq @done
	jsr is_whitespace
	beq @done
	jmp @err

@done:	lda @op
	tax
	cmp #CC_01
	bcc @setcc
	inc cc
	cmp #CC_10
	bcc @setcc
	inc cc
	cmp #CC_IMP
	bcc @setcc
	inc cc

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
	dex
@l0:	ldy #$00
	inx
@l1:	lda directives,x
	beq @found
	inx
	iny
	cmp (line),y
	beq @l1

	cpx #directives_len
	bcc :+
	lda #$ff	; no match
	rts

:	inc @cnt
	dex
@l2:
	inx
	lda directives,x ; move to next directive
	beq @l0
	cpx #directives_len
	bcc @l2
	lda #$ff	; no match
	rts

@found:
	tya
	sec		; +1
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
; processstring reads all characters until the next whitespace
.proc processstring
	ldy #$00
	lda (line),y
	jsr is_whitespace
	beq @done
@l0:
	incw line
	lda (line),y
	jsr is_whitespace
	bne @l0
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
	beq @done
	cmp #$0d
	beq @done
	incw line
	cmp #','
	beq definebyte
	cmp #' '
	beq @commaorws
	; unexpected character
@err:	lda #$ff
@done:	rts
.endproc

;--------------------------------------
.proc defineword
	jsr getvalue
	bcs @err
	; store the extracted value
	tya
	ldy #$01
	sta (zp::asmresult),y
	txa
	dey
	sta (zp::asmresult),y
	incw zp::asmresult
	incw zp::asmresult
@commaorws:
	ldy #$00
	lda (line),y
	beq @done
	cmp #$0d
	beq @done
	incw line
	cmp #','
	beq defineword
	cmp #' '
	beq @commaorws
	; unexpected character
@err:	lda #$ff
@done:	rts
.endproc

;--------------------------------------
.proc includefile

.endproc

;--------------------------------------
.proc defineconst
	jsr islabel
	cmp #ASM_LABEL
	bne @err
	lda line	; save label name's address
	pha
	lda line+1
	pha
	jsr processstring
	jsr processws
	jsr getvalue
	bcc :+
	pla
	pla
@err:
	lda #$ff
	rts
:	stx label_value
	sty label_value+1
	pla
	tay
	pla
	tax
	jmp __asm_addlabel
.endproc

;--------------------------------------
; addlabel adds a ':' terminated label in (YX) to the label table.
; the current value of asm::result is used to define its address
.export __asm_addlabel
.proc __asm_addlabel
@label=zp::tmp6
@src=zp::tmpa
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
	; get the label length (we look for whitespace because this routine is
	; also used to read constants)
:	lda (@src),y
	iny
	jsr is_whitespace
	beq @length_found
	cmp #':'
	bne :-

@length_found:
	dey

	; write the length
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
	lda label_value
	sta label_addresses,x
	lda label_value+1
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
; process_ws reads (line) and updates it to point past ' ' chars.
; .A contains the last character processed on return
; .Z is set if we're at the end of the line ($0d or $00)
.proc process_ws
	ldy #$00
	lda (line),y
	beq @done
	cmp #$0d
	beq @done
	cmp #' '
	bne @done
	incw line
	jmp process_ws
@done:	rts
.endproc

;--------------------------------------
; is_null_space_comma_closingparen returns .Z set if the char in .A is:
; 0,$0d,' ', ',', or ')'
.proc is_null_return_space_comma_closingparen_newline
	cmp #$00
	beq @done
	jsr is_whitespace
	beq @done
	cmp #','
	beq @done
	cmp #')'
@done:	rts
.endproc

;--------------------------------------
; is_whitespace returns .Z set if the character in .A is whitespace
.proc is_whitespace
	cmp #$0d
	beq :+
	cmp #' '
:	rts
.endproc

;--------------------------------------
; reset resets the internal assembly context (labels and pointer to target)
.export __asm_reset
.proc __asm_reset
	ldxy #mem::program
	stxy zp::asmresult

	lda #$00
	tax
	stx numlabels
@clrlabels:
	sta __asm_labels,x
	sta __asm_labels+$100,x
	dex
	bne @clrlabels
	rts
.endproc

;--------------------------------------
.export __asm_labels
__asm_labels: .res 256 * 16
numlabels: .byt 0
.export label_addresses
label_addresses: .res 256 * 2

; if corresponding bit is 0- label represents a label, 1- comment
labelflags:
.res 256/8
