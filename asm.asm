.include "ctx.inc"
.include "codes.inc"
.include "debug.inc"
.include "errors.inc"
.include "expr.inc"
.include "file.inc"
.include "layout.inc"
.include "labels.inc"
.include "macro.inc"
.include "macros.inc"
.include "math.inc"
.include "memory.inc"
.include "string.inc"
.include "text.inc"
.include "source.inc"
.include "util.inc"
.include "state.inc"
.include "zeropage.inc"
.CODE

;******************************************************************************
; TRUE/FALSE values for the active IF blocks
MAX_IFS = 4	; max nesting depth for .if/.endif

;******************************************************************************
; $40-$4B available for assembly
indirect=zp::asm ; 1=indirect, 0=absolute
indexed=zp::asm+1   ; 1=x-indexed, 2=y-indexed, 0=not indexed
immediate=zp::asm+2 ; 1=immediate, 0=not immediate
operandsz=zp::asm+3 ; size of the operand (in bytes) $ff indicates 1 or 2 byttes
cc=zp::asm+4
resulttype=zp::asm+5
opcode=zp::asm+8
lsb = zp::asm+$9
msb = zp::asm+$a

.BSS
;******************************************************************************
.export ifstack
ifstack:   .res MAX_IFS
ifstacksp: .byte 0
pcset:     .byte 0

.export __asm_origin
__asm_origin:
origin:    .word 0	; the lowest address in the program

.DATA
;******************************************************************************
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
.byt "bit" ; 001 3
.byt "jmp" ; 010 4
.byt "jmp" ; 011 5
.byt "sty" ; 100 6
.byt "ldy" ; 101 7
.byt "cpy" ; 110 8
.byt "cpx" ; 111 9
;cc = 01
.byt "ora" ; 000 a
.byt "and" ; 001 b
.byt "eor" ; 010 c
.byt "adc" ; 011 d
.byt "sta" ; 100 e
.byt "lda" ; 101 f
.byt "cmp" ; 110 10
.byt "sbc" ; 111 11
;cc = 10
.byt "asl" ; 000 12
.byt "rol" ; 001 13
.byt "lsr" ; 010 14
.byt "ror" ; 011 15
.byt "stx" ; 100 16
.byt "ldx" ; 101 17
.byt "dec" ; 110 18
.byt "inc" ; 111 19
opcode_branches:
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
opcode_singles_strings:
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

;******************************************************************************
; OPCODETAB
; This table is used for instructions (mostly single byte) that don't follow
; the encoding of the other instructions well.
; They are thus considered separately during assembly/disassembly
opcodetab:
.byt $10, $30, $50, $70, $90, $B0, $D0, $F0 	;branches
opcode_singles:
.byt $00, $20, $40, $60				; BRK, JSR, RTI, RTS
.byt $08, $28, $48, $68, $88, $A8, $C8, $E8	; PHP, PLP, PHA, PLA, DEY, TAY, INY, INX
.byt $18, $38, $58, $78, $98, $B8, $D8, $F8	; CLC, SEC, CLI, SEI, TYA, CLV, CLD, SED
.byt $8A, $9A, $AA, $BA, $CA, $EA		; TXA, TXS, TAX, TSX, DEX, NOP
num_opcode_singles=*-opcode_singles

; directives
directives:
.byte "db",0
.byte "eq",0
.byte "dw",0
.byte "inc",0
.byte "org",0
.byte "rorg",0
.byte "rep",0
.byte "mac",0
.byte "if",0
.byte "else",0
.byte "endif",0
.byte "ifdef",0
.byte "endmac",0

directives_len=*-directives

directive_vectors:
.word definebyte
.word defineconst
.word defineword
.word includefile
.word defineorg
.word define_psuedo_org
.word repeat
.word macro
.word do_if
.word do_else
.word do_endif
.word do_ifdef
.word create_macro

; current file being assembled
filename: .res 12

.CODE
;******************************************************************************
; VALIDATE
; Verifies that the string at (YX) is a valid instrcution
; The size of the assembled operation is returned in .A (negative indicates an error occurred).
; If the instruction contains a label, this proc will check that it is a valid label, but
; it does not require that the label is defined.
.export __asm_validate
__asm_validate:
	jmp tokenize

;******************************************************************************
; TOKENIZE
; Assembles the string at (YX) into an instruction in (asm::result)
; if (YX) contains an instruction.  Any labels or comments encountered are
; saved at the address in (pc).
; in:
;  - .XY: the string to assemble
;  - zp::asmresult: pointer to the location to assemble the instruction
; out:
;  - .A: the type of the result e.g. ASM_OPCODE or the error code
;  - .C: set if an error occurred
.export __asm_tokenize
__asm_tokenize:
.proc tokenize
	; copy the line to a new buffer and make it uppercase (assembly is
	; case-insensitive)
	lda #<mem::linebuffer2
	sta zp::tmp0
	lda #>mem::linebuffer2
	sta zp::tmp0+1
	jsr str::copy
	ldxy #mem::linebuffer2
	stxy zp::line
	ldxy #mem::linebuffer2
	jsr str::toupper

	jsr process_ws
	beq @noasm 	; empty line

; check if we're in an .IF (FALSE) and if we are, return
@checkifs:
	ldx #$ff
:	inx
	cpx ifstacksp
	beq @directive
	lda ifstack,x
	bne :-
@noasm:	RETURN_OK

; 1. check if the line contains a directive
@directive:
	jsr getdirective
	bcs @ctx

	lda #ASM_DIRECTIVE
	sta resulttype
	jmp handle_directive

; after directives, handle context if any
@ctx:
	jsr handle_ctx
	bcc @noctx
	RETURN_OK

@noctx:
	ldy #$00
	sty indirect
	sty indexed
	sty operandsz
	sty immediate
	sty lsb
	sty msb

; check if the line is a comment
@full_line_comment:
	lda (zp::line),y
	cmp #';'
	bne @opcode

	; rest of the line is a comment, we're done
	lda #ASM_COMMENT
	sta resulttype
	RETURN_OK

; check if the line contains an instruction
@opcode:
	jsr getopcode
	bcs @macro
	lda #ASM_OPCODE
	sta resulttype
	stx opcode	; save the opcode
	txa
	ldy #$00
	jsr writeb
	bcc @getopws
	rts

; check if the line contains a macro
@macro:
	ldxy zp::line
	jsr mac::get
	bcs @label
	pha
	jsr process_word	; read past macro name
	pla
	jsr assemble_macro
	bcs :+			; error
	lda #ASM_MACRO
	sta resulttype
	clc
:	rts

@label:
	jsr lbl::isvalid
	bcs @getopws
	sta resulttype
	ldx zp::line
	ldy zp::line+1
	lda zp::virtualpc
	sta zp::label_value
	lda zp::virtualpc+1
	sta zp::label_value+1
	jmp lbl::add

; from here onwards we are either reading a comment or an operand
@getopws:
	jsr process_ws
	bne @pound
	jmp @done

@pound:
	ldy #$00
	lda (zp::line),y
	cmp #'#'
	bne @lparen
	inc immediate
	incw zp::line
	lda (zp::line),y
	jmp @hi_or_low_byte	; if immediate, skip parentheses (treat as part of expression)
@lparen:
	cmp #'('
	bne @hi_or_low_byte
	inc indirect
	incw zp::line
	lda (zp::line),y

@hi_or_low_byte:
	cmp #'<'
	bne :+
	inc lsb
	incw zp::line
:	cmp #'>'
	bne @abslabelorvalue
	inc msb
	incw zp::line

@abslabelorvalue:
	jsr expr::eval
	bcc @store_value
	rts

; store the value, note that we don't really care if
; we write 2 bytes when we only need one (the next
; instruction will overwrite it and it doesn't affect our program size count).
@store_value:
	sta operandsz
	lda lsb		; handle '<' character
	beq :+
	lda #$01
	sta operandsz
	bne @store_lsb
:	lda msb		; handle '>' character
	beq @store_msb
	lda #$01
	sta operandsz
	tya
	tax
	jmp @store_lsb
@store_msb:
	tya
	ldy #$02
	jsr writeb
	bcc @store_lsb
@ret:	rts		; return err
@store_lsb:
	txa
	ldy #$01
	jsr writeb
	bcs @ret

@cont:	ldy #$00
	lda indirect
	beq @index
@rparen:
; look for closing paren or ",X"
	lda (zp::line),y
	incw zp::line
	cmp #','
	bne @rparen_noprex
	lda (zp::line),y
	incw zp::line
	cmp #'x'
	beq :+
	jmp @err
:	lda (zp::line),y
	incw zp::line
	cmp #')'
	beq :+
	jmp @err
:	inc indexed	; inc once to flag ,X indexed
	jmp @getws2

@rparen_noprex:
	cmp #')'
	beq @index
	jmp @err

@index:
	ldy #$00
	lda (zp::line),y
	cmp #','
	bne @getws2
	incw zp::line
@getindexx:
	lda (zp::line),y
	cmp #'x'
	bne @getindexy
	jsr is_ldx_stx
	bcs :+
	RETURN_ERR ERR_ILLEGAL_ADDRMODE	 ; ,X is illegal for LDX/STX
:	inc indexed	 ; inc once to flag ,X indexed
	incw zp::line
@getindexy:
	cmp #'y'
	bne @getws2
	inc indexed	 ; inc twice to flag ,Y indexed
	jsr is_ldx_stx
	bcc :+		 ; treat like ,X for encoding if LDX ,Y or STX ,Y
	inc indexed
:	incw zp::line

;------------------
; finish the line by checking for whitespace and/or a comment
@getws2:
	jsr process_ws

; check for comment or garbage
@comment:
	lda (zp::line),y
	jsr islineterminator
	beq @done
	RETURN_ERR ERR_UNEXPECTED_CHAR

; done, create the assembled result based upon the opcode, operand, and addr mode
@done:
	lda resulttype
	cmp #ASM_OPCODE
	beq :+
	RETURN_OK	; if not an instruction, we're done

:	jsr getaddrmode
	bcc @checkjmp
	rts

@checkjmp:
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
	jsr writeb
	bcc @noerr
	rts
@jmpabs:
	cpx #ABS
	bne @err 	; only ABS supported for JMP XXXX
	lda #$4c
	jsr writeb
	bcc @noerr
	rts

@getbbb:
; get bbb bits based upon the address mode and cc
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
	RETURN_ERR ERR_BRANCH_OUT_OF_RANGE
	bne @err		; address out of range

	; replace 2 byte operand with 1 byte relative address
:	txa
	sec
	sbc #$02	; offset is -2 from current instruction's address
	dey
	jsr writeb
	bcc :+
	rts
:	lda #$01
	sta operandsz
	jmp @noerr

@verifyimm:
	; remaining opcodes are single byte- implied/accumulator only
	cpx #IMPLIED
	beq @noerr
@err:	RETURN_ERR ERR_ILLEGAL_ADDRMODE

@noerr: ;------------------
; store debug info if enabled
@dbg:	lda zp::gendebuginfo
	beq @updatevpc
	ldxy zp::virtualpc	; current PC (address)
	stxy zp::tmp0
	ldxy dbg::srcline	; current line
	jsr dbg::storeline	; map them

;------------------
; update virtualpc by (1 + operand size)
@updatevpc:
	lda operandsz
	sec			; +1
	adc zp::virtualpc
	sta zp::virtualpc
	bcc @updatepc
	inc zp::virtualpc+1

;------------------
; update asmresult pointer by (1 + operand size)
@updatepc:
	lda operandsz
	sec			; +1
	adc zp::asmresult
	sta zp::asmresult
	bcc @retop
	inc zp::asmresult+1

@retop:	lda #ASM_OPCODE
	RETURN_OK

;------------------
; check that the BBB and CC combination we have is valid
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

; finally, check for invalid instructions ("gaps" in the ISA)
	ldx #@num_illegals-1
:	cmp @illegal_opcodes,x
	beq @err
	dex
	bpl :-
	jsr writeb
	bcc @noerr
	rts

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

;******************************************************************************
; GETADDRMODE
; Returns the address mode according to the global assembly flags:
; immediate, indexed, and indirect.
; This may or may not be legal for the instruction, it is constructed just
; from the syntax of the user's line.
; OUT:
;  - .A: the address mode
;  - .C: clear on success, set on error
.export getaddrmode
.proc getaddrmode
	; get addressing mode index for bbb tables
	lda operandsz
	cmp #$ff
	bne @sizedone
	; if we don't know the size yet, check both zp and abs
	jsr @zp
	bcc @ok
	jmp @abs

; we have the size, now dispatch as appropriate to zp, impl, or abs
@sizedone:
	cmp #$00
	beq @impl
	cmp #2
	beq @abs
	cmp #1
	beq @zp
@err:   RETURN_ERR ERR_OVERSIZED_OPERAND

;------------------
@zp:	lda immediate
	bne @imm
	ldx indexed
	lda indirect
	beq :+
	dex
	bpl :+
	; error- indirect zeropage not a valid addressing mode
@illegalmode:
	RETURN_ERR ERR_ILLEGAL_ADDRMODE
:	txa
	clc
	adc indirect
	adc indirect
	adc #ZEROPAGE
@ok:	RETURN_OK

;------------------
@abs:   lda immediate
	bne @oversized	; error- immediate abs illegal (operand too large)
	lda indirect
	beq :+
	lda indexed
	bne @err 	; error- indirect absolute doesn't support indexing
	lda #ABS_IND
	RETURN_OK
:	lda indexed
	clc
	adc #ABS
	RETURN_OK

@imm:	lda indirect
	bne @illegalmode ; error- immediate doesn't support indirection
	lda indexed
	bne @illegalmode ; error- immediate doesn't support indexing
	lda #IMMEDIATE
	RETURN_OK

@impl:	lda #IMPLIED
@done:	RETURN_OK
@oversized:
	RETURN_ERR ERR_OVERSIZED_OPERAND
.endproc

;******************************************************************************
; ADDRESS MODE TABLES
; The following tables store the bbb values for the encoding for various
; configurations of addressing, e.g. zeropage, x-indexed.
; They are stored consistently such that the same addressing type maps to
; the same location in each table.  This makes it easy to translate from the
; type of addressing we're doing and the bit representation of the bbb encoding
; for the instruction.
; There are 3 tables, each represents the bbb values for a given set of cc
; instructions: cc 01, cc 10, and cc 00.
; A $ff in the table represents an invalid addressing mode for that type of
; instruction.
IMPLIED=0
IMMEDIATE=1
ZEROPAGE=2
ZEROPAGE_X=3
ZEROPAGE_X_INDIRECT=4
ZEROPAGE_Y_INDIRECT=5
ABS=6
ABS_X=7
ABS_Y=8
ABS_IND=9

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

MODE_IMMEDIATE=$01
MODE_ZP=$02
MODE_ABS=$04
MODE_INDIRECT=$08
MODE_X_INDEXED=$10
MODE_Y_INDEXED=$20
MODE_IMPLIED=$40

bbb_modes:
bbb00_modes:
	.byte MODE_IMMEDIATE | MODE_ZP	; 000
	.byte MODE_ZP		; 001
	.byte $ff		; 010
	.byte MODE_ABS		; 011
	.byte $ff		; 100
	.byte MODE_ZP | MODE_X_INDEXED ; 101
	.byte $ff		; 110
	.byte MODE_ABS | MODE_X_INDEXED	; 111
bbb01_modes:
	.byte MODE_ZP | MODE_X_INDEXED | MODE_INDIRECT
	.byte MODE_ZP
	.byte MODE_IMMEDIATE | MODE_ZP
	.byte MODE_ABS
	.byte MODE_ZP | MODE_INDIRECT | MODE_Y_INDEXED
	.byte MODE_ZP | MODE_X_INDEXED
	.byte MODE_ABS | MODE_Y_INDEXED
	.byte MODE_ABS | MODE_X_INDEXED

bbb10_modes:
	.byte MODE_IMMEDIATE | MODE_ZP	; 000
	.byte MODE_ZP		; 001
	.byte MODE_IMPLIED	; 010
	.byte MODE_ABS		; 011
	.byte $ff		; 100
	.byte MODE_ZP | MODE_X_INDEXED ; 101 (Y_INDEXED for STX,LDX)
	.byte $ff		; 110
	.byte ABS | MODE_X_INDEXED	; 111 (Y_INDEXED for STX,LDX)

;******************************************************************************
; GETTEXT
; Parses an enquoted text string in zp::line and returns it in mem::spare
; returns the length in .A ($ff if no string was found)
.proc gettext
	ldy #$00
	lda (zp::line),y
	cmp #'"'
	bne @err

	ldx #$00
@l0:
	incw zp::line
	lda (zp::line),y
	beq @err	; no closing quote
	cmp #'"'
	beq @done
	sta mem::spare,x
	inx
	bne @l0
@done:
	incw zp::line
	txa
	RETURN_OK
@err:	RETURN_ERR ERR_SYNTAX_ERROR
.endproc


;******************************************************************************
; GETOPCODE
; Parses zp::line for an instruction and returns information about it if it
; is determined to be an instruction
; OUT:
;  - .A: ASM_OPCODE (on success) else error
;  - .X: the opcode's ID
;  - .C: set if (line) is not an opcode
;  - cc: updated with the cc part of the opcode
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
@l1:	lda (zp::line),y
	cmp (@optab),y
	bne @next
	dey
	bpl @l1

	; make sure there are no trailing characters
	ldy #$03
	lda (zp::line),y
	beq @done
	jsr util::is_whitespace
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

; if we reached this point, instruction is a CC 00 encoding, look up the opcode
; from a table
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
	lda zp::line
	clc
	adc #$03
	sta zp::line
	bcc :+
	inc zp::line+1
:	RETURN_OK

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

@err:	RETURN_ERR ERR_ILLEGAL_OPCODE
.endproc

;******************************************************************************
; GETDIRECTIVE
; checks if (zp::line) contains a directive and handles it if it does.
; OUT:
;  - .C: set if the contents of zp::line is not a directive
.proc getdirective
@cnt=zp::tmp2
	ldy #$00
	lda (zp::line),y
	cmp #'.'
	beq :+
	RETURN_ERR ERR_INVALID_DIRECTIVE

:	ldx #$00
	stx @cnt
@l0:	ldy #$01
@l1:	lda directives,x
	cmp #$00
	beq @found
	cmp (zp::line),y
	bne @next
	inx
	iny
	bne @l1

@next:
	cpx #directives_len
	bcc @ok
	RETURN_ERR ERR_INVALID_DIRECTIVE
@ok:	inc @cnt

@l2:	lda directives,x ; move to next directive
	inx
	cmp #$00
	beq @l0
	cpx #directives_len
	bcc @l2
	RETURN_ERR ERR_INVALID_DIRECTIVE

@found:
	; make sure there are no trailing characters
	lda (zp::line),y
	beq :+
	cmp #' '
	bne @next

:	tya
	clc
	adc zp::line
	sta zp::line
	bcc :+
	inc zp::line+1
:	jsr processws

	lda @cnt
	asl
	tax
	lda directive_vectors+1,x
	tay
	lda directive_vectors,x
	tax
	RETURN_OK
.endproc

;******************************************************************************
; HANDLE_DIRECTIVE
; Jumps to the given directive vector
; IN:
;  - .XY: the directive handler to jump to
.proc handle_directive
	stxy @vec
@vec=*+1
	jmp $fadd
.endproc

;******************************************************************************
; HANDLE_REPEAT
; Context handler for .rep/.endrep blocks
.proc handle_repeat
	ldxy #mem::linebuffer
	jsr ctx::write		; copy the linebuffer to the context
	ldxy #mem::linebuffer
	streq @endrep, 7	; are we at .endrep?
	beq @do_rep		; yes, assemble the REP block
	RETURN_OK

@do_rep:
	; disable the context for assembling
	lda #$00
	sta ctx::type

@l0:	; define a label with the value of the iteration
	jsr ctx::rewind
	ldxy zp::ctx+repctx::iter
	stxy zp::label_value
	ldxy zp::ctx+repctx::params
	jsr lbl::add

@l1:	; assemble the lines until .endrep
	jsr ctx::getline
	bcc :+
	rts			; propagate error, exit
:	streq @endrep, 7	; are we at .endrep?
	beq @next		; yep, do next iteration
	ldxy #mem::ctxbuffer

	; save the context
	lda zp::ctx
	pha
	lda zp::ctx+1
	pha

	jsr __asm_tokenize ; nope, assemble and repeat
	bcc :+
	rts	; return err

:	; restore the context
	pla
	sta zp::ctx+1
	pla
	sta zp::ctx

	jmp @l1

@next:	; increment iterator and repeat if there are more iterations left
	incw zp::ctx+repctx::iter
	ldxy zp::ctx+repctx::iter
	cmpw zp::ctx+repctx::iter_end
	bne @l0

@done:
	ldxy zp::ctx+repctx::params
	jsr lbl::del	; delete the iterator label
	jsr ctx::pop	; pop the context
	RETURN_OK

@endrep: .byte ".endrep"
.endproc

;******************************************************************************
; HANDLE_CTX
; OUT:
;  - .C: set if the line was handled by this handler
.proc handle_ctx
	; if verifying, don't handle context at all
	lda state::verify
	bne @done

	lda ctx::type
	beq @done
	lda ctx::type
	and #CTX_REPEAT
	beq :+
	jsr handle_repeat
	sec		; flag context handled
	rts
:	lda ctx::type
	and #CTX_MACRO
	beq @done
	jsr handle_macro
	sec		; flag context handled
	rts
@done:	clc
	rts
.endproc

;******************************************************************************
; PROCESS_WS
; Reads zp::line until a non-whitespace character is encountered
.proc processws
	ldy #$00
	lda (zp::line),y
	cmp #' '
	bne @done
@l0:
	incw zp::line
	lda (zp::line),y
	cmp #' '
	beq @l0
@done:
	rts
.endproc

;******************************************************************************
; PROCESSSTRING
; Reads all characters in zp::line until the next whitespace
.proc processstring
	ldy #$00
	lda (zp::line),y
	jsr util::is_whitespace
	beq @done
@l0:
	incw zp::line
	lda (zp::line),y
	jsr util::is_whitespace
	bne @l0
@done:
	rts
.endproc


;******************************************************************************
; DEFINEBYTE
; Defines 0 or more bytes and stores them in (asmresult)
; OUT:
;  - .A: the number of bytes written
.proc definebyte
	jsr process_ws
	ldxy zp::line
	jsr expr::getval
	bcs @text
	cmp #$01
	beq @ok
	RETURN_ERR ERR_OVERSIZED_OPERAND
@ok:	; store the extracted value
	ldy #$00
	txa
	jsr writeb
	bcs @ret
	incw zp::asmresult
	incw zp::virtualpc
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
	jsr writeb
	bcs @ret
	dey
	bpl :-

	txa
	sec
	adc zp::virtualpc
	sta zp::virtualpc
	bcc :+
	inc zp::virtualpc+1

:	txa
	sec
	adc zp::asmresult
	sta zp::asmresult
	bcc :+
	inc zp::asmresult+1
:	txa
	rts

@commaorws:
	ldy #$00
	lda (zp::line),y
	beq @done
	incw zp::line
	cmp #','
	beq definebyte
	cmp #' '
	beq @commaorws
	; unexpected character
@err:	RETURN_ERR ERR_SYNTAX_ERROR
@done:	clc
@ret:	rts
.endproc

;******************************************************************************
; DEFINEWORD
; Parses zp::line for a word value and stores it to zp::asmresult if possible.
; OUT:
;  - .C: set if a word could not be parsed
.proc defineword
	jsr process_ws
	ldxy zp::line
	jsr expr::getval
	bcs @err
	; store the extracted value
	tya
	ldy #$01
	jsr writeb
	bcs @ret		; return error
	txa
	dey
	jsr writeb
	bcs @ret		; return error

	incw zp::asmresult
	incw zp::asmresult
	incw zp::virtualpc
	incw zp::virtualpc
@commaorws:
	ldy #$00
	lda (zp::line),y
	beq @done
	incw zp::line
	cmp #','
	beq defineword
	cmp #' '
	beq @commaorws
	; unexpected character
@err:	RETURN_ERR ERR_SYNTAX_ERROR
@done:	clc
@ret:	rts
.endproc

;******************************************************************************
; INCLUDEFILE
; Include file assembles the contents of the given file.
;
; Also generates debug info; the debug info generated will depend on the pass
; Pass 1:
;  Gets the number of lines/segments in the file
; Pass 2:
;  Stores the corresponding lines for addresses of assembled code
.proc includefile
@filename=$100
@numlines=zp::tmp4
@numsegments=zp::tmp6
@type=zp::tmp7
	jsr processws
	ldy #$00
	sty @numlines
	sty @numlines+1
	sty @numsegments
@quote1:
	lda (zp::line),y
	cmp #'"'
	beq :+
@unenquoted:
	RETURN_ERR ERR_UNEXPECTED_CHAR
:	ldx #$00
	incw zp::line
@getfilename:
	lda (zp::line),y
	jsr util::is_whitespace
	beq @unenquoted
	cmp #'"'
	beq @readfile
	sta @filename,x
	incw zp::line
	inx
	bne @getfilename

@readfile:
	lda #$00
	sta @filename,x
	ldxy #@filename
	jsr file::open
	sta zp::file

	lda dbg::file
	pha		  ; save the current debug-info file
	ldxy #@filename
	jsr dbg::setfile  ; set the file for debug-info

@doline:
	ldxy #mem::spare
	lda zp::file
	jsr file::getline	; read a line from the file
	bcc @ok
	pla		  	; clean stack
	RETURN_ERR ERR_IO_ERROR	; return error

@ok:	cmp #$00
	beq @done
@asm:	ldxy #mem::spare
	jsr __asm_tokenize_pass
	jmp @doline

@done:  pla
	sta dbg::file	   ; restore debug file that we INCluded from
	lda zp::file
	jmp file::close	   ; close the file
.endproc

;******************************************************************************
; DEFINEORG
; Hanldes the .ORG directive.
; Parses an expression for a value and sets the asmresult and virtualpc
; addresses to it
; e.g.: `.ORG $1000` or `ORG $1000+LABEL`
.proc defineorg
	jsr processws
	ldxy zp::line
	jsr expr::getval
	bcc :+
	rts		; error
:	stxy zp::asmresult
	stxy zp::virtualpc
	lda pcset
	bne @chkorg
	inc pcset
	stxy origin
	jmp @done

	; check if this is lower than current base origin
@chkorg:
	cpy origin+1
	bcs @done
	bcc @set
	cpx origin
	bcc @done
@set:	stxy origin

@done:	lda #ASM_ORG
	RETURN_OK
.endproc

;******************************************************************************
; DEFINE_PSUEDO_ORG
; Hanldes the .RORG directive.
; Parses an expression for a value and sets the virtualpc  address to it.
; Note that the physical assembly target (asmresult) is unaffected.
; e.g.: `.RORG $1000` or `RORG $1000+LABEL`
.proc define_psuedo_org
	jsr processws
	ldxy zp::line
	jsr expr::getval
	bcc :+
	rts		; error
:	stxy zp::virtualpc
	RETURN_OK
.endproc

;******************************************************************************
; DEFINECONST
.proc defineconst
	jsr lbl::isvalid
	bcs @err
	lda zp::line	; save label name's address
	pha
	lda zp::line+1
	pha
	jsr processstring	; move past label name
	jsr processws		; eat whitespace
	jsr expr::getval	; get constant value
	bcc @ok
	pla
	pla
@err:
	RETURN_ERR ERR_SYNTAX_ERROR
@ok:
	stx zp::label_value
	sty zp::label_value+1
	pla
	tay
	pla
	tax
	jmp lbl::add
.endproc

;******************************************************************************
; REPEAT
; generates assembly for the parameterized code between this directive
; and the lines that follow until '.endrep'
; .rep 10,I
;   asl
; .endrep
; will produce 10 'asl's
.proc repeat
	jsr ctx::push	; push a new context

	jsr expr::eval ; get the number of times to repeat the code
	bcc @ok
	rts	 ; error

@ok:
	stxy zp::ctx+repctx::iter_end
	jsr process_ws
	ldy #$00
	lda (zp::line),y
	cmp #','
	beq :+
	RETURN_ERR ERR_UNEXPECTED_CHAR ; comma must follow the # of times to repeat

:	; get the name of the parameter
	incw zp::line
	ldy #$00
@saveparam:
	ldxy zp::line
	jsr ctx::addparam
	bcc @cont
	rts		; err

@cont:
	stxy zp::line
	lda #$00
	sta zp::ctx+repctx::iter
	sta zp::ctx+repctx::iter+1
	lda #CTX_REPEAT
	sta ctx::type
	RETURN_OK
.endproc

;******************************************************************************
; MACRO
; Begins the definition of a macro, which will continue until '.endmac' is
; found and the lines that follow until '.endrep'.
; .mac add8 A, B
;   lda #A
;   clc
;   adc #B
; .endmac
; will define a macro that can be used like:
;   add8 10, 20
.proc macro
	jsr ctx::push	; push a new context
; get the first parameter (the name)
@getname:
	jsr process_ws	; sets .Y to 0
	jsr islineterminator
	bne :+
	RETURN_ERR ERR_NO_MACRO_NAME
:	ldxy zp::line
	jsr ctx::addparam
	bcc :+
	rts		; return err
:	stxy zp::line	; update line pointer

@getparams:
	jsr process_ws	; sets .Y to 0
	lda (zp::line),y
	jsr islineterminator
	beq @done
	ldxy zp::line
	jsr ctx::addparam
	stxy zp::line

	; look for the comma or line-end
	jsr process_ws	; sets .Y to 0
	lda (zp::line),y
	jsr islineterminator
	beq @done
	cmp #','
	beq :+
	RETURN_ERR ERR_UNEXPECTED_CHAR

:	incw zp::line
	bne @getparams
@done:	lda #CTX_MACRO
	sta ctx::type
	lda #ASM_DIRECTIVE
	RETURN_OK
.endproc

;******************************************************************************
; HANDLE_MACRO
; when the macro context is active, reads the the current line into the
; context buffer
.proc handle_macro
	ldxy #mem::linebuffer
	jmp ctx::write		; copy the linebuffer to the context
.endproc

;******************************************************************************
; CREATE_MACRO
; This is the handler for the .endmac directive
; It uses the active context to finish creating a macro from that context.
.proc create_macro
	lda state::verify
	beq :+
	RETURN_OK	; verifying, don't create macro
:	ldxy #mem::linebuffer
	jsr ctx::write	; copy .ENDMAC to the context
	lda #$00
	sta ctx::type	; done with this context, disable it
	ldxy #$100
	jsr ctx::getparams ; fill $100 with param data
	pha
	ldxy #$100
	stxy zp::tmp0
	jsr ctx::getdata
	pla
	jmp mac::add
.endproc

;******************************************************************************
; PROCESS_WS
; Reads (line) and updates it to point past ' ' chars.
; out:
;  .Z: set if we're at the endo of the line
;  .A: the last character processed
;  .Y: 0
;  zp::line: updated to first non ' ' character
.proc process_ws
	ldy #$00
	lda (zp::line),y
	beq @done	; set .Z if 0
	cmp #' '
	bne @done
	incw zp::line
	bne process_ws
@done:	rts
.endproc

;******************************************************************************
; PROCESS_WORD
; Reads (line) and updates it to point past non whitespace chars.
; OUT:
;  - .A: contains the last character processed
;  - .Z: set if we're at the end of the line ($00)
.proc process_word
	ldy #$00
	lda (zp::line),y
	beq @done
	cmp #' '
	beq @done
	incw zp::line
	jmp process_word
@done:	rts
.endproc


;******************************************************************************
; PROCESS_END_OF_LINE
; Reads (line) and updates it to point to the terminating 0
; OUT:
;  - .C: set if any invalid characters were encountered
.proc process_end_of_line
@l0:
	ldy #$00
	lda (zp::line),y
	beq @done
	cmp #' '
	beq @next
	cmp #';'
	bne @err
@cmnt:
	; read comment
	lda (zp::line),y
	beq @done
	incw zp::line
	jmp @cmnt
@next:
	incw zp::line
	jmp @l0

@err:	RETURN_ERR ERR_SYNTAX_ERROR
@done:	RETURN_OK
.endproc

;******************************************************************************
; RESET
; Resets the internal assembly context (labels and pointer to target)
.export __asm_reset
.proc __asm_reset
	jsr __asm_resetpc
	lda #$00
	sta pcset
	sta ifstacksp
	jsr ctx::init
	jsr mac::init
	jmp lbl::clr
.endproc

;******************************************************************************
; RESETPC
; Resets the PC for, for example, beginning a new pass on the assembler
.export __asm_resetpc
.proc __asm_resetpc
	ldxy #mem::program
	stxy zp::asmresult
	stxy zp::virtualpc
	rts
.endproc

;******************************************************************************
; DISASSEMBLE
; disassembles the given instruction
; IN:
;  - .XY: the address of the instruction to disassemble
;  - zp::tmp0: the address of the buffer to disassemble to
; OUT:
;  - .A: the size of the instruction that was disassembled
;  - .C: clear if instruction was successfully disassembled
.export __asm_disassemble
.proc __asm_disassemble
@dst=zp::tmp0
@cc=zp::tmp2
@op=zp::tmp3
@operand=zp::tmp4

@optab=zp::tmp7
@cc8=zp::tmp7
@xxy=zp::tmp7
@cc8_plus_aaa=zp::tmp7
@modes=zp::tmp7

@bbb=zp::tmp8
@aaa=zp::tmp9
@opaddr=zp::tmpa
	stxy @opaddr
	ldy #$00
	lda (@opaddr),y
	sta @op
	iny
	lda (@opaddr),y
	sta @operand
	iny
	lda (@opaddr),y
	sta @operand+1

; check for single byte opcodes
	lda @op
	ldx #num_opcode_singles-1
:	cmp opcode_singles,x
	beq @implied_or_jsr
	dex
	bpl :-
	bmi @checkbranch

@implied_or_jsr:
	txa	; * 3 to get offset in opcode string table
	sta @op
	asl
	adc @op
	tax
	ldy #$00
:	lda opcode_singles_strings,x
	sta (@dst),y
	inx
	iny
	cpy #$03
	bne :-
	; TODO: handle JSR
	lda #$01
	RETURN_OK

; check for branches/exceptions
@checkbranch:
	lda @op
	and #$0f
	bne @not_branch
@branch:
	; get bits 5, 6 and 7 to determine branch type
	lda @op
	asl
	rol
	rol
	rol
	and #$07
	clc
	adc #$01
	sta @xxy
	asl
	adc @xxy
	tax
	ldy #$02
:	dex
	lda opcode_branches,x
	sta (@dst),y
	dey
	bpl :-

	lda @dst
	clc
	adc #$03
	sta @dst
	bcc @get_branch_target
	inc @dst+1

@get_branch_target:
	; calculate target address PC+2+operand
	; sign extend the operand
	lda @operand
	bpl :+
	lda #$ff
	skw
:	lda #$00
	sta @operand+1

	; operand + opaddr + 2
	lda @operand
	clc
	adc @opaddr
	sta @operand
	lda @operand+1
	adc @opaddr+1
	sta @operand+1
	lda #$02
	clc
	adc @operand
	sta @operand
	lda @operand+1
	adc #$00
	sta @operand+1
	lda #MODE_ABS
	sta @modes
	jmp @cont ; @operand now contains absolute address, display it

@not_branch:
	lda @op
	and #$03	; get cc
	sta @cc
	; get opcodes table offset (each block is 8 opcodes)
	asl
	asl
	asl
	sta @cc8

	; get aaa - opcode offset (each mneumonic is 3 bytes)
	lda @op
	lsr
	lsr
	lsr
	lsr
	lsr
	clc
	adc @cc8
	sta @cc8_plus_aaa
	asl
	adc @cc8_plus_aaa
	adc #<opcodes
	sta @optab
	lda #>opcodes
	adc #$00
	sta @optab+1

	; write the opcode (optab),aaa to the destination
	ldy #$02
:	lda (@optab),y
	sta (@dst),y
	dey
	bpl :-

	lda @dst
	clc
	adc #$03
	sta @dst
	bcc @get_addrmode
	inc @dst+1

@get_addrmode:
	; get bbb and find the addressing mode for the instruction
	lda @op
	lsr
	lsr
	and #$07
	sta @bbb

	; get the cc offset into the bbb_modes table
	lda @cc
	asl
	asl
	asl
	adc @bbb	; add bbb to get the table position of our instruction
	tax

	lda bbb_modes,x
	sta @modes
	and #MODE_IMPLIED
	beq @cont	; if not implied, go on
@implied:
	lda #$01	; 1 byte in size
	RETURN_OK

@cont:  ; add a space before operand
	ldy #$00
	lda #' '
	sta (@dst),y
	incw @dst

	; draw the opcode
	ldy #$00
@drawop:
	lda @modes
	and #MODE_INDIRECT
	beq :+
@indirect:
	lda #'('
	sta (@dst),y
	incw @dst

:	lda @modes
	and #MODE_IMMEDIATE
	beq :+
@immediate:
	lda #'#'
	sta (@dst),y
	incw @dst

:	lda @modes
	and #MODE_ZP
	beq :+
@zeropage:
	ldy #$00
	lda #'$'
	sta (@dst),y
	incw @dst
	lda @operand
	jsr util::hextostr
	tya
	ldy #$00
	sta (@dst),y
	txa
	iny
	sta (@dst),y
	incw @dst
	incw @dst

:	lda @modes
	and #MODE_ABS
	beq @chkindexed

@absolute:
	ldy #$00
	lda #'$'
	sta (@dst),y
	incw @dst
	lda @operand+1
	jsr util::hextostr
	tya
	ldy #$00
	sta (@dst),y
	txa
	iny
	sta (@dst),y
	incw @dst
	incw @dst
	lda @operand
	jsr util::hextostr
	tya
	ldy #$00
	sta (@dst),y
	txa
	iny
	sta (@dst),y
	incw @dst
	incw @dst

@chkindexed:
	lda @modes
	and #MODE_X_INDEXED
	beq :+
@xindexed:
	lda #','
	sta (@dst),y
	incw @dst
	lda #'x'
	sta (@dst),y
	incw @dst

:	lda @modes
	and #MODE_INDIRECT
	beq :+
@indirect2:
	lda #')'
	sta (@dst),y
	incw @dst

:	lda @modes
	and #MODE_Y_INDEXED
	beq @done
@yindexed:
	lda #','
	sta (@dst),y
	incw @dst
	lda #'y'
	sta (@dst),y
@done:  ldx #$02
	lda @modes
	and #MODE_ZP
	bne :+
	inx
:	txa
	RETURN_OK
.endproc

;******************************************************************************
; ASSEMBLE_MACRO
; Takes the contents of (line) and expands it to the corresponding macro.
; IN:
;  - .A the id of the macro to assemble
.proc assemble_macro
@cnt=zp::macros+$0e
@params=zp::macros
	pha

	ldx #$fe
@l0:	ldy #$00
	inx
	inx
@l1:	lda (zp::line),y
	beq @done
	iny
	cmp #' '
	bne @l1

	stx @cnt
	jsr process_ws
	jsr expr::eval
	bcc :+
	pla	; clean stack
	RETURN_ERR ERR_INVALID_EXPRESSION

:	txa
	ldx @cnt
	sta @params,x
	tya
	sta @params+1,x

	ldy #$00
@nextparam:
	lda (zp::line),y ; read unitl comma or endline
	beq @done
	cmp #';'
	beq @done
	incw zp::line
	cmp #','
	beq :+
	cmp #' '
	beq @nextparam
	RETURN_ERR ERR_INVALID_MACRO_ARGS

:	jmp @l0

@done:	pla
	lda state::verify
	beq :+
	RETURN_OK
:	jmp mac::asm
.endproc

;******************************************************************************
; DO_IF
; handles .IF during assembly
; OUT:
;  - .C: set if error
.proc do_if
	lda ifstacksp
	cmp #MAX_IFS
	bcc :+
	RETURN_ERR ERR_STACK_OVERFLOW

:	; evaluate the condition for the .IF
	jsr expr::eval
	cmpw #$00
	beq :+
	ldx #$01

	; store the TRUE/FALSE value to the if stack
	txa
	ldx ifstacksp
	sta ifstack,x
	inc ifstacksp
	lda #ASM_DIRECTIVE
	RETURN_OK
.endproc

;******************************************************************************
; DO_ENDIF
; Handles .ENDIF during assembly
.proc do_endif
	lda ifstacksp
	bne :+
	RETURN_ERR ERR_UNMATCHED_ENDIF

:	dec ifstacksp
	lda #ASM_DIRECTIVE
	RETURN_OK
.endproc

;******************************************************************************
; DO_ELSE
; handles .ELSE during assembly
.proc do_else
	ldx ifstacksp
	lda #$01
	eor ifstack,x
	sta ifstack,x
	lda #ASM_DIRECTIVE
	RETURN_OK
.endproc

;******************************************************************************
; DO_IFDEF
; handles the .IFDEF directive during assembly
.proc do_ifdef
	lda ifstacksp
	cmp #MAX_IFS
	bcc :+
	RETURN_ERR ERR_STACK_OVERFLOW

:	; check if the label exists
	ldxy zp::line
	jsr lbl::find
	lda #$00
	bcs :+	; label not defined
	lda #$01

:	; store TRUE/FALSE to the if stack
	ldx ifstacksp
	sta ifstack,x
	inc ifstacksp
@done:
	jsr process_word
	lda #ASM_DIRECTIVE
	RETURN_OK
.endproc

;******************************************************************************
; TOKENIZE_PASS
; Based on the current pass (zp::pass), calls the appropriate routine to
; handle assembly for that pass
.export __asm_tokenize_pass
.proc __asm_tokenize_pass
	lda zp::pass
	cmp #$01
	beq __asm_tokenize_pass1
	bne __asm_tokenize_pass2
.endproc

;******************************************************************************
; TOKENIZE_PASS1
; Calls tokenize and updates debug info (if enabled) as appropriate for the
; first pass.  This involves ending initializing segments on .ORG directives
; IN:
;  - .XY: the line to assemble
.export __asm_tokenize_pass1
.proc __asm_tokenize_pass1
	jsr __asm_tokenize
	bcc @ok
	rts		   ; return err
@ok:	ldx zp::gendebuginfo
	beq @done          ; if debug info off, we're done
	cmp #ASM_ORG
	bne @done
	jsr dbg::endseg	   ; end previous segment (if any)
	ldxy zp::virtualpc ; start address of segment
	jsr dbg::initseg   ; init a new segment
@done:	RETURN_OK
.endproc

;******************************************************************************
; TOKENIZE_PASS2
; Calls tokenize and updates debug info (if enabled) as appropriate for the
; second pass.  This involves switching segments on .ORG directives
; IN:
;  - .XY: the line to assemble
.export __asm_tokenize_pass2
.proc __asm_tokenize_pass2
	jsr __asm_tokenize
	bcc @ok
	rts		; return err

@ok:	; store debug info (if enabled)
	ldx zp::gendebuginfo
	beq @done
	cmp #ASM_ORG
	bne @done
	ldxy zp::virtualpc	; address of segment
	jmp dbg::startseg_addr	; set segment
@done:	RETURN_OK
.endproc

;******************************************************************************
; ISLINETERMINATOR
; IN:
;  .A: the character to check
; OUT:
;  - .Z: set if the .A is a 0 or ';'
.proc islineterminator
	cmp #$00
	beq :+
	cmp #';'
:	rts
.endproc

;******************************************************************************
; IS_LDX_STX
; Checks if the given opcode is a LDX/STX
; OUT:
;  - .C: clear if the given opcode is a LDX/STX
.proc is_ldx_stx
	pha
	lda cc
	cmp #$02	; only applicable if cc = 10
	bne @no

	lda opcode
	cmp #$80	; aaa = 100 STX ($8)
	beq @yes
	cmp #$a0	; aaa = 101 LDX ($a)
	beq @yes
@no:	pla
	sec
	rts
@yes:	pla
	clc
	rts
.endproc

;******************************************************************************
; WRITEB
; Stores a byte to (zp::asmresult),y
; Also checks if the origin has been set
; OUT:
;  - .C: set on error, clear on success
.proc writeb
	pha
	lda pcset
	bne :+
	pla
	RETURN_ERR ERR_NO_ORIGIN

:	pla
	sta (zp::asmresult),y
	RETURN_OK
.endproc
