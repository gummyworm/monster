.include "ctx.inc"
.include "codes.inc"
.include "debug.inc"
.include "errors.inc"
.include "expr.inc"
.include "file.inc"
.include "finalex.inc"
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
; ASSEMBLER OVERVIEW
; The assembler operates in 2 passes.
; Pass 1:
;  - generate symbol table (label names and addresses)
;  - create macro definitions (.MAC)
;
; Pass 2:
;  - write the program binary by assembling:
;   - instructions
;   - macro invocations
;   - .REP blocks
;   - directives like .DB and .DW
;
; There are (sensible) limitations due to the 2 pass nature of the assembler
;  1. Macros must be defined before first use
;  2. The size of all labels must be known (or correctly implied) in the first
;     pass. For example:
;     ```
;      LDA TARGET
;      LOOP:
;     ```
;     To generate the correct addresse for LOOP, the assembler needs to know if
;     TARGET is a zeropage or absolute address.
;     With insufficient data (e.g. forward references), labels are assumed to be
;     absolute (2 byte) addresses, which is usually a safe assumption unless
;     you're writing code in the zeropage.
;  3. Constants (.EQ) must be defined before first use (for similar reasons
;     that labels must be defined before use)
;     For example, consider this erroneous case:
;     ```
;       LDA ADDR
;       .EQ ADDR $10
;     ```
;     In this example, we don't know whether to use zeropage or absolute
;     addressing for the LDA, which could lead to incorrect addresses being
;     generated for the rest of the pass.
;     ```
;       .EQ ADDR $10
;       LDA ADDR
;     ```
;     In this ^ correct example, we know to use zeropage addressing and the
;     first and second passes will generate the same labels hereafter.
;******************************************************************************

;******************************************************************************
; TRUE/FALSE values for the active IF blocks
MAX_IFS      = 4 ; max nesting depth for .if/.endif
MAX_CONTEXTS = 3 ; max nesting depth for contexts (activated by .MAC, .REP, etc)

;******************************************************************************
; $40-$4B available for assembly
indirect   = zp::asm    ; 1=indirect, 0=absolute
indexed    = zp::asm+1   ; 1=x-indexed, 2=y-indexed, 0=not indexed
immediate  = zp::asm+2 ; 1=immediate, 0=not immediate
operandsz  = zp::asm+3 ; size of the operand (in bytes) $ff indicates 1 or 2 byttes
cc         = zp::asm+4
resulttype = zp::asm+5
opcode     = zp::asm+8
lsb        = zp::asm+$9
msb        = zp::asm+$a

.BSS
;******************************************************************************
.export ifstack
ifstack:   .res MAX_IFS
ifstacksp: .byte 0

.export __asm_pcset
__asm_pcset:
pcset: .byte 0

contextstack:   .res MAX_CONTEXTS
contextstacksp: .byte 0

.export __asm_origin
__asm_origin:
origin: .word 0	; the lowest address in the program

;******************************************************************************
; ASMBUFFER
; Source is copied here so that it can be messed with while assembling
.export asmbuffer
asmbuffer: .res 40

.DATA
;******************************************************************************
NUM_OPCODES = 58
CC_00       = 0
CC_01       = 8
CC_10       = 16
CC_IMP      = 24
AAA_JMP     = $02
AAA_JMP_IND = $03

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

;******************************************************************************
; DIRECTIVES
DIRECTIVE_ELSE = 9
DIRECTIVE_ENDIF = 10
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
.byte "endrep",0
directives_len=*-directives

;******************************************************************************
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
.word handle_repeat

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
	; check if there is a breakpoint on this line and set it if there is
	stxy zp::line

	ldy #$00
	lda (zp::line),y
	cmp #BREAKPOINT_CHAR
	bne @copy

@setbrk:
	lda zp::pass
	cmp #2
	bne @copy		; only set breakpoints in pass 2
	ldxy zp::virtualpc	; current PC (address)
	jsr dbg::toggle_breakpoint	; set the breakpoint
	incw zp::line		; advance line beyond the breakpoint

@copy:	; copy the line to a new buffer and make it uppercase (assembly is
	; case-insensitive)
	lda #<asmbuffer
	sta zp::tmp0
	lda #>asmbuffer
	sta zp::tmp0+1
	ldxy zp::line
	jsr str::copy
	ldxy #asmbuffer
	stxy zp::line
	jsr str::toupper

	jsr process_ws
	beq @noasm 	; empty line

; check if we're in an .IF (FALSE) and if we are, return
@checkifs:
	lda ifstacksp
	beq @directive	; no active .IF
	ldx #$00
:	inx
	lda ifstack,x
	beq @if_false
	cpx ifstacksp
	beq @directive
	bne :-

@if_false:
	; asm is off, check for ENDIF or ELSE
	jsr getdirective
	bcs @noasm
	cmp #DIRECTIVE_ENDIF
	beq @exec_directive
	cmp #DIRECTIVE_ELSE
	beq @exec_directive
@noasm:	RETURN_OK

; 1. check if the line contains a directive
@directive:
	jsr getdirective
	bcs @ctx

@exec_directive:
	lda #ASM_DIRECTIVE
	sta resulttype
	stxy zp::jmpvec
	jmp zp::jmpaddr

; after directives, handle context if any
@ctx:	jsr handle_ctx
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
	rts		; return err

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

; check if the line is a label definition
@label: jsr lbl::isvalid
	bcs @getopws
	sta resulttype
	ldxy zp::line
	lda zp::virtualpc
	sta zp::label_value
	lda zp::virtualpc+1
	sta zp::label_value+1
	jsr lbl::add
	ldxy zp::line
	jsr lbl::islocal
	bne :+
	jsr lbl::clrlocals	; clear locals if this is a non-local label
:	jsr process_word
	jsr @finishline
	bcs :+
	lda #ASM_LABEL
:	rts

; from here on we are either reading a comment or an operand
@getopws:
	jsr process_ws
	bne @pound
	jmp @done

@pound: ldy #$00
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
	rts		; eval failed

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
	bcs @ret	; if error, return

; we've written the value of the operand, now look for
; ',X' or ',Y' to see if this is indexed addressing
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
@finishline:
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
	jsr readb
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
	rts		; return err
@jmpabs:
	cpx #ABS
	bne @err 	; only ABS supported for JMP XXXX
	lda #$4c
	jsr writeb
	bcc @noerr
	rts		; return err

@getbbb:
; get bbb bits based upon the address mode and cc
	lda cc
	cmp #$03
	beq :+
	jmp @validate_cc

:	; check if opcode was a JSR
	jsr readb
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
	jsr readb
	sec
	sbc zp::asmresult
	iny
	tax
	jsr readb
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
	rts		; return err
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
@dbg:	lda state::verify
	bne @updatevpc
	lda zp::gendebuginfo
	beq @updatevpc
	ldxy zp::virtualpc	; current PC (address)
	stxy zp::tmp0
	ldxy dbg::srcline
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
@optmp=zp::tmp0
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
	sta @optmp
	jsr readb
	ora @optmp

; finally, check for invalid instructions ("gaps" in the ISA)
	ldx #@num_illegals-1
:	cmp @illegal_opcodes,x
	beq @err
	dex
	bpl :-
	jsr writeb
	bcc @noerr
	rts		; return err

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
	.byte MODE_ZP		        ; 001
	.byte $ff		        ; 010
	.byte MODE_ABS		        ; 011
	.byte $ff		        ; 100
	.byte MODE_ZP | MODE_X_INDEXED  ; 101
	.byte $ff		        ; 110
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
	.byte MODE_ZP		        ; 001
	.byte MODE_IMPLIED	        ; 010
	.byte MODE_ABS		        ; 011
	.byte $ff		        ; 100
	.byte MODE_ZP | MODE_X_INDEXED  ; 101 (Y_INDEXED for STX,LDX)
	.byte $ff		        ; 110
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

@found: ; make sure there are no trailing characters
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
:	jsr process_ws

	lda @cnt
	asl
	tax
	ldy directive_vectors+1,x
	lda directive_vectors,x
	tax
	lda @cnt
	RETURN_OK
.endproc

;******************************************************************************
; HANDLE_REPEAT
; Handler for .endrep.
; Generates the repeated assembly block defined between here and the previous
; .endrep
.proc handle_repeat
	; copy .endrep to the buffer
	ldxy #asmbuffer
	jsr ctx::write

	; disable this context
	lda #$00
	jsr set_ctx_type

	; if this is pass 1, exit out; REP is handled in pass 2
	lda zp::pass
	cmp #$02
	bcc @done

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

	; save the context
	lda zp::ctx
	pha
	lda zp::ctx+1
	pha

	; assemble the current line
	ldxy #mem::ctxbuffer
	jsr __asm_tokenize
	bcc :+
	sta zp::tmp0		; save errcode
	pla			; clean stack
	pla
	lda zp::tmp0		; get errcode
	rts			; return err

:	; restore the context
	pla
	sta zp::ctx+1
	pla
	sta zp::ctx
	jmp @l1			; repeat until .ENDREP is found

@next:	; increment iterator and repeat if there are more iterations left
	incw zp::ctx+repctx::iter
	ldxy zp::ctx+repctx::iter
	cmpw zp::ctx+repctx::iter_end
	bne @l0

	; cleanup iterator label and context
	ldxy zp::ctx+repctx::params
	jsr lbl::del	; delete the iterator label
@done:	jmp ctx::pop	; pop the context

@endrep: .byte ".endrep"
.endproc

;******************************************************************************
; HANDLE_CTX
; If this is the first pass, copies the contents of the asmbuffer to the current
; context.
; OUT:
;  - .C: set if the line was handled by this handler
.proc handle_ctx
	; if verifying, don't handle context at all
	jsr get_ctx_type
	beq @done
	ldxy #asmbuffer
	jsr ctx::write	; copy the linebuffer to the context
	sec		; flag context handled
	rts
@done:	clc		; flag context NOT handled
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
	jsr expr::eval
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

; update program pointer
:	txa
	sec			; +1
	adc zp::asmresult
	sta zp::asmresult
	bcc @commaorws
	inc zp::asmresult+1

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
	jsr expr::eval
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
	jsr process_ws
	ldy #$00
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
	beq @doinc
	sta @filename,x
	incw zp::line
	inx
	bne @getfilename

@doinc: lda #$00
	sta @filename,x
	ldxy #@filename

; entry point for assembling a given file
.export __asm_include
__asm_include:
@err=zp::tmpa
@fname=zp::tmpc
@readfile:
	stxy @fname
	jsr file::open
	bcc :+
	rts		; return err
:	pha		; save the id of the file we're working on
	sta zp::file

	; save the current debug-info file
	lda dbg::file
	pha
	lda dbg::srcline
	pha
	lda dbg::srcline+1
	pha

	; add the filename to debug info (if it isn't yet) and reset line no.
	ldxy @fname
	jsr dbg::setfile
	ldxy #1
	stxy dbg::srcline

; read a line from file
@doline:
	ldxy #mem::spare
	lda zp::file
	jsr file::getline	; read a line from the file
	bcc @ok
	jmp @close		; close file and return the error

@ok:	cmp #$00
	beq @close

; assemble the line
@asm:	ldxy #mem::spare
	lda zp::file
	pha
	jsr __asm_tokenize_pass
	ldx #$00
	bcc :+
	tax
:	stx @err
	pla
	sta zp::file
	bcs @close

@next:	incw dbg::srcline	; next line
	jmp @doline		; repeat

@close:	; restore debug line and file info
	pla
	sta dbg::srcline
	pla
	sta dbg::srcline+1
	pla
	sta dbg::file

	pla		; get the file ID for the include file
	jsr file::close	; close the file
	lda @err	; get err code
	cmp #$01	; set carry if > 0
	rts
.endproc

;******************************************************************************
; DEFINEORG
; Hanldes the .ORG directive.
; Parses an expression for a value and sets the asmresult and virtualpc
; addresses to it
; e.g.: `.ORG $1000` or `ORG $1000+LABEL`
.proc defineorg
	jsr process_ws
	ldxy zp::line
	jsr expr::eval
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
	jsr process_ws
	ldxy zp::line
	jsr expr::eval
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
	lda zp::line		; save label name's address
	pha
	lda zp::line+1
	pha
	jsr processstring	; move past label name
	jsr process_ws		; eat whitespace
	jsr expr::eval		; get constant value
	bcc @ok
	pla
	pla
@err:	RETURN_ERR ERR_SYNTAX_ERROR

@ok:	stxy zp::label_value
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

	lda zp::pass
	cmp #$01
	beq @done	; REP is handled in pass 2

	jsr expr::eval  ; get the number of times to repeat the code
	bcc @ok
	rts	 	; error evaluating # of reps expression

@ok:	stxy zp::ctx+repctx::iter_end
	jsr process_ws
	ldy #$00
	lda (zp::line),y
	cmp #','
	beq @getparam
	RETURN_ERR ERR_UNEXPECTED_CHAR ; comma must follow the # of reps

@getparam:
	; get the name of the parameter
	incw zp::line
	ldy #$00
@saveparam:
	ldxy zp::line
	jsr ctx::addparam
	bcc @cont
	rts		; error adding parameter

@cont:	stxy zp::line	; update line pointer to after parameter
	lda #$00
	sta zp::ctx+repctx::iter	; initialize iterator
	sta zp::ctx+repctx::iter+1
@done:	lda #CTX_REPEAT
	jsr set_ctx_type	; store REPEAT as current context type
	RETURN_OK
.endproc

;******************************************************************************
; MACRO
; Begins the definition of a macro, which will continue until '.endmac' is
; .mac add8 A, B
;   lda #A
;   clc
;   adc #B
; .endmac
; will define a macro that can be used like:
;   add8 10, 20
.proc macro
	jsr ctx::push	; push a new context

	lda zp::pass
	cmp #$02
	bcs @done	; macro definition handled in pass 1

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
	jsr set_ctx_type	; store MACRO as current context type
	lda #ASM_DIRECTIVE
	RETURN_OK
.endproc

;******************************************************************************
; CREATE_MACRO
; This is the handler for the .endmac directive
; It uses the active context to finish creating a macro from that context.
.proc create_macro
	lda zp::pass
	cmp #$02
	bcs @done	; done, macros are defined in pass 1

	; copy .ENDMAC to the context
	ldxy #asmbuffer
	jsr ctx::write

	; fill $100 with param data
	ldxy #$100
	jsr ctx::getparams

	; get the context data (the macro definition)
	pha
	ldxy #$100
	stxy zp::tmp0
	jsr ctx::getdata
	pla

	; create the macro
	jsr mac::add

@done:	; done with this context, disable it
	lda #$00
	jsr set_ctx_type
	jmp ctx::pop	; cleanup; pop the context
.endproc

;******************************************************************************
; PROCESS_WS
; Reads (line) and updates it to point past ' ' chars and non-printing chars
; out:
;  .Z: set if we're at the endo of the line
;  .A: the last character processed
;  .Y: 0
;  zp::line: updated to first non ' ' character
.proc process_ws
	ldy #$00
	lda (zp::line),y
	bmi :+
	beq @done	; set .Z if 0
	cmp #' '
	bne @done
:	incw zp::line
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
@l0:	ldy #$00
	lda (zp::line),y
	beq @done
	cmp #' '
	beq @next
	cmp #';'
	bne @err

@cmnt:	; read comment
	lda (zp::line),y
	beq @done
	incw zp::line
	jmp @cmnt

@next:	incw zp::line
	jmp @l0

@err:	RETURN_ERR ERR_SYNTAX_ERROR
@done:	RETURN_OK
.endproc

;******************************************************************************
; RESET
; Resets the internal assembly context (labels and pointer to target)
.export __asm_reset
.proc __asm_reset
	lda #$00
	sta pcset
	sta ifstacksp
	sta contextstacksp
	jsr ctx::init
	jsr mac::init
	jsr lbl::clr
	; fall through to RESETPC
.endproc

;******************************************************************************
; RESETPC
; Resets the PC for, for example, beginning a new pass on the assembler
.export __asm_resetpc
.proc __asm_resetpc
	lda #$00
	sta pcset
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
.ifdef USE_FINAL
	bank_read_byte #FINAL_BANK_USER, @opaddr
	sta @op
	bank_read_byte_rel #FINAL_BANK_USER, @opaddr, #1
	sta @operand
	bank_read_byte_rel #FINAL_BANK_USER, @opaddr, #2
	sta @operand+1
.else
	ldy #$00
	lda (@opaddr),y
	sta @op
	iny
	lda (@opaddr),y
	sta @operand
	iny
	lda (@opaddr),y
	sta @operand+1
.endif

; check for single byte opcodes
	lda @op
	ldx #num_opcode_singles-1
:	cmp opcode_singles,x
	beq @implied_or_jsr
	dex
	bpl :-
	bmi @checkbranch

@implied_or_jsr:
	pha
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
	pla
	cmp #$20	; JSR
	bne @implied_

	iny
	lda #' '
	sta (@dst),y

	lda @dst
	clc
	adc #$04
	sta @dst
	bcc :+
	inc @dst+1
:	lda #MODE_ABS
	sta @modes
	jmp @absolute

@implied_:
	lda #$01
	RETURN_OK

; check for branches/exceptions
@checkbranch:
	lda @op
	and #$1f
	cmp #$10
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
@id=zp::macros+$0f
@params=zp::macros
	sta @id
	ldx #$fe	; -2

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
	bcc @setparam
	rts		; return err

@setparam:
	txa
	ldx @cnt
	sta @params,x
	tya
	sta @params+1,x

	ldy #$00
@nextparam:
	lda (zp::line),y ; read until comma or endline
	beq @done
	cmp #';'
	beq @done
	incw zp::line
	cmp #','
	beq @l0
	cmp #' '
	beq @nextparam
	RETURN_ERR ERR_INVALID_MACRO_ARGS

@done:	lda @id
	jmp mac::asm
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
	inc ifstacksp
	ldx ifstacksp
	sta ifstack,x
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
	inc ifstacksp
	ldx ifstacksp
	sta ifstack,x
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
@done:	lda #$00
	RETURN_OK
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
; GET_CTX_TYPE
; Returns the active context type
.proc get_ctx_type
	ldx contextstacksp
	dex
	bpl :+
	lda #$00	; stack is empty; return 0 for NO CONTEXT
	rts
:	lda contextstack,x
	sta ctx::type
	rts
.endproc

;******************************************************************************
; SET_CTX_TYPE
; Sets the active context type
; If 0 is given, the active context is popped from the context stack. Otherwise
; the context type is pushed
.proc set_ctx_type
	ldx contextstacksp
	cmp #$01
	bcs @push

@pop:	cpx #$01
	bcs :+
	RETURN_ERR ERR_STACK_UNDERFLOW
:	dec contextstacksp
	rts

@push:	cpx #MAX_CONTEXTS+1
	bcc :+
	RETURN_ERR ERR_STACK_OVERFLOW
:	inc contextstacksp
	sta contextstack,x
	sta ctx::type
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
.ifdef USE_FINAL
	sta zp::bankval
	txa
	pha
	tya
	pha
	clc
	adc zp::asmresult
	tax
	lda zp::asmresult+1
	adc #$00
	tay
	lda #FINAL_BANK_USER
	jsr fe3::store
	pla
	tay
	pla
	tax
	lda zp::bankval
.else
	sta (zp::asmresult),y
.endif
	RETURN_OK
.endproc

;******************************************************************************
; READB
; Reads a byte from (zp::asmresult),y
; OUT:
;  - .A: contains the byte from (zp::asmresult),y
.proc readb
.ifdef USE_FINAL
	txa
	pha
	tya
	pha
	sty zp::bankval	 ; offset to read from
	lda #FINAL_BANK_USER
	ldxy zp::asmresult
	jsr fe3::load_off
	sta zp::bankval
	pla
	tay
	pla
	tax
	lda zp::bankval
.else
	lda (zp::asmresult),y
.endif
	rts
.endproc
