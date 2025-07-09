;*******************************************************************************
; ASM.ASM
; This file contains the entrypoint for the primary assembly procedure as well
; as much of the code used to assemble a given line of text to its binary
; representation.
;
; ASSEMBLER OVERVIEW
; The assembler operates in 2 passes.
; Pass 1:
;  - generate symbol table (label names and addresses)
;  - create macro definitions (.MAC)
;
; Pass 2:
;  - validate labels (make sure we correctly inferred their sizes if not
;    forward declared)
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
;
;     If an address is incorrectly implied in pass 1, there will be an error
;     generated when we validate it in pass 2
;*******************************************************************************

.include "asmflags.inc"
.include "codes.inc"
.include "config.inc"
.include "ctx.inc"
.include "debug.inc"
.include "debuginfo.inc"
.include "errors.inc"
.include "errlog.inc"
.include "expr.inc"
.include "file.inc"
.include "layout.inc"
.include "labels.inc"
.include "line.inc"
.include "linker.inc"
.include "macro.inc"
.include "macros.inc"
.include "math.inc"
.include "memory.inc"
.include "object.inc"
.include "string.inc"
.include "text.inc"
.include "source.inc"
.include "util.inc"
.include "state.inc"
.include "strings.inc"
.include "vmem.inc"
.include "zeropage.inc"

.include "ram.inc"

;******************************************************************************
MAX_IFS      = 4 ; max nesting depth for .if/.endif
MAX_CONTEXTS = 3 ; max nesting depth for contexts (activated by .MAC, .REP, etc)

;*******************************************************************************
; ASM INFORMATION
; These zeropage locations are filled with information after each call to
; asm::tokenize
indirect   = zp::asmtmp   ; 1=indirect, 0=absolute
indexed    = zp::asmtmp+1 ; 1=x-indexed, 2=y-indexed, 0=not indexed
immediate  = zp::asmtmp+2 ; 1=immediate, 0=not immediate
operandsz  = zp::asmtmp+3 ; size of the operand (in bytes) $ff indicates 1 or 2 byttes
cc         = zp::asmtmp+4
resulttype = zp::asmtmp+5 ; how to format line (ASM_COMMENT, ASM_OPCODE, etc.)
opcode     = zp::asmtmp+6 ; opcode (if there was one)
operand    = zp::asmtmp+7 ; operand (if there was one)

SEG_CODE = 1	; flag for CODE segment
SEG_BSS  = 2	; flag for BSS segment (all data must be 0, PC not updated)

.BSS
;*******************************************************************************
.export ifstack
ifstack:   .res MAX_IFS	; contains TRUE/FALSE values for the active IF blocks
ifstacksp: .byte 0	; stack pointer to "if" stack

.export __asm_pcset
__asm_pcset:
pcset: .byte 0

contextstack:   .res MAX_CONTEXTS
contextstacksp: .byte 0

.export __asm_origin
__asm_origin:
origin: .word 0	; the lowest address in the program

.export __asm_top
__asm_top:
top: .word 0	; the highest address in the program

; TOKENIZE uses this as the line number to map the address of the assembled
; instruction to
.export __asm_linenum
__asm_linenum: .word 0

; TODO: use
; the type of the segment being stored e.g. SEG_BSS or SEG_CODE
;segment_type: .byte 0

;*******************************************************************************
; ASM MODE
; object assembly flag
; if !0 (object mode):
;   tokenize will assemble the given line as object code to the current
;   input file (file::open + CHKIN)
; if 0 (direct mode):
;   lines will be assembled directly to memory at the address in
;   zp::asmresult
.export __asm_mode
__asm_mode: .byte 0

;*******************************************************************************
; ASMBUFFER
; Source is copied here so that it can be messed with while assembling
.export asmbuffer
asmbuffer = mem::asmbuffer

.RODATA
;*******************************************************************************
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

;*******************************************************************************
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

;*******************************************************************************
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
.byte "incbin",0
.byte "import",0
.byte "export",0
.byte "seg",0
directives_len=*-directives

;*******************************************************************************
.linecont +
.define directive_vectors definebyte, defineconst, defineword, includefile, \
defineorg, define_psuedo_org, repeat, macro, do_if, do_else, do_endif, \
do_ifdef, create_macro, handle_repeat, incbinfile, import, export, \
directive_seg
.linecont -

directive_vectorslo: .lobytes directive_vectors
directive_vectorshi: .hibytes directive_vectors

;*******************************************************************************
; see MODE_ constants in asmflags.inc
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
	.byte MODE_ABS | MODE_X_INDEXED	; 111 (Y_INDEXED for STX,LDX)

;*******************************************************************************
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

illegal_opcodes:
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
num_illegals = *-illegal_opcodes

.CODE

;*******************************************************************************
; START PASS
; Resets assembly context in preparation for the given pass
; IN:
;  - .A: the pass # (1 or 2)
.export __asm_startpass
.proc __asm_startpass
	pha

	; disable VERIFY (assemble)
	lda #$00
	sta state::verify

	jsr ctx::init		; init the context
	pla
	sta zp::pass		; set pass #
	cmp #$01
	bne @pass2

	lda #$00
	sta top			; set top of program to 0
	sta top+1
	jmp __asm_reset		; reset assembly state

@pass2: jsr __asm_resetpc	; reset PC
	jmp ctx::init		; re-init the context
.endproc

;*******************************************************************************
; TOKENIZE
; Assembles the string at (YX) into an instruction in (asm::result)
; if (YX) contains an instruction.  Any labels or comments encountered are
; saved at the address in (pc).
; in:
;  - .XY: the string to assemble
;  - .A:  the bank of the string to assemble
;  - zp::asmresult: pointer to the location to assemble the instruction
; out:
;  - .A: the type of the result e.g. ASM_OPCODE or the error code
;  - .C: set if an error occurred
.export __asm_tokenize
.proc __asm_tokenize
	; copy the line to the main RAM bank and make it uppercase (assembly is
	; case-insensitive)
	stxy zp::bankaddr0
	ldxy #asmbuffer
	stxy zp::bankaddr1
	jsr ram::copyline

	ldxy #asmbuffer
	stxy zp::line
	jsr str::toupper

	ldy #$00
	sty mem::asmbuffer+LINESIZE
	lda (zp::line),y
	beq @noasm		; empty line, early out
@setbrk:
	lda zp::pass
	cmp #2
	bne :+

	; handle breakpoints (only set breakpoints in pass 2)
	ldxy zp::virtualpc	; current PC (address)
	stxy r0
	lda dbgi::file
	ldxy __asm_linenum
	jsr dbg::brksetaddr	; if there is a breakpoint, set its address

:	jsr line::process_ws
	beq @noasm 		; empty line

; check if we're in an .IF (FALSE) and if we are, return
@checkifs:
	lda ifstacksp
	beq @assemble	; no active .IF
	ldx #$00
:	inx
	lda ifstack,x
	beq @if_false
	cpx ifstacksp
	beq @assemble
	bne :-

@if_false:
	; asm is off, check for ENDIF or ELSE
	; anything else: return without assembling
	jsr is_directive
	bcs @noasm		; if not directive continue
	jsr getdirective
	bcs :+			; if error, return it
	cmp #DIRECTIVE_ENDIF
	beq @exec_directive
	cmp #DIRECTIVE_ELSE
	beq @exec_directive
@noasm:	lda #ASM_NONE
	clc
:	rts

;---------------------------------------
; assembly entrypoint for successive single-line assembly
; if a label is found, for example, we will reenter here after adding the label
; to assemble any opcode, directive, etc. that may still be in the line
@assemble:
	ldy #$00
	lda (zp::line),y
	beq @noasm
	jsr line::process_ws

; check if the line is a full line comment
@chk_comment:
	lda (zp::line),y
	cmp #';'
	bne @directive
	; rest of the line is a comment, we're done
	lda #ASM_COMMENT
	sta resulttype
	RETURN_OK

; 1. check if the line contains a directive
@directive:
	jsr is_directive
	bcs @ctx		; if not directive -> continue
	jsr getdirective
	bcs @ret0		; return error

@exec_directive:
	lda #ASM_DIRECTIVE
	sta resulttype
	stxy zp::jmpvec
	jmp (zp::jmpvec)

; after directives, handle context if any. this is used for things like an
; active macro definition (the lines between .mac and .endmac)
@ctx:	jsr handle_ctx
	bcs @ret0		; error
	beq @ret0		; context was handled

@noctx:	ldy #$00
	sty indirect
	sty indexed
	sty operandsz
	sty immediate

; check if the line contains an instruction
@opcode:
	jsr getopcode
	bcs @macro	; not opcode -> check macro

	; we found an opcode, store it and continue to operand, etc.
	lda #ASM_OPCODE
	sta resulttype
	stx opcode	; save the opcode

	txa
	ldy #$00
	jsr writeb	; store the opcode
	bcs @ret0	; return err
	jmp @getopws	; continue if no error

; check if the line contains a macro
@macro:	ldxy zp::line
	CALL FINAL_BANK_MACROS, mac::get

	bcs @chklabels		; if not macro, skip
	pha			; save macro id
	jsr line::process_word	; read past macro name
	pla			; restore macro id

	jsr assemble_macro
	bcs @ret0		; error
	lda #ASM_MACRO
	sta resulttype
	;clc
@ret0:	rts

; check if the line is a label definition
@chklabels:
	jsr is_anonref		; anonymous label (:)?
	bne @label		; not an anonymous label definition
@anonlabel:
	jsr line::incptr
	lda (zp::line),y
	beq :+
	jsr util::is_whitespace	; anon label must be followed by whitespace
	bne @retlabel		; if not whitespace, go on
:	lda state::verify
	bne @label_done		; if verifying, don't add a label
	lda zp::pass
	ldxy zp::virtualpc
	cmp #$01		; pass 1?
	bne @validate_anon	; if not, just validate
	jsr lbl::addanon	; add the anonymous label
	bcc @label_done
	rts			; return error (too many anonymous labels?)

@validate_anon:
	; make sure the anonymous label was correctly assigned in pass 1
	lda #$01
	jsr lbl::get_banon
	bcs :+
	cmpw zp::virtualpc
	beq @label_done
	lda #ERR_LABEL_NOT_KNOWN_PASS1
:	rts

@label:	jsr is_label
	bcs @getopws
	jsr do_label
	bcc @label_done
	rts			; return error

@label_done:
	jsr storedebuginfo	; store debug info for label
	jsr line::process_word	; read past the label name
	ldxy zp::line

	lda state::verify
	bne @retlabel		; if verifying, don't bother assembling rest
	jsr @assemble		; assemble the rest of the line
	bcs @ret0		; return error
	cmp #ASM_LABEL
	bne @retlabel

	; if we found another label, return error
	RETURN_ERR ERR_UNEXPECTED_CHAR

@retlabel:
	lda #ASM_LABEL		; return as LABEL (don't indent this line)
	RETURN_OK

; from here on we are either reading a comment or an operand
@getopws:
	jsr line::process_ws
	bne @pound
	jmp @done

@pound: cmp #';'		; are we at a comment?
	bne @parse_operand
	jmp @done		; if comment, we're done

@parse_operand:
	cmp #'#'
	bne @lparen		; if not '#' check for a paren (indirect)
	inc immediate		; flag operand as IMMEDIATE
	jsr line::incptr
	lda (zp::line),y	; get the next character

	; if IMMEDIATE, skip parentheses (treat as part of expression)
	jmp @evalexpr

@lparen:
	; not immediate, assume expressions are 2 bytes
	cmp #'('
	bne @evalexpr
	inc indirect
	jsr line::incptr

; all chars not part of expression have been processed, evaluate the expression
@evalexpr:
	; first check if this is an anonymous label reference
	jsr is_anonref
	bne @eval		; not anon ref -> continue
	jsr anonref
	lda #$02
	bcc @eval_done
@evalfailed:
	rts			; return error

@eval:  jsr expr::eval
	bcs @evalfailed		; return error, eval failed

@eval_done:
	stxy operand	; save the operand to store later
	sta operandsz	; save size of the operand

	lda immediate
	beq @cont
	lda #$01
	sta operandsz

; we've evaluated the expression, now look for a right parentheses,
; ',X' or ',Y' to conclude if this is indirect or indexed addressing
@cont:	jsr line::process_ws
	ldy #$00
	lda indirect		; is indirect flagged? (we saw a '(' earlier)?
	beq @index		; if not, skip to absolute

; handle indirect. May be x pre-indexed, indirect or indirect: ',X)' or ')'
@rparen:
; look for closing paren or ",X"
	lda (zp::line),y	; get first char
	cmp #','		; is it a ','?
	bne @rparen_noprex	; if not, only valid string is a plain ')'

	jsr line::nextch	; eat any WS and get next char
	cmp #'x'		; is it an .X?
	bne @unexpected_char

	jsr line::nextch		; get next char after ",X"
	inc indexed		; inc once to flag X-indexed
	cmp #')'
	beq @finishline		; if ')', continue

@unexpected_char:
	RETURN_ERR ERR_UNEXPECTED_CHAR

; look for a plain ')' (indirect addressing) or '),y'  (indirect y-indexed)
@rparen_noprex:
	jsr line::incptr
	cmp #')'
	bne @unexpected_char

@index:
	ldy #$00
	lda (zp::line),y
	cmp #','
	bne @getws2
	jsr line::nextch		; get next char (past any whitespace)

@getindexx:
	cmp #'x'
	bne @getindexy		; if not X check Y
	jsr is_ldx_stx		; check the special case of LDX y-indexed
	bcs :+			; if not LDX y-indexed continue
	; ,X is illegal for LDX/STX
	RETURN_ERR ERR_ILLEGAL_ADDRMODE

:	inc indexed	 	; inc once to flag ,X indexed
	bne @finishline		; validate nothing but a comment from here on

@getindexy:
	cmp #'y'
	beq :+
	RETURN_ERR ERR_UNEXPECTED_CHAR

:	inc indexed	 ; inc once for X-indexed
	jsr is_ldx_stx	 ; check LDX y-indexed
	bcc @finishline	 ; treat like ,X for encoding if LDX ,Y or STX ,Y
	inc indexed	 ; if NOT LDX y-indexed, inc twice for Y-indexed

;------------------------------------------------------------------------------
; finish the line by checking for whitespace and/or a comment
@finishline:
	jsr line::incptr		; next char
@getws2:
	jsr line::process_ws

	; check for comment or garbage
	jsr islineterminator
	beq @done
	RETURN_ERR ERR_UNEXPECTED_CHAR

;------------------------------------------------------------------------------
; done, create the assembled result based upon the opcode, operand, and addr mode
@done:	lda resulttype
	cmp #ASM_OPCODE
	beq @chkaddrmode
	; if not an instruction, we're done (nothing to write)
	clc
@ret:	rts

@chkaddrmode:
	jsr getaddrmode
	bcs @ret	; failed to get a valid address mode for instruction

@checkjmp:
	tax
	; JMP (xxxx) has a different opcode than JMP
	lda opcode
	cmp #$40
	bne @getbbb	; if not, not a JMP
	lda cc		; if cc is not 00,
	bne @getbbb	; not a JMP
	lda indirect	; get indirect flag
	beq @jmpabs	; if not set, this is an ABS JMP

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
	lda opcode
	cmp #$20
	bne :+
	cpx #ABS
	bne @err	; only ABS supported for JSR
	beq @noerr

:	; check if opcode was a branch
	and #$1f
	cmp #$10
	bne @verifyimm

	cpx #ZEROPAGE
	beq :+
	cpx #ABS	; only ABS/ZP supported for branches
	bne @err

:	; convert operand to relative address (operand - zp::asmresult)
@tmp=r0
	lda zp::asmresult
	clc
	adc #$02
	sta @tmp
	lda zp::asmresult+1
	adc #$00
	sta @tmp+1

	lda operand		; LSB of operand
	sec
	sbc @tmp
	sta operand		; overwrite operand with new relative address
	tax
	lda operand+1		; MSB of operand
	sbc @tmp+1		; MSB - >PC
	beq @store_offset	; $00xx might be in range
	cmp #$ff		; $ffxx might be in range
	beq @store_offset	; continue

	lda zp::pass
	cmp #$01		; on pass 1, offset might not be correct: allow
	beq @store_offset
	lda #ERR_BRANCH_OUT_OF_RANGE
	rts

@verifyimm:
	; remaining opcodes are single byte- implied/accumulator only
	cpx #IMPLIED
	beq @noerr
@err:	RETURN_ERR ERR_ILLEGAL_ADDRMODE

@store_offset:
	stx operand
	lda #$01
	sta operandsz	; force operand size to 1 for branches

@noerr:
; now store the operand byte(s)
@store_value:
	lda operandsz
	beq @dbg		; if no operand, continue
	cmp #$01
	beq @store_lsb		; if 1 byte operand, skip writing MSB
@store_msb:
	ldy #$02
	lda operand+1
	jsr writeb	; write the MSB (or garbage)
	bcs @opdone	; if unsucessful, return err
@store_lsb:
	ldy #$01
	lda operand
	jsr writeb	; write the LSB
	bcs @opdone	; if error, return
	jsr write_reloc	; write relocation info (if assembling to OBJ)

;------------------
; store debug info if enabled
@dbg:	jsr storedebuginfo

;------------------
; update virtualpc and asmresult by (1 + operand size)
@updatepc:
	ldx operandsz
	inx
	txa
	jsr addpc		; add operand size + 1 to assembly pointers

	lda #ASM_OPCODE
	clc			; ok
@opdone:
	rts

;------------------
; check that the BBB and CC combination we have is valid
@validate_cc:
@optmp=r0
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
	ldx #num_illegals-1
:	cmp illegal_opcodes,x
	beq @err
	dex
	bpl :-
	jsr writeb
	bcc @noerr
	rts		; return err
.endproc

;*******************************************************************************
; IS_LABEL
; IN:
;  - zp::line: string to check if label
; OUT:
;  - .C: set if NOT a label
.proc is_label
	ldxy zp::line
	jmp lbl::isvalid
.endproc

;*******************************************************************************
; DO LABEL
; State machine component
; Extracts the label from the line
; On pass 1, adds the label to the symbol table
.proc do_label
	lda #ASM_LABEL
	sta resulttype
	ldxy zp::line
	lda zp::virtualpc
	sta zp::label_value
	lda zp::virtualpc+1
	sta zp::label_value+1

	jsr lbl::islocal
	cmp #$00		; check flag
	bne :+
	ldxy zp::line
	jsr lbl::setscope	; set the non-local label as the new scope

:	lda state::verify
	bne @ok			; if verifying, don't add/check label
	lda zp::pass
	ldxy zp::line
	cmp #$01
	bne @validate		; if not pass 1, don't add the label
	jsr lbl::add
	bcc @ok
	rts

@validate:
	; make sure the label's address hasn't moved since pass 1
	jsr lbl::find
	bcs @ret
	jsr lbl::getaddr
	bcs @ret
	cmpw zp::label_value
	beq @addinfo
	lda #ERR_LABEL_NOT_KNOWN_PASS1
	sec
	rts

@addinfo:
	; on pass 2 add info about the symbol (if assembling to object code)
	jsr obj::add_symbol_info
@ok:	ldxy zp::line
@done:	clc
@ret:	rts
.endproc

;*******************************************************************************
; IS_ANONREF
; Returns .Z set if zp::line points to an anonymous label reference
; OUT:
;  - .Z: Set if zp::line is on an anonymous label reference
.proc is_anonref
	ldy #$00
	lda (zp::line),y
	cmp #':'
	rts
.endproc

;*******************************************************************************
; ANONREF
; evaluates the anonymous reference in (line) and returns
; the address it corresponds to
; IN:
;  - zp::line points to the anonymous reference e.g. ":++"
; OUT:
;  - .XY:      the address that is referenced
;  - .A:       the size of the label
;  - .C:       set if invalid reference or on error
;  - zp::line: if there was an anonymous reference, updated to point past it
.proc anonref
	lda zp::pass
	cmp #$02
	beq @pass2

@pass1:	; if pass 1, return success with dummy value
	jsr line::process_word
	ldxy zp::virtualpc	; TODO: dummy address
	lda #2
	RETURN_OK

@pass2:	ldy #$01		; past the ':'
	lda (zp::line),y	; forward or backward?
	cmp #'+'
	beq @f			; if +, forward
	cmp #'-'
	bne @err		; if not -, invalid

@b:	; count the '-'s
	iny
	lda (zp::line),y
	beq @bdone
	jsr util::is_whitespace
	beq @bdone
	cmp #'-'
	beq @b
@err:	RETURN_ERR ERR_UNEXPECTED_CHAR

@bdone: tya
	clc
	adc zp::line
	sta zp::line
	bcc :+
	inc zp::line+1
:	dey
	tya
	ldxy zp::virtualpc
	jmp lbl::get_banon

@f:	; count the '+'s
	iny
	lda (zp::line),y
	beq @fdone
	jsr util::is_whitespace
	beq @fdone
	cmp #'+'
	beq @f
	RETURN_ERR ERR_UNEXPECTED_CHAR

@fdone:	tya
	clc
	adc zp::line
	sta zp::line
	bcc :+
	inc zp::line+1
:	dey
	tya
	ldxy zp::virtualpc
	jmp lbl::get_fanon
.endproc

;*******************************************************************************
; STOREDEBUGINFO
; Stores the current VPC to the current source line
; If debug info generation is disabled, does nothing
.proc storedebuginfo
	lda zp::pass
	cmp #$01		; are we on pass 1?
	beq @skip		; if so, don't generate debug info

	lda zp::gendebuginfo
	bne @gen
@skip:	rts

@gen:	ldxy zp::virtualpc	; current PC (address)
	stxy r0
	ldxy __asm_linenum	; get line # to map to address
	jmp dbgi::storeline	; map them
.endproc

;*******************************************************************************
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
	lda zp::pass
	cmp #$01
	beq @done
	RETURN_ERR ERR_OVERSIZED_OPERAND
.endproc

;*******************************************************************************
; GETTEXT
; Parses an enquoted text string in zp::line and returns it in mem::spare
; returns the length in .A ($ff if no string was found)
.proc gettext
	ldy #$00
	lda (zp::line),y
	cmp #'"'
	bne @err

	ldx #$00
@l0:	jsr line::incptr
	lda (zp::line),y
	beq @err		; no closing quote
	cmp #'"'
	beq @done
	sta mem::spare,x
	inx
	bne @l0

@done:	jsr line::incptr
	txa
	RETURN_OK
@err:	RETURN_ERR ERR_SYNTAX_ERROR
.endproc

;*******************************************************************************
; GETOPCODE
; Parses zp::line for an instruction and returns information about it if it
; is determined to be an instruction
; OUT:
;  - .A: ASM_OPCODE (on success) else error
;  - .X: the opcode's ID
;  - .C: set if (line) is not an opcode
;  - cc: updated with the cc part of the opcode
.proc getopcode
@optab = r6
@op = r8
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

;*******************************************************************************
; IS_DIRECTIVE
; Checks if the contents of (zp::line) represent a directive
; OUT:
;   - .C: set if (zp::line) is not a directive
.proc is_directive
	ldy #$00
	lda (zp::line),y
	cmp #'.'
	bne @no
@yes:	clc			; zp::line is a directive
	rts
@no:	sec			; not a directive
	rts
.endproc

;*******************************************************************************
; GETDIRECTIVE
; Returns the handler and ID for the directive in (zp::line)
; OUT:
;  - .A:  the ID of the directive (if one is found)
;  - .XY: the address of the directive's handler
;  - .C:  set if the contents of zp::line is not a valid directive
.proc getdirective
@cnt=r2
	ldx #$00
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

@next:	cpx #directives_len
	bcc @ok
	RETURN_ERR ERR_INVALID_DIRECTIVE

@ok:	inc @cnt	; move to next directive

@l2:	lda directives,x
	inx
	cmp #$00
	beq @l0
	cpx #directives_len
	bcc @l2
	RETURN_ERR ERR_INVALID_DIRECTIVE

@found: ; make sure there are no trailing characters
	lda (zp::line),y
	beq :+
	jsr util::is_whitespace
	bne @next		; trailing char -> continue

:	; move the line pointer to after the directive
	tya
	clc
	adc zp::line
	sta zp::line
	bcc :+
	inc zp::line+1
:	jsr line::process_ws

	ldx @cnt
	ldy directive_vectorshi,x
	lda directive_vectorslo,x
	tax
	lda @cnt		; get the ID of the directive
	RETURN_OK
.endproc

;*******************************************************************************
; HANDLE_REPEAT
; Handler for .endrep.
; Generates the repeated assembly block defined between here and the previous
; .endrep
.proc handle_repeat
@errcode=r0
	; copy .endrep to the buffer
	ldxy #asmbuffer
	jsr ctx::write

	; disable this context
	lda #$00
	jsr set_ctx_type

	; before rewinding, move debug line back to line we're repeating
	jsr rewind_ctx_dbg

	; don't define iterator label until pass 2
	lda zp::pass
	cmp #$01
	beq @l1

@l0:	; define a label with the value of the iteration
	jsr rewind_ctx_dbg
	lda zp::ctx+repctx::iter
	sta zp::label_value
	lda zp::ctx+repctx::iter+1
	sta zp::label_value+1
	ldxy zp::ctx+repctx::params

	ora zp::label_value		; set .Z if label_value is 0
	bne @set
	jsr lbl::add			; first iteration- add instead of set
	jmp :+
@set:	jsr lbl::set
:	bcs @err

@l1:	; assemble the lines until .endrep
	incw __asm_linenum
	jsr ctx::getline
	bcc :+
@err:	rts				; propagate error, exit
:	streq strings::endrep, 7	; are we at .endrep?
	beq @next			; yep, do next iteration

	; save the context
	lda zp::ctx
	pha
	lda zp::ctx+1
	pha

	; assemble the current line
	ldxy #mem::ctxbuffer
.ifdef vic20
	lda #FINAL_BANK_MAIN	; bank doesn't matter for ctx
.endif
	jsr __asm_tokenize
	bcc :+
	sta @errcode		; save errcode
	pla			; clean stack
	pla
	lda @errcode		; get errcode
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
.endproc

;*******************************************************************************
; REWIND CTX DBG
; Rewinds the context and debug source line by the number of lines that are
; rewound.
.proc rewind_ctx_dbg
@tmp=r0
	; before rewinding, move debug line back to line we're repeating
	jsr ctx::numlines	; get # of lines we're rewinding
	sta @tmp
	lda __asm_linenum
	sec
	sbc @tmp
	sta __asm_linenum
	lda __asm_linenum+1
	sec
	sbc #$00
	sta __asm_linenum+1

	jmp ctx::rewind
.endproc

;*******************************************************************************
; HANDLE_CTX
; If this is the first pass, copies the contents of the asmbuffer to the current
; context.
; NOTE: during vierification contexts are not handled at all.
; OUT:
;  - .Z: set if the line was handled by this handler
;  - .C: set on error
.proc handle_ctx
	; if verifying (ctx type == 0), don't handle context at all
	jsr get_ctx_type
	beq @ok		; done, context not handled
	ldxy #asmbuffer
	jsr ctx::write	; copy the lineer to the context
	bcs @done
	lda #$00	; flag that the context was handled
	skw
@ok:	lda #$01	; flag context NOT handled
	clc		; no error
@done:	rts
.endproc

;*******************************************************************************
; DEFINEBYTE
; Defines 0 or more bytes and stores them in (asmresult)
; OUT:
;  - .A: the number of bytes written
.proc definebyte
	jsr line::process_ws
	ldxy zp::line
	jsr expr::eval
	bcs @text
	cmp #$01
	beq @ok
	RETURN_ERR ERR_OVERSIZED_OPERAND
@ok:	; store the extracted value
	ldy #$00
	txa
	jsr writeb_with_reloc
	bcs @ret
	jsr incpc
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

	; update program pointers
	inx
	txa
	jsr addpc

@commaorws:
	ldy #$00
	lda (zp::line),y
	beq @done
	cmp #';'		; comment?
	beq @done
	jsr line::incptr
	cmp #','
	beq definebyte
	jsr util::is_whitespace
	beq @commaorws
	; unexpected character
@err:	RETURN_ERR ERR_SYNTAX_ERROR
@done:	clc
@ret:	rts
.endproc

;*******************************************************************************
; DEFINEWORD
; Parses zp::line for a word value and stores it to zp::asmresult if possible.
; OUT:
;  - .C: set if a word could not be parsed
.proc defineword
	jsr line::process_ws
	ldxy zp::line
	jsr expr::eval
	bcs @err
	; store the extracted value
	tya
	ldy #$01
	jsr writeb_with_reloc	; this byte is relocatable
	bcs @ret		; return error
	txa
	dey
	jsr writeb
	bcs @ret		; return error

	lda #$02
	jsr addpc
@commaorws:
	ldy #$00
	lda (zp::line),y
	beq @done
	cmp #';'
	beq @done
	jsr line::incptr
	cmp #','
	beq defineword
	jsr util::is_whitespace
	beq @commaorws
	; unexpected character
@err:	RETURN_ERR ERR_SYNTAX_ERROR
@done:	clc
@ret:	rts
.endproc

;*******************************************************************************
; IMPORT
; Imports the label following this directive
; e.g. `IMPORT LABEL`
; The label is assumed to be in the absolute ($100-$ffff) address range.
; The actual resolution of the label will happen when the object code is linked
.proc import
	; TODO
	; define a label for the import so that references to it succeed
	;lda #$fe
	;sta zp::label_value
	;sta zp::label_value+1
	;ldxy zp::line
	;jmp lbl::add
.endproc

;*******************************************************************************
; IMPORTZP
; Imports the label following this directive as a zeropage label reference
; e.g. `IMPORT LABEL`
; The label is assumed to be in the zeropage ($00-$ff) address range.
; The actual resolution of the label will happen when the object code is linked
.proc importzp
	; TODO
	; define a label for the import so that references to it succeed
	;lda #$00
	;sta zp::label_value
	;sta zp::label_value+1
	;ldxy zp::line
	;jmp lbl::add
.endproc

;*******************************************************************************
; EXPORT
; Exports the label following this directive
; e.g. `EXPORT LABEL`
; The label is assumed to be in the absolute ($100-$ffff) address range.
.proc export
	;  TODO
	; if producing an object file, add to its EXPORTs
	;ldxy zp::line
	;jmp link::add_export
.endproc

;*******************************************************************************
; DIRECTIVE SEG
; Handles the `.SEG` directive
; This directive is only valid when assembling to object. It creates a new
; SECTION in the object file, closing the current one (if one exists)
.proc directive_seg
@name=$100
	; set the PC to 0 so that labels have the value of their offset
	; from the beginning of their section
	ldx #$00
	stx zp::virtualpc
	stx zp::virtualpc+1
	stx zp::asmresult
	stx zp::asmresult+1

	inx
	stx pcset	; linker will take care of setting PC

	; get the name of the segment
	jsr util::parse_enquoted_line
	bcc @add
	rts		; error

@add:	; create a new SECTION for the parsed SEGMENT name
	jmp obj::add_section
.endproc

;*******************************************************************************
; INCBINFILE
; Includes the enquoted binary file
; The contents of the file are inserted directly as binary values at the
; current assembly address.
.proc incbinfile
@filename=$100
	lda state::verify
	beq @cont
	lda #ASM_DIRECTIVE
	RETURN_OK		; don't include a file when verifying

@cont:	jsr util::parse_enquoted_line
	bcs @ret

	ldxy #@filename
	jsr file::exists
	beq :+
	RETURN_ERR ERR_FAILED_OPEN_INCLUDE

:	ldxy #@filename
	jsr file::open
	bcs @ret
	pha		; save file handle
	tax
	jsr $ffc6	; CHKIN (use file as input)

@l0:	; read the binary file contents byte-by-byte
	jsr file::readb
	bcs @err
	ldx file::eof
	bne @eof	; loop until EOF

	ldy #$00	; no offset
	jsr writeb	; write the byte
	bcs @err
	jsr incpc
	jmp @l0

@eof:	clc		; return without err
@err:	pla		; restore file handle
	php		; save success status flag
	jsr file::close	; cleanup (close the file)
	plp		; restore success flag
@ret:	lda #ASM_DIRECTIVE
	rts
.endproc

;*******************************************************************************
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
	jsr util::parse_enquoted_line
	bcc :+
	rts			; failed to parse filename
:	ldxy #@filename

; entry point for assembling a given file
.export __asm_include
__asm_include:
@fname=rc
@readfile:
	stxy @fname

	lda state::verify
	beq :+
	lda #ASM_DIRECTIVE	; format type
	RETURN_OK		; don't include a file when verifying

:	sta @err

	; save current file
	lda dbgi::file
	pha

	ldxy @fname
	jsr file::exists
	beq :+
	lda #ERR_FAILED_OPEN_INCLUDE
@reterr:
	plp		; clean stack
	sec
	rts

:	ldxy @fname
	jsr file::open	; open the file we are including
	bcs @reterr

	pha		; save the id of the file we're working on (for closing)
	sta zp::file

	; add the filename to debug info (if it isn't yet), reset line number
	; and finally create a new block of debug information
	ldxy @fname
	jsr dbgi::setfile
	ldxy #1
	stxy dbgi::srcline
	stxy __asm_linenum

	lda zp::pass
	cmp #$02
	bne @doline		; only create new block in pass 2

	; end current file's block and start a new one at the current address
	ldxy zp::asmresult	; current address
	jsr dbgi::endblock	; end the current block
	ldxy zp::asmresult	; current address
	jsr dbgi::newblock	; start new block for included file

; read a line from file
@doline:
	ldxy #mem::spare
	lda zp::file
	jsr file::getline	; read a line from the file
	bcc @asm
	ldx file::eof
	bne @close		; failed to get a line and not at end of file
	sta @err
	bne @close		; branch always

; assemble the line
@asm:	ldxy #mem::spare
	lda zp::file
	pha

.ifdef vic20
	lda #FINAL_BANK_MAIN	; any bank that is valid (low mem is used)
.endif
	jsr __asm_tokenize_pass
	bcc @ok
	jsr errlog::log
	bcc @ok
@fatal:	sta @err		; too many errors or fatal error

@ok:	pla			; restore the file ID we included from
	sta zp::file		; and temporarily store it
	bcs @close

@next:	incw __asm_linenum	; next line
	lda file::eof		; EOF?
	beq @doline		; repeat til we are

@close: pla			; get the file ID for the include file to close
	jsr file::close		; close the file
	pla

	sta dbgi::file

	lda zp::pass
	cmp #$02
	bne @done		; if not pass 2, don't mess with debug info

	ldxy zp::asmresult
	jsr dbgi::endblock	; end the block for the included file
	; restore the file we included from
	ldxy zp::asmresult
	jsr dbgi::newblock	; start a new block in original file
@done:
@err=*+1
	lda #$00	; get err code
	cmp #$01	; set carry if >= 1
	rts
.endproc

;*******************************************************************************
; DEFINEORG
; Hanldes the .ORG directive.
; Parses an expression for a value and sets the asmresult and virtualpc
; addresses to it
; e.g.: `.ORG $1000` or `ORG $1000+LABEL`
.proc defineorg
	jsr line::process_ws
	ldxy zp::line
	jsr expr::eval
	bcs @ret		; error
	stxy zp::asmresult
	stxy zp::virtualpc
	lda pcset
	bne @chkorg
	lda pcset
	bne :+

	; if PC isn't set yet, set TOP to the new PC
	inc pcset
	stxy top

:	stxy origin
	bne @done		; branch always

	; check if this is lower than current base origin
@chkorg:
	cpy origin+1
	bcs @done
	bcc @set
	cpx origin
	bcc @done
@set:	stxy origin

@done:	lda #ASM_ORG
	clc			; ok
@ret:	rts
.endproc

;*******************************************************************************
; SET PC
; Sets the address to assemble at next time tokenize is called.
; This is only useful in the context of assembling a line directly to
; memory.  It should not be used when assembling a file/buffer
; IN:
;   - .XY: the address to assemble the next instruction to
.export __asm_set_pc
.proc __asm_set_pc
	stxy zp::asmresult
	stxy zp::virtualpc
	lda #$01
	sta pcset
	rts
.endproc

;*******************************************************************************
; DEFINE_PSUEDO_ORG
; Hanldes the .RORG directive.
; Parses an expression for a value and sets the virtualpc  address to it.
; Note that the physical assembly target (asmresult) is unaffected.
; e.g.: `.RORG $1000` or `RORG $1000+LABEL`
.proc define_psuedo_org
	jsr line::process_ws
	ldxy zp::line
	jsr expr::eval
	bcs @ret		; error
	stxy zp::virtualpc
	clc			; ok
@ret:	rts
.endproc

;*******************************************************************************
; DEFINECONST
; Hanldes the .EQ directive
; Effective on 1st pass only
.proc defineconst
	lda zp::pass
	cmp #$01
	beq :+
	RETURN_OK

:	ldxy zp::line
	jsr lbl::isvalid
	bcs @err

	lda zp::line		; save label name's address
	pha
	lda zp::line+1
	pha

	; read all characters in zp::line until the next whitespace
	ldy #$00
	lda (zp::line),y
	jsr util::is_whitespace
	beq @cont

@l0:	jsr line::incptr
	lda (zp::line),y
	jsr util::is_whitespace
	bne @l0

@cont:	jsr line::process_ws	; eat whitespace
	jsr expr::eval		; get constant value
	bcc @ok

	; clean the stack and return error
	plp
	plp
	sec
@err:	rts

@ok:	stxy zp::label_value
	pla
	tay
	pla
	tax
	jmp lbl::add
.endproc

;*******************************************************************************
; REPEAT
; generates assembly for the parameterized code between this directive
; and the lines that follow until '.endrep'
; .rep 10,I
;   asl
; .endrep
; will produce 10 'asl's
.proc repeat
	jsr ctx::push	; push a new context

	jsr expr::eval  ; get the number of times to repeat the code
	bcs @ret	; error evaluating # of reps expression

@ok:	stxy zp::ctx+repctx::iter_end
	jsr line::process_ws
	ldy #$00
	lda (zp::line),y
	cmp #','
	beq @getparam
	RETURN_ERR ERR_UNEXPECTED_CHAR ; comma must follow the # of reps

@getparam:
	; get the name of the parameter
	jsr line::incptr
	ldy #$00
@saveparam:
	ldxy zp::line
	jsr ctx::addparam
	bcs @ret	; error adding parameter

@cont:	stxy zp::line	; update line pointer to after parameter
	lda #$00
	sta zp::ctx+repctx::iter	; initialize iterator
	sta zp::ctx+repctx::iter+1
@done:	lda #CTX_REPEAT
	jsr set_ctx_type	; store REPEAT as current context type
	clc		; ok
@ret:	rts
.endproc

;*******************************************************************************
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
	jsr line::process_ws	; sets .Y to 0
	jsr islineterminator
	bne :+
	RETURN_ERR ERR_NO_MACRO_NAME
:	ldxy zp::line
	jsr ctx::addparam
	bcs @ret		; return err
	stxy zp::line		; update line pointer

@getparams:
	jsr line::process_ws	; sets .Y to 0
	lda (zp::line),y
	jsr islineterminator
	beq @done
	ldxy zp::line
	jsr ctx::addparam
	stxy zp::line

	; look for the comma or line-end
	jsr line::process_ws	; sets .Y to 0
	lda (zp::line),y
	jsr islineterminator
	beq @done
	cmp #','
	beq :+
	RETURN_ERR ERR_UNEXPECTED_CHAR

:	jsr line::incptr
	bne @getparams
@done:	lda #CTX_MACRO
	jsr set_ctx_type	; store MACRO as current context type
	lda #ASM_DIRECTIVE
	clc			; ok
@ret:	rts
.endproc

;*******************************************************************************
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
	stxy r0
	jsr ctx::getdata
	pla

	; create the macro
	CALL FINAL_BANK_MACROS, mac::add

@done:	; done with this context, disable it
	lda #$00
	jsr set_ctx_type
	jmp ctx::pop	; cleanup; pop the context
.endproc

;*******************************************************************************
; RESET
; Resets the internal assembly context (labels and pointer to target)
.export __asm_reset
.proc __asm_reset
	lda #$00
	sta ifstacksp
	sta contextstacksp
	jsr ctx::init
	CALL FINAL_BANK_MACROS, mac::init
	jsr lbl::clr
	; fall through to RESETPC
.endproc

;*******************************************************************************
; RESETPC
; Resets the PC for, for example, beginning a new pass on the assembler
.export __asm_resetpc
.proc __asm_resetpc
	lda #$00
	sta pcset
	rts
.endproc

;*******************************************************************************
; DISASSEMBLE
; disassembles the given instruction
; IN:
;  - .XY: the address of the instruction to disassemble
;  - r0:  the address of the buffer to disassemble to
; OUT:
;  - .A:   the size of the instruction that was disassembled
;  - .X:   the address modes for the instruction
;  - .C:   clear if instruction was successfully disassembled
;  - (r0): the (0-terminated) disassembled instruction string
.export __asm_disassemble
.proc __asm_disassemble
@dst=r0
@cc=r2

@op=r3
@operand=r4

@optab=r7
@illegals=r7
@cc8=r7
@xxy=r7
@cc8_plus_aaa=r7
@modes=r7

@bbb=r8
@aaa=r9
@opaddr=ra
	stxy @opaddr		; opcode
	jsr vmem::load
	sta @op
	ldxy @opaddr		; operand
	lda #$01
	jsr vmem::load_off
	sta @operand
	ldxy @opaddr		; operand byte 2
	lda #$02
	jsr vmem::load_off
	sta @operand+1

; check for single byte opcodes
@chksingles:
	lda @op
	ldx #num_opcode_singles-1
:	cmp opcode_singles,x
	beq @implied_or_jsr
	dex
	bpl :-
	bmi @chkillegals

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
	lda #$00
	sta (@dst),y		; 0-terminate
	lda #$01
	ldx @modes
	clc			; ok
@ret:	rts

@chkillegals:
	; check if the opcode is "illegal"
	ldxy #illegal_opcodes
	stxy @illegals
	ldy #num_illegals-1
	lda @op
:	cmp (@illegals),y
	beq @ret		; if illegal, quit with .C set
	dey
	bpl :-

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
	jsr @cont 	; @operand now contains absolute address, render it
	lda #$02	; size is 2
	rts

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
	cmp #(opcode_branches-opcodes)/3
	bcs @invalid
	sta @cc8_plus_aaa
	asl
	adc @cc8_plus_aaa
	bne :+
	sec
@invalid:
	rts			; optab code 0 is invalid

:	adc #<opcodes
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

	; update @dst to after the opcode
	lda @dst
	clc
	adc #$03
	sta @dst
	bcc @get_addrmode
	inc @dst+1

@get_addrmode:
	; get bbb and find the addressing mode for the instruction
	lda @op
	cmp #$6c	; handle JMP (ind) - it doesn't match "normal" encoding
	bne :+
	lda #MODE_INDIRECT | MODE_ABS
	sta @modes
	bne @cont

:	lsr
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
	lda #$00
	tay
	sta (@dst),y	; 0-terminate
	lda #$01	; 1 byte in size
	ldx @modes
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
	lda #'$'
	sta (@dst),y
	incw @dst
	lda @operand
	jsr util::hextostr
	tya
	ldy #$00
	sta (@dst),y
	incw @dst
	txa
	sta (@dst),y
	incw @dst

:	lda @modes
	and #MODE_ABS
	beq @chkindexed

@absolute:
	lda #'$'
	ldy #$00
	sta (@dst),y

	incw @dst
	lda @operand+1
	jsr util::hextostr
	tya
	ldy #$00
	sta (@dst),y
	incw @dst
	txa
	sta (@dst),y
	incw @dst

	lda @operand
	jsr util::hextostr
	tya
	ldy #$00
	sta (@dst),y
	incw @dst
	txa
	sta (@dst),y
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
	incw @dst

@done:  ldx #$02
	lda @modes
	and #MODE_ZP
	bne :+
	inx
:	lda #$00
	tay
	sta (@dst),y

	txa			; .A = size
	ldx @modes		; .X = address modes
	RETURN_OK
.endproc

;*******************************************************************************
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

	; read all the parameters for the macro
@l0:	ldy #$00
	inx
	inx
@l1:	lda (zp::line),y
	beq @done
	iny
	jsr util::is_whitespace
	bne @l1

	stx @cnt
	jsr line::process_ws
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
	lda (zp::line),y 	; read until comma or endline
	beq @done		; 0 (end of line) we're done, assemble
	cmp #';'		; ';' (comment) - also done
	beq @done
	jsr line::incptr
	cmp #','
	beq @l0
	jsr util::is_whitespace
	beq @nextparam
	RETURN_ERR ERR_INVALID_MACRO_ARGS

@done:	lda @id
	JUMP FINAL_BANK_MACROS, mac::asm
.endproc

;*******************************************************************************
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
	txa
	bne @true
	tya
	beq @false

@true:	ldx #$01

@false:	; store the TRUE/FALSE value to the if stack
	txa
	inc ifstacksp
	ldx ifstacksp
	sta ifstack,x
	lda #ASM_DIRECTIVE
	RETURN_OK
.endproc

;*******************************************************************************
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

;*******************************************************************************
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

;*******************************************************************************
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
	bcs :+		; label not defined
	lda #$01

:	; store TRUE/FALSE to the if stack
	inc ifstacksp
	ldx ifstacksp
	sta ifstack,x
@done:
	jsr line::process_word
	lda #ASM_DIRECTIVE
	RETURN_OK
.endproc

;*******************************************************************************
; TOKENIZE_PASS
; Based on the current pass (zp::pass), calls the appropriate routine to
; handle assembly for that pass
; IN:
;  - .XY: the string to tokenize
.export __asm_tokenize_pass
.proc __asm_tokenize_pass
	pha
	lda zp::pass
	cmp #$02
	pla
	bcs __asm_tokenize_pass2

; fall through
.endproc

;*******************************************************************************
; TOKENIZE_PASS1
; Calls tokenize on the given line
; IN:
;  - .A:  the bank that the line to assemble resides in
;  - .XY: the line to assemble
;  - .C:  set if an error occurred
.export __asm_tokenize_pass1
.proc __asm_tokenize_pass1
	jmp __asm_tokenize	; assemble the line
.endproc

;*******************************************************************************
; TOKENIZE_PASS2
; Calls tokenize and generated debug info (if enabled)
; IN:
;  - .A:  the bank that the line to assemble resides in
;  - .XY: the line to assemble
;  - .C:  set if an error occurred
.export __asm_tokenize_pass2
.proc __asm_tokenize_pass2
	jsr __asm_tokenize
	bcs @done	; return err

	; store debug info (if enabled)
	ldx zp::gendebuginfo
	beq @retok
	cmp #ASM_ORG
	bne @ok

@org:	; if we assembled a .ORG in pass 2, create a new block at the new address
	ldxy zp::virtualpc	; address of new block
	jmp dbgi::newblock	; create a block
	bcs @done
@retok:	lda #$00
@ok:	clc
@done:	rts
.endproc

;*******************************************************************************
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

;*******************************************************************************
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
@no:	sec
	skb
@yes:	clc
	pla
	rts
.endproc

;*******************************************************************************
; GET_CTX_TYPE
; Returns the active context type
; OUT:
;   - .A: the type of the context (0 for NO CONTEXT)
;   - .Z: set if no context is active
.proc get_ctx_type
	ldx contextstacksp
	dex
	bpl :+
	lda #$00	; stack is empty; return 0 for NO CONTEXT
	rts

:	lda contextstack,x
	rts
.endproc

;*******************************************************************************
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
	rts
.endproc

;*******************************************************************************
; ADD_PC
; Adds the given value to the virtual PC and asmresult pointers
; IN:
;  - .A: the value to add to the assembly pointers (virtualpc and asmresult)
.proc addpc
	pha
	clc
	adc zp::asmresult
	sta zp::asmresult
	tax
	lda zp::asmresult+1
	bcc :+
	inc zp::asmresult+1

:	tay
	pla
	clc
	adc zp::virtualpc
	sta zp::virtualpc
	bcc :+
	inc zp::virtualpc+1

:	; update the top pointer if we are at the top of the program
	cmpw top
	bcc :+
	stxy top

:	rts
.endproc

;*******************************************************************************
; INCPC
; Updates the asmresult and virtualpc pointers by 1
.proc incpc
	incw zp::asmresult
	incw zp::virtualpc

	; update the top pointer if we are at the top of the program
	ldxy zp::asmresult
	cmpw top
	bcc :+
	stxy top
:	rts
.endproc

;*******************************************************************************
; WRITEB
; Stores a byte to (zp::asmresult),y
; Also checks if the origin has been set
; IN:
;  - .A: the value to write to (zp::asmresult),y
;  - .Y: the offset from (zp::asmresult) to write to
; OUT:
;  - .C: set on error, clear on success
.proc writeb
@savex=re
@savey=rf
	sta zp::bankval
	lda pcset
	bne :+
	lda state::verify
	bne @ok
	RETURN_ERR ERR_NO_ORIGIN

:	stx @savex
	sty @savey
	tya			; .A = offset
	ldxy zp::asmresult

	jsr vmem::writable
	bcc :+
	lda #ERR_PC_TARGET_UNWRITABLE
	;sec
	rts			; address is not writable

:	jsr vmem::store_off
@done:	ldx @savex
	ldy @savey
:				; <- write_reloc
@ok:	clc
	rts
.endproc

;*******************************************************************************
; WRITE RELOC
; If assembling to object code, writes the relocation information
; IN:
;   - .A:                       size of the value to relocate (1 or 2)
;   - expr::require_relocation: !0 if we should use symbol as base address
;   - expr::global_sym:         the symbol ID to relocate relative to
;   - expr::num_globals:        !0 if symbol should be used as relocation base
.proc write_reloc
	ldx __asm_mode
	beq :-				; -> rts (no relocation in DIRECT mode)
	ldx zp::pass
	cpx #$01
	beq :-				; -> rts (no relocation on pass 1)
	ldx expr::requires_reloc
	beq :-				; -> rts (expression fully resolved)

	; add  new relocation entry to the relocation table
	; if a global was referenced (expr::global_sym) this will produce
	; relocation relative to the referenced symbol
	; if not, it will just mark the address for relocation and the PC
	; at link time will be added to it
	jmp obj::addreloc
.endproc

;*******************************************************************************
; WRITE BYTE WITH RELOC
; Writes the given byte out with relocation information
.proc writeb_with_reloc
	jsr writeb
	bcs :-			; -> rts
	jmp write_reloc
.endproc

;*******************************************************************************
; READB
; Reads a byte from (zp::asmresult),y
; IN:
;  - .Y: the offset from (zp::asmresult) to read from vmem
; OUT:
;  - .A: contains the byte from (zp::asmresult),y
.proc readb
@savex=re
@savey=rf
	stx @savex
	sty @savey
	tya			; .A = offset to load
	ldxy zp::asmresult
	jsr vmem::load_off	; load the byte from VMEM
	ldy @savey
	ldx @savex
	rts
.endproc
