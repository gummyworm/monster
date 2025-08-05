;*******************************************************************************
; OBJ.ASM
; This file contains procedures used to construct object files.
;*******************************************************************************

.include "asm.inc"
.include "errors.inc"
.include "expr.inc"
.include "file.inc"
.include "labels.inc"
.include "macros.inc"
.include "ram.inc"
.include "vmem.inc"
.include "zeropage.inc"

.export __obj_opcode  = r0
.export __obj_operand = r1
.export __obj_opsize  = r3
.export __obj_symbol  = r4
.export __obj_addend  = r6
.export __obj_info    = r8

;*******************************************************************************
; RECORD TYPES
RECORD_BYTES = $01
RECORD_FILL  = $02
RECORD_EXPR  = $03

;*******************************************************************************
; CONSTANTS
MAX_SECTIONS         = 8	; max number of memory sections per OBJ file
MAX_OBJS             = 16	; max number of object files that may be used
MAX_SECTION_NAME_LEN = 8	; max length of a single section name
MAX_SEGMENT_NAME_LEN = 8	; max length of a single segment name

MAX_SYMBOL_NAME_LEN = 32
MAX_SYMBOLS         = 256	; max # of symbols per object file

SYM_IMPORT_BYTE     = 1
SYM_IMPORT_WORD     = 2
SYM_REL_EXPORT_BYTE = 3
SYM_REL_EXPORT_WORD = 4
SYM_ABS_EXPORT_BYTE = 5
SYM_ABS_EXPORT_WORD = 6

;*******************************************************************************
.segment "OBJBSS"
reloctop: .word 0	; pointer to top of relocation table being built

;*******************************************************************************
; SECTIONS
; These variables contain the data for the sections
sections_startlo:  .res MAX_SECTIONS
sections_starthi:  .res MAX_SECTIONS
sections_sizelo:   .res MAX_SECTIONS
sections_sizehi:   .res MAX_SECTIONS
sections_segments: .res MAX_SEGMENT_NAME_LEN*MAX_SECTIONS ; name of target SEG
sections_info:     .res MAX_SECTIONS

sections_relocstartlo: .res MAX_SECTIONS
sections_relocstarthi: .res MAX_SECTIONS
sections_relocsizelo:  .res MAX_SECTIONS
sections_relocsizehi:  .res MAX_SECTIONS

numsections: .byte 0	; total number of sections in obj file being written
numsymbols:  .word 0	; total number of symbols in obj file being written

;*******************************************************************************
; SYMBOL INFO
; This table contains the name for each symbol used in the object file
symbol_info:
.ifdef vic20
	; info, section-id
	.res MAX_SYMBOLS*(1+1)
.else
.endif

;*******************************************************************************
; RELOC TABLES
; This buffer contains the relocation tables for the object file
; sections_relocstartlo/hi contain the start address for each SECTION's
; relocation table, and each table is sections_relocsizelo/hi bytes long
; Calling obj::addreloc appends a relocation to this table
reloc_tables:
.ifdef vic20
	.res $3000
.else
.endif

.RODATA

;*******************************************************************************
.export __obj_init
__obj_init:
	JUMP FINAL_BANK_LINKER, init

;*******************************************************************************
.export __obj_add_symbol_info
__obj_add_symbol_info:
	lda asm::mode
	bne :+
	rts
:	JUMP FINAL_BANK_LINKER, add_symbol_info

;*******************************************************************************
.export __obj_add_reloc
__obj_add_reloc:
	JUMP FINAL_BANK_LINKER, add_reloc

.segment "OBJCODE"

;*******************************************************************************
; INIT
; Clears the object state in preparation for a new object file to be assembled
.proc init
	lda #$00
	sta numsections
	sta numsymbols
	sta numsymbols+1
	ldxy #reloc_tables
	stxy reloctop
	rts
.endproc

;*******************************************************************************
; GLOBAL
; Adds/updates a symbol in the symbol table for the object file being generated
; and marks it as GLOBAL.
; The symbol is added if it does not exist.
; If the symbol already exists, its value is updated (RESOLVED)
; If it does not exist, the symbol is created as UNRESOLVED
; e.g. `.global func`
; All globals must have corresponding symbol definitions (see __obj_add_symbol)
; at link time
.export __obj_global
.proc __obj_global
.endproc

;*******************************************************************************
; ADD SECTION
; Adds a new section to the current object file in construction at the given
; address. This address is the where the section is stored while
; building the object file. The actual address of the code within the section
; will be determined by the linker when the program is linked.
; IN:
;   - .A:             size/addressing mode (0=ZP, 1=ABS)
;   - zp::asmresult:  the physical address to begin the section at
;   - $100:           the name of the SEGMENT for the SECTION
; OUT:
;   - .C: set if the section could not be added
.export __obj_add_section
.proc __obj_add_section
@name=$100
@namedst=r0
	; set start address and size
	; start[numsections] = .XY
	; if numsections > 0:
	;   size = (start[numsections-1] - start[numsections])
	ldx numsections
	beq @sizedone
	cpx #MAX_SECTIONS
	bne :+
	;sec
	lda #ERR_TOO_MANY_SEGMENTS
	rts

:	pha				; save the size

	; set the size for the previous section
	lda zp::asmresult
	sec
	sbc sections_startlo-1,x
	sta sections_sizelo-1,x
	lda zp::asmresult+1
	sbc sections_starthi-1,x
	sta sections_sizehi-1,x

	; set the size for the previous section's relocation table
	lda reloctop
	sec
	sbc sections_relocstartlo-1,x
	sta sections_relocsizelo-1,x
	lda reloctop+1
	sbc sections_relocstarthi-1,x
	sta sections_relocsizehi-1,x
	pla

@sizedone:
	sta sections_info,x

	lda zp::asmresult
	sta sections_startlo,x	; set obj section start LSB
	lda zp::asmresult+1
	sta sections_starthi,x	; set obj section start MSB

	lda reloctop
	sta sections_relocstartlo,x	; set reloc start LSB
	lda reloctop+1
	sta sections_relocstarthi,x	; set reloc start MSB

	; copy the SEGMENT name
	lda numsections
	asl
	asl
	asl				; *8
	adc #<sections_segments
	sta @namedst
	lda #>sections_segments
	adc #$00
	sta @namedst+1

	ldy #$00
@l0:	lda @name,y
	sta (@namedst),y
	beq :+
	iny
	cpy #MAX_SEGMENT_NAME_LEN
	bcc @l0

:	; pad remainder of buffer with 0's
	lda #$00
@l1:	sta (@namedst),y
	iny
	cpy #MAX_SEGMENT_NAME_LEN
	bcc @l1

@done:	inc numsections
	RETURN_OK
.endproc

;*******************************************************************************
; ADD RELOC
; Adds a new relocation entry to the current object file in construction
; IN:
;   - .A:                   size of value to relocate (0=ZP, 1=ABS)
;   - asm::section:         the section to relocate within
;   - zp::asmresult:        offset to apply the relocation at
;   - expr::rpnlist:        the tokenized relocation expression
;   - expr::rpnlistlen:     length of the tokenized expression
;   - expr::requires_reloc: !0 if the expression needs a relocation entry
.proc add_reloc
@sz=r0
@info=r0
@rel=r1
@sec=r3
	sta @sec
	stx @sz

; encode the "info" byte for the relocation based on the result of the
; expression evaluation and the size of the relocation
;  field    bit(s)   description
; size        0    size of target value to modify 0=1 byte, 1=2 bytes
; mode       1-2   type of relocation: 1=symbol-relative, 0=PC-relative
@encode_size:
	lda #$00
	rol			; .C set if symbol-relative
	asl
	ora @sz
	sta @info

	ldxy reloctop
	stxy @rel

	; store relocation record
	ldy #$00

	; write info byte
	lda @info
	sta (@rel),y
	iny			; .Y=1

	; write offset in obj file (current "assembly" address)
	lda zp::asmresult
	sta (@rel),y
	iny			; .Y=2
	lda zp::asmresult+1
	sta (@rel),y

@pcrel:	; pc-relative: just write the value of the expression
	CALL FINAL_BANK_MAIN, expr::eval_list
	bcs @done

	tya
	ldy #$04
	sta (@rel),y		; write MBS
	txa
	dey			; .Y=3
	sta (@rel),y		; write LSB
	bne @update		; branch always

@expr:  ; write the tokenized RPN expression
	ldx #$00
	ldy #$03
:	lda expr::rpnlist,x
	sta (@rel),y
	iny
	inx
	cpx expr::rpnlistlen
	bne :-

@update:
	; update reloctop
	tya
	clc
	adc reloctop
	sta reloctop
	bcc :+
	inc reloctop+1
	clc			; ok
@done:	rts
.endproc

;******************************************************************************
; GET SYMBOL INFO
; Gets the section ID and type for the given symbol ID.
; The ID corresponds to the ID's returned by lbl::by_addr
; IN:
;   - .XY: the ID of the symbol to get info for
; OUT:
;   - .A: the size of the symbol (in bytes)
;   - .X: raw info byte for the symbol
;   - .Y: section ID of the label
.export __obj_get_symbol_info
.proc __obj_get_symbol_info
@symtab=r0
	txa
	asl			; ID * 2
	sta @symtab
	tya
	rol
	sta @symtab+1

	lda @symtab
	adc #<symbol_info
	sta @symtab
	bcc :+
	inc @symtab+1

:	ldy #$00
	lda (@symtab),y		; get the binding info
	tax
	iny
	lda (@symtab),y		; get the section
	tay

	lda sections_info,y	; read the mode for the symbol's section
	clc
	adc #$01		; get # of bytes from address mode
	rts
.endproc

;******************************************************************************
; ADD SYMBOL INFO
; Adds symbol info to the symbol table for the current object file
; IN:
;    - .X: the section ID
;    - .Y: mode of the label
.proc add_symbol_info
@cnt=r0
@info=r2
@symtab=r0
	lda #$00
	sta @symtab+1
	lda numsymbols
	asl
	rol @symtab+1
	adc #<symbol_info
	sta @symtab
	lda numsymbols+1
	adc @symtab+1
	sta @symtab+1

	tya			; get info byte
	ldy #$00
	sta (@symtab),y

	iny
	txa			; get section ID
	sta (@symtab),y

	incw numsymbols
	rts
.endproc

;*******************************************************************************
; DUMP SYMBOLS
; Writes the symbol table to file
.proc dump_symbols
@cnt=r2
@cnt2=r4
@symtab=r6
@id=r8
@buff=$100
	lda numsymbols
	asl
	sta @cnt
	lda numsymbols+1
	rol
	sta @cnt+1
	asl @cnt
	rol @cnt+1

	lda @cnt
	sta @cnt2
	lda @cnt+1
	sta @cnt2+1

	ldxy #symbol_info
	stxy @symtab

@dumpmeta:
	; write the metadata table
	lda (@symtab),y
	jsr $ffd2
	incw @symtab
	dec @cnt
	bne @dumpmeta
	dec @cnt+1
	bpl @dumpmeta

	lda #$00
	sta @id
	sta @id+1

@dumpnames:
	;write the symbol names
	ldxy #@buff
	stxy r0
	ldxy @id
	CALL FINAL_BANK_MAIN, lbl::getname

	; write out the name
	ldy #$00
:	lda @buff,y
	jsr $ffd2
	cmp #$00
	beq @nextname
	iny
	bne :-

@nextname:
	dec @cnt
	bne @dumpnames
	dec @cnt+1
	bpl @dumpnames

	rts
.endproc

;*******************************************************************************
; DUMP SECTION
; Dumps a single section for the object file in construction.
; IN:
;   - .A: the section ID to dump
.proc dump_section
.endproc

;*******************************************************************************
; DUMP SECTION HEADERS
; Dumps the sections for the object file in construction.
.proc dump_section_headers
@name=r0
@cnt=r2
	ldxy #sections_segments
	stxy @name

	ldx #$00
@dump_headers:
	; get offset to name for this section
	txa
	pha
	asl
	asl
	asl
	tay

	; write the name of the SEGMENT for the SECTION
	ldx #$08
:	lda (@name),y
	jsr $ffd2
	iny
	dex
	bne :-

	; write the OBJ and RELOC sizes
	pla
	tax
	lda sections_sizelo,x
	jsr $ffd2
	lda sections_sizehi,x
	jsr $ffd2
	lda sections_relocsizelo,x
	jsr $ffd2
	lda sections_relocsizehi,x
	jsr $ffd2
	inx
	cpx numsections
	bne @dump_headers
	rts
.endproc

;*******************************************************************************
; DUMP SECTIONS
; Dumps the sections for the object file in construction.
.proc dump_sections
@sec=r0
@sz=r2
@cnt=r4
	lda #$00
	sta @cnt

@l0:	; get address and size of the section to dump
	ldx @cnt
	lda sections_startlo,x
	sta @sec
	lda sections_starthi,x
	sta @sec+1
	lda sections_sizelo,x
	sta @sz
	lda sections_sizehi,x
	sta @sz+1

@objloop:
	; dump the object code for the section
	ldxy @sec
	CALL FINAL_BANK_MAIN, vmem::load	; load a byte of object code
	jsr $ffd2
	incw @sec

	lda @sz
	beq :+
	dec @sz+1
	bmi @reloc
:	dec @sz
	jmp @objloop

@reloc:	; then dump the relocation table
	lda sections_relocstartlo,x
	sta @sec
	lda sections_relocstarthi,x
	sta @sec+1
	lda sections_relocsizelo,x
	sta @sz
	lda sections_relocsizehi,x
	sta @sz+1
	ldy #$00
@relocloop:
	lda (@sec),y
	jsr $ffd2
	incw @sec
	lda @sz
	beq :+
	dec @sz+1
	bmi @dbgi
:	dec @sz
	jmp @relocloop

@dbgi:	; and finally, dump the debug info for the table
	; TODO:

@next:	inc @cnt
	lda @cnt
	cmp numsections
	bne @l0
.endproc

;*******************************************************************************
; DUMP
; Writes the complete object file to the given filename using the state built
; from the most recent successful assembly.
; The file to dump to should be open and set as the output file before calling
; this procedure.
; OUT:
;   - .C: set on error
.export __obj_dump
.proc __obj_dump
@src=r0
@cnt=r2
	; write the main OBJ header
	lda numsections			; # of sections
	jsr $ffd2
	lda numsymbols			; # of symbols
	jsr $ffd2
	lda numsymbols+1
	jsr $ffd2

	; write the SECTION headers
	jsr dump_section_headers

	; write the SYMBOL TABLE
	jsr dump_symbols

	; write each SECTION that was built
	jsr dump_sections

	clc			; ok
@done:	rts
.endproc
