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
; ZEROPAGE VARIABLES
top    = r0	; TODO: pointer to end of object code for current section
symobl = r0	; TODO: pointer to current symbol table end
reloctop: .word 0	; pointer to top of relocation table being built

;*******************************************************************************
.segment "OBJBSS"

;*******************************************************************************
; SECTIONS
; These variables contain the data for the sections
sections_startlo:  .res MAX_SECTIONS
sections_starthi:  .res MAX_SECTIONS
sections_sizelo:   .res MAX_SECTIONS
sections_sizehi:   .res MAX_SECTIONS
sections_segments: .res MAX_SEGMENT_NAME_LEN*MAX_SECTIONS ; name of target SEG

sections_relocstartlo: .res MAX_SECTIONS
sections_relocstarthi: .res MAX_SECTIONS
sections_relocsizelo:  .res MAX_SECTIONS
sections_relocsizehi:  .res MAX_SECTIONS

numsections:   .byte 0	; total number of sections in obj file being written

numsymbols: .word 0

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
.export __obj_add_symbol_info
__obj_add_symbol_info:
	lda asm::mode
	bne :+
	rts
	JUMP FINAL_BANK_LINKER, add_symbol_info

;*******************************************************************************
.export __obj_add_reloc
__obj_add_reloc:
	lda asm::mode
	bne :+
	rts
:	JUMP FINAL_BANK_LINKER, add_reloc

.segment "OBJCODE"

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
; address. This address is the address where the section is stored while
; building the object file. The actual address of the code within the section
; will be determined by the linker when the program is linked.
; IN:
;   - .XY:  the physical address to begin the section at
;   - $100: the name of the SEGMENT for the SECTION
.export __obj_add_section
.proc __obj_add_section
@name=$100
@namedst=r0
	txa
	ldx numsections
	sta sections_startlo,x

	; set start address and size
	; start[numsections] = .XY
	; if numsections > 0:
	;   size = (start[numsections-1] - start[numsections])
	cpx #$00
	beq :+
	sec
	sbc sections_startlo-1,x
	sta sections_sizelo-1,x

:	tya
	sta sections_starthi,x

	sbc sections_startlo-1,x
	cpx #$00
	beq :+
	sta sections_sizelo-1,x

:	; copy the SEGMENT name
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
@l1:	sta (@namedst),y
	beq :+
	iny
	cpy #MAX_SEGMENT_NAME_LEN
	bne @l0
:	; pad remainder of buffer with 0's
	iny
	cpy #MAX_SEGMENT_NAME_LEN
	bne @l0

@done:	inc numsections
	rts
.endproc

;*******************************************************************************
; ADD RELOC
; Adds a new relocation entry to the current object file in construction
; IN:
;   - .X:                    the relocation size in bytes (1 or 2)
;   - zp::asmresult:         the offset to apply the relocation at
;   - expr::contains_global: !0 if symbol should be used as relocation base
;   - expr::global_id:       the symbol ID to relocate relative to (if relevant)
;   - expr::global_op:       the operation to apply the relocation with
;   - expr::global_postproc: postprocessing to apply to global (if relevant)
.proc add_reloc
@sz=r0
@info=r0
@rel=r1
	dex			; get in rage [0, 1]
	stx @sz

	lda #$00
	ldx expr::contains_global
	beq @encode_size
;
; encode the "info" byte for the relocation based on the result of the
; expression evaluation and the size of the relocation
;  field    bit(s)   description
; size        0    size of target value to modify 0=1 byte, 1=2 bytes
; mode       1-2   type of relocation: 0=symbol-relative, 1=PC-relative
; postproc   3-4   0=no post processing, 1=LSB, 2=MSB
; operation  5-7   the operation to use to apply the operand (0=add, 1=subtract)
	lda expr::global_postproc
	asl
	asl
	asl
	asl
	; TODO: handle expr::global_op

@encode_size:
	ora @sz
	sta @info

	ldxy reloctop
	stxy @rel

	; store relocation record
	ldy #$00

	; write info byte
	lda @info
	sta (@rel),y
	iny

	; write offset (current "assembly" address)
	lda zp::asmresult
	sta (@rel),y
	iny
	lda zp::asmresult+1
	sta (@rel),y
	iny

	; write global ID (whether it's relevant or not)
	lda expr::global_id
	sta (@rel),y
	iny
	lda expr::global_id+1
	sta (@rel),y

	; update reloctop
	lda reloctop
	clc
	adc #$05
	sta reloctop
	bcc :+
	inc reloctop+1
:	rts

;******************************************************************************
; REQUIRES RELOC
; Set if the expression contains 1 or more labels.
; In the context of assembly, this tells the assembler that it must produce a
; relocation for this expression
.export quiroc
quiroc: .byte 0
	rts
.endproc

;******************************************************************************
; GET SYMBOL INFO
; Gets the section ID and type for the given symbol ID.
; The ID corresponds to the ID's returned by lbl::by_addr
; IN:
;   - .XY: the ID of the symbol to get info for
; OUT:
;   - .X: info
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
	lda (@symtab),y
	txa
	iny
	lda (@symtab),y
	tay
	rts
.endproc

;******************************************************************************
; ADD SYMBOL INFO
; Adds symbol info to the symbol table for the current object file
; IN:
;    - .X:              the section ID
;    - .Y:              the type of symbol (GLOBAL, LOCAL)
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
@cnt=r0
@cnt2=r2
@symtab=r4
@id=r6
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
	jsr lbl::getname

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
; DUMP
; Writes the complete object file to the given filename using the state built
; from the most recent successful assembly.
; IN:
;   - .XY: the output filename
; OUT:
;   - .C: set on error
.export __obj_dump
.proc __obj_dump
@src=r0
@cnt=r2
	; open the output file for writing
	CALL FINAL_BANK_MAIN, file::open_w
	bcs @done				; failed to open file

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
	; TODO:

	clc			; ok
@done:	rts
.endproc
