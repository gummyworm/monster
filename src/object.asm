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

MAX_SYMBOL_INDEXES = $200	; max number of symbols that may be referenced

MAX_SYMBOL_NAME_LEN = 32
MAX_IMPORTS         = 128
MAX_EXPORTS         = 32

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

; relocation table offsets/sizes for each section
sections_relocstartlo: .res MAX_SECTIONS
sections_relocstarthi: .res MAX_SECTIONS
sections_relocsizelo:  .res MAX_SECTIONS
sections_relocsizehi:  .res MAX_SECTIONS

numsections: .byte 0	; number of sections in obj file being written

numsymbols:  .word 0	; number of symbols in obj file being written

;*******************************************************************************
; SYMBOL INDEX MAP
; Each entry in this array contains the index that we will map the corresponding
; label to (see labels.asm)
; This lets us emit a more compact list of only symbols that are used
; If the symbol is unused, we store $ff
.export symbol_index_map
symbol_index_map: .res MAX_LABELS*2

;*******************************************************************************
; NUM SYMBOLS MAPPED
; The number of symbols to store to the symbol table.
; Also the index that the next mapped symbol will be stored at
num_symbols_mapped: .word 0

num_reloctables_mapped: .byte 0

;*******************************************************************************
; SYMBOL INFO
; This table contains the name for each symbol used in the object file
; The contents of the symbol table depend on if the symbol is "local"
; o "imported".
; For symbols defined within the object code:
;  section + offset within section
; For imported symbols:
;  name of the symbol
; For exported symbols:
;  name of the symbol, section-index, and offset from that section
; This gives us enough information to lookup the addresses of global symbols
; and also to provide that information to other assembly units.


;*******************************************************************************
; EXPORTS
export_indexes_lo: .res MAX_EXPORTS	; LSB index in symbol table for exports
export_indexes_hi: .res MAX_EXPORTS	; MSB index in symbol table for exports
numexports:        .byte 0

;*******************************************************************************
; RELOC TABLES
; This buffer contains the relocation tables for the object file
; sections_relocstartlo/hi contain the start address for each SECTION's
; relocation table, and each table is sections_relocsizelo/hi bytes long
; Calling obj::addreloc appends a relocation to this table
.export reloc_tables
reloc_tables:
.ifdef vic20
	.res $3000
.else
.endif
reloc_tables_end=*

.RODATA

;*******************************************************************************
.export __obj_init
__obj_init:
	JUMP FINAL_BANK_LINKER, init

;*******************************************************************************
.export __obj_add_reloc
__obj_add_reloc:
	JUMP FINAL_BANK_LINKER, add_reloc

;*******************************************************************************
.export __obj_close_section
__obj_close_section:
	JUMP FINAL_BANK_LINKER, close_section

.segment "OBJCODE"

;*******************************************************************************
; INIT
; Clears the object state in preparation for a new object file to be assembled
.proc init
	lda #$00
	sta numsections
	sta numsymbols
	sta numsymbols+1
	sta numexports
	sta num_symbols_mapped
	sta num_symbols_mapped+1
	sta num_reloctables_mapped

	; reset relocation tables "top" pointer
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
;   - .A: the ID of the section added
;   - .C: set if the section could not be added
.export __obj_add_section
.proc __obj_add_section
@name=$100
@namedst=r0
	ldy zp::pass
	cpy #$01
	beq @pass1

	; in pass 2 we just need to store relocation table
	; start address
	ldx num_reloctables_mapped
	lda reloctop
	sta sections_relocstartlo,x	; set reloc start LSB
	lda reloctop+1
	sta sections_relocstarthi,x	; set reloc start MSB
	inc num_reloctables_mapped
	RETURN_OK

@pass1:	ldx numsections
	cpx #MAX_SECTIONS
	bne :+
	;sec
	lda #ERR_TOO_MANY_SEGMENTS
	rts

:	sta sections_info,x	; store address mode
	lda zp::asmresult
	sta sections_startlo,x	; set obj section start LSB
	lda zp::asmresult+1
	sta sections_starthi,x	; set obj section start MSB

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
	lda numsections
@ret:	RETURN_OK
.endproc

;*******************************************************************************
; CLOSE SECTION
; Closes the open section (if there is one)
.proc close_section
	ldx numsections
	beq @done		; no section to close
	ldy zp::pass
	cpy #$01
	beq @pass1

@pass2:	; in pass 2 we just need to calculate relocation table size
	; calculate size for the previous section's relocation table
	lda reloctop
	sec
	ldx num_reloctables_mapped
	sbc sections_relocstartlo-1,x
	sta sections_relocsizelo-1,x
	lda reloctop+1
	sbc sections_relocstarthi-1,x
	sta sections_relocsizehi-1,x
	RETURN_OK

@pass1:	; calculate/set the size for the previous section
	lda zp::asmresult
	sec
	sbc sections_startlo-1,x
	sta sections_sizelo-1,x
	lda zp::asmresult+1
	sbc sections_starthi-1,x
	sta sections_sizehi-1,x

@done:	RETURN_OK
.endproc

;*******************************************************************************
.export __obj_add_export
.proc __obj_add_export
	; TODO
	rts
.endproc

;*******************************************************************************
; ADD RELOC
; Adds a new relocation entry to the current object file in construction
; NOTE: the addend is written by the assembler
; IN:
;   - .A:      size of value to relocate (0=ZP, 1=ABS)
;   - .Y:      offset to apply relocation at
;   - expr::*: various values containing result of expression eval
; OUT:
;   - .C: set on error
.proc add_reloc
@sz=r0
@rel=r1
@offset=r3
	sta @sz
	sty @offset

	lda expr::kind
	cmp #VAL_REL
	bne @ok		; expression doesn't require relocation

	ldxy reloctop
	cmpw #(reloc_tables_end-4)
	bcc :+
	RETURN_ERR ERR_OOM

:	stxy @rel

; encode the "info" byte for the relocation based on the result of the
; expression evaluation and the size of the relocation
;  field   bit(s)   description
; size       0   size of target value to modify 0=1 byte, 1=2 bytes
; mode       1   type of relocation: 1=section-relative, 0=symbol-relative
; postproc  2-3  post-processing (0=NONE, 1=LSB, 2=MSB)
@encode_size:
	lda expr::postproc
	asl
	asl
	ora @sz

	; is symbol in the expression is unresolved (section_id == SEC_UNDEF)?
	; yes -> use symbol-based relocation
	; no  -> use section-based relocation
	ldx expr::section
	cpx #SEC_UNDEF
	beq :+
	ora #1<<1		; flag section based relocation

:	ldy #$00
	sta (@rel),y		; write info byte
	pha			; save info byte

	; write offset in obj file (current "assembly" address)
	iny			; .Y=1
	lda zp::asmresult	; write offset LSB
	clc
	adc @offset
	sta (@rel),y
	iny			; .Y=2
	lda zp::asmresult+1
	adc #$00
	sta (@rel),y		; write offset MSB
	iny			; .Y=3

	pla			; restore info byte
	and #$02		; mask "type" bit
	beq @sym_based		; if !0, write symbol index

@sec_based:
	lda expr::section
	sta (@rel),y		; write symbol-id LSB
	lda #$00		; MSB of section is always 0
	iny			; .Y=4
	sta (@rel),y		; write symbol-id MSB
	bne @done		; branch always

@sym_based:
	lda expr::symbol
	sta (@rel),y		; write symbol-id LSB
	lda expr::symbol+1
	iny			; .Y=4
	sta (@rel),y		; write symbol-id MSB

@done:  ; update reloctop
	lda #$05
	clc
	adc reloctop
	sta reloctop
	bcc @ok
	inc reloctop+1
@ok:	RETURN_OK
.endproc

;*******************************************************************************
; BUILD SYMBOL INDEX MAP
; Constructs the map of symbols to the index to store for them in the symbol
; table.
.proc build_symbol_index_map
@idx=r0
@symtab=r2
@reltab=r4
	; walk the relocation tables to determine which symbols are referenced
	; in relocations.
	; only these will be emitted.
	ldxy #reloc_tables
	stxy @reltab
	cmpw reloctop
	bne :+
	rts		; no relocations

:	; initialize symbol index map to all $ff's (unmapped)
	ldxy #symbol_index_map
	stxy @symtab

	lda #$ff
@init:	ldy #$00
	sty @idx
	sty @idx+1
	sta (@symtab),y
	incw @symtab
	ldxy @symtab
	cmpw #symbol_index_map+(MAX_LABELS*2)
	bcc @init

@l0:	ldy #$00
	sty @symtab+1
	lda (@reltab),y
	and #$02		; mask mode bit
	bne @next		; if 1-> no symbol
	ldy #$03
	lda (@reltab),y		; get symbol ID
	asl
	rol @symtab+1
	adc #<symbol_index_map
	sta @symtab
	iny
	lda (@reltab),y
	adc #>symbol_index_map
	sta @symtab+1

	; check if symbol is already mapped
	ldy #$00
	lda (@symtab),y
	cmp #$ff
	bne @next		; not $ffff (already mapped) -> continue
	iny
	lda (@symtab),y
	cmp #$ff
	bne @next		; not $ffff (already mapped) -> continue

	; not mapped, assign this symbol an index
	lda @idx
	dey			; .Y=0
	sta (@symtab),y
	iny
	lda @idx+1
	sta (@symtab),y
	incw @idx
	incw num_symbols_mapped

@next:	lda @reltab
	clc
	adc #$05
	tax
	sta @reltab
	bcc :+
	inc @reltab+1
:	ldy @reltab+1
	cmpw reloctop
	bcc @l0
@done:	rts
.endproc

;*******************************************************************************
; DUMP LOCALS
; Dumps the referenced symbols to file.  These are all symbols used within
; the object code's relocation tables.
; Externals are also dumped here (if referenced), but separate procedures
; are needed to dump names for these (see dump_exports and dump_imports)
.proc dump_locals
@i=r0
@cnt=r2
@cnt2=r4
@map=r6
@id=r8
@buff=$100
	ldxy num_symbols_mapped
	stxy @cnt
	cmpw #0
	beq @done

	ldxy #symbol_index_map
	stxy @map

@dump_locals:
	; check if label should be dumped (mapped index != $ffff)
	ldy #$00
	lda (@map),y
	cmp #$ff
	beq @next
	iny
	lda (@map),y
	cmp #$ff
	beq @next

	; get the symbol offset and section
	ldxy @i
	CALL FINAL_BANK_MAIN, lbl::getsection	; get section
	jsr $ffd2				; write section out
	CALL FINAL_BANK_MAIN, lbl::by_id	; get address
	txa
	jsr $ffd2				; write offset LSB
	tya
	jsr $ffd2				; write offset MSB

	decw @cnt
	iszero @cnt
	beq @done

@next:	incw @i				; next symbol
	lda @map
	clc
	adc #$02
	sta @map
	bcc @dump_locals
	inc @map+1
	bne @dump_locals		; branch always

@done:	rts
.endproc

;*******************************************************************************
; DUMP IMPORTS
; Stores the names of the imported symbols along with their mapped symbol
; indices.
; Imports must be declared with the .IMPORT directive to map them to the object
; file.
; They are indentified by a SEC_UNDEF section index in the relocation tables at
; link time
.proc dump_imports
	; TODO:
	RETURN_OK
.endproc

;*******************************************************************************
; DUMP EXPORTS
; Stores the names of the exported symbols along with their section indices
; and section offsets.
; Exports may or may not be referenced within the object code
; They must be explicitly mapped to the object code by a ".EXPORT" directive
.proc dump_exports
@dumpnames:
@i=r0
@buff=$100
	lda #$00
	sta @i
	sta @i+1
	cmp numexports
	beq @done			; if no exports -> done

@l0:	; get the symbol name
	ldxy #@buff
	stxy r0
	ldy @i
	lda export_indexes_lo,y
	tax
	lda export_indexes_hi,y
	tay
	CALL FINAL_BANK_MAIN, lbl::getname

	; write out the name
	ldy #$00
:	lda @buff,y
	jsr $ffd2
	cmp #$00
	beq @cont
	iny
	bne :-

@cont:	; get/write the section index
	ldxy @i
	CALL FINAL_BANK_MAIN, lbl::getsection	; write section index
	jsr $ffd2

	; get/write the section offset
	ldxy @i
	CALL FINAL_BANK_MAIN, lbl::by_id
	txa
	jsr $ffd2				; write offset LSB
	tya
	jsr $ffd2				; write offset MSB

	inc @i
	cmp numexports
	bcc @l0

@done:	rts
.endproc

;*******************************************************************************
; DUMP SECTION HEADERS
; Dumps the sections for the object file in construction.
.proc dump_section_headers
@name=r0
@i=rc
	lda numsections
	beq @done

	lda #$00
	sta @i
@dump_headers:
	; get offset to name for this section (*8)
	lda @i
	asl
	asl
	asl
	adc #<sections_segments
	sta @name
	lda #>sections_segments
	adc #$00
	sta @name+1

	; write the name of the SEGMENT for the SECTION
	ldy #$00
:	lda (@name),y
	jsr $ffd2
	iny
	cpy #$08
	bne :-

	; write the OBJ and RELOC sizes
	ldx @i
	lda sections_sizelo,x
	jsr $ffd2
	lda sections_sizehi,x
	jsr $ffd2
	lda sections_relocsizelo,x
	jsr $ffd2
	lda sections_relocsizehi,x
	jsr $ffd2
	inx
	stx @i
	cpx numsections
	bne @dump_headers
@done:	rts
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
	ora @sz
	beq @done

@objloop:
	; dump the object code for the section
	ldxy @sec
	CALL FINAL_BANK_MAIN, vmem::load	; load a byte of object code
	jsr $ffd2
	incw @sec
	decw @sz
	iszero @sz
	bne @objloop

@reloc:	; then dump the relocation table
	ldx @cnt
	lda sections_relocstartlo,x
	sta @sec
	lda sections_relocstarthi,x
	sta @sec+1
	lda sections_relocsizelo,x
	sta @sz
	lda sections_relocsizehi,x
	sta @sz+1
	ora @sz
	beq @dbgi		; if no relocation table, skip

@relocloop:
	ldy #$00
	lda (@sec),y
	jsr $ffd2
	incw @sec
	decw @sz
	iszero @sz
	bne @relocloop

@dbgi:	; and finally, dump the debug info for the table
	; TODO:

@next:	inc @cnt
	lda @cnt
	cmp numsections
	beq @done
	jmp @l0
@done:	RETURN_OK
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
	jsr build_symbol_index_map

	lda numsections			; # of sections
	jsr $ffd2
	lda numsymbols			; # of symbols
	jsr $ffd2
	lda numsymbols+1
	jsr $ffd2

	; write the SECTION headers
	jsr dump_section_headers

	; write the SYMBOL TABLE
	jsr dump_locals
	jsr dump_imports
	jsr dump_exports

	; write each SECTION (object code, relocation data, debug info)
	jsr dump_sections

	RETURN_OK
.endproc
