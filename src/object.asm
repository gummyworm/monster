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

reloc = zp::link	; when linking, pointer to current relocation

.export __obj_sections_sizelo
.export __obj_sections_sizehi
.export __obj_sections_segments

;*******************************************************************************
; SECTIONS
; These variables contain the data for the sections
sections_startlo:  .res MAX_SECTIONS
sections_starthi:  .res MAX_SECTIONS
__obj_sections_sizelo:
sections_sizelo:   .res MAX_SECTIONS
__obj_sections_sizehi:
sections_sizehi:   .res MAX_SECTIONS
__obj_sections_segments:
sections_segments: .res MAX_SEGMENT_NAME_LEN*MAX_SECTIONS ; name of target SEG
__obj_segments_sizelo:
segments_sizelo: .res MAX_SECTIONS
__obj_segments_sizehi:
segments_sizehi: .res MAX_SECTIONS
sections_info:     .res MAX_SECTIONS

; relocation table offsets/sizes for each section
sections_relocstartlo: .res MAX_SECTIONS
sections_relocstarthi: .res MAX_SECTIONS
sections_relocsizelo:  .res MAX_SECTIONS
sections_relocsizehi:  .res MAX_SECTIONS

.export __obj_numsections
__obj_numsections:
numsections: .byte 0	; number of sections in obj file being written/read

.export __obj_numsegments
__obj_numsegments:
numsegments: .byte 0	; number of SEGMENTs in obj file being written/read

numsymbols:  .word 0	; number of symbols in obj file being written

;*******************************************************************************
; SYMBOL INDEX MAP
; Each entry in this array contains the index that we will map the corresponding
; label to (see labels.asm)
; This lets us emit a more compact list of only symbols that are used
; If the symbol is unused, we store $ff
; The indexes in this array represent the id of the symbol at assembly time
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

symbol_sections: .res MAX_IMPORTS+MAX_EXPORTS
symbol_offsets:  .res MAX_IMPORTS+MAX_EXPORTS

;*******************************************************************************
; EXPORTS
; We store the id's for each export defined so that we can find its name when we
; dump the object file
numexports: .byte 0
export_label_idslo: .res MAX_EXPORTS	; LSB of label ID for exports
export_label_idshi: .res MAX_EXPORTS	; MSB of label ID for exports

;*******************************************************************************
; IMPORTS
; We store the id's for each import defined so that we can find its name when we
; dump the object file as well as the section in the symbol_index_map so that
; we know how to use
numimports: .word 0
import_label_idslo: .res MAX_EXPORTS	; LSBs of label ID for imports
import_label_idshi: .res MAX_EXPORTS	; MSBs of label ID for imports
import_indexeslo:   .res MAX_EXPORTS	; LSBs for index (in symbol_index_map)
import_indexeshi:   .res MAX_EXPORTS	; MSBs for index (in symbol_index_map)

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
	sta numimports
	sta numimports+1
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
	beq @sym_based		; if 0, write symbol index

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
	lda expr::postproc
	cmp #$01		; set .C if post-processing is used
	lda #$05
	adc reloctop		; +5 (no post-proc), +6 (post-proc)
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
	pha			; save info byte

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

@next:	pla			; restore info byte
	and #$0c		; mask postproc bits (2, 3)
	cmp #$01		; set .C if post-processing is used
	lda @reltab
	adc #$05		; +5 (no post-proc), +6 (post-proc)
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
	CALL FINAL_BANK_MAIN, lbl::by_id	; get address offset
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
@i=r0
@idx=r2
@buff=$100
	lda #$00
	sta @i
	sta @i+1
	iszero numimports
	beq @done			; if no imports -> done

@l0:	; get the symbol name
	ldxy #@buff
	stxy r0
	ldx @i
	ldy import_indexeshi,x		; get LSB of index for symbol
	sty @idx+1
	lda import_indexeslo,y		; get MSB of index for symbol
	sta @idx
	CALL FINAL_BANK_MAIN, lbl::getname

	; write out the name
	ldy #$00
:	lda @buff,y
	jsr $ffd2
	cmp #$00
	beq @cont
	iny
	bne :-

@cont:	; write the object-local index for the import
	lda @idx	; restore index LSB
	jsr $ffd2	; and write it
	ldy @idx	; and MSB
	jsr $ffd2

	; next symbol
	incw @i
	ldxy @i
	cmpw numimports
	bne @l0

@done:	rts
.endproc

;*******************************************************************************
; DUMP EXPORTS
; Stores the names of the exported symbols along with their section indices
; and section offsets.
; Exports may or may not be referenced within the object code
; They must be explicitly mapped to the object code by a ".EXPORT" directive
.proc dump_exports
@i=r0
@buff=$100
	lda #$00
	sta @i
	cmp numexports
	beq @done			; if no exports -> done

@l0:	; get the symbol name by looking up its label ID
	ldxy #@buff
	stxy r0
	ldx @i
	ldy export_label_idshi,x	; get LSB of index for symbol
	lda export_label_idslo,y	; get MSB of index for symbol
	tax
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
	ldxy @sec				; address to load
	CALL FINAL_BANK_MAIN, vmem::load	; load a byte of object code
	jsr $ffd2				; and dump it
	incw @sec
	decw @sz
	iszero @sz
	bne @objloop				; repeat til done

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

	ldy #$00
@relocloop:
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
	lda numsymbols			; # of symbols (LSB)
	jsr $ffd2
	lda numsymbols+1		; # of symbols (MSB)
	jsr $ffd2
	lda numexports			; # of EXPORTs
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

;*******************************************************************************
; APPLY RELOCATION
; Produces the final binary for a section by applying its relocation
; table to its base object code.
; IN:
;   - .A: the index of the section to apply the relocation for
; OUT:
;   - .C: set if there is no remaining relocation to apply for the section
.export __obj_apply_relocation
.proc __obj_apply_relocation
@symbol_id=r0
@tmp=r0
@addrmode=r2
@pc=r4
	ldy #$00

	; get the address mode (size of the target to update)
	lda (reloc),y	; get the info byte
	and #$01	; mask size bit
	sta @addrmode	; and save address mode for later

	; check if we are using a symbol or another section as the base
	; address for the relocation
	lda (reloc),y	; get the info byte again
	and #$02	; mask type bit
	bne @sec	; 1 = section, 0 = symbol

@sym:	; apply a symbol based relocation
	ldy #$03		; index to symbol ID LSB
	lda (reloc),y		; get symbol LSB
	sta @symbol_id
	iny			; .Y=4
	lda (reloc),y		; get symbol MSB
	sta @symbol_id+1

	; look up the symbol that we mapped for this index
	lda @symbol_id
	clc
	adc #<symbol_index_map
	tax
	lda @symbol_id+1
	adc #>symbol_index_map
	tay

	; and finally look up the value for the actual symbol
	CALL FINAL_BANK_MAIN, lbl::by_id
	stxy @tmp
	jmp @add_offset		; continue to calculate target address

@sec:	; apply section based relocation
	ldy #$03		; index to section ID
	lda (reloc),y
	jsr get_mapped_section	; look up addr of section mapped to this ID

@add_offset:
	; add the encoded offset (bytes 1 and 2 of the record) to our base
	; address to get the relocation target
	lda @tmp
	clc
	ldy #$01
	adc (reloc),y		; add LSB of offset
	sta @pc
	iny
	lda @tmp+1
	adc (reloc),y		; add MSB of offset
	sta @pc+1

@apply_addend:
	stxy @tmp		; save base address to apply addend to

	ldy #$00
	lda @addrmode		; get address mode
	beq @postproc		; if 0 -> apply zeropage relocation

@abs:	lda (@pc),y		; get LSB of addend
	clc
	adc @tmp
	sta (@pc),y		; store LSB of relocated operand

	iny
	lda (@pc),y
	clc
	adc @tmp+1
	sta (@pc),y		; store MSB of relocated operand
	jmp @update_reloc

@zp:	lda (@pc),y
	clc
	adc @tmp
	sta @tmp

	lda (reloc),y
	and #$0c		; is any postproc needed (bit 2 or 3 set)?
	beq :+

@nopostproc:
	; no post-processing, just add 1 byte addend and we're done
	lda @tmp
	adc (@pc),y
	sta (@pc),y

@update_reloc:
	; update the pointer in our relocation table
	lda reloc
	clc
	adc #$05
	sta reloc
	bcc :+
	inc reloc+1
:	RETURN_OK

@postproc:
	pha
	ldy #$05		; offset to addend MSB
	lda (reloc),y		; get MSB of addend
	adc @tmp+1
	sta @tmp+1

	; update relocation address by 6 (5 + 1 for MSB of addend)
	lda reloc
	clc
	adc #$06
	sta reloc
	bcc :+
	inc reloc+1

:	pla
	cmp #POSTPROC_LSB<<2	; are we taking LSB?
	beq @postproc_lsb
@postproc_msb:
	lda @tmp+1		; get the MSB
	sta (@pc),y		; and just store the MSB

@postproc_lsb:
	lda @tmp		; get the LSB
	sta (@pc),y		; and store the LSB
@done:	RETURN_OK
.endproc

;*******************************************************************************
; GET MAPPED SECTION
; Returns the address for the given index in the current object file
; e.g. for an object file with the following section table:
;   0: "CODE"
;   1: "DATA"
; Returns the base address of the "CODE" section at link time.
; NOTE: the "CODE" section's base address may differ from the base address of
; the CODE segment as defined in the linker config file.  Only the first "CODE"
; section will be placed at the defined start address.
; IN:
;   - .A: the index of the section
; OUT:
;   - .XY: the base address of the section
.proc get_mapped_section
	tax
	ldy sections_startlo,x
	lda sections_startlo,x
	tax
	rts
.endproc

;*******************************************************************************
; LOAD HEADERS
; Extracts the section usage and global symbols for the given object file.
; This is called by the linker for each object file to build the global link
; state.
.export __obj_load_headers
.proc __obj_load_headers
@cnt=r0
@seccnt=r2
@sec_idx=r4
@sym_idx=r4
@name=r6
@lbl=r6
@symcnt=r8
@importcnt=ra
@symoff=rc
@symsec=re
@i=zp::tmp10
@namebuff=$100
	jsr $ffb7		; READST (read status byte)
	beq :+
	sec
	rts			; err or EOF

:	; read number of sections used
	jsr $ffcf		; load byte
	sta numsections
	sta @seccnt

	; read number of SEGMENTs used
	jsr $ffcf
	sta numsegments

	; read number of symbols defined
	jsr $ffcf		; get # of symbols LSB
	sta numsymbols
	sta @symcnt
	jsr $ffcf		; get # of symbols MSB
	sta numsymbols+1
	sta @symcnt+1

	lda #$00
	sta @i
	lda #<sections_segments
	sta @name
	lda #>sections_segments
	sta @name+1

@load_headers:
@segname:
	; read the SEGMENT name and number of bytes used in it
	sty @cnt
	jsr $ffcf
	ldy @cnt
	sta (@name),y
	iny
	cpy #MAX_SECTION_NAME_LEN
	bne @segname

@segusage:
	; get the number of bytes used in each SEGMENT in the obj file
	ldy @i
	jsr $ffcf			; get LSB
	sta __obj_segments_sizelo,y
	jsr $ffcf			; get MSB
	sta __obj_segments_sizehi,y

	inc @i
	lda @i
	cmp numsegments
	bne @load_headers
	RETURN_OK
.endproc

;*******************************************************************************
; LOAD
; Loads the object state for the file with the given filename so that it may
; be linked. A preliminary pass is assumed to have taken place (e.g. labels
; created for any IMPORTs used in the program being linked)
.export __obj_load
.proc __obj_load
@cnt=r0
@seccnt=r2
@sec_idx=r4
@sym_idx=r4
@name=r6
@lbl=r6
@symcnt=r8
@importcnt=ra
@symoff=rc
@symsec=re
@namebuff=$100
	jsr __obj_load_headers
	bcc @load_symbols
	rts

	ldxy #symbol_sections
	sta @symsec
	ldxy #symbol_offsets
	sta @symoff
@load_symbols:
	jsr $ffcf		; get section ID for symbol
	ldy #$00
	sta (@symsec),y

	; get offset for the symbol
	jsr $ffcf
	ldy #$00
	sta (@symoff),y		; LSB
	jsr $ffcf
	ldy #$00
	sta (@symoff),y		; MSB

	; next label
	incw @symsec
	incw @symoff
	incw @symoff

	decw @symcnt
	iszero @symcnt
	bne @load_symbols

@load_imports:
	; get the name of a symbol
	ldx #$00
:	jsr $ffcf
	sta @namebuff,x
	beq @mapimport
	inx
	bne :-

@mapimport:
	; look up the import's offset and section by name
	; imports/exports are added as labels by the linker in its first pass
	ldxy #@namebuff
	CALL FINAL_BANK_MAIN, lbl::find
	stxy @lbl
	CALL FINAL_BANK_MAIN, lbl::addr	; get section offset
	tya
	ldy #$01
	sta (@symoff),y		; store MSB of offset
	dey
	txa
	sta (@symoff),y		; store LSB of offset
	ldxy @lbl
	CALL FINAL_BANK_MAIN, lbl::getsection
	ldy #$00
	sta (@symsec),y
	incw @symoff
	incw @symoff
	incw @symsec

	decw @importcnt
	iszero @importcnt
	bne @mapimport

	; skip over exports (not needed when loading obj file for linking)

@load_sections:
	lda #$00
	sta @sec_idx
	cmp @seccnt
	beq @done	; if no sections, we're done

	lda #<sections_segments
	sta @name
	lda #>sections_segments
	sta @name+1
@load_section:
	ldy #$00

; read the name for the section
@secname:
	sty @cnt
	jsr $ffcf
	ldy @cnt
	sta (@name),y
	iny
	cpy #MAX_SECTION_NAME_LEN
	bne @secname

	lda @name
	;sec
	adc #MAX_SECTION_NAME_LEN-1	; -1 for .C set
	sta @name
	bcc @secsizes
	inc @name+1

; read the size of each section
@secsizes:
	ldx @sec_idx
	jsr $ffcf			; get section size LSB
	sta sections_sizelo,x
	jsr $ffcf			; get section size MSB
	sta sections_sizehi,x
	jsr $ffcf
	sta sections_relocsizelo,x	; get relocation table size LSB
	jsr $ffcf
	lda sections_relocsizehi,x	; get relocation table size MSB

	jsr $ffb7			; READST
	bne @eof			; either EOF or read error

	dec @seccnt			; decrement section counter
	bne @load_section		; repeat for all sections

@done:	clc
@eof:	rts
.endproc
