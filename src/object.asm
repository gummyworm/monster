;*******************************************************************************
; OBJ.ASM
; This file contains procedures used to construct object files.
;*******************************************************************************

.include "asm.inc"
.include "errors.inc"
.include "expr.inc"
.include "file.inc"
.include "labels.inc"
.include "linker.inc"
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
MAX_SEGMENTS         = 8
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
.export __obj_segments
.export __obj_segments_sizelo
.export __obj_segments_sizehi

;*******************************************************************************
; SECTIONS
; These variables contain the data for the sections
sections_startlo:  .res MAX_SECTIONS
sections_starthi:  .res MAX_SECTIONS
__obj_sections_sizelo:
sections_sizelo:   .res MAX_SECTIONS
__obj_sections_sizehi:
sections_sizehi:   .res MAX_SECTIONS
__obj_segments_sizelo:
segments_sizelo: .res MAX_SECTIONS
__obj_segments_sizehi:
segments_sizehi: .res MAX_SECTIONS
sections_info:     .res MAX_SECTIONS

__obj_segments:
segments: .res MAX_SEGMENT_NAME_LEN*MAX_SECTIONS ; name of target SEG

; link-time start addresses of each SEGMENT
segments_startlo: .res MAX_SECTIONS
segments_starthi: .res MAX_SECTIONS

; SEGMENT id for each SECTION
segment_ids: .res MAX_SECTIONS

; relocation table offsets/sizes for each section
sections_relocstartlo: .res MAX_SECTIONS
sections_relocstarthi: .res MAX_SECTIONS
sections_relocsizelo:  .res MAX_SECTIONS
sections_relocsizehi:  .res MAX_SECTIONS

.export __obj_numsections
__obj_numsections:
numsections: .byte 0	; number of sections in obj file being written/read

.export __obj_filename
__obj_filename: .word 0	; pointer to name of object file being loaded

.export __obj_numsegments
__obj_numsegments:
numsegments: .byte 0	; number of SEGMENTs in obj file being written/read

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
; This table contains the fully resolved addresses for each symbol index used
; in the object file.
symbol_addresses: .res MAX_IMPORTS+MAX_EXPORTS

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
	sta numsegments
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
; ADD SEGMENT
; Adds a segment with the given name or creates it if it doesn't exist
; IN:
;   - .XY: address of the SEGMENT name to add
; OUT:
;   - .A: the ID of the segment added
;   - .C: set on error
.proc add_segment
@name=r0
@dst=r2
	stxy @name
	jsr get_segment_by_name
	bcc @done

	; segment not found, add it
	stxy @dst		; address to store new name to

	ldy #$00
@l0:	lda (@name),y
	sta (@dst),y
	beq @pad
	iny
	cpy #MAX_SEGMENT_NAME_LEN
	bcc @l0
	bcs @ok

@pad:	; pad remainder of buffer with 0's
	lda #$00
@l1:	sta (@dst),y
	iny
	cpy #MAX_SEGMENT_NAME_LEN
	bcc @l1

@ok:	lda numsegments
	cmp #MAX_SEGMENTS
	bcc :+
	;sec
	lda #ERR_TOO_MANY_SEGMENTS
	rts

:	inc numsegments
	lda numsegments		; get 1-based section ID
	clc			; ok
@done:	rts
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

	; in pass 2 we just need to store relocation table start address
	ldx num_reloctables_mapped
	lda reloctop
	sta sections_relocstartlo,x	; set reloc start LSB
	lda reloctop+1
	sta sections_relocstarthi,x	; set reloc start MSB
	inc num_reloctables_mapped
	RETURN_OK

@pass1:	ldx numsections
	cpx #MAX_SECTIONS
	bcc :+
	;sec
	lda #ERR_TOO_MANY_SEGMENTS
	rts

:	sta sections_info,x	; store address mode
	lda zp::asmresult
	sta sections_startlo,x	; set obj section start LSB
	lda zp::asmresult+1
	sta sections_starthi,x	; set obj section start MSB

	ldxy #@name
	jsr add_segment		; add/get SEGMENT id

	ldx numsections
	sta segment_ids,x	; set SEGMENT id for this SECTION

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
; table.  Also replaces the ID's in the relocation table with the indices with
; the indices in the IMPORTS table.
.proc build_symbol_index_map
@idx=r0
@symtab=r2
@reltab=r4
	; walk the relocation tables to determine which symbols are referenced
	; in relocations. only these will be emitted.
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
	bne @next		; if 1-> not a symbol-based relocation

	; get position of symbol (symbol_index_map + id*2)
	lda (@reltab),y		; get symbol ID (LSB)
	asl
	rol @symtab+1
	adc #<symbol_index_map
	sta @symtab
	iny
	lda (@reltab),y		; symbol ID (MSB)
	adc #>symbol_index_map
	sta @symtab+1

	; check if symbol is already mapped
	ldy #$00
	lda (@symtab),y
	cmp #$ff
	bne @update_rel		; not $ffff (already mapped) -> update table
	iny
	lda (@symtab),y
	cmp #$ff
	bne @update_rel		; not $ffff (already mapped) -> update table

	; not mapped, assign this symbol the next available index
	lda @idx
	dey			; .Y=0
	sta (@symtab),y
	iny
	lda @idx+1
	sta (@symtab),y
	incw @idx
	incw num_symbols_mapped

@update_rel:
	; rewrite the relocation table's stored index with the mapped index
	ldy #$00
	lda (@symtab),y
	ldy #$03
	sta (@reltab),y
	ldy #$01
	lda (@symtab),y
	ldy #$04
	sta (@reltab),y

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
; DUMP SEGMENTS
; Dumps the SEGMENTS used in the object file and their sizes
.proc dump_segments
@name=r0
@i=rc
	lda numsegments
	beq @done

	; compute the size of each segment (sum of all sections that use it)
	; TODO:
	lda __obj_sections_sizelo
	sta __obj_segments_sizelo
	lda __obj_sections_sizehi
	sta __obj_segments_sizehi

	lda #$00
	sta @i
@dump_headers:
	; get offset to name for this section (*8)
	lda @i
	asl
	asl
	asl
	adc #<segments
	sta @name
	lda #>segments
	adc #$00
	sta @name+1

	; write the name of the SEGMENT
	ldy #$00
:	lda (@name),y
	jsr $ffd2
	iny
	cpy #$08
	bne :-

	; write the number of bytes used for this SEGMENT (2 bytes)
	ldx @i
	lda __obj_segments_sizelo,x
	jsr $ffd2
	lda __obj_segments_sizehi,x
	jsr $ffd2

	; next SEGMENT
	inc @i
	lda @i
	cmp numsegments
	bne @dump_headers

@done:	RETURN_OK
.endproc

;*******************************************************************************
; LOAD SECTION META
; Loads the SEGMENT id, SEGMENT offset, and address mode for each section.
; The linker uses this to determine the address of each section before
; reading/applying the relocation tables
.proc load_section_meta
	; read the section sizes from the header in this section
	ldy #$00
	cpy numsections
	beq @done

@l0:	jsr readb		; read the SEGMENT id for this section
	bcs @ret
	sta segment_ids,y
	jsr readb		; read "info" (addr-mode) byte
	bcs @ret
	sta sections_info,y
	jsr readb		; read LSB of section offset
	bcs @ret
	sta sections_startlo,y
	jsr readb		; read MSB of section offset
	bcs @ret
	sta sections_starthi,y
	iny
	cpy numsections
	bne @l0
@done:	clc
@ret:	rts
.endproc

;*******************************************************************************
; DUMP SECTION META
; Writes the SEGMENT id, SEGMENT offset, and address mode for each section.
; The linker uses this to determine the address of each section before
; reading/applying the relocation tables
.proc dump_section_meta
	ldx #$00
	cpx numsections
	beq @done		; no sections

@l0:	; write the SEGMENT id used for the SECTION
	lda segment_ids,x
	jsr $ffd2

	; write the "info" byte for the SECTION
	lda sections_info,x
	jsr $ffd2

	; write offset of section from SEGMENT base
	lda sections_startlo,x
	jsr $ffd2			; LSB
	lda sections_starthi,x
	jsr $ffd2			; MSB

	inx
	cpx numsections
	bne @l0

@done:	RETURN_OK
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
	cmp numsections
	bne @l0
	RETURN_OK		; no sections

@l0:	ldx @cnt

	; write the size of the section
	lda sections_sizelo,x
	sta @sz
	jsr $ffd2			; dump size of the CODE table (LSB)
	lda sections_sizehi,x
	sta @sz+1
	jsr $ffd2			; dump size of CODE table (MSB)
	ora @sz
	php				; save .Z flag

	lda sections_relocsizelo,x	; dump size of RELOC table (LSB)
	jsr $ffd2
	lda sections_relocsizehi,x	; dump size of RELOC table (MSB)
	jsr $ffd2

	plp				; restore .Z flag (is OBJ table empty?)
	beq @done			; if no OBJ code, we're done

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
	lda numsegments			; # of segments
	jsr $ffd2
	lda numexports			; # of EXPORTS
	jsr $ffd2
	lda numimports			; # of IMPORTS (LSB)
	jsr $ffd2
	lda numimports+1		; # of IMPORTS (MSB)
	jsr $ffd2

	; write the SEGMENTS used (names and sizes)
	jsr dump_segments

	; write the SYMBOL TABLE (in order: IMPORTS, EXPORTS)
	jsr dump_imports
	jsr dump_exports

	; write SECTION metadata (segment-ids, offsets, and address modes)
	jsr dump_section_meta

	; write each SECTION (object code, relocation data, debug info)
	jsr dump_sections

	RETURN_OK
.endproc

;*******************************************************************************
; APPLY RELOCATION
; Produces the final binary for the object table by iteratively applying each
; section's relocation table to its base object code.
; OUT:
;   - .C: set if there is no remaining relocation to apply for the section
.export __obj_apply_relocation
.proc __obj_apply_relocation
@symbol_id=r0
@symbol_addr=r0
@tmp=r0
@addrmode=r2
@pc=r4
@sec_idx=r6
@rec=r7
@sz=re
	lda #$00
	sta @sec_idx

@apply: ldx @sec_idx
	lda sections_relocstartlo,x
	sta reloc
	lda sections_relocstarthi,x
	sta reloc+1
	lda sections_sizelo,x
	sta @sz
	lda sections_sizehi,x
	sta @sz+1

	; read a record from the RELOCATION table
	ldy #$00
:	jsr $ffcf
	sta @rec,y
	iny
	cpy #$05	; sizeof(relocation_record)
	bne :-

@section_loop:
	; get the address mode (size of the target to update)
	lda @rec	; get the info byte
	and #$01	; mask size bit
	sta @addrmode	; and save address mode for later

	; check if we are using a symbol or another section as the base
	; address for the relocation
	lda @rec	; get the info byte again
	and #$02	; mask type bit
	bne @sec	; 1 = section, 0 = symbol

@sym:	; apply (global) symbol based relocation
	; symbols are fully resolved by the time we apply relocation, so
	; just look up the address in symbol_addresses
	lda @rec+3	; get symbol LSB
	sta @symbol_id
	lda @rec+4	; get symbol MSB
	sta @symbol_id+1

	; look up the address that we resolved for this symbol id (index)
	lda @symbol_id
	clc
	adc #<symbol_addresses
	sta @symbol_addr
	lda @symbol_id+1
	adc #>symbol_addresses
	sta @symbol_addr+1
	ldy #$00
	lda (@symbol_addr),y
	tax
	iny
	lda (@symbol_addr),y
	tay
	jmp @add_offset		; continue to calculate target address

@sec:	; apply section (local symbol) based relocation
	lda @rec+3		; 3 = index to section ID

	; get the link-time address of the section
	jsr get_section_base
	jmp *

@add_offset:
	stxy @tmp		; save the base address we will apply addend to

	; get the address of the byte/word to apply relocation to
	txa			; base relocation address (LSB)
	adc @rec+1		; add LSB of offset
	sta @pc
	tya			; base relocation address (MSB)
	adc @rec+2		; add MSB of offset
	sta @pc+1

@apply_addend:
	ldy #$00
	lda @addrmode		; get address mode
	beq @zp			; if 0 -> apply zeropage relocation

@abs:	ldxy @pc
	CALL FINAL_BANK_MAIN, vmem::load	; load LSB of addend
	clc
	adc @tmp
	ldxy @pc
	CALL FINAL_BANK_MAIN, vmem::store	; store updated value

	incw @pc
	ldxy @pc
	CALL FINAL_BANK_MAIN, vmem::load	; load MSB of addend
	clc
	adc @tmp+1
	ldxy @pc
	CALL FINAL_BANK_MAIN, vmem::store	; store MSB of relocated operand
	jmp @next

@zp:	ldxy @pc
	CALL FINAL_BANK_MAIN, vmem::load	; load addend
	clc
	adc @tmp
	sta @tmp

	lda @rec		; read INFO byte again
	and #$0c		; is any postproc needed (bit 2 or 3 set)?
	bne @postproc		; if so, continue to apply it

@nopostproc:
	; no post-processing, just add 1 byte addend and we're done
	lda @tmp
	CALL FINAL_BANK_MAIN, vmem::store	; store relocated value
	jmp @next

@postproc:
	jsr $ffcf		; read another byte to get the MSB of addend
	adc @tmp+1		; add with operand MSB
	sta @tmp+1

	lda @rec		; get info again
	and #$0c		; mask postproc bits (2, 3)
	cmp #POSTPROC_LSB<<2	; are we taking LSB?
	beq @postproc_lsb

@postproc_msb:
	lda @tmp+1				; get the MSB (post-proc)
	ldxy @pc
	CALL FINAL_BANK_MAIN, vmem::store	; store relocated value

@postproc_lsb:
	lda @tmp				; get the LSB ldxy @pc
	ldxy @pc
	CALL FINAL_BANK_MAIN, vmem::store	; and store

@next:	decw @sz
	iszero @sz
	beq :+
	jmp @section_loop

:	inc @sec_idx
	lda @sec_idx
	cmp numsections
	beq @done
	jmp @apply

@done:	RETURN_OK
.endproc

;*******************************************************************************
; LOAD INFO
; Loads the first part of the object file and extracts basic info from it
; (e.g. number of symbols)
; OUT:
;   - .C: set on error
.proc load_info
@cnt=r0
@i=r2
@name=r4
@symoff=r8
@namebuff=$100
	lda #<segments
	sta @name
	lda #>segments
	sta @name+1

	; read number of sections used
	jsr readb
	bcc :+
@ret:	rts

:	sta numsections

	; read number of SEGMENTs used
	jsr readb
	bcs @ret
	sta numsegments

	; read number of EXPORTS (1 byte)
	jsr readb
	bcs @ret
	sta numexports

	; read number of IMPORTS (2 bytes)
	jsr readb
	bcs @ret
	sta numimports
	jsr readb
	bcs @ret
	sta numimports+1

;--------------------------------------
; read the SEGMENTS used in the object file (names and sizes)
	lda #$00
	sta @i

@load_segments:
	ldy #$00
@segname:
	; read the SEGMENT name
	jsr readb
	bcs @ret
	sta (@name),y
	iny
	cpy #MAX_SECTION_NAME_LEN
	bne @segname

	; get the number of bytes used in the SEGMENT
	ldy @i
	jsr readb			; get LSB
	bcs @ret
	sta __obj_segments_sizelo,y
	jsr readb			; get MSB
	bcs @ret
	sta __obj_segments_sizehi,y

	; get the base address of this SEGMENT in the linker
	ldxy @name
	jsr link::segaddr_for_file
	tya
	ldy @i
	sta segments_starthi,y		; store MSB of segment base
	txa
	sta segments_startlo,y		; store LSB of segment base

	; move name pointer to next location
	lda @name
	clc
	adc #MAX_SECTION_NAME_LEN
	sta @name
	bcc :+
	inc @name+1

:	; increment counter and loop til we've done all SEGMENTS
	iny
	sty @i
	cpy numsegments
	bne @load_segments

	RETURN_OK
.endproc

;*******************************************************************************
; LOAD HEADERS
; Extracts the SEGMENT usage info and global symbols for the given object file.
; This is called by the linker for each object file to build the global link
; state.
.export __obj_load_headers
.proc __obj_load_headers
@cnt=r0
@name=r6
@i=zp::tmp10
@namebuff=$100
	jsr load_info
; read the IMPORTS and EXPORTS and add them to the global symbol table
@imports:
	lda #SEC_UNDEF
	sta zp::label_sectionid	; imports' section is UNDEFINED
	lda #$00
	sta @i
	cmp numimports
	beq @exports
@import_loop:
	jsr load_import
	inc @i
	lda @i
	cmp numimports
	bne @import_loop

@exports:
	lda #$00
	sta @i
	cmp numexports
	beq @ok

	; prepend the filename as scope so that we know the file the symbol
	; was defined in when we resolve it
	ldxy __obj_filename
	CALL FINAL_BANK_MAIN, lbl::setscope

@export_loop:
	jsr load_export
	bcs @ret
	inc @i
	lda @i
	cmp numexports
	bne @export_loop

@ok:	clc
@ret:	rts
.endproc

;*******************************************************************************
; LOAD IMPORT
; Adds the next IMPORT in the open OBJECT file to the symbol table unless
; it is already defined.
; Returns an error if the symbol already exists and conflicts
; OUT:
;   - .C: set on error
.proc load_import
@namebuff=$100
	; get the name of a symbol
	ldx #$00
	stx zp::label_value
	stx zp::label_value+1
:	jsr readb
	bcs @ret
	sta @namebuff,x
	beq @cont
	inx
	bne :-

@cont:
	jsr readb				; get info byte (address mode)
	bcs @ret
	sta zp::label_mode

	ldxy #@namebuff
	CALL FINAL_BANK_MAIN, lbl::find		; was label already added?
	bcs :+					; if no -> add it

	; validate: does address mode match existing symbol?
	CALL FINAL_BANK_MAIN, lbl::addrmode
	cmp zp::label_mode
	beq @ok					; matches -> ok

	; error: address mode doesn't match
	RETURN_ERR ERR_ADDRMODE_MISMATCH	; conflicting import/exports

:	JUMP FINAL_BANK_MAIN, lbl::add
@ok:	clc
@ret:	rts
.endproc

;*******************************************************************************
; LOAD EXPORT
; Adds the next EXPORT in the open OBJECT file to the global symbol table
; OUT:
;   - .C: set on error
.proc load_export
@namebuff=$100
	; get the name of a symbol
	ldx #$00
:	jsr readb
	bcs @ret
	sta @namebuff,x
	beq @addexport
	inx
	bne :-

@addexport:
	jsr readb				; get info (address mode)
	bcs @ret
	sta zp::label_mode

	jsr readb				; get SEGMENT id
	bcs @ret
	sta zp::label_sectionid
	jsr readb				; get LSB of symbol offset
	bcs @ret
	sta zp::label_value
	jsr readb				; get MSB of symbol offset
	bcs @ret
	sta zp::label_value+1

	ldxy #@namebuff
	CALL FINAL_BANK_MAIN, lbl::find		; was label already added?
	bcs :+					; no -> add it

	; validate: is section SEC_UNDEF?
	; if not, error (only 1 EXPORT is allowed per symbol)
	; TODO:
	RETURN_ERR ERR_ALREADY_EXPORTED		; multiple exports

	; validate: does address mode match existing symbol?
	CALL FINAL_BANK_MAIN, lbl::addrmode
	cmp zp::label_mode
	beq @ok					; matches -> ok

	; error: address mode doesn't match
	RETURN_ERR ERR_ADDRMODE_MISMATCH	; conflicting import/exports

:	JUMP FINAL_BANK_MAIN, lbl::add
@ok:	clc
@ret:	rts
.endproc

;*******************************************************************************
; GET SECTION BASE
; Returns the base address for the given section at link time
; NOTE: all indexing in this procedure is relative to table-1 because section
; id's are 1-based.
; IN:
;   - .A: the section ID to return the address of
; OUT:
;   - .XY: the base address of the section
.proc get_section_base
@tmp=r0
	; segments_start[segment_ids[sec_id]] + sections_start[sec_id]
	; look up base address for the section's SEGMENT (segments_start[segment_ids[sec_id]])
	tax
	ldy segment_ids-1,x		; segment_ids[sec_id]
	lda segments_startlo-1,y	; segments_start[segment_ids[sec_id]] (LSB)
	pha
	lda segments_starthi-1,y	; segments_start[segment_ids[sec_id]] (MSB)
	sta @tmp

	; now add the base offest of the SECTION (sections_start[sec_id])
	pla
	clc
	adc sections_startlo-1,x	; resolve LSB
	tax
	lda @tmp
	adc sections_starthi-1,x	; resolve MSB
	tay
	rts
.endproc

;*******************************************************************************
; LOAD
; Loads the object state for the file with the given filename so that it may
; be linked. A preliminary pass is assumed to have taken place.
; The global symbol table is expected to be built (labels created for any
; IMPORTs used in the program being linked)
.export __obj_load
.proc __obj_load
@tmp=r0
@seccnt=r2
@sec_idx=r4
@i=r4
@name=r6
@addr=r6
@symcnt=r8
@sz=r8
@symaddr=ra
@symoff=rc
@sec=re
@namebuff=$100
	jsr load_info
	bcc :+
@ret:	rts

:	iszero numimports	; are there any IMPORTS?
	beq @eat_exports	; if not, skip ahead

	lda #$00
	sta @i
@load_imports:
	; get the name of a symbol
	ldy #$00
:	jsr readb
	bcs @ret
	sta @namebuff,y
	beq @mapimport
	iny
	bne :-

@mapimport:
	; look up the import's fully resolved address by its name
	ldxy #@namebuff
	CALL FINAL_BANK_MAIN, lbl::find	; find label ID by name
	CALL FINAL_BANK_MAIN, lbl::addr	; get the resolved address
	stxy @addr

	jsr readb			; eat info byte
	bcs @ret

	; get pointer to the resolved address for this symbol's index
	lda #$00
	sta @symaddr+1
	lda @i
	asl
	rol @symaddr+1
	adc #<symbol_addresses
	sta @symaddr
	lda #$00
	adc #>symbol_addresses
	sta @symaddr+1

	; store the resolved address for this symbol's index
	ldy #$00
	lda @addr
	sta (@symaddr),y
	iny
	lda @addr
	sta (@symaddr),y

	incw @i
	ldxy @i
	cmp numimports
	bne @load_imports	; repeat for all IMPORTS

@eat_exports:
	lda numexports
	sta @symcnt
	beq @load_section_meta	; no EXPORTS? skip ahead

	; skip over exports (not needed when loading obj file for linking)
@eat_export:
@eat_name:
	jsr readb
	bcc :+
@export_error:
	rts			; return error

:	cmp #$00		; did we read terminating 0 for filename?
	bne @eat_name		; loop til we found it
	ldy #4			; sizeof(info+segment_idx+address)
:	jsr readb
	bcs @export_error
	dey
	bne :-
	dec @symcnt
	bne @eat_export

@load_section_meta:
	jsr load_section_meta

; done with symbols, now load all the SECTION information to get the sizes
; of each table we will need to walk
@load_sections:
	lda #$00
	sta @sec_idx
	cmp numsections
	beq @done	; if no sections, we're done

@load_section:
	; read the section sizes from the header in this section
	ldy @sec_idx
	jsr readb			; get section size LSB
	bcs @eof
	sta sections_sizelo,y
	sta @sz
	jsr readb			; get section size MSB
	bcs @eof
	sta sections_sizehi,y
	sta @sz+1
	jsr readb
	bcs @eof
	sta sections_relocsizelo,y	; get relocation table size LSB
	jsr readb
	bcs @eof
	sta sections_relocsizehi,y	; get relocation table size MSB

	; calculate start address of relocation table (sections_start + sections_size)
	lda sections_startlo,y
	;clc
	adc @sz
	sta sections_relocstartlo,y
	lda sections_starthi,y
	adc @sz+1
	sta sections_relocstarthi,y

	; get the address to write the object code to
	lda @sec_idx
	jsr get_section_base
	stxy @sec

	lda numsections
	sta @seccnt

@objcode:
	; finally, load the object code for the section to vmem
	jsr readb
	bcs @eof
	ldxy @sec				; address to store to
	CALL FINAL_BANK_MAIN, vmem::store	; store a byte of object code
	incw @sec
	decw @sz
	iszero @sz
	bne @objcode				; repeat til done
	inc @sec_idx
	dec @seccnt				; decrement section counter
	bne @load_section			; repeat for all sections

@done:	clc
@eof:	rts
.endproc

;******************************************************************************
; READB
; Reads a byte and checks for error (READST)
; OUT:
;   - .A: the byte read or error code (0=eof)
;   - .C: set on error/eof
.proc readb
	jsr $ffb7     ; call READST (read status byte)
	bne @eof      ; either EOF or read error
	jsr $ffa5     ; call CHRIN (get a byte from file)
	RETURN_OK

; read drive err chan and translate CBM DOS error code to ours if possible
@eof:  	and #$40
	beq @err
	lda #$00	; EOF
	RETURN_OK

@err:	JUMP FINAL_BANK_MAIN, file::geterr
.endproc

;******************************************************************************
; GET SEGMENT NAME BY ID
; Returns the (object-local) name of the segment from its id
; IN:
;  - .A: the id of the SEGMENT to get the name of
; OUT:
;  - .A: the name of the SEGMENT (object-local)
.proc get_segment_name_by_id
@seg=r0
	ldx #$00
	stx @seg+1
	asl
	rol @seg+1
	asl
	rol @seg+1
	asl			; *8 (MAX_SEGMENT_NAME_LEN)
	rol @seg+1
	adc #<segments
	tax
	lda @seg+1
	adc #>segments
	tay
	RETURN_OK
.endproc

;******************************************************************************
; GET SEGMENT BY NAME
; Returns the ID of the segment from its name
; IN:
;  - .XY: the name of the segment
; OUT:
;  - .A:  the ID of the segment
;  - .XY: if not found, address of the next available SEGMENT name
;  - .C:  set if no segment exists by the given name
.proc get_segment_by_name
@name=zp::str0
@other=zp::str2
@cnt=r0
	stxy @name
	ldxy #segments
	stxy @other

	lda #$00
	sta @cnt
	cmp numsegments
	beq @notfound

@l0:	lda #MAX_SEGMENT_NAME_LEN
	jsr strcmp
	beq @found
	lda @other
	clc
	adc #MAX_SEGMENT_NAME_LEN
	sta @other
	ldx @cnt
	inx
	stx @cnt
	cpx numsegments
	bcc @l0
@notfound:
	ldxy @other
	rts

@found: lda @cnt
	RETURN_OK
.endproc

;*******************************************************************************
; STRCMP
; Compares the strings in (zp::str0) and (zp::str2) up to a length of .A
; IN:
;  zp::str0: one of the strings to compare
;  zp::str1: the other string to compare
; OUT:
;  .Z: set if the strings are equal
.proc strcmp
	ldy #$00
@l0:	lda (zp::str0),y
	beq :+
	jsr is_ws
	beq :+
	cmp (zp::str2),y
	bne @ret
	iny
	bne @l0

:	lda (zp::str2),y	; make sure strings terminate at same index
@ret:	rts
.endproc

;*******************************************************************************
; IS WS
; Checks if the given character is a whitespace character
; IN:
;  - .A: the character to test
; OUT:
;  - .Z: set if if the character in .A is whitespace
.proc is_ws
	cmp #$0d	; newline
	beq :+
	cmp #$09	; TAB
	beq :+
	cmp #$0a	; UNIX newline
	beq :+
	cmp #' '
:	rts
.endproc
