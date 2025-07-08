;*******************************************************************************
; OBJ.ASM
; This file contains procedures used to construct object files.
;*******************************************************************************

.include "asm.inc"
.include "errors.inc"
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
MAX_SYMBOLS         = 128	; max # of symbols per object file

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

;*******************************************************************************
.segment "OBJBSS"

;*******************************************************************************
; SECTIONS
; These variables contain the data for the sections
sections_sizelo:   .res MAX_SECTIONS
sections_sizehi:   .res MAX_SECTIONS
sections_segments: .res MAX_SEGMENT_NAME_LEN*MAX_SECTIONS ; name of target SEG

numsections:   .byte 0	; total number of sections in obj file being written
activesection: .byte 0	; the current SECTOIN (id) being written

numsymbols:  .byte 0

;*******************************************************************************
; SYMBOL INFO
; This table contains the name for each symbol used in the object file
symbol_info:
.ifdef vic20
	; info, section-id
	.res MAX_SYMBOLS*(1+1)
.else
.endif

.RODATA

.export __obj_add_symbol_info
__obj_add_symbol_info:
	lda asm::mode
	bne :+
	rts
	JUMP FINAL_BANK_LINKER, add_symbol_info

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
; ADD RELOC
; Adds a new relocation entry to the current object file in construction
; IN:
;   - expr::global_sym:  the symbol ID to relocate relative to
;   - expr::num_globals: !0 if symbol should be used as relocation base
.proc add_reloc
	; TODO:
	rts
.endproc

;******************************************************************************
; SYMTAB APPEND
; Writes a byte to the symbol table
; IN:
;   - .A: the byte to write
;   - r4: the address of the symbol table
.proc symtab_append
@symtab=r4
	ldy #$00
	sta (@symtab),y
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
	bcc :+
	inc @symtab+1

:	tya			; get info byte
	ldy #$00
	sta (@symtab),y

	iny
	txa			; get section ID
	sta (@symtab),y
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
; OUT
; Writes the current object file to the given filename.
; This is called after the first pass of the assembler to write all the exports,
; imports, and other header data to the object file.
; The assembler will then output the object code in pass 2.
; IN:
;   - .XY: the output filename
; OUT:
;   - .C: set on error
.export __obj_out
.proc __obj_out
@src=r0
@cnt=r2
	; open the output file for writing
	CALL FINAL_BANK_MAIN, file::open_w
	bcc @write_header
	rts			; failed to open file

@write_header:
	; write the SECTION
	lda numsections
	sta @cnt

	; write the names of the segments for each SECTION
	ldxy #@sections_segments
	stxy @src
@sections_segments:
	ldy #$00
:	lda (@src),y
	jsr $ffd2
	iny
	cpy #MAX_SEGMENT_NAME_LEN
	bcc :-
	lda @src
	clc
	adc #MAX_SEGMENT_NAME_LEN
	sta @src
	bcc :+
	inc @src+1
:	dec @cnt
	bne @sections_segments

	; write the number of bytes used in each segment
	ldy #$00
@sec_sizes:
	lda sections_sizelo,y
	jsr $ffd2
	lda sections_sizehi,y
	jsr $ffd2
	iny
	cpy numsections
	bcc @sec_sizes

	; write the SYMBOL TABLE
	jsr dump_symbols

	; the OBJECT block (object code) will be written by the assembler
	RETURN_OK
.endproc
