;*******************************************************************************
; OBJ.ASM
; This file contains procedures used to construct object files.
;*******************************************************************************

.include "errors.inc"
.include "file.inc"
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
top = r0	 ; TODO

;*******************************************************************************
.segment "OBJBSS"

numrecords: .word 0

;*******************************************************************************
; SYMBOL INFO
; This table contains corresponding flags for each symbol.
; It defines the type of symbol in the object file.
; GLOBAL: an exported/imported symobl. These must have a label definition
; when linked. e.g.
;  ```
;  .export LABEL
;  .import LABEL2
;  ````
;
; DEFINITION: these are labels that are actually defined e.g.
; ```
; LABEL:
; ```
symbol_info:
.ifdef vic20
	.res MAX_SYMBOLS
.else
.endif

;*******************************************************************************
; SYMBOL SECTION IDS
; This table contains the index of each SYMBOL to its SECTION in the SECTIONS
; table.
symbol_section_ids:
.ifdef vic20
	.res MAX_SYMBOLS
.else
.endif

;*******************************************************************************
; SYMBOL OFFSETS
; This table contains the relative offsets for each symbol from their
; designated SECTION (identified in the SECTION IDs table).
symbol_offsetslo:
.ifdef vic20
	.res MAX_SYMBOLS
.else
.endif
symbol_offsetshi:
.ifdef vic20
	.res MAX_SYMBOLS
.else
.endif

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
; SYMBOL NAMES
; This table contains the name for each symbol used in the object file
symbol_names:
.ifdef vic20
	.res MAX_SYMBOLS*MAX_SYMBOL_NAME_LEN
.else
.endif

;*******************************************************************************
; RECORDS
; This buffer stores the object code records in pass 1
; In pass 2, it is read to produce the object file
records: .res $4000

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
; DEFINE SYMBOL
; Defines a symbol in the symbol table for the object file being generated
; Its value is relative to the object file.  The linker will compute its true
; address during linkage.
; e.g. `func:`
.export __obj_add_symbol
.proc __obj_add_symbol

.endproc

;*******************************************************************************
; ADD RECORD
; Writes a new record for the instruction IN MEMORY
; This is done in pass 1 to build the buffer that will be emitted in pass 2
.export __obj_add_record
.proc __obj_add_record
@rec=r0
	; TODO:
	incw numrecords
	rts
.endproc

;*******************************************************************************
; CLOSE RECORD
; Closes the open record and updates "top" to point past it
.proc close_record
	ldy #$00
	lda (top),y
	cmp #RECORD_BYTES
	bne :+
	lda (top),y
	bcs @update	; branch always
:

@update:
	sec		; +1 (record descr)
	adc top
	sta top
	bcc :+
	inc top+1
:	rts
.endproc

;*******************************************************************************
; EMIT BYTE
; Appends to an existing BYTES record (if one is open), or creates a new
; bytes record if there is not an existing one
; IN:
;  - .A: the byte to output
.proc emit_byte
	pha

	ldy #$00
	lda (top),y
	cmp #RECORD_BYTES
	bne @new_record
@update_record:
	iny
	lda (top),y	; get length
	clc
	adc #$01
	sta (top),y	; update length
	tay
	iny		; offset to byte is length+1
	pla
	sta (top),y

@new_record:
	jsr close_record
	lda #RECORD_BYTES
	rts
.endproc

;*******************************************************************************
; EMIT INSTRUCTION
; Writes a pre-built instruction to the open object file.
; This is called after assembling an instruction in the second pass.
; If the instruction was resolved in the second pass, this procedure will emit
; the direct bytes for that instruction (as a BYTES record)
; If the instruction references unresolved (external) symbols, it will emit an
; EXPRESSION record, which contains the token for the referenced symbol as well
; as its offset.
; IN:
;   - zp::opcode: the opcode of the instruction - e.g. $AD
;   - zp::symbol: the symbol referenced (if any) - e.g. label
;   - zp::offset: the symbol offset (if any) - e.g. label+2
;   - zp::info:   LSB is operand size, MSB contains post processing info (
;                 whether to use LSB (<), MSB (>), or neither)
.export __obj_emit_instruction
.proc __obj_emit_instruction
@opcode    = __obj_opcode
@operandsz = __obj_opsize
@symbol    = __obj_symbol
@addend    = __obj_addend
@info      = __obj_info
@operand   = __obj_operand
	; determine if we need an EXPRESSION record or just a BYTES one

; direct bytes
	; get the size and encode it into the record type
	jsr emit_byte
	lda @operandsz
	beq @done		; if operand is 0 bytes, we're done
	lda @operand
	jsr emit_byte		; write first byte of the operand
	lda @operandsz
	cmp #$01
	beq @done		; if operand is 1 byte, we're done
	lda @operand+1
	jmp emit_byte		; write second byte of the operand
@done:	rts

; expression
	lda #RECORD_EXPR
	jsr $ffd2		; write expression record type
	lda @opcode
	jsr $ffd2		; write the opcode

	; write the symbol ID (2 bytes)
	lda @symbol
	jsr $ffd2
	lda @symbol+1
	jsr $ffd2

	; write the symbol offset
	lda @addend
	jsr $ffd2

	; write the post-processing flag
	lda @info
	jsr $ffd2

	; write the size of the operand
	lda @operandsz
	jmp $ffd2
.endproc

;******************************************************************************
; WRITE SYMTAB
; Writes all symbols to the symbol table, thus generating the symbol table for
; the object file
.export write_symtab
.proc write_symtab
@cnt=r0
@name=r2
	ldxy #symbol_names
	stxy @name
	ldx #$00

@l0:	ldy #$00

	; write the symbol name to the object file
@l1:	lda (@name),y
	beq :+
	jsr $ffd2
	iny
	cpy #MAX_SYMBOL_NAME_LEN
	bcc @l1

:	; write the symbol type identifier
	lda symbol_info,x
	jsr $ffd2

	; if the symbol is an IMPORT, we're done with this one
	cmp #SYM_IMPORT_BYTE
	beq @next
	cmp #SYM_IMPORT_WORD
	beq @next

	; if symbol is an EXPORT, write the SEGMENT ID and SEGMENT OFFSET
	lda symbol_section_ids,x
	jsr $ffd2
	lda symbol_offsetslo,x
	jsr $ffd2
	lda symbol_offsetshi,x
	jsr $ffd2

@next:	lda @name
	clc
	adc #MAX_SYMBOL_NAME_LEN
	sta @name
	bcc :+
	inc @name+1

:	inx
	cpx numsymbols
	bne @l0

	RETURN_OK
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
	jsr write_symtab

	; the OBJECT block (object code) will be written by the assembler
	RETURN_OK
.endproc
