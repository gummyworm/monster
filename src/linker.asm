.include "errors.inc"
.include "labels.inc"
.include "macros.inc"
.include "string.inc"
.include "zeropage.inc"

;******************************************************************************
; CONSTANTS
MAX_SECTIONS = 8
MAX_SEGMENTS = 8

;******************************************************************************
; SECTION flags
SECTION_FILL = $01	; flag to pad section's unused bytes with 0

;******************************************************************************
; SEGMENT flags
SEGMENT_RO     = $01
SEGMENT_DEFINE = $02

;******************************************************************************
; ZEROPAGE variables
objptr=zp::asm
segptr=zp::asm+2

;******************************************************************************
.BSS

numsegments: .byte 0
numsections: .byte 0
numfiles:    .byte 0

activeseg: .byte 0	; the current SEGMENT being assembled to

;******************************************************************************
; SECTIONS
; The memory section contains up to MAX_SECTIONS of memory blocks.  Each
; defines the start and end addresses of the section plus a byte of flag
; data, which is used to, for example, flag that the section should be padded
; if the SEGMENTs that map to this section do not span the entire
; [start, stop] address space.
; The format of a SECTION is:
;  .word start addr
;  .word stop addr
;  .byte flags
sections_startlo: .res MAX_SECTIONS
sections_starthi: .res MAX_SECTIONS
sections_stoplo:  .res MAX_SECTIONS
sections_stophi:  .res MAX_SECTIONS
sections_flags:   .res MAX_SECTIONS

;******************************************************************************
; SEGMENTS
; Segments define where the code or data that occupies them will be mapped to
; the memory defined in the SECTIONS blocks
; SEGMENTs may LOAD to one address but RUN in another. Note the corresponding
; bytes in the SEGMENT structure that select this.
;
; SEGMENTs have the format:
;  .byte[8] name
;  .byte    LOAD SEGMENT id
;  .byte    RUN SEGMENT id
;  .byte    flags
segments_load:   .res MAX_SEGMENTS
segments_run:    .res MAX_SEGMENTS
segments_flags:  .res MAX_SEGMENTS
segments_sizelo: .res MAX_SEGMENTS
segments_sizehi: .res MAX_SEGMENTS

segments_startlo: .res MAX_SEGMENTS
segments_starthi: .res MAX_SEGMENTS
segments_stoplo:  .res MAX_SEGMENTS
segments_stophi:  .res MAX_SEGMENTS

segment_names: .res 8*MAX_SEGMENTS

;******************************************************************************
; OBJECT CODE overview
; Object code is stored in a simple block format as follows
;  - the 1st block contains all IMPORTs required to link the file
;  - the 2nd contains all EXPORTs exported by the file (and their offsets)
;  - the 3rd contains the SEGMENTs used and the # of bytes they use in each
;  - the 4th contains the relocatable data
;
;  IMP LAB1
;  IMP LAB2
;  EXP my_proc  CODE+$1000
;  EXP my_proc2 SETUP+$20
;
;  CODE $1032
;  DATA $0062
;
;  Obj Code Def        | Description                               |
;  ----------------------------------------------------------------|
;  SEG CODE            | Switches to the CODE segment              |
;  b $01 $33 $44       | defines bytes at CODE+0, CODE+1, CODE+2   |
;  b $ad               | defines a byte at CODE+3                  |
;  w LAB+3             | defines WORD with the value of LAB        |
;
; The value of segments (e.g. CODE) will change after each compilation unit.
; At the end of each unit, they are updated by the number of bytes produced
; for that segment by that unit.
; So if our unit has $200 bytes of code/data in its CODE segment, the
; hypothetical next unit will start assembling its CODE segment at CODE+$200
;
; Exports are calculated during linkage according to the current offset +
; SEGMENT values

;******************************************************************************
; OBJ Code Constants
; These represent the "instructions" of the object code
OBJ_BYTES   = $01 	; defines literal byte values e.g. "B 4 0 1 2 3"
OBJ_RELWORD = $02	; defines a value of an imported symbol "W LAB"
OBJ_SETSEG  = $03       ; switches to the given segment e.g. "SEG DATA"

;******************************************************************************
.CODE

;******************************************************************************
; ADD SECTION
; Adds a new section using the given parameters
; IN:
;  - zp::tmp0: the start address for the segment
;  - zp::tmp2: the stop address for the segment
;  - .A:       flags for the segment
.export __link_add_section
.proc __link_add_section
@start=zp::tmp0
@stop=zp::tmp2
@flags=zp::tmp4
	ldx numsections

	sta sections_flags,x

	lda @start
	sta sections_startlo,x
	lda @start+1
	sta sections_starthi,x

	lda @stop
	sta sections_stoplo,x
	lda @stop+1
	sta sections_stophi,x

	inc numsections
	rts
.endproc

;******************************************************************************
; ADD SEGMENT
; Adds a new segment using the given parameters
; IN:
;  - zp::tmp0: pointer to the name for the segment
;  - .A:       LOAD section id
;  - .X:       RUN section id
;  - .Y:       flags
.export __link_add_segment
.proc __link_add_segment
@run=zp::tmp0
	stx @run

	sta segments_load,x
	tya
	sta segments_flags,x
	lda @run
	sta segments_run,x

	inc numsegments
	rts
.endproc

;******************************************************************************
; PARSE LINK FILE
; Parses the given file into sections and segments, loading those into the
; linker's own section and segment state.
; The file must follow the format:
;
; ```
; MEMORY [
; SECTIONA:
;  start=$0000
;  end=$1000
; SECTIONB:
;  ...
; ]
; SEGMENTS [
;  SEGA: load = SECTIONA, run = SECTIONB
; ]
; ```
;
; IN:
;  - .XY: pointer to the link filename
; OUT:
;  - .C: set if the file could not be successfully opened or parsed
.export __link_parse
.proc __link_parse

.endproc

;******************************************************************************
; LINK
; Links all files that were added to the linker (link::addfile) and produces
; the linked executable as a file with the given name.
; IN:
;  - .XY: the filename to produce from the linked files
; OUT:
;  - .C: set on error
.proc __link_link
@segptrs=zp::tmp0			; 2*MAX_SEGMENTS bytes
@secptrs=zp::tmp10			; 2*MAX_SECTIONS bytes
	; init the segment/section pointers using the linker state defined by
	; the link file
	ldx numsegments
	bne :+
	; TODO:create a default CODE segment
	sec
	rts

:	dex
	txa
	asl
	tay
@l0:	lda segments_startlo
	sta @segptrs,y
	lda segments_starthi
	sta @segptrs+1,y
	dex
	dey
	dey
	bpl @l0

	; init secptrs to the start addresses of each section
	ldx numsections
	bne :+
	RETURN_ERR ERR_NO_SECTIONS

:	dex
	txa
	asl
	tay
@l1:	lda sections_startlo
	sta @secptrs,y
	lda sections_starthi
	sta @secptrs+1,y
	dex
	dey
	dey
	bpl @l1

	; extract header data foreach file and update pointers to each file
	; to the main block of the .obj file
	jsr extract_headers

	; now that we have the sizes of each SEGMENT, build the start
	; addresses of each segment and update SECTION start pointer by the
	; SIZE of the SEGMENT we've assigned to that SECTION.
	ldx #$00
	ldy segments_load,x
	lda sections_startlo,y	; LSB of current SECTION start address
	sta segments_startlo,x	; store as SEGMENT's start address
	adc segments_sizelo,x
	sta sections_startlo,y	; update SECTION's new start address by SIZE
	lda sections_starthi,y
	sta segments_starthi,x
	adc segments_sizehi,x
	sta sections_starthi,y

	; make sure START is still less than STOP for the SECTION
	lda sections_starthi,y
	cmp sections_stophi,y
	bcc @ok
	bne @err			; if STARTHI > STOPHI, return err
	lda sections_startlo,y
	cmp sections_stoplo,y
	bcc @ok
@err:	RETURN_ERR ERR_SECTION_TOO_SMALL

@ok:	jmp link_object
	; finally, read through each .obj file's body and build the binary
.endproc

;******************************************************************************
; LINK OBJECT
; Handles the main block of the object code defintion using the data extracted
; from the OBJ headers.
.proc link_object
	; TODO: set IMPORTs for this OBJ file

@l0:	ldy #$00
	lda (objptr),y	; get an "instruction" from the OBJ code
	incw objptr	; move past "opcode"
	clc
	beq @done

	cmp #OBJ_BYTES		; bytes
	bne :+
	jsr obj_bytes
	jmp @l0

:	cmp #OBJ_RELWORD	; relative word?
	bne :+
	jsr obj_rel_word
	jmp @l0

:	cmp #OBJ_SETSEG		; set segment?
	bne @done		; unrecognized "opcode"
	jsr set_seg
	bcc @l0			; if no, error continue loop

@done:	rts
.endproc

;******************************************************************************
; OBJ_BYTES
; Handles the OBJ_BYTES command
; Assembles the bytes that follow to the address of the current segment pointer,
; which is updated upon doing so
.proc obj_bytes
@ptr=zp::tmp0
@cnt=zp::tmp2
	ldy #$00
	lda (objptr),y ; read the number of bytes to output
	sta @cnt

	incw objptr
	lda (objptr),y ; read the number of bytes to output (hi)
	sta @cnt+1

	incw objptr

@l0:	lda (objptr),y	; get the byte to write to the binary
	sta (segptr),y	; output the byte from the object file
	incw segptr
	incw objptr
	decw @cnt
	bne @l0

	rts
.endproc

;******************************************************************************
; OBJ_REL_WORD
; Handles the OBJ_REL_WORD command
; Inserts a WORD with the value of the symobl that follows + an offset to
; the current address of the segment pointer
; A textual representation of this command looks like this:
;  `RW LABEL 12`
; LABEL is defined in the IMPORT section for the object code, so its binary
; representation refers to the offset in the IMPORT table
; The binary representation of the above command looks like this:
;  ` $02 $0030 $0c`
.proc obj_rel_word
	ldy #$00
	lda (objptr),y
	sta (segptr),y
	incw objptr
	incw segptr
	lda (objptr),y
	sta (segptr),y

	; update object code and segment pointers
	incw objptr
	incw segptr
	rts
.endproc

;******************************************************************************
; SET SEG
; Sets the segment to the given segment name
; e.g.
;  SEG "DATA"
; The binary representatino is similar
;  $03 "DATA",0
; Where $03 is the binary opcode for "SEG" and "DATA" is still the literal
; segment data (0-terminated)
.proc set_seg
	; store the current end address of the active segment
	ldx activeseg
	lda segptr
	sta segments_stoplo,x
	lda segptr+1
	sta segments_stophi,x

	ldxy objptr
	jsr get_segment_by_name
	bcs @done		; return error if not found
	sta activeseg
	tax
	lda segments_stoplo,x
	sta segptr
	lda segments_stophi,x
	sta segptr+1

	; move past the name
	ldy #$ff
:	iny
	lda (objptr),y
	bne :-

	tya
	sec			; +1 (get past the 0)
	adc objptr
	bcc @done
	inc objptr+1
@done:	rts
.endproc

;******************************************************************************
; EXTRACT HEADERS
; Extracts the header data from the .obj file and builds the absolute address
; of any EXPORTs for the .obj file. These are added as labels (lbl::add).
; Then updates the SEGMENT pointers by the sizes of each SEGMENT used in the
; .obj file.
;
; The first block contains the EXPORTs for the block
; --------------------------------------------------
; | PROC [8 bytes] |  SEG [8 bytes | SIZE [2 bytes]|
; -------------------------------------------------|
; |                             ...                |
; --------------------------------------------------
;
; The second block contains the IMPORTs for the block
; ----------------------
; | EXT_PROC [8 bytes] |
; ----------------------
; | EXTPROC2 [8 bytes] |
; ----------------------
; |        ...         |
; ----------------------
;
; The 3rd block of headers tells the linker the size of each SEGMENT in the
; .obj file
; -----------------------------------
; | SEG1 [8 bytes] | size [2 bytes] |
; | SEG2 [8 bytes] | size [2 bytes] |
; | 0 (end of seg) |
; ------------------
;
; IN:
;  - .XY: address to the contents of the .obj file
; OUT:
;  - .XY:             address of the remainder of the .obj file (after headers)
;  - segments_stoplo: updated with new LSBs of end of affected segments
;  - segments_stophi: updated with new MSBs of end of affected segments
;  - labels:          updated with any EXPORTed symbols from .obj file
EXPORT_SEG  = 8 		; offset to SEGMENT name in IMPORT header
EXPORT_SIZE = 8+8		; offset to SIZE in IMPORT header

EXPORT_BLOCK_ITEM_SIZE = 8 + EXPORT_SEG + EXPORT_SIZE
.proc extract_headers
@fptr=zp::tmp0
@segaddr=zp::tmp2
@buff=zp::tmp4
	stxy @fptr
	ldx numfiles

@getexports:
	; get the absolute address of the segment
	lda #EXPORT_SEG
	clc
	adc @buff
	tax
	lda @buff+1
	adc #$00
	tay
	jsr get_segment_by_name
	stxy @segaddr

	; calclulate the absolute address for the EXPORT
	ldy #EXPORT_SIZE
	lda (@buff),y
	clc
	adc @segaddr
	sta zp::label_value
	iny
	lda (@buff),y
	adc @segaddr+1
	sta zp::label_value+1

	ldxy #@buff	; name of the EXPORT
	jsr lbl::add	; add the EXPORT to the symbol table

	; move fptr to next EXPORT or block
	lda @fptr
	clc
	adc #EXPORT_BLOCK_ITEM_SIZE
	sta @fptr
	bcc :+
	inc @fptr
:	ldy #$00
	lda (@fptr),y
	bne @getexports ; if not end of block, continue
	incw @fptr	; move past terminating 0

; TODO: deal with IMPORT block

; sum the SEGMENT sizes for the header with our existing sizes for those
; segments and set the STOP addresses accordingly
@updatesegments:
	ldxy #@fptr		; SEGMENT name
	jsr get_segment_by_name
	tax
	ldy #8			; offset to SIZE
	lda (@fptr),y
	clc
	adc segments_sizelo,x
	sta segments_sizelo,x
	iny
	lda (@fptr),y
	adc segments_sizehi,x
	sta segments_sizehi,x

	lda @fptr
	adc #10			; sizeof(NAME) + sizeof(SIZE)
	sta @fptr
	bcc :+
	inc @fptr+1
:	ldy #$00
	lda (@fptr),y
	bne @updatesegments	; loop if not end of SEGMENT block

	incw @fptr
	ldxy @fptr
	rts
.endproc

;******************************************************************************
; GET SEGMENT BY NAME
; Returns the ID of the segment from its name
; IN:
;  - .XY: the name of the segment
; OUT:
;  - .A: the ID of the segment
;  - .C: set if no segment exists by the given name
.proc get_segment_by_name
@name=zp::str0
@other=zp::str2
@cnt=zp::tmp0
	stxy @name
	ldxy #segment_names
	stxy @other

	lda #$00
	sta @cnt
@l0:	lda #$08
	jsr str::compare
	beq @found
	lda @other
	clc
	adc #$08
	sta @other
	ldx @cnt
	inx
	stx @cnt
	cpx numsegments
	bcc @l0
@notfound:
	rts

@found: lda @cnt
	rts
.endproc
