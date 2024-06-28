;******************************************************************************
; DEBUGINFO.ASM
; This file contains definitions for procedures to store debug info for
; assembled programs.  This info is the map of file and line-number to address.
;******************************************************************************

.include "errors.inc"
.include "finalex.inc"
.include "macros.inc"
.include "string.inc"
.include "zeropage.inc"

; Debug info constants
MAX_FILES       = 24	; max files that debug info may be generated for
MAX_SEGMENTS    = 8	; max segments that debug info may be generated for
MAX_LINES       = 8000	; max number of lines across all segments

SEG_START_ADDR = 0      ; offset in segment header for start address
SEG_STOP_ADDR  = 2      ; offset in segment header for stop address
SEG_LINE_COUNT = 4	; offset in segment header for line count
DATA_FILE      = 0	; offset of FILE ID in debug info
DATA_LINE      = 1	; offset of line number in debug info
DATA_ADDR      = 3	; offset of line address in debug info

;******************************************************************************
; Debug info pointers
file           = zp::debug       ; current file id being worked on
addr           = zp::debug+1     ; address of next line/addr to store
seg            = zp::debug+3     ; address of current segment pointer
line           = zp::debug+5
srcline        = zp::debug+7
segstart       = zp::debug+9
segstop        = zp::debug+$b
numlines       = zp::debug+$d
debugtmp       = zp::debug+$11

.export __debug_src_line
__debug_src_line = srcline ; the line # stored by dbg::storeline
.export __debug_file
__debug_file = file

;******************************************************************************
; NOTE: this data is stored in its own bank (FINAL_BANK_DEBUG)
; the per-file debug info as described in the above table
.segment "DEBUGINFO"
.export debuginfo
debuginfo: .res $6000

.BSS

;******************************************************************************
; Debug symbol variables
; number of files that we have debug info for. The ID of a file is its index
.export numfiles
numfiles: .byte 0

.export __debug_numbreakpoints
__debug_numbreakpoints:
numbreakpoints: .byte 0 		; number of active break points

; table of 0-terminated filenames
.export __debug_filenames
__debug_filenames:
filenames: .res MAX_FILES * 16

; number of segments that we have debug info for
.export numsegments
numsegments: .byte 0

; 0-terminated names for each segment, can be 0-length for unnamed segments
segmentnames: .res MAX_SEGMENTS * 8

; table of start addresses for each segment
.export segaddresses
segaddresses: .res MAX_SEGMENTS * 2

;******************************************************************************
; # DEBUG INFO
; The structure of this info is somewhat optimized for generating while the
; source is being assembled.  It is generated in 2 passes, which nicely
; matches the way that the source is assembled.
;
; In PASS 1, the user counts the number of lines (any instruction) and segment
; switches (.ORG) and calls `dbg::initsegment` with that info.
; This will allocate the amount of space needed for the file's debug info
;
; In PASS 2, the user simply calls:
;  - `dbg::startsegment` on each segment switch (.ORG)
;  - `dbg::storeline` on each instruction
; After which debug info will be stored for every line of generated code
;
; The assembler will call these functions itself because some directives
; (e.g. INC, REP, and MAC) will generate more than one address per line.
; Each address must be mapped to a line in the debug info in order to
; meaningfully debug these directives.
;
; The format for a file's debug info is stored in the following format:
;	 ---------------------------------------------
;	 | size     | description                    |
;	 |----------|--------------------------------|
;	 |    2     | segment 1 start addr           |
;	 |    2     | segment 1 stop addr            |
;	 |    2     | segment 1 number of lines      |
;	 |   ...    |         ...                    |
;	 |    2     | segment n start addr           |
;	 |    2     | segment n stop addr            |
;	 |    2     | segment n number of lines      |
;	 |##########|################################|
;	 |    1     | segment 1 instruction 1 file id|
;	 |    2     | segment 1 instruction 1 line # |
;	 |    2     | segment 1 instruction 1 addr   |
;	 |   ...    |         ...                    |
;	 |    1     | segment 1 instruction n file id|
;	 |    2     | segment 1 instruction n line # |
;	 |    2     | segment 1 instruction n addr   |
;	 |   ...    |         ...                    |
;	 |    1     | segment n instruction 1 file id|
;	 |    2     | segment n instruction 1 line # |
;	 |    2     | segment n instruction 1 addr   |
;	 |   ...    |         ...                    |
;	 |    1     | segment n instruction m file id|
;	 |    2     | segment n instruction m line # |
;	 |    2     | segment n instruction m addr   |
;	 |-------------------------------------------|
; In words: a file's debug info is organized as the filename, followed by the
; number of segments, followed by a _list_ of those segments (as start and stop
; addresses), and lastly a list of pairs of lines/addresses for each segment
;
; Clearly, this is a costly amount of memory, so it requires a Final Expansion
;
; A new "segment" begins with a .ORG directive.
; Address/lines are stored sequentially within a segment until a .ORG is
; encountered at which point a new segment begins.
;
; NOTE: the term "segment" in the debugger refers to a contiguous block of
; lines. That is, lines that all reside within the segment's start and stop
; addresses.
;
; TODO: store more compactly? e.g. store offsets for line/addr from previous
;******************************************************************************

.CODE

;******************************************************************************
; pointers used when building the debug info
; may be used for other purposes after debug info is generated
nextsegment: .res MAX_FILES ; offset to next free segment start/end addr in file

.segment "DEBUGGER"

;******************************************************************************
; INIT
; Clears any debug state that exists
.export __debug_init
.proc __debug_init
	; init debugger state variables
	lda #$00
	sta numsegments
	sta numbreakpoints
	sta numfiles
	rts
.endproc

;******************************************************************************
; GETLINE
; Returns the current line that we're working on
; OUT:
;  - .XY: the line we're actively working on in the debugger
.export __debug_getline
.proc __debug_getline
	ldxy srcline
	rts
.endproc

;******************************************************************************
; SETLINE
; Sets the current line that we're working on
; IN:
;  - .XY: the line number to set the internal line to
.export __debug_setline
.proc __debug_setline
	stxy srcline
	rts
.endproc

;******************************************************************************
; SETUP
; Uses the line counts (generated by dbg::storeline) to calculate the start
; and stop addresses for each segment and sets them accordingly.
; Resets line counts to zero (it is used as a counter by dbg::storeline).
; out:
;  - .C: set if an error occurred
.export __debug_setup
.proc __debug_setup
@lines=r4
@tmp=r6
@segend=r8
@cnt=ra
	lda numsegments
	bne :+
	rts

:	; start of data is 6*numsegments
	ldx #$00
	stx @segend+1
	asl		; *2
	sta @tmp
	rol @segend+1
	asl		; *4
	rol @segend+1
	adc @tmp	; *6
	sta @segend
	lda @segend+1
	adc #$00
	sta @segend+1

	; get the address of the start of data
	; and init 1st segment to it
	lda @segend
	adc #<debuginfo
	sta @segend
	sta segaddresses
	lda @segend+1
	adc #>debuginfo
	sta @segend+1
	sta segaddresses+1

	; get the address of the start of the segments
	ldxy #debuginfo
	stxy seg

	lda #$00
	sta @cnt
@l0:	ldy #SEG_LINE_COUNT	; get the line count for the segment
	jsr read_from_seg
	sta @lines
	iny
	jsr read_from_seg
	sta @lines+1

	; numlines*5 to get the size of this segment's data
	lda @lines+1
	sta @tmp
	lda @lines
	asl		; *2
	rol @lines+1
	asl		; *4
	rol @lines+1
	adc @lines	; *5
	sta @lines
	lda @lines+1
	adc @tmp
	sta @lines+1

	; calculate the address for the next segment's info
	lda @cnt
	asl
	tax
	lda @lines
	adc @segend
	sta @segend
	sta segaddresses,x
	lda @lines+1
	adc @segend+1
	sta @segend+1
	sta segaddresses+1,x

	; reset line count
	ldy #SEG_LINE_COUNT
	lda #$00
	jsr write_to_seg
	iny
	jsr write_to_seg

	; update seg pointer
	lda seg
	adc #$06
	sta seg
	bcc @next
	inc seg+1

@next:	inc @cnt
	lda @cnt
	cmp numsegments
	bcc @l0

@done:	RETURN_OK
.endproc

;******************************************************************************
; INITSEG
; Initializes a segment (as with .ORG)
; IN:
;  .XY:      the start address of the segment
;  zp::tmp0: the name of the segment
.export __debug_init_segment
.proc __debug_init_segment
@name=r0
@tmp=r2
@addr=r3
	stxy @addr

	; copy the segment name
	lda numsegments
	asl
	asl
	asl
	tax
	ldy #$00
@l0:	lda (@name),y
	sta segmentnames,x
	iny
	inx
	cpy #$08
	bne @l0

	; get the address of the segment
	lda numsegments	; *6 to get offset for this segment
	asl		; *2
	sta @tmp
	asl		; *4
	adc @tmp	; *6
	adc #<debuginfo
	sta seg
	lda #$00
	adc #>debuginfo
	sta seg+1

	; store the start address of the segment
	lda @addr+1
	ldy #SEG_START_ADDR+1
	jsr write_to_seg
	lda @addr
	dey
	jsr write_to_seg

	; end address will be determined by dbg::endseg

	; initialize the line count to 0
	ldy #SEG_LINE_COUNT
	lda #$00
	jsr write_to_seg
	iny
	jsr write_to_seg
	inc numsegments
	RETURN_OK
.endproc

;******************************************************************************
; READ_FROM_SEG
; Reads a byte to (seg)+y in the DEBUGGER bank
; IN:
;  - .Y: the offset of (seg) to read
; OUT:
;  - .A: the byte that was read
; CLOBBERS:
;  - NONE
.proc read_from_seg
	txa
	pha
	tya
	pha

	sty zp::bankval
	ldxy seg
	lda #FINAL_BANK_DEBUG
	jsr fe3::load_off ; read the byte

	sta zp::bankval
	pla
	tay
	pla
	tax
	lda zp::bankval
	rts
.endproc

;******************************************************************************
; WRITE_TO_SEG
; Writes a byte to (seg)+y in the DEBUGGER bank
; IN:
;  .A: the byte to write
;  seg: the address to write to
; CLOBBERS:
;  - NONE
.proc write_to_seg
	sta zp::bankval
	pushregs

	sty zp::bankoffset
	ldxy seg
	lda #FINAL_BANK_DEBUG
	jsr fe3::store_off	; write the byte

	popregs
	rts
.endproc

;******************************************************************************
; WRITE_TO_LINE
; Writes a byte to (line)+y in the DEBUGGER bank
; IN:
;  .A: the byte to write
;  seg: the address to write to
.proc write_to_line
	sta zp::bankval
	pushregs

	sty zp::bankoffset
	ldxy line
	lda #FINAL_BANK_DEBUG
	jsr fe3::store_off	; write the byte
	popregs
	rts
.endproc

;******************************************************************************
; READ_FROM_LINE
; Reads a byte to (line)+y in the DEBUGGER bank
; IN:
;  - .Y: the offset of (line) to read
; OUT:
;  - .A: the byte that was read
.proc read_from_line
	txa
	pha
	tya
	pha

	sty zp::bankval
	ldxy line
	lda #FINAL_BANK_DEBUG
	jsr fe3::load_off ; read the byte

	sta zp::bankval
	pla
	tay
	pla
	tax
	lda zp::bankval
	rts
.endproc

;******************************************************************************
; WRITE_TO_ADDR
; Writes a byte to (addr)+y in the DEBUGGER bank
; IN:
;  .A: the byte to write
;  seg: the address to write to
.proc write_to_addr
	sta zp::bankval
	pushregs

	sty zp::bankoffset
	ldxy addr
	lda #FINAL_BANK_DEBUG
	jsr fe3::store_off	; write the byte

	popregs
	rts
.endproc

;******************************************************************************
; READ_FROM_ADDR
; Reads a byte to (addr)+y in the DEBUGGER bank
; IN:
;  - .Y: the offset of (addr) to read
; OUT:
;  - .A: the byte that was read
.proc read_from_addr
	txa
	pha
	tya
	pha

	sty zp::bankval
	ldxy addr
	lda #FINAL_BANK_DEBUG
	jsr fe3::load_off ; read the byte

	sta zp::bankval
	pla
	tay
	pla
	tax
	lda zp::bankval
	rts
.endproc

;******************************************************************************
; STARTSEGMENT_BYNAME
; Activates the initialized segment (see dbg::initseg) from the given name
; This is for use with the .SEG directive
; IN:
;  - .XY: name of the segment (as 0-terminated string)
; OUT:
;  - .C: set on error
.export __debug_startsegment_byname
.proc __debug_startsegment_byname
@name=zp::str0
@other=zp::str2
@cnt=r0
	stxy @name
	ldxy #segmentnames
	stxy @other
	lda #$00
	sta @cnt
@l0:	jsr str::comparez
	beq @found
	lda @other
	clc
	adc #$08
	sta @other
	bcc :+
	inc @other+1
:	inc @cnt
	lda @cnt
	cmp numsegments
	bne @l0

	sec		; segment not found
	rts

@found:
	; load pointers for this segment and return
	; TODO: verify stop address is within designated memory section)
	lda @cnt
	jsr get_segment_by_id

	; set addr to the current STOP address
	lda segstop
	sta addr
	lda segstop+1
	sta addr+1
	RETURN_OK
.endproc

;******************************************************************************
; STARTSEGMENT_BYADDR
; Activates the initialized segment (see dbg::initseg) from the given address
; Lines stored after the call to this routine will be stored in the debug info
; for this segment.
; This is for use with the .ORG directive
; IN:
;  - .XY: address of the segment
; OUT:
;  - .C: set on error
.export __debug_startsegment_byaddr
.proc __debug_startsegment_byaddr
@addr=r0
@cnt=r2
	stxy @addr
	lda numsegments
	beq @done	; no segments

	lda #$00
	sta @cnt

	; find the segment with this start address
	ldxy #debuginfo
	stxy seg

@l0:	bank_read_byte_rel #FINAL_BANK_DEBUG, seg, #SEG_START_ADDR
	cmp @addr
	bne @next

	bank_read_byte_rel #FINAL_BANK_DEBUG, seg, #SEG_START_ADDR+1
	cmp @addr+1
	beq @found

@next:	lda seg
	clc
	adc #$06
	sta seg
	bcc :+
	inc seg+1
:	inc @cnt
	lda @cnt
	cmp numsegments
	bcc @l0
	RETURN_ERR ERR_UNKNOWN_SEGMENT

@found: lda @cnt
	asl
	tax
	lda segaddresses,x
	sta addr
	lda segaddresses+1,x
	sta addr+1
@done:	RETURN_OK
.endproc

;******************************************************************************
; ENDSEGMENT
; Ends the current segment by writing the end address of it
; IN:
;  .XY: the address to end the segment at
.export __debug_end_segment
.proc __debug_end_segment
	lda numsegments
	bne :+
	rts	; nothing to end

:	; store the end address of the segment
	tya
	ldy #SEG_STOP_ADDR+1
	jsr write_to_seg
	dey
	txa
	jmp write_to_seg
.endproc

;******************************************************************************
; STORE_LINE
; The behavior depends on which pass of assembly we are doing.
; Pass 1:
;  Increments the line count for the active segment
; Pass 2:
;  Stores the given address and line number in the debug info for the current
;  file
;
; in:
;  - .XY: the line number
;  - zp::tmp0: the address corresponding to the given line number
.export __debug_store_line
.proc __debug_store_line
@addr=r0
	lda zp::pass
	cmp #$02
	bne @update_linecnt

; pass 2- store the file, line number and corresponding address
@pass2:
	; store the line number
	tya
	ldy #DATA_LINE+1
	jsr write_to_addr
	txa
	dey
	jsr write_to_addr

	; store the file-id
	ldy #DATA_FILE
	lda file
	jsr write_to_addr

	; store the address
	ldy #DATA_ADDR
	lda @addr
	jsr write_to_addr
	iny
	lda @addr+1
	jsr write_to_addr

	; update pointer for line/addr
	lda addr
	clc
	adc #$05
	sta addr
	bcc @update_linecnt
	inc addr+1

@update_linecnt:
	; update line count for this segment
	ldy #SEG_LINE_COUNT
	jsr read_from_seg
	clc
	adc #$01
	php
	jsr write_to_seg
	plp
	bcc @done
	iny
	jsr read_from_seg
	clc
	adc #$01	; +1 if carry was set
	jsr write_to_seg
@done:	RETURN_OK
.endproc

;******************************************************************************
; ADDR2LINE
; returns the filename and address that correspond
; to the given address
; in:
;  - .XY: the address to get the location of
; out:
;  - .A: the file-id of the address
;  - .XY: the line number of the address
;  - .C: set on error
.export __debug_addr2line
.proc __debug_addr2line
@addr=r2
@cnt=r4
	stxy @addr
	lda #$00
	sta @cnt

@l0:	lda @cnt
	jsr get_segment_by_id
	bcc @checkstop
	RETURN_ERR ERR_LINE_NOT_FOUND

; is the address we're looking for is in the range [segstart, segstop]?
@checkstop:
	lda @addr+1
	cmp segstop+1
	bcc @checkstart
	beq :+
	bcs @next	; addr > segstop, skip to next segment
:	lda @addr
	cmp segstop
	beq @findline	; addr == segstop, find the line #
	bcs @next

@checkstart:
	lda @addr+1
	cmp segstart+1
	bcc @next	; addr < segstart, skip to the next segment
	beq :+
	bcs @findline	; segstart <= addr < segstop, find the line #
:	lda @addr
	cmp segstart
	bcs @findline	; segstart <= addr < segstop, find the line #

@next:	inc @cnt
	lda @cnt
	cmp numsegments
	bcc @l0	 	; repeat until we've checked all segments
	RETURN_ERR ERR_LINE_NOT_FOUND

; find the line that the address we were given is on
@findline:
	ldy #DATA_ADDR
	jsr read_from_line
	cmp @addr
	bne @nextline
	iny
	jsr read_from_line
	cmp @addr+1
	bne @nextline

; get the line corresponding to the address
@found: ldy #DATA_FILE
	jsr read_from_line
	pha			; save file ID
	ldy #DATA_LINE
	jsr read_from_line
	tax
	iny
	jsr read_from_line
	tay
	pla			; get file ID
	RETURN_OK

@nextline:
	jsr nextline
	bcc @findline
	rts
.endproc

;******************************************************************************
; LINE2ADDR
; Returns the address of the given line.
; IN:
;  - .XY: the line to get the address of
; OUT:
;  - .XY: the address of the given line
.export  __debug_line2addr
.proc __debug_line2addr
@line=r2
@cnt=r4
	stxy @line
	lda #$00
	sta @cnt

; we don't store the line/file range for a segment (yet? TODO: ???)
; so just iterate over every line in every segment
@l0:	lda @cnt
	jsr get_segment_by_id
	bcc @checklines
@done:	rts

; check every line in the segment for a match
@checklines:
@l1:	ldy #DATA_LINE
	jsr read_from_line
	cmp @line
	bne @next
	iny
	jsr read_from_line
	cmp @line+1
	bne @next

@found:	ldy #DATA_ADDR
	jsr read_from_line
	tax
	iny
	jsr read_from_line
	tay
	RETURN_OK

@next:	jsr nextline
	bcc @l1
	inc @cnt
	bcs @l0
.endproc

;******************************************************************************
; GET_FILEID
; Returns the ID for the given file name
; IN:
;  - .XY: the filename to return the id of
; OUT:
;  - .A: the file ID
;  - .C: set if there was no match
.proc get_fileid
@other=zp::str0
@filename=zp::str2
@cnt=debugtmp
@len=debugtmp+1
	stxy @filename
	jsr str::len
	sta @len
	bne :+
	sec		; if string is 0-length, return with "not found" flag
	rts

:	lda numfiles
	beq @notfound

	lda #$00
	sta @cnt
	lda #<filenames
	sta @other
	lda #>filenames
	sta @other+1

@l0:	lda @len
	jsr str::compare
	bne @next

@found: ldxy @filename	; restore .XY
	lda @cnt	; get the file ID
	RETURN_OK	; file found

@next:	lda @other
	clc
	adc #$10
	sta @other
	inc @cnt
	lda @cnt
	cmp numfiles
	bne @l0

@notfound:
	ldxy @filename	; restore .XY
	sec		; not found
	rts
.endproc

;******************************************************************************
; GET_FILENAME
; Returns the file name for the file from its ID
; IN:
;  - .A: the file ID to get the filename of
; OUT:
;  - .XY: the filename for the given file ID
;  - .C: set if there is no filename for the given file (XY will STILL
;        point to the address the filename WOULD exist at)
.export __debug_get_filename
.proc __debug_get_filename
	pha
	asl
	asl
	asl
	asl
	adc #<filenames
	tax
	lda #>filenames
	adc #$00
	tay
	pla
	cmp numfiles		; set .C if file ID is >= numfiles
	rts
.endproc

;******************************************************************************
; SETFILE
; Sets the active file-id to the the ID for given filename.
; If no file-id exists for the provided filename, one is first created.
; IN:
;  - .XY: the 0-terminated file to set as the current file
.export __debug_set_file
.proc __debug_set_file
	jsr get_fileid
	bcc :+
	jsr storefile
:	sta file
	rts
.endproc

;******************************************************************************
; STOREFILE
; Copies the given filename thereby creating an ID for that file
; IN:
;  -.XY: address of 0-terminated filename
; OUT:
;  - .A: the file ID for the newly copied file
;  - .C: clear on success, set on error
.proc storefile
@filename=r0
@src=r2
	stxy @src

	; if filename is empty, return an error
	ldy #$00
	lda (@src),y
	bne @getfiledst
	RETURN_ERR ERR_UNNAMED_BUFFER
@getfiledst:
	; find the location to store the filename to
	lda numfiles
	jsr __debug_get_filename ; get the address where the new file will be
	stxy @filename		 ; store as filename dest

	ldxy @src
	jsr str::copy		; copy @src to zp::tmp0 (@filename)

	lda numfiles
	inc numfiles
	RETURN_OK
.endproc

;******************************************************************************
; GETSEGMENT_BY_ID
; Returns the start and stop addresses of a segment by its ID.
; ID's are sequential, so to iterate over all segments, you can call this
; routine with an incrementing counter over the range [0, numsegments)
; IN:
;  .A: the ID of the segment to get
; OUT:
;  seg: the address of the segment data
;  segstart: the start address of the segment
;  segstop: the stop address of the segment
;  line: the address of the line data for the segment
;  numlines: the number of lines in the segment
;  .C: set if the segment doesn't exist, clear on success
.proc get_segment_by_id
@info=r0
	cmp numsegments
	bcs @done	; return error

	pha

	; multiply segment number by 6 (sizeof(segdata))
	sta @info
	asl
	adc @info
	asl
	adc #<debuginfo
	sta seg
	lda #>debuginfo
	adc #$00
	sta seg+1

; get the start address of the segment, stop address, and number of lines
	ldy #SEG_START_ADDR + (2*3) - 1
@copy:	jsr read_from_seg
	sta segstart,y
	dey
	bpl @copy

	; get the address of the segment data
	pla
	asl
	tax
	lda segaddresses,x
	sta line
	lda segaddresses+1,x
	sta line+1

	clc			; OK
@done:	rts
.endproc

;******************************************************************************
; NEXT_LINE
; Updates the line pointer to point to the next line
; OUT:
;  - .C: set if there are no lines left in the active segment (seg)
.proc nextline
	lda line
	clc
	adc #$05
	sta line
	bcc :+
	inc line+1
:	decw numlines
	lda numlines
	bne @ok
	lda numlines+1
	bne @ok
	sec
	rts
@ok:	RETURN_OK
.endproc
