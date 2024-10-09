;******************************************************************************
; DEBUGINFO.ASM
; This file contains definitions for procedures to store debug info for
; assembled programs.  This info is the map of file and line-number to address.
; Refer to docs/debug-info.md for more information about the format of the
; debug information
;******************************************************************************

.include "errors.inc"
.include "finalex.inc"
.include "macros.inc"
.include "ram.inc"
.include "string.inc"
.include "zeropage.inc"

; Debug info constants
MAX_FILES = 24		; max files debug info may be generated for

BLOCK_START_ADDR = 0    ; offset in block header for start address
BLOCK_STOP_ADDR  = 2    ; offset in block header for stop address
BLOCK_LINE_BASE  = 4	; offset in block header for line count
BLOCK_LINE_COUNT = 6	; offset in block header for line count
BLOCK_FILE_ID    = 8	; offset in block header to file id

DATA_FILE = 0	; offset of FILE ID in debug info
DATA_LINE = 1	; offset of line number in debug info
DATA_ADDR = 3	; offset of line address in debug info

;******************************************************************************
; Debug info pointers
; active line data, these represent the HEAD state of the line program
; state machine
file    = zp::debug		; current file id being worked on
addr    = zp::debug+1		; address of next line/addr to store
block   = zp::debug+3		; address of current block pointer
cmd     = zp::debug+5		; address of PC in line program
srcline = zp::debug+7		; address of current source line

; active block header data
blockstart    = zp::debug+9	; base of active block's address range
blockstop     = zp::debug+$b	; top of active block's address range
blocklinebase = zp::debug+$d	; base of active block's line range
numlines      = zp::debug+$f	; number of lines in active block

debugtmp = zp::debug+$11	; scratchpad

.export __debug_file
.export __debug_src_line
__debug_file     = file		; the active file
__debug_src_line = srcline	; the line # stored by dbg::storeline

.block "DEBUGINFO"

;******************************************************************************
; DEBUGINFO
; This large block of memory stores the line programs for each block.
; The location of each block is found by summing the sizes of each block before
; the block in question
.export debuginfo
debuginfo: .res $6000

.segment "DEBUGINFO_CODE"

;******************************************************************************
; Debug symbol variables
.export __debug_numbreakpoints
__debug_numbreakpoints:
numbreakpoints: .byte 0 		; number of active break points

; number of files that we have debug info for
numfiles: .byte 0

; file table (stored as table of 0-terminated filenames)
.export __debug_filenames
__debug_filenames:
filenames: .res MAX_FILES * 16

numblocks: .byte 0	; number of blocks that we have debug info for

freeptr: .word 0	; pointer to next available address in debuginfo

; table of headers for each block
blockheaders: .res MAX_BLOCKS * 9

; table of line program's start addresses for each block.
; these will map to somewhere in the debuginfo buffer
blockaddresseslo: .res MAX_FILES
blockaddresseshi: .res MAX_FILES

;******************************************************************************
; INIT
; Clears any debug info state that exists
.export __debug_init
.proc __debug_init
	; init debugger state variables
	lda #$00
	sta numblocks
	sta numfiles
	rts
.endproc

;******************************************************************************
; SETLINE
; Sets the current line number.  When calling dbgi::storeline this is the line
; that will be mapped to its corresponding address
; IN:
;  - .XY: the line number to set the internal line to
.export __debug_setline
.proc __debug_setline
	stxy srcline
	rts
.endproc

;******************************************************************************
; NEW BLOCK
; Creates a new block (as with .ORG)
; IN:
;   - .XY:  the start address of the block
;   - line: the base line for this block
;   - file: the ID of the file for this block
; OUT:
;   - .C: set if an error occurred
.export __debug_new_block
.proc __debug_new_block
	lda numsegments
	bne :+
	rts

:	stxy addr

	; get offset to new block header (each header is 9 bytes)
	asl			; *2
	asl			; *4
	asl			; *8
	adc numblocks		; *9
	tax

	; store the address that this block's line data will live at
	lda freeptr
	sta blockaddresseslo,x
	lda freeptr+1
	sta blockaddresseshi,x

	; store the base address the block maps to
	lda addr+1
	ldy #BLOCK_START_ADDR+1
	sta (block),y
	lda addr
	dey
	sta (block),y

	; initialize the base line to the value of dbgi::line
	ldy #BLOCK_LINE_BASE
	lda line
	sta (block),y
	iny
	lda line+1
	sta (block),y
	inc numblocks

	; initialize the line count to 0
	ldy #BLOCK_LINE_COUNT
	lda #$00
	sta (block),y
	iny
	sta (block),y
	inc numblocks

	; store the file ID 
	ldy #BLOCK_FILE_ID
	lda file
	sta (block),y

	RETURN_OK
.endproc

;******************************************************************************
; END BLOCK
; Updates values that may have changed with calls to storeline
; This includes the end address and number of lines in the block
; This is called when we are done working on a given block.
.export __debug_end_block
.proc __debug_end_block
	; write updated address end
	ldy #BLOCK_STOP_ADDR
	lda addr
	sta (block),y
	iny
	lda addr
	sta (block),y

	; write updated number of lines
	ldy #BLOCK_LINE_COUNT
	lda numlines
	sta (block),y
	iny
	lda numlines+1
	sta (block),y

	rts
.endproc

;******************************************************************************
; START BLOCK BY ADDR
; Activates the initialized block (see dbg::initblock) from the given address
; Lines stored after the call to this routine will be stored in the debug info
; for this block.
; This is for use with the .ORG directive
; IN:
;  - .XY: address of the block
; OUT:
;  - .C: set on error
.export __debug_startblock_byaddr
.proc __debug_startblock_byaddr
@addr=r0
@cnt=r2
	stxy @addr
	lda numblocks
	beq @done	; no blocks

	lda #$00
	sta @cnt

	; find the block with this start address
	ldxy #debuginfo
	stxy block

@l0:	lda24 #FINAL_BANK_DEBUG, block, #BLOCK_START_ADDR
	cmp @addr
	bne @next

	lda24 #FINAL_BANK_DEBUG, block, #BLOCK_START_ADDR+1
	cmp @addr+1
	beq @found

@next:	lda block
	clc
	adc #$06
	sta block
	bcc :+
	inc block+1
:	inc @cnt
	lda @cnt
	cmp numblocks
	bcc @l0
	RETURN_ERR ERR_UNKNOWN_SEGMENT

@found: ldx @cnt
	lda blockaddresseslo,x
	sta addr
	lda blockaddresseshi,x
	sta addr+1

@done:	RETURN_OK
.endproc

;******************************************************************************
; END BLOCK
; Ends the current block by writing the end address of it
; IN:
;  .XY: address to end the block at
.export __debug_end_block
.proc __debug_end_block
	lda numblocks
	beq @done	; no blocks exist, nothing to "end"

:	; store the end address of the block
	tya
	ldy #BLOCK_STOP_ADDR+1
	sta (block),y		; write MSB
	dey
	txa
	sta (block),y		; write LSB
@done:	rts
.endproc

;******************************************************************************
; STORE LINE
; Write an instruction to the active line program to map the given line to the
; given address within the active block.
; IN:
;  - .XY: line number
;  - r0:  address corresponding to the given line number
.export __debug_store_line
.proc __debug_store_line
@addr=r0
@line=r2
@dline=r4
	stxy @line

	lda #$00
	tay
	sta @itype	; default to BASIC instruction

	; get the line and address delta for our new line
	lda line
	sec
	sbc @line
	sta @dline
	lda line+1
	sbc @line+1
	sta @dline+1

	lda addr
	sec
	sbc @addr
	sta @daddr
	lda addr+1
	sbc @addr+1
	sta @daddr

	; check if address is encodable in basic instruction
	; in order to use a basic instruction, both must be in range [$01, $0f]
	ora @dline+1
	bne @extended	; if either delta's MSB is !0, need extended
	lda @dline
	ora @daddr
	cmp #$10	; if either's LSB is >= $10, need extended
	bcs @extended

@basic:
	; encode the instruction ((daddr << 4) | dline)
	lda @daddr
	asl
	asl
	asl
	asl
	ora @dline
	sta (liney),y		; write the encoded instruction
	jmp @done

@extended:
@advanceline:
	; get 16 bit signed offset for target line
	ldy #$02
	sta (line),y	; write LSB
	lda #$00
	sbc @line+1
	iny
	sta (line),y	; write MSB
	ldy #$00
	lda #OP_ADVANCE_LINE
	sta (line),y

@advanceaddr:
	; get 16 bit signed offset for target address
	ldy #$02
	lda @daddr
	sta (line),y	; write LSB
	lda #$00
	sbc @line+1
	iny
	sta (line),y	; write MSB
	ldy #$00
	lda #OP_ADVANCE_LINE
	sta (line),y

; pass 2- store the file, line number and corresponding address
@pass2:
	; store the line number
	tya
	ldy #DATA_LINE+1
	sta (addr),y
	txa
	dey
	sta (addr),y

	; store the file-id
	ldy #DATA_FILE
	lda file
	sta (addr),y

	; store the address
	ldy #DATA_ADDR
	lda @addr
	sta (addr),y
	iny
	lda @addr+1
	sta (addr),y

	; update pointer for line/addr
	lda addr
	clc
	adc #$05
	sta addr
	bcc @update_linecnt
	inc addr+1

@update_linecnt:
	; update line count for this block
	ldy #BLOCK_LINE_COUNT
	lda (block),y
	clc
	adc #$01
	php
	sta (block),y
	plp
	bcc @done
	iny
	lda (block),y
	clc
	adc #$01	; +1 if carry was set
	sta (block),y

@done:	incw line	; move PC for line program
	RETURN_OK
.endproc

;******************************************************************************
; ADDR2LINE
; returns the filename and address that correspond
; to the given address
; in:
;  - .XY: the address to get the location of
; out:
;  - .A:  file-id of the address
;  - .XY: line number of the address
;  - .C:  set on error
.export __debug_addr2line
.proc __debug_addr2line
@addr=r2
@cnt=r4
	stxy @addr
	lda #$00
	sta @cnt

@l0:	lda @cnt
	jsr get_block_by_id
	bcc @checkstop
	RETURN_ERR ERR_LINE_NOT_FOUND

; is the address we're looking for in the range [blockstart, blockstop]?
@checkstop:
	lda @addr+1
	cmp blockstop+1
	bcc @checkstart
	beq :+
	bcs @next	; addr > blockstop, skip to next block
:	lda @addr
	cmp blockstop
	beq @findline	; addr == blockstop, find the line #
	bcs @next

@checkstart:
	lda @addr+1
	cmp blockstart+1
	bcc @next	; addr < blockstart, skip to the next block
	beq :+
	bcs @findline	; blockstart <= addr < blockstop, find the line #
:	lda @addr
	cmp blockstart
	bcs @findline	; blockstart <= addr < blockstop, find the line #

@next:	inc @cnt
	lda @cnt
	cmp numblocks
	bcc @l0	 	; repeat until we've checked all blocks
	RETURN_ERR ERR_LINE_NOT_FOUND

; run the line program until we find the line that is mapped to the given addr
@findline:
	lda addr
	cmp @addr
	bne @nextline
	lda addr+1
	cmp @addr+1
	bne @nextline

@found:	ldxy line
	lda file
	RETURN_OK

@nextline:
	jsr advance
	bcc @findline
@done:	rts
.endproc

;******************************************************************************
; LINE2ADDR
; Returns the address of the given line.
; IN:
;  - .XY: the line to get the address of
;  - .A:  the file ID of the file containing the line
; OUT:
;  - .XY: the address of the given line
;  - .C:  set if no address is mapped to the current line
.export  __debug_line2addr
.proc __debug_line2addr
@line=r2
@cnt=r4
@file=r5
@linestop=debugtmp
	sta @file
	stxy @line
	lda #$00
	sta @cnt

; find the first block that contains the file and line we are looking for
@l0:	lda @cnt
	jsr get_block_by_id
	bcc @checkstop
	RETURN_ERR ERR_LINE_NOT_FOUND

; does the file ID match the file we're looking for?
	lda file
	cmp @file
	bne @next	; if the file doesn't match, try the next block

	; calculate top line in block (linebase + numlines)
	lda blocklinebase
	clc
	adc numlines
	sta @linestop
	lda blocklinebase+1
	adc #$00
	sta @linestop+1

; is the line we're looking for in the range [blockstart, blockstop]?
@checkstop:
	lda @line+1
	cmp @linestop+1
	bcc @checkstart
	beq :+
	bcs @next	; line > (linebase+numlines), skip to next block
:	lda @line
	cmp @linestop
	beq @findline	; line == (linebase+numlines), find the line #
	bcs @next

@checkstart:
	lda @addr+1
	cmp blocklinebase+1
	bcc @next	; line < linebase, skip to the next block
	beq :+
	bcs @findline	; linebase <= line < (linebase+numlines), find line #
:	lda @addr
	cmp blocklinebase
	bcs @findline	; linebase <= line < (linebase+numlines), find line #

@next:	inc @cnt
	lda @cnt
	cmp numblocks
	bcc @l0	 	; repeat until we've checked all blocks
	RETURN_ERR ERR_LINE_NOT_FOUND

; run the line program for the block until we find a match for our line
@findaddr:
	lda line
	cmp @line
	bne @nextline
	lda line+1
	cmp @line+1
	bne @nextline

@found:	ldxy addr
	RETURN_OK

@nextline:
	jsr advance
	bcc @findaddr
@done:	rts
.endproc

;******************************************************************************
; GET FILEID
; Returns the ID for the given file name by looking it up in the file table
; IN:
;  - .XY: the filename to return the id of
; OUT:
;  - .A: the file ID
;  - .C: set if there was no match
.export __debuginfo_get_fileid
__debuginfo_get_fileid:
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
; GET FILENAME
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
; SET FILE
; Sets the active file-id to the the ID for given filename.
; If no file-id exists for the provided filename, one is first created.
; IN:
;  - .XY: the 0-terminated file to set as the current file
.export __debug_set_file
.proc __debug_set_file
	jsr get_fileid	; get the file ID (if the file is already stored)
	bcc :+		; if an ID was found, no need to copy filename
	jsr storefile	; copy the filename and get its new ID
:	sta file	; store the ID as the active file ID
	rts
.endproc

;******************************************************************************
; SET NAME
; Renames the entry for the given ID in the file table to the given name
; IN:
;   - .A:  the debug file ID of the handle to (re)name
;   - .XY: the filename to set for the handle
;   - .C:  set if there is no file for the given ID
.export __debug_set_name
.proc __debug_set_name
@filename=r2
@dst=r0
	stxy @filename
	jsr __debug_get_filename	; get the destination address for ID
	bcc @ok
	rts				; return err

@ok:	stxy @dst			; r0 = dest address
	ldxy @filename			; restore filename
	jmp str::copy			; copy @filename to r0 (@filename)
.endproc

;******************************************************************************
; STORE FILE
; Copies the given filename to the file table thereby creating an ID for that
; file.
; IN:
;   -.XY: address of 0-terminated filename
; OUT:
;   - .A: the file ID for the newly copied file
;   - .C: clear on success, set on error
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
	ldxy @src
	jsr __debug_set_name	; copy @filename to the file's ID

	lda numfiles
	inc numfiles
	RETURN_OK
.endproc

;******************************************************************************
; GET BLOCK BY ID
; Returns the start and stop addresses of a block by its ID.
; ID's are sequential, so to iterate over all blocks, you can call this
; routine with an incrementing counter over the range [0, numblocks)
; IN:
;  .A: the ID of the block to get
; OUT:
;  block:      the address of the block data
;  blockstart: the start address of the block
;  blockstop:  the stop address of the block
;  line:       the address of the line data for the block
;  numlines:   the number of lines in the block
;  .C:         set if the block doesn't exist, clear on success
.proc get_block_by_id
@info=r0
	cmp numblocks
	bcs @done	; return error

	pha		; save ID

	; multiply block number by 9 (sizeof(blockdata))
	sta @info
	asl		; *2
	asl		; *4
	asl		; *8
	adc @info	; *9
	adc #<debuginfo
	sta block
	lda #>debuginfo
	adc #$00
	sta block+1

; copy the header state for the block:
; start addr, stop addr, base line, # lines, file id
	ldy #BLOCK_START_ADDR+9-1
	lda (block),y
	sta file	; file is non-contiguous
	dey
@copy:	lda (block),y
	sta blockstart,y
	dey
	bpl @copy

	; get the address of the block data
	pla		; restore block ID
	tax
	lda blockaddresseslo,x
	sta line
	lda blockaddresseshi,x
	sta line+1

	clc	; OK
@done:	rts
.endproc

;******************************************************************************
; ADVANCE
; Runs one command in the active line program
; Updates the line pointer to point to the next line in the active file.
; OUT:
;  - line: set to the location of the next line in the code
;  - addr: set to the line that's mapped to line
;  - .C:   set if there are no lines left in the active block (block)
.proc nextline
	ldy #$00
	lda (line),y
	bne @extended_i	; if opcode is !0, this is an extended instruction
@basic_i:
	; decode the opcode; top 4 bits contain PC offset and bottom 4 line
	pha
	and #$0f	; get line offest and add it to the current line
	clc
	adc line
	sta line
	bcc :+
	inc line+1

:	pla
	lsr		; get PC offset and add it to the current address
	lsr
	lsr
	lsr
	clc
	adc addr
	sta addr
	bcc @ok
	inc @addr+1
	bcs @ok

@extended_i:
	incw line	; move line program PC to byte 2 of opcode
	lda (line),y	; get byte 2 of opcode
	beq @end	; if byte 2 is 0, we're at the end of the program

	incw line	; move to operand

	cmp #OP_SET_ADDR
	bne :+
@setaddr:
	lda (line),y
	sta addr
	incw line
	lda (line),y
	sta addr+1
	jmp @ok
	
:	cmp #OP_SET_FILE
	bne :+
@setfile:
	lda (line),y
	sta file
	jmp @ok

:	cmp #OP_ADVANCE_LINE
	bne :+
@adv_line:
	lda line
	clc
	adc (line),y
	sta line
	incw line
	lda line+1
	adc (line),y
	sta line+1
	jmp @ok

:	cmp #OP_ADVANCE_PC
	bne :+
@adv_pc:
	lda addr
	clc
	adc (line),y
	sta addr
	incw line
	lda line+1
	adc (line),y
	sta addr+1
	jmp @ok

:	cmp #OP_SET_PC
	bne @end	; invalid opcode
@set_pc:
	lda (line),y
	sta addr
	incw line
	lda (line),y
	sta addr+1
	skw		; fall through to @ok
@end:	sec
	rts
@ok:	incw line
	RETURN_OK
.endproc
