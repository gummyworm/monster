;*******************************************************************************
; DEBUGINFO.ASM
; This file contains definitions for procedures to store debug info for
; assembled programs.  This info is the map of file and line-number to address.
; Refer to docs/debug-info.md for more information about the format of the
; debug information
;*******************************************************************************
.include "../errors.inc"
.include "../macros.inc"
.include "../string.inc"
.include "../ram.inc"
.include "../zeropage.inc"

; Debug info constants
MAX_FILES  = 64		; max files debug info may be generated for
MAX_BLOCKS = 256

BLOCK_START_ADDR     = 0    	; offset in block header for base address
BLOCK_STOP_ADDR      = 2    	; offset in block header for stop address
BLOCK_LINE_BASE      = 4	; offset in block header for base line
BLOCK_LINE_COUNT     = 6	; offset in block header for line count
BLOCK_FILE_ID        = 8	; offset in block header to file id
BLOCK_LINE_PROG      = 9	; offset to address of the line program data
BLOCK_PROG_STOP_ADDR = 11	; offset to address of end of line program

DATA_FILE = 0	; offset of FILE ID in debug info
DATA_LINE = 1	; offset of line number in debug info
DATA_ADDR = 3	; offset of line address in debug info

; Opcodes for extended instructions
OP_SET_ADDR     = $01
; FREE          = $02
OP_SET_LINE     = $03
OP_ADVANCE_LINE = $04
OP_ADVANCE_ADDR = $05
OP_SET_PC       = $06

SIZEOF_BLOCK_HEADER = 13

;*******************************************************************************
.macro BANKJUMP proc_id
	pha
	lda #proc_id
	bpl do_proc
.endmacro

.enum dbgi_proc_ids
ADDR2LINE
LINE2ADDR
END_BLOCK
INIT
INITONCE
GET_FILENAME
NEW_BLOCK
SET_FILE
SET_NAME
SET_ADDR
STORE_LINE
PUSH_BLOCK
POP_BLOCK
GET_FILEID
INCREMENT_AT
.endenum

.RODATA
.linecont +
.define dbgi_procs addr2line, line2addr, end_block, init, initonce, get_filename, new_block, \
	set_file, set_name, set_addr, store_line, push_block, pop_block, get_fileid
.linecont -
dbgi_procs_lo: .lobytes dbgi_procs
dbgi_procs_hi: .hibytes dbgi_procs

;*******************************************************************************
; Debug info pointers
; active line data, these represent the HEAD state of the line program
; state machine
addr    = zp::debug	; address of next line/addr to store
srcline = zp::debug+2	; address of current source line
line    = zp::debug+4	; address of current line program instruction
block   = zp::debug+6	; address of current block pointer

; the offsets of these state variables correspond to the offsets
; of the BLOCK_ constants. e.g. BLOCK_START_ADDR -> blockstart
blockstate    = block+2		; base of block state
blockstart    = block+2		; base of active block's address range
blockstop     = block+4		; top of active block's address range
blocklinebase = block+6 	; base of active block's line range
linestop      = block+8		; number of lines in active block
file          = block+10 	; current file id being worked on
progstart     = block+11	; address of start of line program for block
progstop      = block+13	; address of end of line program

; scratchpad
debugtmp = block+15

.export __debug_file
.export __debug_src_line
__debug_file     = file		; the active file
__debug_src_line = srcline	; the line # stored by dbg::storeline

.segment "DEBUGINFO"

;*******************************************************************************
; DEBUGINFO
; This large block of memory stores the line programs for each block.
; The location of each block is found by summing the sizes of each block before
; the block in question
.export debuginfo

.ifdef vic20
debuginfo: .res $6000
.else
debuginfo:
.endif

.segment "DEBUGINFO_CODE"

.export __debug_addr2line
.export __debug_end_block
.export __debug_line2addr
.export __debuginfo_init
.export __debuginfo_initonce
.export __debug_get_filename
.export __debug_new_block
.export __debug_set_file
.export __debug_set_name
.export __debug_store_line
.export __debug_set_addr
.export __debug_push_block
.export __debug_pop_block

.export __debuginfo_get_fileid

;******************************************************************************
.if FINAL_BANK_MAIN=FINAL_BANK_DEBUG
	__debug_addr2line         = addr2line
	__debug_end_block         = end_block
	__debug_line2addr         = line2addr
	__debuginfo_init          = init
	__debuginfo_initonce      = initonce
	__debug_get_filename      = get_filename
	__debug_new_block         = new_block
	__debug_set_file          = set_file
	__debug_set_name          = set_name
	__debug_set_addr          = set_addr
	__debug_store_line        = store_line
	__debug_push_block        = push_block
	__debug_pop_block         = pop_block
	__debuginfo_get_fileid    = get_fileid
.else
.PUSHSEG
.RODATA

__debug_addr2line:        BANKJUMP dbgi_proc_ids::ADDR2LINE
__debug_line2addr:        BANKJUMP dbgi_proc_ids::LINE2ADDR
__debug_end_block:        BANKJUMP dbgi_proc_ids::END_BLOCK
__debuginfo_init:         BANKJUMP dbgi_proc_ids::INIT
__debuginfo_initonce:     BANKJUMP dbgi_proc_ids::INITONCE
__debug_get_filename:     BANKJUMP dbgi_proc_ids::GET_FILENAME
__debug_new_block:        BANKJUMP dbgi_proc_ids::NEW_BLOCK
__debug_set_file:         BANKJUMP dbgi_proc_ids::SET_FILE
__debug_set_name:         BANKJUMP dbgi_proc_ids::SET_NAME
__debug_set_addr:         BANKJUMP dbgi_proc_ids::SET_ADDR
__debug_store_line:       BANKJUMP dbgi_proc_ids::STORE_LINE
__debug_push_block:       BANKJUMP dbgi_proc_ids::PUSH_BLOCK
__debug_pop_block:        BANKJUMP dbgi_proc_ids::POP_BLOCK
__debuginfo_get_fileid:   BANKJUMP dbgi_proc_ids::GET_FILEID

;*******************************************************************************
; Entrypoint for routines
.proc do_proc
	stx @savex
	tax
	lda dbgi_procs_lo,x
	sta zp::bankjmpvec
	lda dbgi_procs_hi,x
	sta zp::bankjmpvec+1
	lda #FINAL_BANK_DEBUG
	sta zp::banktmp
@savex=*+1
	ldx #$00
	pla
	jmp __ram_call
.endproc

.POPSEG
.endif

.segment "DEBUGINFO_VARS"

;*******************************************************************************
; number of files that we have debug info for
numfiles: .byte 0

numblocks: .byte 0	; number of blocks that we have debug info for

block_open: .byte 0	; if !0, we are creating a block, when this is set
			; creating a new block will first close the open one

freeptr: .word 0	; pointer to next available address in debuginfo

blocksp:    .byte 0	; stack pointer for block stack

;*******************************************************************************
; BSS
.segment "DEBUGINFO_BSS"

; file table (stored as table of 0-terminated filenames)
.export __debug_filenames
__debug_filenames:
filenames: .res MAX_FILES * 16

blockstack: .res $100	; stack for line program state machine

; table of headers for each block
.export blockheaders
blockheaders: .res MAX_BLOCKS * SIZEOF_BLOCK_HEADER

; table of line program's start addresses for each block.
; these will map to somewhere in the debuginfo buffer
blockaddresseslo: .res MAX_FILES
blockaddresseshi: .res MAX_FILES

.segment "DEBUGINFO_CODE"

;*******************************************************************************
; INITONCE
; Clears state that should only be cleared on boot (file table)
.proc initonce
	lda #$00
	sta numfiles

	; fall through to init
.endproc

;*******************************************************************************
; INIT
; Clears the debug info state that is valid for a single assembled program
; This is the line table and line program information, which is regenerated
; from scratch each time a program is assembled
.proc init
	; init the address for the next free line program location
	lda #<debuginfo
	sta freeptr
	lda #>debuginfo
	sta freeptr+1

	; init debugger state variables
	lda #$00
	sta blocksp
	sta numblocks
	sta block_open
	rts
.endproc

;*******************************************************************************
; PUSH BLOCK
; Pushes the current state machine values for the line program
; This allows a new block to be created and then the current block restored
; OUT:
;   - .C: set on stack overflow
.proc push_block
	ldx #$00
	ldy blocksp
@l0:	lda zp::debug,x
	sta blockstack,y
	iny
	beq @overflow
	inx
	cpx #$06
	bne @l0
	lda file
	sta blockstack,y
	iny
	sty blocksp	; save new stack pointer
	RETURN_OK	; success

@overflow:
	RETURN_ERR ERR_STACK_OVERFLOW
.endproc

;*******************************************************************************
; POP BLOCK
; Pops the last pushed values (push_block) for the line program state machine
; OUT:
;   - .C: set on stack underflow
.proc pop_block
	ldx #$06
	ldy blocksp
	lda blockstack-1,y	; file
	sta file
	dey
@l0:	lda blockstack-1,x
	sta zp::debug-1,y
	dey
	cpy #$ff
	beq @underflow
	dex
	bne @l0
	sty blocksp	; save new stack pointer
	RETURN_OK	; success

@underflow:
	RETURN_ERR ERR_STACK_UNDERFLOW
.endproc

;*******************************************************************************
; NEW BLOCK
; Creates a new block of debug information
; Blocks are created any time the file is changed or the address is changed
; (as with a .ORG directive)
; e.g.
;   .org $1000  ; block 0
;   asl		; block 1
;   .inc "a"	; block 2 (everything in file "a")
;   asl		; block 3
;   .org $1200	; block 4
; IN:
;   - .XY:     the start address of the block
;   - srcline: the base line for the new block
;   - file:    the ID of the file for this block
; OUT:
;   - addr: the base address for the new block
;   - line: the address of the line program for the new block
;   - .C: set if an error occurred
.proc new_block
@tmp=r0
	stxy addr		; init addr pointer

	lda block_open		; is there a block already open?
	beq @cont		; continue to create new block if not
	jsr end_block		; end the open block if there is

@cont:	; get offset to block header ((BLOCK_HEADER_SIZE * numblocks)
	lda numblocks
	asl			; *2
	asl			; *4
	sta @tmp
	asl			; *8
	adc @tmp		; *12
	adc numblocks		; *13
	adc #<blockheaders
	sta block
	lda #>blockheaders
	adc #$00
	sta block+1

	lda freeptr
	sta progstart
	sta line
	adc #$02
	sta progstop

	lda freeptr+1
	sta progstart+1
	sta line+1
	adc #$00
	sta progstop+1

	lda addr
	sta blockstart
	lda addr+1
	sta blockstart+1

	; store the address that this block's line program will live at
	ldy #BLOCK_LINE_PROG
	lda line
	sta (block),y
	iny
	lda line+1
	sta (block),y

	; store the base address the block maps to
	ldy #BLOCK_START_ADDR
	lda addr
	sta (block),y
	iny
	lda addr+1
	sta (block),y

	; initialize the base line to the value of srcline
	ldy #BLOCK_LINE_BASE
	lda srcline
	sta (block),y
	sta blocklinebase
	sta linestop
	iny
	lda srcline+1
	sta (block),y
	sta blocklinebase+1
	sta linestop+1

	; store the file ID
	ldy #BLOCK_FILE_ID
	lda file
	sta (block),y

	; initialize the line count to 0
	ldy #BLOCK_LINE_COUNT
	lda #$00
	sta (block),y
	iny
	sta (block),y

	; initialize program to empty (2 $00 bytes)
	tay
	sta (line),y
	iny
	sta (line),y

	inc numblocks
	inc block_open	; flag that we are creating a new block
	RETURN_OK
.endproc

;*******************************************************************************
; END BLOCK
; Updates values that may have changed with calls to storeline
; This includes the end address and number of lines in the block
; This is called when we are done working on a given block.
; IN:
;   - .XY: the address to end the block at
.proc end_block
@numlines=debugtmp
	lda numblocks
	beq @done	; no blocks exist, nothing to "end"

	lda block_open	; is there an open block?
	beq @done	; if not, nothing to "end"

	stxy addr

	; write updated address end
	ldy #BLOCK_STOP_ADDR
	lda addr
	sta (block),y
	iny
	lda addr+1
	sta (block),y

	; compute number of lines and write updated value
	; numlines = (linestop - linebase) + 1
	lda linestop
	sec
	sbc blocklinebase
	sta @numlines
	lda linestop+1
	sbc blocklinebase+1
	sta @numlines+1
	incw @numlines

	ldy #BLOCK_LINE_COUNT
	lda @numlines
	sta (block),y
	iny
	lda @numlines+1
	sta (block),y

	; store new end of line program
	ldy #BLOCK_PROG_STOP_ADDR
	lda progstop
	sta (block),y	; right the LSB of stop address to the block header
	iny
	lda progstop+1
	sta (block),y	; right the MSB of stop address to the block header

	; check if progstop > freeptr and set freeptr to progstop if it is
	cmp freeptr+1
	bcc @close	; progstop is < freeptr, this isn't the new free ptr
	bne @set_freetop

	; check the LSB
	lda progstop
	cmp freeptr
	bcc @close	; progstop < freeptr, don't set freeptr

@set_freetop:
	; freeptr is > progstop, set freeptr to progstop
	lda progstop+1
	sta freeptr+1
	lda progstop
	sta freeptr

@close: dec block_open	; flag that there is no open block

@done:	rts
.endproc

;*******************************************************************************
; SET ADDR
; Writes an instruction to set the address to the given value
; This is for use with the .ORG directive
; IN:
;  - .XY: address to force PC to
; OUT:
;  - line: the address to store subsequent line program data to
;  - .C:   set on error
.proc set_addr
@addr=r0
	sty @addr
	lda #$00
	sta (line),y

	lda #OP_SET_ADDR
	iny
	sta (line),y

	iny
	txa
	sta (line),y

	lda @addr
	sta (line),y

	lda line
	clc
	adc #$04
	sta line
	bcc :+
	inc line+1
:	RETURN_OK
.endproc

;*******************************************************************************
; STORE LINE
; Write an instruction to the active line program to map the given line to the
; given address within the active block.
; IN:
;  - .XY: line number
;  - r0:  address corresponding to the given line number
.proc store_line
@addr=r0
@line=r2
@dline=r4
@daddr=r6
@isize=r8
	stxy @line

	; get the line and address delta for our new line (dline = new - prev)
	lda @line
	sec
	sbc srcline
	sta @dline
	lda @line+1
	sbc srcline+1
	sta @dline+1

	; set new srcline value to the line we're moving to
	stxy srcline

	ldy #$00
	sty @isize

	; get the relative address from the block's base address
	lda @addr
	sec
	sbc addr
	sta @daddr
	lda @addr+1
	sbc addr+1
	sta @daddr+1

	; check if address is encodable in basic instruction
	; in order to use a basic instruction, both must be in range [$01, $0f]
	ora @dline+1
	bne @extended	; if either delta's MSB is !0, need extended

	lda @dline
	ora @daddr
	bne :+
	rts		; no line/addr change, no instruction needed
:	and #$f0	; if either's LSB is >= $10, need extended
	bne @extended

@basic:	; encode the instruction ((daddr << 4) | dline)
	lda @daddr
	asl
	asl
	asl
	asl
	ora @dline
	sta (line),y		; write the encoded instruction
	lda #$01		; advance 1 byte
	sta @isize
	bne @done

@extended:
	ldy #$00
@advanceline:
	lda @dline
	ora @dline+1
	beq @advanceaddr	; skip if line delta is 0

	lda #$00		; extended instruction opcode byte 0
	sta (line),y

	; get 16 bit signed offset for target line
	iny
	lda #OP_ADVANCE_LINE	; extended instruction opcode byte 1
	sta (line),y

	iny
	lda @dline
	sta (line),y		; write LSB of line delta
	iny
	lda @dline+1
	sta (line),y		; write MSB of line delta

	iny

	lda #$04		; advance 4 bytes
	sta @isize

@advanceaddr:
	lda @daddr
	ora @daddr+1
	beq @done	; no need for instruction if addr delta is 0 - skip

	lda #$00		; extended instruction opcode byte 0
	sta (line),y

	iny
	lda #OP_ADVANCE_ADDR	; extended instruction opcode byte 1
	sta (line),y

	; get 16 bit signed offset for target address
	iny
	lda @daddr
	sta (line),y	; write LSB
	iny
	lda @daddr+1
	sta (line),y	; write MSB

	lda #$04	; advance 4 bytes
	clc
	adc @isize
	sta @isize

@done:	; add instruction size to program pointer and program-stop pointer
	lda @isize
	clc
	adc progstop
	sta progstop	; progstop += size of instruction
	bcc :+
	inc progstop+1

	; update line pointer by instruction size
:	lda @isize
	clc
	adc line
	sta line
	bcc :+
	inc line+1

:	; write new program terminator ($00 $00)
	lda #$00
	tay
	sta (line),y
	iny
	sta (line),y

	; if this line is > line top, set as new top
	ldxy srcline
	cmpw linestop
	bcc :+
	stxy linestop

:	; udpate addr
	lda addr
	clc
	adc @daddr
	sta addr
	lda addr+1
	adc @daddr+1
	sta addr+1
	rts
.endproc

;*******************************************************************************
; ADDR2LINE
; returns the filename and address that correspond
; to the given address
; IN:
;  - .XY: the address to get the location of
; OUT:
;  - .A:  file-id of the address
;  - .XY: line number of the address
;  - .C:  set on error
.proc addr2line
@addr=r2
@cnt=r4
	stxy @addr
	lda #$00
	sta @cnt

@l0:	; load the next block to search for the address in
	lda @cnt
	jsr get_block_by_id
	bcc @checkstop
	RETURN_ERR ERR_LINE_NOT_FOUND	; no next block, address isn't mapped

; is the address we're looking for in the range [blockstart, blockstop)?
@checkstop:
	lda @addr+1
	cmp blockstop+1
	bcc @checkstart
	beq :+
	bcs @next	; addr > blockstop, skip to next block
:	lda @addr
	cmp blockstop
	bcs @next	; if addr >= blockstop, skip to next block

@checkstart:
	lda @addr+1
	cmp blockstart+1
	bcc @next	; addr < blockstart, skip to the next block
	beq :+
	bcs @findline 	; blockstart <= addr < blockstop, find the line #
:	lda @addr
	cmp blockstart
	bcs @findline   ; blockstart <= addr < blockstop, find the line #

@next:	inc @cnt
	bne @l0
	RETURN_ERR ERR_LINE_NOT_FOUND

; run the line program until we find the line that is mapped to the given addr
@findline:
	lda addr
	cmp @addr
	bne @nextline
	lda addr+1
	cmp @addr+1
	bne @nextline

@found:	ldxy srcline	; return the line that the program ended on
	lda file	; and file
	RETURN_OK

@nextline:
	jsr advance
	bcc @findline
	lda #ERR_LINE_NOT_FOUND
	;sec
@done:	rts
.endproc

;*******************************************************************************
; LINE2ADDR
; Returns the address of the given line.
; IN:
;  - .XY: the line to get the address of
;  - .A:  the file ID of the file containing the line
; OUT:
;  - .XY: the address of the given line
;  - .C:  set if no address is mapped to the current line
.proc line2addr
@line=r2
@cnt=r4
@file=r5
@closest=r7		; nearest line < the one we're looking for
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

; is the line we're looking for in the range [blockstart, blockstop]?
@checkstop:
	lda @line+1
	cmp linestop+1
	bcc @checkstart
	beq :+
	bcs @next	; line > (linebase+numlines), skip to next block
:	lda @line
	cmp linestop
	beq @findaddr	; line == (linebase+numlines), find the line #
	bcs @next

@checkstart:
	lda addr+1
	cmp blocklinebase+1
	bcc @next	; line < linebase, skip to the next block
	beq :+
	bcs @findaddr	; linebase <= line < (linebase+numlines), find address
:	lda addr
	cmp blocklinebase
	bcs @findaddr	; linebase <= line < (linebase+numlines), find address

@next:	inc @cnt
	lda @cnt
	cmp numblocks
	bcc @l0	 	; repeat until we've checked all blocks
	RETURN_ERR ERR_LINE_NOT_FOUND

; run the line program for the block until we find a match for our line
@findaddr:
	lda srcline
	cmp @line
	bne @nextline

	lda srcline+1
	cmp @line+1
	bne @nextline

@found:	ldxy addr
	RETURN_OK

@nextline:
	jsr advance
	bcc @findaddr
@done:	rts
.endproc

;*******************************************************************************
; GET FILEID
; Returns the ID for the given file name by looking it up in the file table
; IN:
;  - .XY: the filename to return the id of
; OUT:
;  - .A: the file ID
;  - .C: set if there was no match
.proc get_fileid
@other=zp::str0
@filename=zp::str2
@buff=$120
@cnt=debugtmp
	stxy @filename

	; copy the filename to compare to a temp buffer
	lda #FINAL_BANK_MAIN	; copy from MAIN bank
	stxy zp::bankaddr0	; copy from given address
	ldxy #@buff
	stxy zp::bankaddr1	; copy to temp buffer
	jsr ram::copyline	; copy the filename to a buffer in shared RAM
	lda @buff
	beq @notfound	; if string is 0-length, return with "not found" flag
	lda numfiles
	beq @notfound	; if no files are stored, return with not found

	lda #$00
	sta @cnt

; foreach file in the filetable, check if the name matches the one requested
@l0:	lda @cnt
	jsr get_filename_addr	; get the filename to compare
	stxy @other
	jsr strcompare		; @filename == @other?
	bne @next		; if not, try the next filename

@found: ldxy @filename	; restore .XY
	lda @cnt	; get the file ID
	RETURN_OK	; file found

@next:	inc @cnt
	lda @cnt
	cmp numfiles
	bne @l0

@notfound:
	ldxy @filename	; restore .XY
	sec		; not found
	rts
.endproc

;*******************************************************************************
; GET FILENAME ADDR
; Returns the address of the filename for the given file ID
; IN:
;  - .A: the file ID to get the filename of
; OUT:
;  - .XY: address of filename for the given file ID
;  - .C:  set if there is no filename for the given file (XY will STILL
;         point to the address the filename WOULD exist at)
.proc get_filename_addr
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

;*******************************************************************************
; GET FILENAME
; Copies the filename for the given ID to a scratch buffer and returns the
; address to it
; IN:
;  - .A: the file ID to get the filename of
; OUT:
;  - .XY: address of filename for the given file ID
;  - .C:  set if there is no filename for the given file (XY will STILL
;         point to the address the filename WOULD exist at)
.if FINAL_BANK_MAIN=FINAL_BANK_DEBUG
get_filename = get_filename_addr
.else
.proc get_filename
@buff=$120
	cmp numfiles		; set .C if file ID is >= numfiles
	bcs @done
	jsr get_filename_addr
	stxy zp::bankaddr0
	ldxy #@buff
	stxy zp::bankaddr1
	lda #FINAL_BANK_DEBUG	; copy from DEBUG bank
	jsr ram::copyline	; copy the filename to a buffer in shared RAM
	ldxy #@buff
	clc
@done:	rts
.endproc
.endif

;*******************************************************************************
; SET FILE
; Sets the active file-id to the the ID for given filename.
; If no file-id exists for the provided filename, one is first created.
; The filename given is assumed to be in the main bank
; IN:
;   - .XY: the 0-terminated file to set as the current file
; OUT:
;   - .A: the ID of the file that was activated
;   - .C: set on error
.proc set_file
	jsr get_fileid	; get the file ID (if the file is already stored)
	bcc :+		; if an ID was found, no need to copy filename
	jsr set_name	; copy the filename and get its new ID
:	sta file	; store the ID as the active file ID
	rts
.endproc

;*******************************************************************************
; SET NAME
; Renames the entry for the given ID in the file table to the given name
; IN:
;   - .A:  the debug file ID of the handle to (re)name
;   - .XY: the filename to set for the handle
;   - .C:  set if there is no file for the given ID
.proc set_name
@filename=r2
@dst=r4
	stxy @filename
	jsr get_filename_addr	; get the destination address for ID
	stxy @dst
	ldxy @filename		; restore filename
	CALL FINAL_BANK_MAIN, str::len

	; bytes to copy = strlen(@filename)+1
	tax
	inx
	ldy #$00
	lda #FINAL_BANK_DEBUG		; dest bank
	sta r7
	lda #FINAL_BANK_MAIN		; source bank
	CALL FINAL_BANK_MAIN, ram::copybanked

	lda numfiles	; get ID of new file
	inc numfiles
	RETURN_OK
.endproc

;*******************************************************************************
; HEADER ADDR
; Returns the address of the block header for the given ID
; IN:
;   - .A: the ID of the block to get
; OUT:
;   - .XY:
; CLOBBERS:
;   - r0-r1
.proc header_addr
@tmp0=r0
@tmp1=r1
	; multiply block number by sizeof(blockdata)
	sta @tmp0
	asl			; *2
	asl			; *4
	sta @tmp1
	asl			; *8
	adc @tmp1		; *12
	adc @tmp0		; *13
	adc #<blockheaders
	tax
	lda #>blockheaders
	adc #$00
	tay
	rts
.endproc

;*******************************************************************************
; SAVE BLOCK
; Copies the state of the work variables (zp::block) to the given block ID
; IN:
;   - .A: the ID of the block to save to.
.proc save_block
@block=r0
	jsr header_addr
	stxy @block

; save the header state for the block:
; start addr, stop addr, base line, # lines, file id, program addr
	ldy #(BLOCK_START_ADDR+SIZEOF_BLOCK_HEADER) - 1
@copy:	lda block,y
	sta (@block),y
	dey
	bpl @copy

	rts
.endproc

;*******************************************************************************
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
;  file:       the file ID used by the block
;  line:       the address of the line data for the block
;  numlines:   the number of lines in the block
;  srcline:    initialized to the first line in the block
;  .C:         set if the block doesn't exist, clear on success
.proc get_block_by_id
	cmp numblocks
	bcs @done	; return error

	; multiply block number by sizeof(blockdata)
	jsr header_addr
	stxy block

; copy the header state for the block:
; start addr, stop addr, base line, # lines, file id, program addr
	ldy #(BLOCK_START_ADDR+SIZEOF_BLOCK_HEADER) - 1
@copy:	lda (block),y
	sta blockstate,y
	dey
	bpl @copy

	; initialize line/address values to the base for the block
	lda blockstart
	sta addr
	lda blockstart+1
	sta addr+1

	lda blocklinebase
	sta srcline
	lda blocklinebase+1
	sta srcline+1

	; initialize line program for the block
	lda progstart
	sta line
	lda progstart+1
	sta line+1

	; compute linestop by adding numlines to linebase and subtracting 1
	lda linestop		; numlines
	clc
	adc blocklinebase
	sta linestop
	lda linestop+1		; numlines+1
	adc blocklinebase+1
	sta linestop+1
	decw linestop

	clc	; OK
@done:	rts
.endproc

;*******************************************************************************
; ADVANCE
; Runs one command in the active line program
; Updates the line pointer to point to the next line in the active file.
; OUT:
;  - line: set to the location of the next line in the code
;  - addr: set to the line that's mapped to line
;  - .C:   set if there are no lines left in the active block (block)
.proc advance
	ldy #$00
	lda (line),y
	beq @extended_i	; if opcode is 0, this is an extended instruction

@basic_i:
	; decode the opcode; top 4 bits contain PC offset and bottom 4 line
	pha		; save instruction
	and #$0f	; get line offest and add it to the current line
	clc
	adc srcline
	sta srcline
	bcc :+
	inc srcline+1

:	pla		; restore instruction
	lsr		; get PC offset and add it to the current address
	lsr
	lsr
	lsr
	clc
	adc addr
	sta addr
	bcc @ok
	inc addr+1
	bcs @ok		; branch always

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

:	cmp #OP_ADVANCE_LINE
	bne :+
@adv_line:
	lda srcline		; current line
	clc
	adc (line),y		; + LSB of amount to advance
	sta srcline
	incw line
	lda srcline+1		; current line (MSB)
	adc (line),y		; + MSB of amount to advance
	sta srcline+1
	jmp @ok

:	cmp #OP_ADVANCE_ADDR
	bne :+
@adv_addr:
	lda addr
	clc
	adc (line),y
	sta addr
	incw line
	lda addr+1
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
@ok:	incw line
	RETURN_OK

@end:	sec		; flag end of porgram
	rts
.endproc

;*******************************************************************************
; COMPARE
; Compares the strings in (str0) to the buffer @ $120
; IN:
;  zp::str0: one of the strings to compare
;  $120:     the string to compare to
; OUT:
;  .Z: set if the strings are equal
.proc strcompare
@cnt=zp::bankval
@buff=$120
	ldy #$00
@l0:	lda (zp::str0),y
	cmp @buff,y		; compare it with the string in this bank
	bne @done		; not equal

@next:	cmp #$00		; are we at the terminating 0?
	beq @done		; if so, return
	iny			; next char
	bne @l0
	lda #$ff		; not equal
@done:	rts
.endproc
