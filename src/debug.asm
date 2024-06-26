;******************************************************************************
; DEBUG.ASM
; This file contains the debugger code, which is the main loop while debugging
; and assembled program.
;******************************************************************************

.include "asm.inc"
.include "asmflags.inc"
.include "bitmap.inc"
.include "breakpoints.inc"
.include "config.inc"
.include "cursor.inc"
.include "edit.inc"
.include "errors.inc"
.include "fastcopy.inc"
.include "file.inc"
.include "finalex.inc"
.include "flags.inc"
.include "labels.inc"
.include "irq.inc"
.include "key.inc"
.include "keycodes.inc"
.include "layout.inc"
.include "macros.inc"
.include "memory.inc"
.include "sim6502.inc"
.include "source.inc"
.include "string.inc"
.include "strings.inc"
.include "text.inc"
.include "util.inc"
.include "watches.inc"
.include "view.inc"
.include "vmem.inc"
.include "zeropage.inc"

.import __DEBUGGER_LOAD__
.import __DEBUGGER_SIZE__

; address in user program where the BRK handler will reside
BRK_HANDLER_ADDR = $8000-(brkhandler1_size+brkhandler2_size-2)
RTI_ADDR         = BRK_HANDLER_ADDR + 3

;******************************************************************************
; Debug info constants
MAX_FILES       = 24	; max files that debug info may be generated for
MAX_SEGMENTS    = 8	; max segments that debug info may be generated for
MAX_BREAKPOINTS = 16	; max number of breakpoints that may be set
MAX_WATCHPOINTS = 8	; max number of watchpoints that may be set
MAX_LINES       = 8000	; max number of lines across all segments

SEG_START_ADDR = 0      ; offset in segment header for start address
SEG_STOP_ADDR  = 2      ; offset in segment header for stop address
SEG_LINE_COUNT = 4	; offset in segment header for line count
DATA_FILE      = 0	; offset of FILE ID in debug info
DATA_LINE      = 1	; offset of line number in debug info
DATA_ADDR      = 3	; offset of line address in debug info

AUX_MEM   = 1		; enables the memory viewer in the debug view
AUX_BRK   = 2		; enables the breakpoint view in the debug view
AUX_WATCH = 3		; enables the watchpoint view in the debug view

BREAKPOINT_ENABLED = 1

; layout constants for the register view
REG_PC_OFFSET = 0
REG_A_OFFSET = 5
REG_X_OFFSET = 8
REG_Y_OFFSET = 11
REG_SP_OFFSET = 14

;******************************************************************************
; ACTION constants
; These tell us what command the user last executed when we return to the
; debugger via a BRK or NMI
ACTION_STEP      = 1	; action for STEP command
ACTION_STEP_OVER = 2	; action for STEP OVER command
ACTION_START     = 3	; action for initial debug entry
ACTION_GO_START  = 4	; action for first instruction of GO command
ACTION_GO        = 5	; action for subsequent GO instructions
ACTION_TRACE     = 6	; action for TRACE command

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
debugtmp       = zp::debug+$10

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

;******************************************************************************
; Program state variables
.BSS

;--------------------------------------
; Up to 4 bytes need to be saved in a given STEP.
;  1. The instruction at the PC (up to 3 bytes)
;  2. The memory value read/written by the instruction (up to 1 byte)
;
; We need to save all these before we complete a STEP and execute that
; instruction:
;  1. save user instruction at (PC)
;  2. get effective address of instruction's operand
;   2a. if there is an address and it's in [0,$2000), save the value there
;
; To restore the debugger:
;  1. save user value at sim::effective_addr (address of effected memory)
;  2. restore 3 bytes of debug memory at (prev_pc)

startsave:
stepsave:  .byte 0	; opcode to save under BRK
brkaddr:   .word 0 	; address where our brakpoint is set

;******************************************************************************
; Debug state values for internal RAM locations
; NOTE:
; these must be stored next to each other as they are backed up/restored as
; a unit.
debug_state_save:
debug_instruction_save: .res 3	; buffer for debugger's
mem_debugsave:          .byte 0 ; byte under effective address during STEP
debug_stepsave: 	.byte 0 ; debugger byte under BRK (if internal)

;--------------------------------------

; previous values for registers etc.
prev_reg_a:  .byte 0
prev_reg_x:  .byte 0
prev_reg_y:  .byte 0
prev_reg_p:  .byte 0
prev_reg_sp: .byte 0
prev_pc:     .word 0

sw_valid:    .byte 0    ; if !0, stopwatch is valid

prev_mem_save:     .byte 0
prev_mem_saveaddr: .word 0

step_mode: .byte 0	; which type of stepping we're doing (INTO, OVER)
lineset:   .byte 0	; not zero if we know the line number we're on
advance:   .byte 0	; not zero if a command continues program execution
swapmem:   .byte 0      ; not zero if we need to swap in user RAM

aux_mode:         .byte 0	; the active auxiliary view
highlight_line:	  .word 0 	; the line we are highlighting
highlight_file:   .word 0	; filename of line we are highlighting

action:	.byte 0		; the last action performed e.g. ACTION_STEP

;******************************************************************************
; Debug symbol variables
; number of files that we have debug info for. The ID of a file is its index
.export numfiles
numfiles: .byte 0

; table of 0-terminated filenames
.export filenames
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

;******************************************************************************
; WATCHES
;******************************************************************************
__debug_numwatches:  .byte 0		    ; number of active watches
__debug_watcheslo:   .res MAX_WATCHPOINTS   ; addresses of the set watchpoints
__debug_watcheshi:   .res MAX_WATCHPOINTS   ; addresses of the set watchpoints
__debug_watch_vals:  .res MAX_WATCHPOINTS   ; values of the set watchpoints
__debug_watch_prevs: .res MAX_WATCHPOINTS   ; previous values of watches
__debug_watch_flags: .res MAX_WATCHPOINTS   ; flags for watches (e.g. DIRTY)

; the following are used for watches that represent a range of values
; e.g. [$1000, $1100)
__debug_watches_changedlo: .res MAX_WATCHPOINTS ; the address that was changed
__debug_watches_changedhi: .res MAX_WATCHPOINTS ; the address that was changed
__debug_watches_stoplo:    .res MAX_WATCHPOINTS ; end address of watch range
__debug_watches_stophi:    .res MAX_WATCHPOINTS ; end address of watch range

.export __debug_watcheslo
.export __debug_watcheshi
.export __debug_watch_vals
.export __debug_watch_prevs
.export __debug_numwatches
.export __debug_watch_flags
.export __debug_watches_stoplo
.export __debug_watches_stophi

;******************************************************************************
; BREAKPOINTS
.export __debug_breakpointslo
.export __debug_breakpointshi
.export __debug_numbreakpoints
__debug_numbreakpoints:
numbreakpoints: .byte 0 		; number of active break points
__debug_breakpointslo:
breakpointslo:      .res MAX_BREAKPOINTS	; LSB's of the break points
__debug_breakpointshi:
breakpointshi:      .res MAX_BREAKPOINTS	; MSB's of the break points
breakpoint_lineslo: .res MAX_BREAKPOINTS	; breakpoint line # (LSB)
breakpoint_lineshi: .res MAX_BREAKPOINTS	; breakpoint line # (MSB)

.export __debug_breakpoint_flags
__debug_breakpoint_flags:
breakpoint_flags: .res MAX_BREAKPOINTS ; active state of breakpoints
breaksave:        .res MAX_BREAKPOINTS ; backup of instructions under the BRKs

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
; Sets the active file-id to the ID for given filename.
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
; START
; Begins debugging at the given address
; Execution will continue until a BRK instruction occurs at which point the
; debugger will take over and allow for interactive debugging from the user.
; in:
;  - .XY: the address to begin debugging at
.export __debug_start
.proc __debug_start
	stxy sim::pc

	jsr save_debug_state 		; save the debug state
	ldxy sim::pc
	stxy prev_pc
	jsr vmem::load
	sta startsave

	lda #CUR_SELECT
	sta cur::mode

	ldxy sim::pc
	lda #$00		; BRK
	jsr vmem::store

	lda #ACTION_START
	sta action

	; initialize auxiliary views
	jsr brkpt::init
	lda #$00
	sta aux_mode

	; init state
	sta __debug_numwatches

	jsr reset_stopwatch

	jsr dummy_irq
	jsr __debug_restore_progstate	; and copy in entire user state to start
	lda #$01
	sta swapmem			; on 1st iteration, swap entire RAM back

	jsr install_brk			; install the IRQ

	jsr save_debug_zp
	jsr restore_user_zp

@runpc:	JUMP FINAL_BANK_USER, sim::pc	; execute the user program until BRK
.endproc


;******************************************************************************
; DUMMY IRQ
; Replaces the IRQ with a dummy (NOP) one
.proc dummy_irq
	; install a NOP IRQ
	ldxy #$eb15
	stxy $0314
	rts
.endproc

;******************************************************************************
; INSTALL BRK
; Installs the given vector to the BRK and NMI vectors
; The virtual vector ($0334) holds the actual address of the interrupt handler.
; The real vectors, $0316-$0317 and $0318-$0319, are updated to a handler that
; switches back to the main RAM bank and dispatches to this vector.
; Care must be taken to choose an address that won't overwrite Monster
; IN:
;  - .XY: the address to install the BRK handler to
.proc install_brk
@dst=r0
@cnt=r2
	ldxy #BRK_HANDLER_ADDR
	stxy @dst
	stxy $0316		; BRK
	stxy $0318		; NMI

	lda #brkhandler1_size-1
	sta @cnt
; copy part 1 of the BRK handler to the user program
@l0:	ldy @cnt
	lda brkhandler1,y
	sta zp::bankval
	sty zp::bankoffset
	ldxy @dst
	lda #FINAL_BANK_USER
	jsr fe3::store_off
	dec @cnt
	bpl @l0

	ldxy #BRK_HANDLER_ADDR+3
	stxy @dst
	lda #brkhandler2_size-1
	sta @cnt
; copy part 2 of the BRK handler to our memory
@l1:	ldy @cnt
	lda brkhandler2,y
	sta zp::bankval
	sty zp::bankoffset
	ldxy @dst
	lda #FINAL_BANK_MAIN
	jsr fe3::store_off
	dec @cnt
	bpl @l1
	rts
.endproc

.CODE

;******************************************************************************
; BRKHANDLER
; Handles the BRK interrupt by returning control to the main bank
; and continuing execution there.
; Part 1 of the handler is copied to the user program and part 2 is copied to
; the editor, where execution will pick up after switching banks
;
; This is the layout of the code in each bank:
; PHA		-- (1 byte)
; LDA #$00	-- (2 bytes)
; STA $9C02	STA $9C02
; PLA		PLA
; RTI		JMP DEBUG_BRK
; -- (2 bytes)  -- (0 bytes)
; handler1 has 2 empty bytes at the end and handler2 has 3 empty bytes at the
; beginning
brkhandler1:
	pha
	lda #$80
	sta $9c02
	pla
	rti
brkhandler1_size=*-brkhandler1
brkhandler2:
	sta $9c02
	pla
	jmp debug_brk
brkhandler2_size=*-brkhandler2

;******************************************************************************
; INSTALL_BREAKPOINTS
; Install all breakpoints from "breakpoints" EXCEPT for those at the current
; PC. This is to prevent a loop of breakpoints being repeatedly set and
; immediately hit. For a breakpoint to be effective, it must be set at least
; one instruction from the current PC.
.proc install_breakpoints
@brkaddr=r0
@cnt=r2
	ldx numbreakpoints
	beq @done
	dex
	stx @cnt

@installbrks:
	ldx @cnt
	lda breakpoint_flags,x
	and #BREAKPOINT_ENABLED
	beq @next

	lda breakpointslo,x
	sta @brkaddr
	lda breakpointshi,x
	sta @brkaddr+1

	; if this breakpoint is at the current PC, don't install it
	ldxy sim::pc
	cmpw @brkaddr
	beq @next

	ldxy @brkaddr
	jsr vmem::load
	beq @next		; already a BRK
	ldx @cnt
	sta breaksave,x		; save the instruction under the new BRK
	lda #$00		; BRK

	ldxy @brkaddr
	jsr vmem::store

@next:	dec @cnt
	bpl @installbrks
@done:	rts
.endproc

;******************************************************************************
; UNINSTALL_BREAKPOINTS
; Restores the source code by removing all breakpoints installed by the debugger
.proc uninstall_breakpoints
@addr=r0
@cnt=r2
	ldx numbreakpoints
	beq @done
	dex
	stx @cnt
@uninstall:
	ldx @cnt
	lda breakpoint_flags,x
	and #BREAKPOINT_ENABLED
	beq @next

	lda breakpointslo,x
	sta @addr
	lda breakpointshi,x
	sta @addr+1

	lda breaksave,x
	beq @next				; already a BRK
	ldxy @addr
	jsr vmem::store
@next:	dec @cnt
	bpl @uninstall
@done:	rts
.endproc

;******************************************************************************
; SAVE_DEBUG_STATE
; saves memory likely to be clobbered by the user's
; program (namely the screen)
.proc save_debug_state
@losave=mem::dbg00+$100
@vicsave=mem::dbg9000
@colorsave=mem::dbg9400
	ldx #$10
@savevic:
	lda $9000-1,x
	sta @vicsave-1,x
	dex
	bne @savevic

@savelo:
	lda $100-1,x
	;sta @losave-1,x
	lda $200-1,x
	;sta @losave-1,x
	lda $300-1,x
	;sta @losave-1,x
	dex
	bne @savelo

	ldx #$f0
; save $9400-$94f0
@savecolor:
	lda $9400-1,x
	sta @colorsave-1,x
	dex
	bne @savecolor

	; backup the screen
	JUMP FINAL_BANK_FASTCOPY2, #fcpy::save
.endproc

;******************************************************************************
; SAVE_PROG_STATE
; saves memory clobbered by the debugger (screen, ZP, etc.)
.export __debug_save_prog_state
.proc __debug_save_prog_state
@losave=mem::prog00+$100
@vicsave=mem::prog9000
@internalmem=mem::prog1000
@colorsave=mem::prog9400
	ldx #$10
@savevic:
	lda $9000-1,x
	sta @vicsave-1,x
	dex
	bne @savevic

; save $1000-$1100
@save1000:
	lda $1000,x
	sta @internalmem,x
	dex
	bne @save1000

	ldx #$f0
; save $9400-$94f0
@savecolor:
	lda $9400-1,x
	sta @colorsave-1,x
	dex
	bne @savecolor

; save $100-$400
@savelo:
	lda $100-1,x
	;sta @losave-1,x
	lda $200-1,x
	;sta @losave+$100-1,x
	lda $300-1,x
	;sta @losave+$200-1,x
	dex
	bne @savelo

	; backup the user $1100-$2000 data
	JUMP FINAL_BANK_FASTCOPY, #fcpy::save
.endproc


;******************************************************************************
; DEBUG_BRK
; This is the BRK handler for the debugger.
; It saves the user program's state and other sensitive memory areas that are
; needed by the debugger for display etc, then it restores the debugger's state
; and finally transfers control to the debugger
.proc debug_brk
	; save the registers pushed by the KERNAL interrupt handler ($FF72)
	pla
	sta sim::reg_y
	pla
	sta sim::reg_x
	pla
	sta sim::reg_a
	pla
	sta sim::reg_p
	pla
	sta sim::pc
	pla
	sta sim::pc+1
	tsx
	stx sim::reg_sp

	; clear decimal in case user set it
	cld

	; BRK pushes PC + 2, subtract 2 from PC
	lda sim::pc
	sec
	sbc #$02
	sta sim::pc
	lda sim::pc+1
	sbc #$00
	sta sim::pc+1

	sei
	; restore the debugger's zeropage
	jsr save_user_zp
	jsr restore_debug_zp

	; reinstall the main IRQ
	ldx #<irq::sys_update
        ldy #>irq::sys_update
	lda #$20
        jsr irq::raster

	; swap the debugger state in
	jsr swapout

	; unless we can figure out the exact RAM we will affect, we'll have to
	; swap in the entire user RAM before we return from this BRK
	lda #$01
	sta swapmem

	lda #$00
	sta lineset		; flag that line # is (yet) unknown
	sta sim::branch_taken 	; clear branch taken flag

@uninstall_brks:
	; uninstall breakpoints (will reinstall the ones we want later)
	jsr uninstall_breakpoints

	; if we're beginning a GO, get on with it
	lda action
	cmp #ACTION_GO_START
	bne @update_watches

	; continue the GO command
	jsr step_restore
	lda #ACTION_GO
	sta action

@continue_debug:
	jsr install_breakpoints	 ; reinstall rest of breakpoints
	jmp @debug_done		 ; continue execution

@update_watches:
	jsr watch::update
	jsr show_aux		; display the auxiliary mode

	; check if action was STEP or TRACE
	lda action
	cmp #ACTION_STEP
	beq @handle_action
	cmp #ACTION_TRACE
	beq @handle_action

@reset_affected:
	; if action wasn't STEP or TRACE, we don't know enough to say
	; what was affected since last BRK
	lda #$00
	sta sim::affected

@handle_action:
	lda action
	cmp #ACTION_START
	bne :+
	lda startsave		; restore opcode
	ldxy sim::pc
	jsr vmem::store
	jmp @reset_state
:	jsr stepping		; are we stepping?
	bne @reset_state
@restore_step:
	jsr step_restore

	; if we are doing a TRACE, install breakpoints and continue
	lda action
	cmp #ACTION_TRACE
	bne @reset_state
@trace:
	; check if the BRK that was triggered is a breakpoint or just the
	; step point. If the latter, continue tracing
	ldxy sim::pc
	jsr get_breakpoint
	bcs :+			; not a breakpoint, continue tracing
	bcc @reset_state	; return control to debugger
:	jsr trace
	jmp @continue_debug

@reset_state:
	lda #$00
	sta action

@showbrk:
	; get the address before the BRK and go to it
	ldxy sim::pc
	jsr __debug_gotoaddr
	bcs @print		; if we failed to get line #, continue
	stxy highlight_line
	inc lineset

	jsr toggle_highlight	; highlight line

@print:	jsr showstate		; show regs/BRK message

; main debug loop
@debugloop:
	cli
	jsr text::update
	jsr key::getch
	beq @debugloop

	pha
	cli
	jsr toggle_highlight	; turn off highlight

	lda #$00
	sta advance	; by default, don't return to program after command
	jsr cur::off
	pla

 ; check if the key has a debugger command associated with it
	ldx #num_commands-1
@getcmd:
	cmp commands,x
	beq @runcmd
	dex
	bpl @getcmd

; check if the key should be ignored (not sent to be handled by the editor)
	ldx #num_disabled_commands-1
@chkdisabled:
	cmp disabled_commands,x
	beq @finishloopiter	; if key is marked as disabled, ignore it
	dex
	bpl @chkdisabled

; propagate the key to the editor
@nocmd:	jsr edit::handlekey
	jmp @finishloopiter

@runcmd:
	sei
	lda command_vectorslo,x	 ; vector LSB
	sta zp::jmpvec
	lda command_vectorshi,x  ; vector MSB
	sta zp::jmpvec+1

	jsr zp::jmpaddr		; call the command
	jsr showstate		; restore the register display (may be changed)

@finishloopiter:
	jsr toggle_highlight	; turn on highlight
	jsr cur::on
	lda advance		; are we ready to execute program? (GO, STEP)
	beq @debugloop		; not yet, loop and get another command

@done:	; unhighlight the BRK line if it's still visible
	jsr toggle_highlight

@debug_done:
	jsr cur::off
	jsr swapin

@restore_regs:
	; from top to bottom: [STATUS, <PC, >PC]
	lda sim::pc+1
	sta prev_pc+1
	pha
	lda sim::pc	; restore PC
	sta prev_pc
	pha

	lda sim::reg_p	; restore processor status
	pha

	; install a NOP IRQ
	jsr dummy_irq

	jsr save_debug_zp
	jsr restore_user_zp

	lda sim::reg_a
	sta prev_reg_a
	ldx sim::reg_x
	stx prev_reg_x
	ldy sim::reg_y
	sty prev_reg_y

	; return from the BRK
	pha
	lda #FINAL_BANK_USER
	jmp RTI_ADDR
.endproc

;******************************************************************************
; LOAD_FILE
; Loads the file (in the editor) for the given filename
; IN:
;  - .A: the file (as returned from addr2line)
; OUT:
;  - .C: set if the file couldn't be opened
.export __debug_load_file
.proc __debug_load_file
	asl
	asl
	asl
	asl
	adc #<filenames
	tax
	lda #>filenames
	adc #$00
	tay
	jmp edit::load
.endproc

;******************************************************************************
; GOTOADDR
; Navigates the editor to the file/line associated with the give address
; IN:
;  - .XY: the address to "goto"
; OUT:
;  - .C:  set on failure
;  - .XY: the line that was navigated to
.export __debug_gotoaddr
.proc __debug_gotoaddr
@line=debugtmp
	jsr __debug_addr2line	; get the line #
	bcs @done		; error
	sta file
	stxy @line
	jsr __debug_load_file	; load file (if not already)
	bcs @done		; error

	ldxy @line
	jsr edit::gotoline
	ldxy @line
	clc
@done:	rts
.endproc

;******************************************************************************
; SYM2LINE
; Returns the line # and file ID associated with the given symbol
; IN:
;  - .XY: the symbol (0-terminated string)
; OUT:
;  - .A:  the file ID of the symbol
;  - .XY: the line # of the symbol
;  - .C:  set on failure
.export __debug_sym2line
.proc __debug_sym2line
.endproc

;******************************************************************************
; SYM2ADDR
; Returns the address associated with the given symbol
; IN:
;  - .XY: the symbol (0-terminated string)
; OUT:
;  - .XY: the address of the symbol
;  - .C:  set on failure
.export __debug_sym2addr
.proc __debug_sym2addr
	rts
.endproc

;******************************************************************************
; TOGGLE_HIGHLIGHT
; Toggles the actively highlighted line's highlight
.proc toggle_highlight
	lda lineset
	beq :+			; line # not known

	jsr src::filename	; get filename (zp::tmp0 = name)
	ldxy highlight_line
	jsr edit::src2screen
	bcs :+			; off screen
	jmp bm::rvsline
:	rts
.endproc

;******************************************************************************
; RESTORE_DEBUG_STATE
; Restores the saved debugger state
.proc restore_debug_state
@losave=mem::dbg00+$100
@vicsave=mem::dbg9000
@colorsave=mem::dbg9400
	ldx #$10
@restorevic:
	lda @vicsave-1,x
	sta $9000-1,x
	dex
	bne @restorevic

; restore $100-$400
@restorelo:
	lda @losave-1,x
	;sta $100-1,x
	lda @losave+$100-1,x
	;sta $200-1,x
	lda @losave+$200-1,x
	;sta $300-1,x
	dex
	bne @restorelo

	ldx #$f0
; save $9400-$94f0
@restorecolor:
	lda @colorsave-1,x
	sta $9400-1,x
	dex
	bne @restorecolor

	; reinit the bitmap
	jsr bm::init

	; restore the screen ($1100-$2000)
	JUMP FINAL_BANK_FASTCOPY2, #fcpy::restore
.endproc

;******************************************************************************
; SAVE DEBUG ZP
; Saves the state of the debugger's zeropage
; TODO: only save/restore the ZP locations clobbered by the debugger
; (will require some overall restructure of ZP usage)
.proc save_debug_zp
@zp=mem::dbg00
	ldx #$00
@l0:	lda $00,x
	sta @zp,x
	dex
	bne @l0
	rts
.endproc

;******************************************************************************
; RESTORE DEBUG ZP
; Restores the state of the debugger's zeropage
.proc restore_debug_zp
@zp=mem::dbg00
	ldx #$00
@l0:	lda @zp,x
	sta $00,x
	dex
	bne @l0
	rts
.endproc

;******************************************************************************
; SAVE USER ZP
; Saves the state of the user's zeropage
.proc save_user_zp
@zp=mem::prog00
	ldx #$00
@l0:	lda $00,x
	sta @zp,x
	dex
	bne @l0
	rts
.endproc

;******************************************************************************
; RESTORE USER ZP
; Restores the state of the user's zeropage
.proc restore_user_zp
@zp=mem::prog00
	ldx #$00
@l0:	lda @zp,x
	sta $00,x
	dex
	bne @l0
	rts
.endproc

;******************************************************************************
; RESTORE_PROGSTATE
; restores the saved program state
.export __debug_restore_progstate
.proc __debug_restore_progstate
@losave=mem::prog00+$100
@internalmem=mem::prog1000
@vicsave=mem::prog9000
@colorsave=mem::prog9400
	ldx #$10
@restorevic:
	lda @vicsave-1,x
	sta $9000-1,x
	dex
	bne @restorevic

; restore $100-$400
@restorelo:
	lda @losave-1,x
	;sta $100-1,x
	lda @losave+$100-1,x
	;sta $200-1,x
	lda @losave+$200-1,x
	;sta $300-1,x
	dex
	bne @restorelo

; restore $1000-$1100
@restore1000:
	lda @internalmem,x
	sta $1000,x
	dex
	bne @restore1000

	ldx #$f0
; restore $9400-$94f0
@restorecolor:
	lda @colorsave-1,x
	sta $9400-1,x
	dex
	bne @restorecolor

	lda #$4c
	sta zp::jmpaddr
	; restore the user $1000 data
	JUMP FINAL_BANK_FASTCOPY, #fcpy::restore
.endproc

;******************************************************************************
; GO
; Runs the user program until the next breakpoint or an NMI occurs
.proc go
	; for the first instruction, just STEP, this lets us keep the breakpoint
	; we are on (if there is one) intact
	jsr step
	lda #$00
	sta sw_valid		; invalidate stopwatch
	lda #ACTION_GO_START
	sta action
	inc advance		; continue program execution
	rts
.endproc

;******************************************************************************
; TRACE
; Puts the debugger into TRACE mode and steps to the next instruction to
; begin the trace
.proc trace
	jsr step
	lda #ACTION_TRACE
	sta action
	rts
.endproc

;******************************************************************************
; QUIT
; Exits the debugger
.proc quit
	ldxy #strings::debug_stop_debugging
	lda #DEBUG_MESSAGE_LINE
	jsr text::print
	lda #DEBUG_MESSAGE_LINE
	jsr bm::rvsline

:	jsr key::getch
	beq :-

	; 'N' or QUIT: return back to debugger, 'Y': exit debugger
	cmp #$4e		; N
	beq @done
	cmp #K_QUIT
	beq @done		; don't quit
	cmp #$59		; Y
	bne :-

@quit:	lda #$00		; clear BRK flag
	pha			; push 0 status
	plp			; clear flags (.P)

	pla			; command return address
	pla
@done:	rts
.endproc

;******************************************************************************
; EDIT_SOURCE
; Disable all views and reenable (almost) fullscreen editing
.proc edit_source
	lda #$00
	sta aux_mode
	lda #REGISTERS_LINE-1
	jmp edit::resize
.endproc

;******************************************************************************
; EDIT_MEM
; Transfers control to the memory viewer/editor until the user exits it
.proc edit_mem
	pushcur
	lda #DEBUG_INFO_START_ROW-1
	jsr edit::resize
	lda #(DEBUG_INFO_START_ROW)*8
	jsr bm::clrpart
	jsr showstate		; restore the state

	lda #AUX_MEM
	sta aux_mode

	jsr view::edit
	popcur
	rts
.endproc

;******************************************************************************
; EDIT_BREAKPOINTS
; Transfers control to the breakpoint viewer/editor until the user exits it
.proc edit_breakpoints
	lda #DEBUG_INFO_START_ROW-1
	jsr edit::resize
	lda #(DEBUG_INFO_START_ROW)*8
	jsr bm::clrpart
	jsr showstate		; restore the state

	lda #AUX_BRK
	sta aux_mode
	jmp brkpt::edit
.endproc

;******************************************************************************
; EDIT_WATCHES
; Transfers control to the watch viewer/editor until the user exits it
.proc edit_watches
	lda #DEBUG_INFO_START_ROW-1
	jsr edit::resize
	lda #(DEBUG_INFO_START_ROW)*8
	jsr bm::clrpart
	jsr showstate		; restore the state

	lda #AUX_WATCH
	sta aux_mode
	jmp watch::edit
.endproc

;******************************************************************************
; SET_BREAKPOINT
; Sets a breakpoint at the current line selection
.proc set_breakpoint
@line=r0
	jsr edit::setbreakpoint		; set a breakpoint in the source
	ldxy src::line
	stxy @line
	jsr __debug_line2addr
	bcs @nobrk			; if no line # for this line, skip
	jsr __debug_toggle_breakpoint	; add the breakpoint to the debugger
	lda aux_mode
	cmp #AUX_BRK
	bne @done			; done if breakpoint viewer isn't active
	jmp show_aux			; refresh viewer
@done:
@nobrk:	rts
.endproc

;******************************************************************************
; SWAP_USER_MEM
; Toggles between the user program memory and the debugger
.proc swap_user_mem
	jsr save_debug_state
	jsr __debug_restore_progstate

	; wait for a key to swap the state back
:	jsr key::getch
	beq :-
	jsr __debug_save_prog_state
	jmp restore_debug_state	; restore debugger state
.endproc

;******************************************************************************
; STEP
; Runs the next instruction from the .PC and returns to the debug prompt.
; This works by inserting a BRK instruction after
; the current instruction and RUNning.
.proc step
@mode=r0
	ldxy #$100		; TODO: use ROM addr? (we don't need the string)
	stxy zp::tmp0		; TODO: make way to not disassemble to string
	ldxy sim::pc		; get address of next instruction
	jsr asm::disassemble  ; disassemble it to get its size (next BRK offset)
	stx sim::op_mode
	bcc @ok
	rts		; return error

@ok:	pha		; save instruction size

	; preemptively disable mem swapping
	; unless we encounter ROM for the next target (in which case we will
	; increment this again) we know exactly what will be written
	dec swapmem

	stx @mode			; address mode (set by asm::disassemble)
	ldxy sim::pc			; address of instruction
	jsr sim::get_side_effects	; get state that will be clobbered/used

	lda #ACTION_STEP
	sta action		; flag that we are STEPing

; for updating watches and just general info for the user, save the current
; state of memory that will be altered

	; clear watch flags
	lda #$00
	ldx __debug_numwatches
	beq @check_effects
:	sta __debug_watch_flags-1,x
	dex
	bne :-

@check_effects:
	lda sim::affected
	and #OP_LOAD|OP_STORE	; was there a write to memory?
	beq @setbrk		; if not, skip ahead to setting the next BRK

	ldxy sim::effective_addr	; if so, mark the watch if there is one
	jsr watch::mark			; if there's a watch at this addr, mark it
	bcc @setbrk			; if there's no watch, continue

	; activate the watch window so user sees change
	lda #(DEBUG_INFO_START_ROW+1)*8
	jsr bm::clrpart
	lda #AUX_WATCH
	sta aux_mode
	jsr watch::view

@setbrk:
	pla			; get instruction size
	pha			; save instruction size again
	ldxy sim::pc		; and address of instruction to-be-executed

	; get the address of the next instruction into sim::next_pc
	jsr sim::next_instruction
	stxy brkaddr

	; check if next instruction is in ROM
	; If so, we can't step, our "step" wil actually be more like a "go".
	; re-increment swapmem to force full RAM swap
	cmpw #$c000
	bcc @notrom
	lda #ACTION_STEP_OVER
	sta action
@notrom:
	lda sim::op
	cmp #$20		; JSR?
	bne @addbrk
	lda #ACTION_STEP_OVER
	cmp action		; are we stepping over
	bne @addbrk		; skip if not

; if stepping over a JSR, set breakpoint at current PC + 3
; also flag that we need to save all RAM
@stepover:
	lda #$00
	sta sw_valid	; invalidate stopwatch
	inc swapmem
	lda sim::pc
	clc
	adc #$03
	sta brkaddr
	lda sim::pc+1
	adc #$00
	sta brkaddr+1

@addbrk:
	ldxy brkaddr
	jsr vmem::load
	sta stepsave
	lda #$00		; BRK
	ldxy brkaddr
	jsr vmem::store

	pla			; get the instruction size
	tax
	lda stepsave		; get the opcode
	jsr sim::count_cycles	; get the # of cycles for the instruction
	clc
	adc sim::stopwatch
	sta sim::stopwatch
	lda sim::stopwatch+1
	adc #$00
	sta sim::stopwatch+1
	lda sim::stopwatch+2
	adc #$00
	sta sim::stopwatch+2

	inc advance		; continue program execution
	rts			; return to the debugger
.endproc

;******************************************************************************
; STEP_RESTORE
; Restores the opcode destroyed by the last STEP.
.proc step_restore
	lda stepsave
	ldxy brkaddr
	jmp vmem::store
.endproc

;******************************************************************************
; STEP_OVER
; Runs the next instruction from the .PC and returns to the debug prompt.
; This works by inserting a BRK instruction after
; the current instruction and RUNning.
; Unlike STEP, if the next procedure is a JSR, execution will continue
; at the line after the subroutine (after the subroutine has run)
.proc step_over
	lda #$00
	sta sw_valid		; invalidate stopwatch
	jmp step
.endproc

;******************************************************************************
; SWAPIN
; Either swaps in the 3-4 bytes that were saved (if we were able to resolve the
; exact memory affected by an instruction or the _entire_ internal RAM
; state [$00-$2000) if we were unable to resolve the exact memory addresses
; affected.
; This routine is called _before_ returning from the BRK interrupt and executing
; the user program.
.proc swapin
@cnt=r0
@addr=r1
@tosave=r3
	lda swapmem
	beq @fastswap

@swapall:
	; swap entire user RAM in (needed if we don't know what memory will
	; be changed before next BRK)
	jsr save_debug_state
	jmp __debug_restore_progstate

@fastswap:
	; save [mem_saveaddr], [step_point], and [pc, pc+2] for the debugger
	lda sim::pc+1
	sta @tosave+1
	sta @tosave+3
	sta @tosave+5
	ldx sim::pc
	stx @tosave
	inx
	bne :+
	inc @tosave+3
	inc @tosave+5
:	stx @tosave+2
	inx
	bne :+
	inc @tosave+5
:	stx @tosave+4

	ldxy sim::effective_addr
	stxy @tosave+6
	ldxy brkaddr
	stxy @tosave+8

; save the debugger's memory values at affected addresses
	ldy #8
	sty @cnt
	ldx #5-1
@save:	ldy @cnt
	lda @tosave,y
	sta @addr
	lda @tosave+1,y
	sta @addr+1
	jsr is_internal_address
	bne :+
	ldy #$00
	lda (@addr),y
	sta debug_state_save,x
:	dec @cnt
	dec @cnt
	dex
	bpl @save

; read the virtual memory values for the affected locations and store
; them to their physical addresses
	ldx #8
	stx @cnt
@store: lda @cnt
	tax

	lda @tosave,x
	sta @addr
	ldy @tosave+1,x
	sty @addr+1
	tax
	jsr is_internal_address
	bne @next		; skip if not internal address

	jsr vmem::load
	ldx @addr+1
	bne :+			; skip zeropage for now (we're still using it)

	ldx @addr
	sta mem::prog00,x	; update virtual ZP
	jmp @next

:	ldy #$00
	sta (@addr),y		; store it to the physical address
@next:	dec @cnt
	dec @cnt
	bpl @store

	rts			; done
.endproc

;******************************************************************************
; SWAPOUT
; Swaps *out* the user memory that needs to be saved in order to restore the
; debug state.
; This occurs after we encounter a BRK.  If we were able to figure out the exact
; areas that would be affected before we encountered the BRK, only swap those
; values for the debugger's values of those.
; If not, swap the entire internal RAM state
.proc swapout
@cnt=r0
@addr=r1
@ysave=r3
@tosave=r4
	lda swapmem
	beq @fastswap
	jmp @swapall

@fastswap:
	; save [prevpc, prevpc+2], [msave], and [sim::next_pc] for the user
	; program
	lda prev_pc+1
	sta @tosave+1
	sta @tosave+3
	sta @tosave+5
	ldx prev_pc
	stx @tosave
	inx
	bne :+
	inc @tosave+3
	inc @tosave+5
:	stx @tosave+2
	inx
	bne :+
	inc @tosave+5
:	stx @tosave+4

	ldxy sim::effective_addr
	stxy @tosave+6
	ldxy brkaddr
	stxy @tosave+8

	; save the user values affected by the previous instruction
	; to their virtual address
	ldx #8
	stx @cnt
@save:	ldx @cnt
	lda @tosave,x
	sta @addr
	ldy @tosave+1,x
	sty @addr+1
	tax
	jsr is_internal_address
	bne :+			; if not internal, don't do this
	ldy #$00
	lda (@addr),y		; read the physical address
	ldy @addr+1
	jsr vmem::store		; store to the virtual (user) address
:	dec @cnt
	dec @cnt
	bpl @save

	; restore the debug values at the affected locations
	ldx #8
	stx @cnt
@store: lda @cnt
	tax
	lsr
	sta @ysave

	ldy @tosave+1,x
	sty @addr+1
	lda @tosave,x
	sta @addr
	tax
	jsr is_internal_address	; if not an internal address, leave it alone
	bne @next

	ldy @ysave
	lda debug_state_save,y	; get the byte to restore

	ldx @addr+1
	bne :+
	ldx @addr
	sta mem::dbg00,x
	jmp @next

:	ldy #$00
	sta (@addr),y		; restore the byte
@next:	dec @cnt
	dec @cnt
	bpl @store

	rts			; done

@swapall:
	; save the program state before we restore the debugger's
	jsr __debug_save_prog_state
	jmp restore_debug_state	; restore debugger state
.endproc

;******************************************************************************
; TOGGLE_BREAKPOINT
; Sets a breakpoint at the address in .XY or removes it if one already exists
; IN:
;  - .XY: the address of the breakpoint to set
;  - r0:  the line # of the breakpoint
.export __debug_toggle_breakpoint
.proc __debug_toggle_breakpoint
@line=r0
	; if this is a duplicate, remove the existing breakpoint
	jsr remove_breakpoint
	bcc @done		; breakpoint existed, but we removed it

	txa
	ldx numbreakpoints

	; store address
	sta breakpointslo,x
	tya
	sta breakpointshi,x

	; store line #
	lda r0
	sta breakpoint_lineslo,x
	lda r1
	sta breakpoint_lineshi,y

	ldx numbreakpoints
	lda #BREAKPOINT_ENABLED
	sta breakpoint_flags,x
	inc numbreakpoints
@done:	rts
.endproc

;******************************************************************************
; REMOVEBREAKPOINT
; Removes a breakpoint at the address in .XY
; IN:
;  - .XY: the address of the breakpoint to remove
; OUT:
;  - .C: set if breakpoint doesn't exist
.export __debug_remove_breakpoint
__debug_remove_breakpoint:
.proc remove_breakpoint
@addr=debugtmp
@end=debugtmp+2
	jsr get_breakpoint
	bcs @ret
	tax

@remove:
	; shift breakpoints down
	lda numbreakpoints
	sta @end
	cpx @end
	beq @removed
@l1:	lda breakpointshi+1,x
	sta breakpointshi,x
	lda breakpointslo+1,x
	sta breakpointslo,x
	inx
	cpx @end
	bcc @l1
@removed:
	dec numbreakpoints
@done:	clc
@ret:	rts
.endproc

;******************************************************************************
; GET BREAKPOINT
; Returns the ID (index) of the breakpoint corresponding to the given address
; IN:
;  - .XY: the address of the breakpoint to get the ID of
; OUT:
;  - .C: set if no breakpoint is found
;  - .A: the id of the breakpoint
.proc get_breakpoint
@addr=debugtmp
	stxy @addr
	ldx numbreakpoints
	beq @notfound
@l0:	lda @addr
	cmp breakpointslo,x
	bne @next
	lda @addr+1
	cmp breakpointshi,x
	beq @found
@next:	dex
	bpl @l0
@notfound:
	ldxy @addr
	sec
	rts		; not found

@found: txa
	RETURN_OK
.endproc

;******************************************************************************
; EDIT STATE
; Moves the cursor to the registers area and allows the user to modify
; their values with hex input
.proc edit_state
	pushcur
	lda #$00
	sta zp::curx
	lda #REGISTERS_LINE+1
	sta zp::cury

	jsr showstate		; fill linebuffer with register state

@edit:	jsr cur::on
:	jsr key::getch
	beq :-
	pha
	jsr cur::off
	pla

	cmp #K_LEFT
	beq @back
	cmp #K_DEL
	bne @ret
@back:	lda zp::curx
	beq @edit		; can't move LEFT
	dec zp::curx
	ldx #6
@prevx:	cmp @offsets,x		; was cursor at start of offset?
	bcc @prev		; if cursor is < offset, check prev offset
	bne @ok			; if it was NOT at offset start, it is now
	lda @offsets-1,x
	adc #$00		; +1
	sta zp::curx
	jmp @refresh
@prev:	dex
	bpl @prevx
	bmi @edit

@ret:	cmp #K_RETURN
	beq @updatevals
@quit:	cmp #K_QUIT
	beq @exit
@right:	cmp #K_RIGHT
	beq @fwd

@hex:	jsr key::ishex
	bcc @refresh

	ldx zp::curx
	sta mem::linebuffer2,x

@fwd:   lda zp::curx
	cmp #REG_SP_OFFSET+1	; check offset
	bcs @refresh		; if already at last column, don't advance
	inc zp::curx		; bump up curx
	ldx #$00
; align x-position to either a value in @offsets or a value in @offsets+1
@nextx:	cmp @offsets,x		; was X at the start of the offset?
	beq @refresh		; if so, incrementing it by 1 was sufficient
	bcs @next		; if X
@ok:	lda @offsets,x		; if @offsets,x <= curx, set curx to it
	sta zp::curx
	jmp @refresh
@next:	inx
	cpx #6
	bne @nextx

@refresh:
	lda #REGISTERS_LINE+1
	ldxy #mem::linebuffer2
	jsr text::puts
	jmp @edit

;--------------------------------------
; parse the linebuffer2 and update all registers
@updatevals:
@val=zp::tmp0
	ldxy #mem::linebuffer2
	stxy @val

	ldx #6-1
@l0:	ldy @offsets,x
	lda (@val),y
	jsr util::chtohex	; get MSB
	asl
	asl
	asl
	asl
	sta sim::register_state,x

	iny
	lda (@val),y		; get LSB
	jsr util::chtohex
	ora sim::register_state,x
	sta sim::register_state,x

	dex
	bpl @l0

	; swap PC LSB and MSB
	ldx sim::pc
	lda sim::pc+1
	sta sim::pc
	stx sim::pc+1

@exit:	popcur
	rts

@offsets:
.byte REG_PC_OFFSET, REG_PC_OFFSET+2, REG_A_OFFSET, REG_X_OFFSET, REG_Y_OFFSET
.byte REG_SP_OFFSET
.endproc

;******************************************************************************
; SHOWSTATE
; Shows the current debug state (registers and BRK line)
.proc showstate
	jsr showbrk
	jmp showregs
.endproc

;******************************************************************************
; SHOWREGS
; prints the contents of the registers in the format
;   PC  A  X  Y  SP NV-BDIZC ADDR
;  f59c 02 02 00 f7 00100000 1003
.proc showregs
@tmp=zp::asm+6
@flag=zp::asm+7
@buff=mem::linebuffer2
	; display the register names
	ldxy #strings::debug_registers
	lda #REGISTERS_LINE
	jsr text::print

	ldy #39
	lda #' '
:	sta @buff,y
	dey
	bpl :-

	; draw .Y
	lda sim::reg_y
	jsr util::hextostr
	sty @buff+11
	stx @buff+12

	; draw .X
	lda sim::reg_x
	jsr util::hextostr
	sty @buff+8
	stx @buff+9

	; draw .A
	lda sim::reg_a
	jsr util::hextostr
	sty @buff+5
	stx @buff+6

	; draw .P (status)
	lda sim::reg_p
	sta @tmp
	lda #$a1
	sta @flag
	ldx #$00

@getstatus:
	and @tmp
	bne :+
	lda #'0'
	skw
:	lda #'1'
	sta @buff+17,x
:	lsr @flag
	inx
	cpx #2
	beq :-
	lda @flag
	bne @getstatus

@getsp:
	lda sim::reg_sp
	jsr util::hextostr
	sty @buff+14
	stx @buff+15

@getaddr:
	lda sim::pc+1
	jsr util::hextostr
	sty @buff
	stx @buff+1

	lda sim::pc
	jsr util::hextostr
	sty @buff+2
	stx @buff+3

; if registers were affected, highlight them
	ldx #TEXT_COLOR
	lda sim::affected
	and #OP_REG_A
	beq :+
	ldx #DEBUG_REG_CHANGED_COLOR
:	stx COLMEM_ADDR+(20*$b)+2
	stx COLMEM_ADDR+(20*$b)+3

	ldx #TEXT_COLOR
	lda sim::affected
	and #OP_REG_X
	beq :+
	ldx #DEBUG_REG_CHANGED_COLOR
:	stx COLMEM_ADDR+(20*$b)+4

	ldx #TEXT_COLOR
	lda sim::affected
	and #OP_REG_Y
	beq :+
	ldx #DEBUG_REG_CHANGED_COLOR
:	stx COLMEM_ADDR+(20*$b)+5
	stx COLMEM_ADDR+(20*$b)+6

	ldx #TEXT_COLOR
	lda sim::affected
	and #OP_STACK
	beq :+
	ldx #DEBUG_REG_CHANGED_COLOR
:	stx COLMEM_ADDR+(20*$b)+7

; if memory was loaded or stored, show the effective address
@memaddr:
	lda sim::affected
	and #OP_LOAD|OP_STORE
	bne :+
	lda #'-'
	sta @buff+27
	sta @buff+28
	sta @buff+29
	sta @buff+30
	bne @clk

:	lda sim::effective_addr+1
	jsr util::hextostr
	sty @buff+27
	stx @buff+28
	lda sim::effective_addr
	jsr util::hextostr
	sty @buff+29
	stx @buff+30

@clk:	lda sw_valid
	bne :+
	; if stopwatch is invalid, show ???
	lda #'?'
	sta @buff+37
	sta @buff+38
	sta @buff+39
	bne @print

:	ldx sim::stopwatch
	ldy sim::stopwatch+1
	lda sim::stopwatch+2
	jsr util::todec24

	; set the last character of the stopwatch always
	lda $100+7
	sta @buff+32+7

	; ignore leading zeroes
	ldx #$00
:	inx
	cpx #$07
	beq @print
	lda $100,x
	cmp #'0'
	beq :-

@cpyclk:
	lda $100,x
	sta @buff+32,x
	cpx #$07
	inx
	bcc @cpyclk

@print:	ldxy #@buff
	lda #REGISTERS_LINE+1
	jmp text::puts
.endproc

;******************************************************************************
; SHOWBRK
; Display the BRK line number or address
.proc showbrk
	lda lineset		; is the line # known?
	bne @showline		; if so, show it

@showaddr:
	; we couldn't find the line #; display the address of the BRK
	lda sim::pc
	pha
	lda sim::pc+1
	pha
	ldxy #strings::debug_brk_addr
	jmp @print

@showline:
	; display the BRK message
	lda highlight_line
	pha
	lda highlight_line+1
	pha
	ldxy #strings::debug_brk_line
@print:	lda #DEBUG_MESSAGE_LINE
	jsr text::print		; break in line <line #>
	lda #DEBUG_MESSAGE_LINE
	jmp bm::rvsline
.endproc

;******************************************************************************
; SHOW_AUX
; Displays the memory viewer, breakpoint viewer, or watchpoint viewer depending
; on which is enabled
.proc show_aux
	ldx aux_mode
	beq @done		; no aux mode
	cpx #@numauxviews+1
	bcs @done		; invalid selection
	lda @auxlos-1,x
	sta zp::jmpvec
	lda @auxhis-1,x
	sta zp::jmpvec+1

	lda #(DEBUG_INFO_START_ROW+1)*8
	jsr bm::clrpart

	jmp zp::jmpaddr
@done:	rts
.define auxtab view::mem, brkpt::view, watch::view
@auxlos: .lobytes auxtab
@auxhis: .hibytes auxtab
@numauxviews=*-@auxhis
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
; IS INTERNAL ADDRESS
; Returns with .Z set if the given address is outside of the address ranges
; [$2000,$8000) or [$a000,$c000)
; IN:
;  - .XY: the address to test
; OUT:
;  - .Z: set if the address in [$00,$2000), [$8000,$a000), or [$c000,$10000)
.proc is_internal_address
	cmpw #$2000
	bcc @internal
	cmpw #$8000
	bcc @external
	cmpw #$a000
	bcc @internal 	; VIC or I/O- internal

@external:
	lda #$ff	; banked RAM or ROM, no need to back up
	rts
@internal:
	lda #$00
	rts
.endproc

;******************************************************************************
; Stepping
; Returns with the .Z flag set if we are currently "stepping"
; This means that we are executing in a step-by-step fashion as with
;  STEP
;  TRACE
;  STEP-OVER
;  GO-START
.proc stepping
	lda action
	cmp #ACTION_GO_START
	beq @yes
	cmp #ACTION_STEP
	beq @yes
	cmp #ACTION_STEP_OVER
	beq @yes
	cmp #ACTION_TRACE
@yes:	rts
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

;******************************************************************************
; RESET STOPWATCH
; Resets the stopwatch
.proc reset_stopwatch
	lda #$01
	sta sw_valid
	lda #$00
	sta sim::stopwatch
	sta sim::stopwatch+1
	sta sim::stopwatch+2
	rts
.endproc

.RODATA

;******************************************************************************
; COMMANDS
; This table contains the keys used to invoke the corresponding command
; within the debugger
commands:
	.byte K_QUIT
	.byte K_STEP
	.byte K_STEPOVER
	.byte K_GO
	.byte K_TRACE
	.byte K_SRCVIEW
	.byte K_MEMVIEW
	.byte K_BRKVIEW
	.byte K_WATCHVIEW
	.byte K_SET_BREAKPOINT
	.byte K_SWAP_USERMEM
	.byte K_RESET_STOPWATCH
	.byte K_EDIT_STATE
num_commands=*-commands

.linecont +
.define command_vectors quit, step, step_over, go, \
	trace, edit_source, edit_mem, edit_breakpoints, edit_watches, \
	set_breakpoint, swap_user_mem, reset_stopwatch, edit_state
.linecont -
command_vectorslo: .lobytes command_vectors
command_vectorshi: .hibytes command_vectors

;******************************************************************************
; DISABLED COMMANDS
; the following commands are NOT propagated to the editor. they become a nop
; when handled by the debugger
disabled_commands:
	.byte K_CLOSE_BUFF
	.byte K_ASM
	.byte K_ASM_DEBUG
num_disabled_commands=*-disabled_commands
