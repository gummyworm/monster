.include "asm.inc"
.include "asmflags.inc"
.include "bitmap.inc"
.include "breakpoints.inc"
.include "config.inc"
.include "cursor.inc"
.include "edit.inc"
.include "errors.inc"
.include "fastcopy.inc"
.include "finalex.inc"
.include "flags.inc"
.include "labels.inc"
.include "irq.inc"
.include "key.inc"
.include "keycodes.inc"
.include "layout.inc"
.include "macros.inc"
.include "memory.inc"
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
.import __BANKCODE_LOAD__
.import __BANKCODE_SIZE__

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

OP_LOAD  = $01	; constant for an operation that loads
OP_STORE = $02	; constant for an operation that stores to memory
OP_REG_A = $04	; constant for an operation that stores to the .A register
OP_REG_X = $08	; constant for an operation that stores to the .X register
OP_REG_Y = $10	; constant for an operation that stores to the .Y register
OP_FLAG  = $20	; constant for an operation that writes to .P (flag register)
OP_STACK = $40	; constant for an operation that reads/writes SP
OP_PC    = $80	; constant for an operation that modifies PC (branches)

BREAKPOINT_ENABLED = 1

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
; Program state variables
.BSS
register_state:
pc:     .word 0
reg_a:  .byte 0
reg_x:  .byte 0
reg_y:  .byte 0
reg_sp: .byte 0
reg_p:  .byte 0

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
;  1. save user value at mem_saveaddr (address of effected memory)
;  2. restore 3 bytes of debug memory at (prev_pc)

startsave:
stepsave:	.byte 0	; opcode to save under BRK

effective_addr:
mem_saveaddr:  .word 0  ; address of byte loaded/stored in a given STEP

op_type:       .byte 0  ; REG, LOAD, or STORE
affected:      .byte 0	; OP_REG_A, etc.; the CPU/RAM state affected by a STEP
op_mode:       .byte 0  ; address modes for current instruction
branch_taken:  .byte 0  ; if !0, a relative branch will be taken next STEP

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

stopwatch:   .res 3	; number of cycles counted since stopwatch is reset
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

; NOTE: this data is stored in its own bank (FINAL_BANK_DEBUG)
; the per-file debug info as described in the above table
.export debuginfo
debuginfo = __BANKCODE_LOAD__+__BANKCODE_SIZE__	; start after shared bank code

;******************************************************************************
; WATCHES
;******************************************************************************
__debug_numwatches:  .byte 0		    ; number of active watches
__debug_watches:     .res MAX_WATCHPOINTS*2 ; addresses of the set watchpoints
__debug_watch_vals:  .res MAX_WATCHPOINTS   ; values of the set watchpoints
__debug_watch_prevs: .res MAX_WATCHPOINTS   ; previous values of watches
__debug_watch_flags: .res MAX_WATCHPOINTS   ; flags for watches (e.g. DIRTY)

; the following are used for watches that represent a range of values
; e.g. [$1000, $1100)
__debug_watches_changed: .res MAX_WATCHPOINTS*2 ; the address that was changed
__debug_watches_stop:    .res MAX_WATCHPOINTS*2 ; end address of watch range

.export __debug_watches
.export __debug_watch_vals
.export __debug_watch_prevs
.export __debug_numwatches
.export __debug_watch_flags
.export __debug_watches_stop

;******************************************************************************
; BREAKPOINTS
.export __debug_numbreakpoints
.export __debug_breakpoints
__debug_numbreakpoints:
numbreakpoints: .byte 0 		; number of active break points
__debug_breakpoints:
breakpoints:    .res MAX_BREAKPOINTS*2  ; addresses of the break points

.export __debug_breakpoint_flags
__debug_breakpoint_flags:
breakpoint_flags: .res MAX_BREAKPOINTS ; active state of breakpoints
breaksave:        .res MAX_BREAKPOINTS ; backup of instructions under the BRKs

steppoint: 	.word 0		; address we STEP from

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
@lines=zp::tmp4
@tmp=zp::tmp6
@segend=zp::tmp8
@cnt=zp::tmpa
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
	ldy #SEG_LINE_COUNT+1
	lda #$00
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
@name=zp::tmp0
@tmp=zp::tmp2
@addr=zp::tmp3
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
	lda #$00
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
@cnt=zp::tmp0
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
@addr=zp::tmp0
@cnt=zp::tmp2
@tmp=zp::tmp2
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
@addr=zp::tmp0
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
@addr=zp::tmp2
@cnt=zp::tmp4
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
	bcc @l0	 ; repeat until we've checked all segments
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
@line=zp::tmp2
@cnt=zp::tmp4
	stxy @line
	lda #$00
	sta @cnt

; we don't store the line/file range for a segment (yet? TODO: ???)
; so just iterate over every line in every segment
@l0:	lda @cnt
	jsr get_segment_by_id
	bcc @checklines
	rts

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
	RETURN_ERR ERR_LINE_NOT_FOUND
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
@src=zp::tmp2
@filename=zp::tmp0
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
	stxy pc

	jsr save_debug_state 		; save the debug state
	ldxy pc
	stxy prev_pc
	jsr vmem::load
	sta startsave

	lda #CUR_SELECT
	sta cur::mode

	ldxy pc
	lda #$00		; BRK
	jsr vmem::store

	lda #ACTION_START
	sta action

; install BRK handler
@install_isr:
	ldxy #debug_brk
	jsr irq::break

	; initialize auxiliary views
	jsr brkpt::init
	lda #$00
	sta aux_mode

	; init state
	sta __debug_numwatches

	jsr reset_stopwatch

	jsr __debug_restore_progstate	; and copy in entire user state to start
	lda #$01
	sta swapmem			; on 1st iteration, swap entire RAM back

	jsr save_debug_zp
	jsr restore_user_zp

@runpc:	CALL FINAL_BANK_USER, pc	; execute the user program until BRK
	rts
.endproc

;******************************************************************************
; INSTALL_BREAKPOINTS
; Install all breakpoints from "breakpoints" EXCEPT for those at the current
; PC. This is to prevent a loop of breakpoints being repeatedly set and
; immediately hit. For a breakpoint to be effective, it must be set at least
; one instruction from the current PC.
.proc install_breakpoints
@brkaddr=zp::tmp0
@cnt=zp::tmp2
	ldx numbreakpoints
	beq @done
	dex
	stx @cnt

@installbrks:
	ldx @cnt
	lda breakpoint_flags,x
	and #BREAKPOINT_ENABLED
	beq @next

	lda @cnt
	asl
	tay
	lda breakpoints,y
	sta @brkaddr
	lda breakpoints+1,y
	sta @brkaddr+1

	; if this breakpoint is at the current PC, don't install it
	ldxy pc
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
@addr=zp::tmp0
@cnt=zp::tmp2
	ldx numbreakpoints
	beq @done
	dex
	stx @cnt
@uninstall:
	ldx @cnt
	lda breakpoint_flags,x
	and #BREAKPOINT_ENABLED
	beq @next

	txa
	asl
	tay
	lda breakpoints,y
	sta @addr
	lda breakpoints+1,y
	sta @addr+1

	ldx @cnt
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
@vicsave=mem::debugsave
@colorsave=mem::debugsave+$110
	ldx #$10
@savevic:
	lda $9000-1,x
	sta @vicsave-1,x
	dex
	bne @savevic

	ldx #$f0
; save $9400-$94f0
@savecolor:
	lda $9400-1,x
	sta @colorsave-1,x
	dex
	bne @savecolor

	; backup the screen
	CALL FINAL_BANK_FASTCOPY2, #fcpy::save
	rts
.endproc

;******************************************************************************
; SAVE_PROG_STATE
; saves memory clobbered by the debugger (screen, ZP, etc.)
.export __debug_save_prog_state
.proc __debug_save_prog_state
@zpsave=mem::prog00
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

	; backup the user $1000 data
	CALL FINAL_BANK_FASTCOPY, #fcpy::save
	rts
.endproc

;******************************************************************************
; DEBUG_BRK
; This is the BRK handler for the debugger.
; It saves the user program's state and other sensitive memory areas that are
; needed by the debugger for display etc, then it restores the debugger's state
; and finally transfers control to the debugger
.proc debug_brk
@file=debugtmp
	; save the registers pushed by the KERNAL interrupt handler ($FF72)
	pla
	sta reg_y
	pla
	sta reg_x
	pla
	sta reg_a
	pla
	sta reg_p
	pla
	sta pc
	pla
	sta pc+1
	tsx
	stx reg_sp

	; clear decimal in case user set it
	cld

	; BRK pushes PC + 2, subtract 2 from PC
	lda pc
	sec
	sbc #$02
	sta pc
	lda pc+1
	sbc #$00
	sta pc+1

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
	sta branch_taken 	; clear branch taken flag

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
	sta affected

@handle_action:
	lda action
	cmp #ACTION_START
	bne :+
	lda startsave		; restore opcode
	ldxy pc
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
	ldxy pc
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
	ldxy pc
	jsr __debug_addr2line	; get the line #
	sta file
	bcs @print		; if we failed to get line #, continue

	inc lineset
	stxy highlight_line
	jsr edit::gotoline

; open the file of the line we BRK'd in
; if the buffer is already loaded switch to it. if not, load it into the
; DEBUG bank
@openfile:
	lda file
	asl
	asl
	asl
	asl
	adc #<filenames
	tax
	lda #>filenames
	adc #$00
	tay
	jsr edit::load
	bcs @print	; couldn't load file, just show BRK
	jsr toggle_highlight	; highlight line

@print:	jsr showstate		; show regs/BRK message
	jsr src::get		; update linebuffer with whatever we're at

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
	beq @debugloop		; we're not ready to return to user program

@done:	; unhighlight the BRK line if it's still visible
	jsr toggle_highlight

@debug_done:
	jsr cur::off
	jsr swapin

@restore_regs:
	; from top to bottom: [STATUS, <PC, >PC]
	lda #FINAL_BANK_USER
	sta fe3::rti_bank	; set default RTI bank
	ldxy pc
	jsr is_internal_address
	bne :+
	lda #FINAL_BANK_MAIN	; if internal address, use main bank
	sta fe3::rti_bank
:	lda pc+1

	sta prev_pc+1
	pha
	lda pc		; restore PC
	sta prev_pc
	pha
	lda reg_p	; restore processor status
	pha

	; install a NOP IRQ
	ldxy #$eb15
	stxy $0314

	jsr save_debug_zp
	jsr restore_user_zp

	lda reg_a
	sta prev_reg_a
	ldx reg_x
	stx prev_reg_x
	ldy reg_y
	sty prev_reg_y

	; return from the BRK
	jmp fe3::bank_rti
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
@vicsave=mem::debugsave	; $0-$10
@colorsave=mem::debugsave+$110
	ldx #$10
@restorevic:
	lda @vicsave-1,x
	sta $9000-1,x
	dex
	bne @restorevic

	ldx #$f0
; save $9400-$94f0
@restorecolor:
	lda @colorsave-1,x
	sta $9400-1,x
	dex
	bne @restorecolor

	; reinit the bitmap
	jsr bm::init
	; restore the screen
	CALL FINAL_BANK_FASTCOPY2, #fcpy::restore
	rts
.endproc

;******************************************************************************
; SAVE DEBUG ZP
; Saves the state of the debugger's zeropage
; TODO: only save/restore the ZP locations clobbered by the debugger
; (will require some overall restructure of ZP usage.
.proc save_debug_zp
@zp=mem::debugsave+$10
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
@zp=mem::debugsave+$10
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
@vicsave=mem::prog9000
@internalmem=mem::prog1000
@colorsave=mem::prog9400
	ldx #$10
@restorevic:
	lda @vicsave-1,x
	sta $9000-1,x
	dex
	bne @restorevic

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
	CALL FINAL_BANK_FASTCOPY, #fcpy::restore
	rts
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
	pla			; debug START return address
	pla
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
	jsr showstate		; restore the state

	lda #AUX_WATCH
	sta aux_mode
	jmp watch::edit
.endproc

;******************************************************************************
; SET_BREAKPOINT
; Sets a breakpoint at the current line selection
.proc set_breakpoint
	jsr edit::setbreakpoint		; set a breakpoint in the source
	ldxy src::line
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
:	jsr key::getch
	beq :-
	jsr __debug_save_prog_state
	jsr restore_debug_state	; restore debugger state
	rts
.endproc

;******************************************************************************
; STEP
; Runs the next instruction from the .PC and returns to the debug prompt.
; This works by inserting a BRK instruction after
; the current instruction and RUNning.
.proc step
@mode=zp::tmp0
	ldxy #$100		; TODO: use ROM addr? (we don't need the string)
	stxy zp::tmp0		; TODO: make way to not disassemble to string
	ldxy pc			; get address of next instruction
	jsr asm::disassemble  ; disassemble it to get its size (next BRK offset)
	stx op_mode
	bcc @ok
	rts		; return error

@ok:	pha		; save instruction size

	; preemptively disable mem swapping
	; unless we encounter ROM for the next target (in which case we will
	; increment this again) we know exactly what will be written
	dec swapmem

	stx @mode		; address mode (set by asm::disassemble)
	ldxy pc			; address of instruction
	jsr get_side_effects	; get state that will be clobbered/used

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
	lda affected
	and #OP_LOAD|OP_STORE	; was there a write to memory?
	beq @setbrk		; if not, skip ahead to setting the next BRK
	ldxy mem_saveaddr	; if so, mark the watch if there is one
	jsr watch::mark		; if there's a watch at this addr, mark it
	bcc @setbrk		; if there's no watch, contiue

	; activate the watch window so user sees change
	lda #(DEBUG_INFO_START_ROW+1)*8
	jsr bm::clrpart
	lda #AUX_WATCH
	sta aux_mode
	jsr watch::view

@setbrk:
	lda #ACTION_STEP
	sta action		; flag that we are STEPing
	pla			; get instruction size
	pha			; save instruction size again
	ldxy pc			; and address of instruction to-be-executed

	jsr next_instruction	; get the address of the next instruction
	stxy steppoint

	jsr vmem::load
	sta stepsave
	lda #$00		; BRK
	ldxy steppoint
	jsr vmem::store

	pla			; get the instruction size
	tax
	lda stepsave		; get the opcode
	jsr count_cycles	; get the # of cycles for the instruction
	clc
	adc stopwatch
	sta stopwatch
	lda stopwatch+1
	adc #$00
	sta stopwatch+1
	lda stopwatch+2
	adc #$00
	sta stopwatch+2

	inc advance		; continue program execution
	rts			; return to the debugger
.endproc

;******************************************************************************
; STEP_RESTORE
; Restores the opcode destroyed by the last STEP.
.proc step_restore
	lda stepsave
	ldxy steppoint
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
; COUNT CYCLES
; Counts the number of cycles that the given instruction will execute
; IN:
;  - .A: the opcode of the instruction
;  - .X: the size of the instruction
; OUT:
;  - .A: the number of cycles the instruction will use
.proc count_cycles
@cycles=zp::tmp0
@opcode=zp::tmp1
@operandsz=zp::tmp2
	dex
	stx @operandsz

	sta @opcode
	lda #$02	; minimum cycles an instruction may take
	sta @cycles

	lda op_mode
	and #MODE_IMMEDIATE
	beq @chkload
	jmp @done

@chkload:
	lda affected
	and #OP_LOAD
	beq @chkstore
	lda #$02	; base # of cycles
	adc @operandsz	; +1 if ZP, +2 if ABS
	sta @cycles

@chkstore:
	lda affected
	and #OP_STORE
	beq @chkind
	inc @cycles	; writes require +1 cycle

@chkind:
	lda op_mode
	and #MODE_INDIRECT
	beq @chkzpindex
	inc @cycles	; 1 cycle for each byte read of the indirect address
	inc @cycles

;Zero page,X, zero page,Y, and (zero page,X) addressing modes spend an extra cycle reading the unindexed zero page address.
@chkzpindex:
	lda op_mode
	and #MODE_ZP
	beq @chkrmw
	lda op_mode
	and #MODE_INDIRECT|MODE_Y_INDEXED
	cmp #MODE_INDIRECT|MODE_Y_INDEXED
	beq @chkindexed		; if (zp),y- skip ahead

	; if zp,y (zp,x) or zp,x add a penalty cycle
	lda op_mode
	and #MODE_Y_INDEXED|MODE_X_INDEXED|MODE_INDIRECT
	beq @chkrmw
	inc @cycles		; zp,y zp,x or (zp,x)

@chkrmw:
	lda affected
	and #OP_LOAD|OP_STORE	; RMW instructions need a cycle to modify
	cmp #OP_LOAD|OP_STORE
	bne @chkindexed
	inc @cycles

@chkindexed:
	lda op_mode
	and #MODE_X_INDEXED|MODE_Y_INDEXED
	beq @chkbra

@handleindexed:
	lda affected
	and #OP_STORE
	beq :+
	inc @cycles		; penalty if write to memory

:	lda effective_addr
	cmp #$ff		; is effective address LSB $ff?
	bne @chkbra		; no penalty if not (no page boundary crossed)
	inc @cycles

@chkbra:
	lda branch_taken
	beq @chkstack		; if no branch was taken continue
	inc @cycles		; +1 cycle if branch taken
	lda steppoint+1		; get target MSB
	cmp pc+1		; is the target on the same page?
	bne @penalty		; different page -> 1 MORE cycle penalty

@chkstack:
	lda affected
	and #OP_STACK|OP_LOAD	; pulling off stack requires extra cycle
				; this will also catch the extra JSR byte read
	cmp #OP_STACK|OP_LOAD
	bne @chkrts
	inc @cycles

@chkrts:
	lda @opcode
	cmp #$60	; RTS needs +1 cycle to inc return address
	beq @penalty
	cmp #$20	; JSR needs +1 cycle to handle return address internally
	bne @done
	inc @cycles
@penalty:
	inc @cycles

@done:	lda @cycles
	rts
.endproc

;******************************************************************************
; NEXT_INSTRUCTION
; Given the address of the current instruction, returns the address of the next
; instruction that will be executed.
; Instructions that are considered for branches are:
;  - JSR
;  - JMP
;  - JMP (indirect)
;  - Bxx (all the conditional branches)
;  - RTI
;  - RTS
; IN:
;  - .XY: the addres of the current instruction
;  - .A: the size of the current instruction
; OUT:
;  - .XY: the address of the next instruction that will be executed
.proc next_instruction
@op=zp::tmp0
@sz=zp::tmp2
@y=zp::tmp3
@msb=zp::tmp4
@opcode=zp::tmp5
@operand=zp::tmp6
	sta @sz
	stxy @op

	; get the opcode
	ldxy @op
	jsr vmem::load
	sta @opcode

	ldxy @op
	lda #$01
	jsr vmem::load_off
	sta @operand

	ldxy @op
	lda #$02
	jsr vmem::load_off
	sta @operand+1

	lda @opcode
	cmp #$20	; JSR?
	bne @notjsr
	jsr @jmpjsr
@jsr:
	cmpw #$c000	; is the target in ROM?
	bcc :+
	; we can't step, our "step" wil actually be more like a "go".
	; re-increment swapmem to force full RAM swap
	lda #ACTION_STEP_OVER
	sta action
	lda #$00
	sta sw_valid	; invalidate stopwatch
	inc swapmem
	bcs @nocontrol	; set breakpoint to PC+3

:	lda step_over	; are we stepping OVER?
	bne @nocontrol	; if yes, don't go into the subroutine
	rts
@notjsr:
	beq @jmpjsr
	cmp #$6c	; JMP (ind)?
	bne :+

	ldxy @operand
	jsr vmem::load
	pha
	incw @operand
	ldxy @operand
	jsr vmem::load
	tay
	pla
	tax
	rts

:	cmp #$4c	; JMP?
	bne @notjmp

; for JMP and JSR just set PC to the operand
@jmpjsr:
	ldxy @operand
	rts

@notjmp:
	cmp #$60
	bne @notrts

; next instruction is stack address + 1
@rts:   ldy reg_sp
	lda $100+1,y
	clc
	adc #$01
	tax
	lda $100+2,y
	adc #$00
	tay
	rts

@notrts:
	cmp #$40
	bne @notrti

@rti:	ldy reg_sp
	lda $100,y
	tax
	lda $100+1,y
	tay

@notrti:
	and #$1f
	cmp #$10
	beq @branch

; not a control-flow instruction, just add the size of the instruction to the PC
@nocontrol:
	lda pc
	clc
	adc @sz
	tax
	lda pc+1
	adc #$00
	tay
	rts

; handle branches. branch is taken if corresponding flag for top two bits
; of branch opcode equal bit 5 of the opcode
; e.g. 11010000 will branch if the Z (zero) flag, which is the flag represented
; by bits 6 & 7 (11) is clear- the 0 in bit 5 in this example.
@branch:
	; get top 2 bits (xx) of branch to get type of branch
	lda @opcode
	asl
	rol
	rol
	and #$03
	tax

	; get the y (bit 5) in the same bit position as the flag we're testing
	lda @opcode
	and #$20
	beq :+			; if bit y is 0, use $00
	lda branch_masks,x	; if bit y is 1, use the mask
:	sta @y

	lda branch_masks,x
	and reg_p	; isolate the bit we're interested in
	eor @y		; if y != .P[xx], no branch
	beq @takebranch

; branch isn't taken, just add 2 to the PC
@nobranch:
	lda pc
	clc
	adc #$02
	tax
	lda pc+1
	adc #$00
	tay
	rts

; branch is taken, add the operand (offset) + 2
@takebranch:
	inc branch_taken
	lda @operand
	bpl :+
	lda #$ff
	skw
:	lda #$00
	sta @msb

	lda pc
	clc
	adc @operand
	tax

	iny
	lda pc+1
	adc @msb
	tay

	txa
	clc
	adc #$02
	tax
	tya
	adc #$00
	tay
	rts

.endproc

;******************************************************************************
; GET SIDE EFFECTS
; Checks if the given instruction requires any RAM state and handles the
; creation of any state needed to handle them.
; This essentially involves checking if the instruction accesses any RAM and,
; if it does, settting mem_saveaddr to the address that will be affected.
; IN:
;  - .XY: address of the binary instruction
;  - .A: the size of the instruction
;  - zp::tmp0: the address modes for the instruction (see asm::disassemble)
; OUT:
;  - .C:           clear if the instruction is desctructive
;  - mem_saveaddr: holds the address of the byte that will be loaded/stored
;  - affected:     stores the flags with the CPU/mem state the operation affects
.proc get_side_effects
@instruction=zp::tmp2
@opsz=zp::tmp4
@target=zp::tmp5
@opcode=zp::tmp7
@offset=zp::bankval
	stxy @instruction
	sta @opsz

	; if 0 byte opcode, it's either PHA, PHP or doesn't touch RAM
	cmp #$00
	bne @cont
	sec			; no memory side-effects; TODO: check PHA
	rts

	; save the debugger's contents at the @instruction address
@cont:	; get the instruction opcode/operand at the @instruction address
	ldxy @instruction
	jsr vmem::load			; opcode
	sta @opcode

	incw @instruction
	ldxy @instruction
	jsr vmem::load			; operand (1st byte)
	sta @target

	lda #$00
	ldx @opsz
	cpx #$03
	bcc :+
	incw @instruction
	ldxy @instruction
	jsr vmem::load			; operand (2nd byte)
:	sta @target+1

	; get the side-effects for this operation
	ldx @opcode
	lda side_effects_tab,x
	sta affected			; save the side-effects for aux uses

	and #OP_STORE | OP_LOAD		; is operation a load or store?
	bne :+
	ldxy #$6666
	stxy mem_saveaddr		; don't save anything

:	; get effective target address of this instruction and save it
	; we will save/restore state before/after a BRK using this
	jsr get_effective_addr
	stxy mem_saveaddr
	rts
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
@cnt=zp::tmp0
@addr=zp::tmp1
@tosave=zp::tmp3
	lda swapmem
	beq @fastswap

@swapall:
	; swap entire user RAM in (needed if we don't know what memory will
	; be changed before next BRK)
	jsr save_debug_state
	jmp __debug_restore_progstate

@fastswap:
	; save [mem_saveaddr], [step_point], and [pc, pc+2] for the debugger
	lda pc+1
	sta @tosave+1
	sta @tosave+3
	sta @tosave+5
	ldx pc
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

	ldxy mem_saveaddr
	stxy @tosave+6
	ldxy steppoint
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
@cnt=zp::tmp0
@addr=zp::tmp1
@ysave=zp::tmp3
@tosave=zp::tmp4
	lda swapmem
	beq @fastswap
	jmp @swapall

@fastswap:
	; save [prevpc, prevpc+2], [msave], and [steppoint] for the user
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

	ldxy mem_saveaddr
	stxy @tosave+6
	ldxy steppoint
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
	sta mem::debugsave+$10,x
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
; GET EFFECTIVE ADDR
; Returns the effective address for the given opcode/operand. This may be the
; same as the operand (if the opcode represents a zeropage or absolute address
; mode) or not (indirect or indexed address modes)
; IN:
;  - .XY:      the operand
;  - .A:       the opcode
;  - zp::tmp0: the address mode of the instruction
; OUT:
;  - .XY: the effective address of the instruction
.proc get_effective_addr
@target=zp::tmp5
	lda op_mode
	cmp #MODE_X_INDEXED|MODE_ZP|MODE_INDIRECT	; x,ind?
	bne @check_ind_y
@ind_x:
	; add .X register to ZP target to get target address (wrapping is fine)
	; THEN load the address at this target
	ldx reg_x
	ldy @target
	lda mem::prog00,y
	sta @target
	iny
	lda mem::prog00,x
	sta @target+1
	jmp @get_ind

;--------------------------------------
@check_ind_y:
	lda op_mode
	cmp #MODE_Y_INDEXED|MODE_ZP|MODE_INDIRECT	; y,ind?
	bne @check_rel_y
	; get the value of the ZP location in the *user* ZP
	ldy @target
	lda mem::prog00,y
	iny
	lda mem::prog00,y
	sta @target+1
	; add the .Y register value to the address from the ZP
	lda reg_y
	clc
	adc @target
	sta @target
	bcc :+
	inc @target+1
:	jmp @done

;--------------------------------------
@check_rel_y:
	lda op_mode
	and #MODE_Y_INDEXED		; y indexed?
	beq @check_rel_x
	; add the value of .Y to the target get the target address
	lda reg_y
	clc
	adc @target
	sta @target
	bcc :+
	inc @target+1
:	lda op_mode
	and #MODE_ZP
	beq @done
	lda #$00		; if ZP,y clear the MSB of target
	sta @target+1
	beq @done

;--------------------------------------
@check_rel_x:
	lda op_mode
	and #MODE_X_INDEXED		; x indexed?
	beq @check_ind
	; add the value of .X to get the target address
	lda reg_x
	clc
	adc @target
	sta @target
	bcc @done
	inc @target+1
	lda op_mode
	and #MODE_ZP
	beq @done
	lda #$00		; if ZP,x clear the MSB of target
	sta @target+1
	beq @done

@check_ind:
	lda op_mode
	cmp #MODE_INDIRECT|MODE_ABS	; JMP (ind) ?
	bne @abs_or_zp
	; read the target of the indirect JMP and set @target to it
@get_ind:
	ldxy @target
	jsr vmem::load
	pha
	incw @target
	ldxy @target
	jsr vmem::load
	sta @target+1
	pla
	sta @target

@abs_or_zp:
@done:	ldxy @target
	RETURN_OK
.endproc


;******************************************************************************
; TOGGLE_BREAKPOINT
; Sets a breakpoint at the address in .XY or removes it if one already exists
; IN:
;  - .XY: the address of the breakpoint to set
.export __debug_toggle_breakpoint
.proc __debug_toggle_breakpoint
	; if this is a duplicate, remove the existing breakpoint
	jsr remove_breakpoint
	bcc @done		; breakpoint existed, but we removed it

	txa
	pha

	lda numbreakpoints
	asl
	tax

	pla
	sta breakpoints,x
	tya
	sta breakpoints+1,x

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
	asl
	tax

@remove:
	; shift breakpoints down
	lda numbreakpoints
	asl
	sta @end
	cpx @end
	beq @removed
@l1:	lda breakpoints+2,x
	sta breakpoints,x
	lda breakpoints+3,x
	sta breakpoints+1,x
	inx
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
; IN:
;  - .XY: the address to return the breakpoint for
; OUT:
;  - .C:  set if no breakpoint is found
;  - .A: the id of the breakpoint
.proc get_breakpoint
@addr=debugtmp
	stxy @addr
	lda numbreakpoints
	beq @notfound
	asl
	tax
@l0:	lda @addr
	cmp breakpoints,x
	bne @next
	lda @addr+1
	cmp breakpoints+1,x
	beq @found
@next:	dex
	dex
	bpl @l0
@notfound:
	ldxy @addr
	sec
	rts		; not found - nothing to remove

@found: txa
	lsr
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
; align x-position to either a value in @offsets or a value in @offests+1
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
	sta register_state,x

	iny
	lda (@val),y		; get LSB
	jsr util::chtohex
	ora register_state,x
	sta register_state,x

	dex
	bpl @l0

	; swap PC LSB and MSB
	ldx pc
	lda pc+1
	sta pc
	stx pc+1

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
	jsr text::putz

	ldy #39
	lda #' '
:	sta @buff,y
	dey
	bpl :-

	; draw .Y
	lda reg_y
	jsr util::hextostr
	sty @buff+11
	stx @buff+12

	; draw .X
	lda reg_x
	jsr util::hextostr
	sty @buff+8
	stx @buff+9

	; draw .A
	lda reg_a
	jsr util::hextostr
	sty @buff+5
	stx @buff+6

	; draw .P (status)
	lda reg_p
	sta @tmp
	lda #$80
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
	lda reg_sp
	jsr util::hextostr
	sty @buff+14
	stx @buff+15

@getaddr:
	lda pc+1
	jsr util::hextostr
	sty @buff
	stx @buff+1

	lda pc
	jsr util::hextostr
	sty @buff+2
	stx @buff+3

; if registers were affected, highlight them
	ldx #TEXT_COLOR
	lda affected
	and #OP_REG_A
	beq :+
	ldx #DEBUG_REG_CHANGED_COLOR
:	stx COLMEM_ADDR+(20*$b)+2
	stx COLMEM_ADDR+(20*$b)+3

	ldx #TEXT_COLOR
	lda affected
	and #OP_REG_X
	beq :+
	ldx #DEBUG_REG_CHANGED_COLOR
:	stx COLMEM_ADDR+(20*$b)+4

	ldx #TEXT_COLOR
	lda affected
	and #OP_REG_Y
	beq :+
	ldx #DEBUG_REG_CHANGED_COLOR
:	stx COLMEM_ADDR+(20*$b)+5
	stx COLMEM_ADDR+(20*$b)+6

	ldx #TEXT_COLOR
	lda affected
	and #OP_STACK
	beq :+
	ldx #DEBUG_REG_CHANGED_COLOR
:	stx COLMEM_ADDR+(20*$b)+7

; if memory was loaded or stored, show the effective address
@memaddr:
	lda affected
	and #OP_LOAD|OP_STORE
	bne :+
	lda #'-'
	sta @buff+27
	sta @buff+28
	sta @buff+29
	sta @buff+30
	bne @clk

:	lda mem_saveaddr+1
	jsr util::hextostr
	sty @buff+27
	stx @buff+28
	lda mem_saveaddr
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

:	ldx stopwatch
	ldy stopwatch+1
	lda stopwatch+2
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
	lda pc
	pha
	lda pc+1
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
@info=zp::tmp0
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
	sta stopwatch
	sta stopwatch+1
	sta stopwatch+2
	rts
.endproc
;******************************************************************************
; End the .DEBUG segment

;******************************************************************************
.RODATA
;******************************************************************************
; OERATION SIDE EFFECTS TABLE
; This table contains all opcodes and what state they affect.
; This is used to display changes made by a given instruction as well as
; determine what (internal) state the debugger needs to preserve and restore
; between steps.
; If a value in internal memory is accessed (OP_LOAD/OP_STORE) the user
; value for that memory must be swapped in before executing the instruction
; $0-
side_effects_tab:
.byte $00			; $00 BRK
.byte OP_REG_A|OP_LOAD		; $01 ORA x,ind
.byte $00			; ---
.byte $00			; ---
.byte $00			; ---
.byte OP_REG_A|OP_LOAD		; $05 ORA zpg
.byte OP_LOAD|OP_STORE 		; $06 ASL zpg
.byte $00			; ---
.byte OP_STACK|OP_STORE		; $08 PHP
.byte OP_REG_A			; $09 ORA #
.byte OP_REG_A			; ASL A
.byte $00			; ---
.byte $00			; ---
.byte OP_LOAD|OP_REG_A		; $0d ORA abs
.byte OP_LOAD|OP_STORE		; $0e ASL abs
.byte $00			; ---

; $1-
.byte OP_PC			; $10 BPL rel
.byte OP_REG_A|OP_LOAD		; $11 ORA ind,y
.byte $00			; ---
.byte $00			; ---
.byte $00			; ---
.byte OP_REG_A|OP_LOAD		; $15 ORA zpg,x
.byte OP_LOAD|OP_STORE 		; $16 ASL zpg,x
.byte $00			; ---
.byte OP_FLAG			; $18 CLC
.byte OP_REG_A|OP_LOAD		; $19 ORA abs,y
.byte $00			; ---
.byte $00			; ---
.byte $00			; ---
.byte OP_LOAD|OP_REG_A		; $1d ORA abs,x
.byte OP_LOAD|OP_STORE		; $1e ASL abs,x
.byte $00			; ---

; $2-
.byte OP_PC|OP_STACK|OP_STORE	; $20 JSR abs
.byte OP_REG_A|OP_LOAD		; $21 AND x,ind
.byte $00			; ---
.byte $00			; ---
.byte OP_FLAG|OP_LOAD		; $24 BIT zpg
.byte OP_REG_A|OP_LOAD		; $25 AND zpg
.byte OP_LOAD|OP_STORE 		; $26 ROL zpg
.byte $00			; ---
.byte OP_FLAG|OP_STACK|OP_LOAD	; $28 PLP
.byte OP_REG_A     		; $29 AND #
.byte OP_REG_A			; $2a ROL A
.byte $00			; ---
.byte OP_FLAG|OP_LOAD		; $2c BIT abs
.byte OP_LOAD|OP_REG_A		; $2d AND abs
.byte OP_LOAD|OP_STORE	        ; $2e ROL abs
.byte $00			; ---

; $3-
.byte OP_PC			; $30 BMI rel
.byte OP_REG_A|OP_LOAD		; $31 AND ind,y
.byte $00			; ---
.byte $00			; ---
.byte $00			; ---
.byte OP_REG_A|OP_LOAD		; $35 AND zpg,x
.byte OP_LOAD|OP_STORE 		; $36 ROL zpg,x
.byte $00			; ---
.byte OP_FLAG			; $38 SEC
.byte OP_REG_A|OP_LOAD    	; $39 AND abs,y
.byte $00			; ---
.byte $00			; ---
.byte $00			; ---
.byte OP_LOAD|OP_REG_A		; $3d AND abs,x
.byte OP_LOAD|OP_STORE	        ; $3e ROL abs,x
.byte $00			; ---

; $4-
.byte OP_STACK|OP_PC|OP_LOAD|OP_FLAG	; $40 RTI
.byte OP_REG_A|OP_LOAD			; $42 EOR x, ind
.byte $00				; ---
.byte $00				; ---
.byte $00				; ---
.byte OP_REG_A|OP_LOAD			; $45 EOR zpg
.byte OP_LOAD|OP_STORE 			; $46 LSR zpg
.byte $00				; ---
.byte OP_STACK|OP_STORE			; $48 PHA
.byte OP_REG_A|OP_LOAD    		; $49 EOR #
.byte OP_REG_A                  	; $4a LSR A
.byte $00				; ---
.byte OP_PC				; $4c JMP abs
.byte OP_LOAD|OP_REG_A			; $4d EOR abs
.byte OP_LOAD|OP_STORE			; $4e LSR abs
.byte $00				; ---

; $5-
.byte OP_PC			; $50 BVC rel
.byte OP_REG_A|OP_LOAD		; $51 EOR ind,y
.byte $00			; ---
.byte $00			; ---
.byte $00			; ---
.byte OP_REG_A|OP_LOAD		; $55 EOR zpg,x
.byte OP_LOAD|OP_STORE 		; $56 LSR zpg,x
.byte $00			; ---
.byte OP_FLAG			; $58 CLI
.byte OP_REG_A|OP_LOAD    	; $59 EOR abs,y
.byte $00			; ---
.byte $00			; ---
.byte $00			; ---
.byte OP_LOAD|OP_REG_A		; $5d EOR abs,x
.byte OP_LOAD|OP_STORE	        ; $5e LSR abs,x
.byte $00			; ---

; $6-
.byte OP_STACK|OP_PC|OP_LOAD	; $60 RTS
.byte OP_REG_A|OP_LOAD		; $61 ADC x,ind
.byte $00			; ---
.byte $00			; ---
.byte $00			; ---
.byte OP_REG_A|OP_LOAD		; $65 ADC zpg
.byte OP_LOAD|OP_STORE 		; $66 ROR zpg
.byte $00			; ---
.byte OP_STACK|OP_LOAD|OP_REG_A	; $68 PLA
.byte OP_REG_A|OP_LOAD    	; $69 ADC #
.byte OP_REG_A                  ; $6a ROR A
.byte $00			; ---
.byte OP_PC|OP_LOAD		; $4c JMP ind
.byte OP_LOAD|OP_REG_A		; $4d ADC abs
.byte OP_LOAD|OP_STORE	        ; $4e ROR abs
.byte $00			; ---

; $7-
.byte OP_PC			; $70 BVS rel
.byte OP_REG_A|OP_LOAD		; $71 ADC ind,y
.byte $00			; ---
.byte $00			; ---
.byte $00			; ---
.byte OP_REG_A|OP_LOAD		; $55 ADC zpg,x
.byte OP_LOAD|OP_STORE 		; $56 ROR zpg,x
.byte $00			; ---
.byte OP_FLAG			; $58 SEI
.byte OP_REG_A|OP_LOAD    	; $59 ADC abs,y
.byte $00			; ---
.byte $00			; ---
.byte $00			; ---
.byte OP_LOAD|OP_REG_A		; $5d ADC abs,x
.byte OP_LOAD|OP_STORE  	; $5e ROR abs,x
.byte $00			; ---

; $8-
.byte $00			; ---
.byte OP_STORE			; $81 STA x,ind
.byte $00			; ---
.byte $00			; ---
.byte OP_STORE			; $84 STY zpg
.byte OP_STORE   		; $85 STA zpg
.byte OP_STORE 		        ; $86 STX zpg
.byte $00			; ---
.byte OP_REG_Y              	; $98 DEY
.byte $00			; ---
.byte OP_REG_A                  ; $8a TXA
.byte $00			; ---
.byte OP_STORE			; $8c STY abs
.byte OP_STORE			; $8d STA abs
.byte OP_STORE			; $8e STX abs
.byte $00			; ---

; $9-
.byte OP_PC			; $90 BCC rel
.byte OP_STORE			; $91 STA ind,y
.byte $00			; ---
.byte $00			; ---
.byte OP_STORE			; $94 STY zpg,x
.byte OP_STORE   		; $95 STA zpg,x
.byte OP_STORE 		        ; $96 STX zpg,y
.byte $00			; ---
.byte OP_REG_A              	; $98 TYA
.byte $00			; ---
.byte OP_STORE                	; $99 STA abs,Y
.byte OP_STACK			; $9a TXS
.byte $00			; ---
.byte OP_STORE			; $9d STA abs,x
.byte OP_STORE			; ---
.byte $00			; ---

; $a-
.byte OP_REG_Y			; $a0 LDY #
.byte OP_LOAD|OP_REG_A		; $a1 LDA x,ind
.byte OP_REG_X			; $a2 LDX #
.byte $00			; ---
.byte OP_LOAD|OP_REG_Y		; $a4 LDY zpg
.byte OP_LOAD|OP_REG_A 		; $a5 LDA zpg
.byte OP_LOAD|OP_REG_X	        ; $a6 LDX zpg
.byte $00			; ---
.byte OP_REG_Y              	; $a8 TAY
.byte OP_REG_A			; $a9 LDA #
.byte OP_REG_X                  ; $aa TAX
.byte $00			; ---
.byte OP_LOAD|OP_REG_Y		; $ac LDY abs
.byte OP_LOAD|OP_REG_A		; $ad LDA abs
.byte OP_LOAD|OP_REG_X		; $ae LDX abs
.byte $00			; ---

; $b-
.byte OP_PC                     ; $b0 BCS rel
.byte OP_LOAD|OP_REG_A		; $b1 LDA ind,y
.byte $00			; ---
.byte $00			; ---
.byte OP_LOAD|OP_REG_Y		; $b4 LDY zpg,x
.byte OP_LOAD|OP_REG_A 		; $b5 LDA zpg,x
.byte OP_LOAD|OP_REG_X	        ; $b6 LDX zpg,y
.byte $00			; ---
.byte OP_FLAG               	; $b8 CLV
.byte OP_REG_A|OP_LOAD		; $b9 LDA abs,y
.byte OP_REG_X                  ; $ba TSX
.byte $00			; ---
.byte OP_LOAD|OP_REG_Y		; $bc LDY abs,y
.byte OP_LOAD|OP_REG_A		; $ad LDA abs,x
.byte OP_LOAD|OP_REG_X		; $ae LDX abs,y
.byte $00			; ---

; $c-
.byte OP_FLAG                   ; $c0 CPY #
.byte OP_LOAD|OP_FLAG           ; $c1 CMP x,ind
.byte $00			; ---
.byte $00			; ---
.byte OP_FLAG|OP_LOAD           ; $c4 CPY zpg
.byte OP_LOAD|OP_FLAG 		; $c5 CMP zpg
.byte OP_LOAD|OP_STORE|OP_FLAG  ; $c6 DEC zpg
.byte $00			; ---
.byte OP_REG_Y|OP_FLAG          ; $c8 INY
.byte OP_FLAG                   ; $c9 CMP #
.byte OP_REG_X|OP_FLAG          ; $ca DEX
.byte $00			; ---
.byte OP_FLAG|OP_LOAD   	; $cc CPY abs
.byte OP_FLAG|OP_LOAD     	; $cd CMP abs
.byte OP_LOAD|OP_STORE		; $ce DEC abs
.byte $00			; ---

; $d-
.byte OP_PC                     ; $b0 BNE rel
.byte OP_LOAD|OP_FLAG 		; $b1 CMP ind,y
.byte $00			; ---
.byte $00			; ---
.byte $00			; ---
.byte OP_LOAD|OP_FLAG  		; $d5 CMP zpg,x
.byte OP_LOAD|OP_STORE	        ; $d6 DEC zpg,y
.byte $00			; ---
.byte OP_FLAG               	; $d8 CLD
.byte OP_REG_A|OP_STORE|OP_FLAG	; $d9 CMP abs,y
.byte $00			; ---
.byte $00			; ---
.byte $00			; ---
.byte OP_LOAD|OP_FLAG		; $dd CMP abs,x
.byte OP_LOAD|OP_STORE|OP_FLAG	; $de DEC abs,y
.byte $00			; ---

; $e-
.byte OP_FLAG                   ; $e0 CPX #
.byte OP_LOAD|OP_FLAG|OP_REG_A  ; $e1 SBC x,ind
.byte $00			; ---
.byte $00			; ---
.byte OP_FLAG|OP_LOAD           ; $e4 CPX zpg
.byte OP_LOAD|OP_FLAG|OP_REG_A	; $e5 SBC zpg
.byte OP_LOAD|OP_STORE|OP_FLAG  ; $e6 INC zpg
.byte $00			; ---
.byte OP_REG_X|OP_FLAG          ; $e8 INX
.byte OP_REG_A	                ; $e9 SBC #
.byte $00			; $ea NOP
.byte $00			; ---
.byte OP_FLAG|OP_LOAD   	; $ec CPX abs
.byte OP_FLAG|OP_LOAD|OP_REG_A 	; $ed SBC abs
.byte OP_LOAD|OP_STORE|OP_FLAG	; $ee INC abs
.byte $00			; ---

; $e-
.byte OP_PC                     ; $f0 BEQ rel
.byte OP_LOAD|OP_FLAG 		; $f1 SBC ind,y
.byte $00			; ---
.byte $00			; ---
.byte $00			; ---
.byte OP_LOAD|OP_FLAG|OP_REG_A 	; $f5 SBC zpg,x
.byte OP_LOAD|OP_STORE	        ; $e6 INC zpg,x
.byte $00			; ---
.byte OP_FLAG               	; $f8 SED
.byte OP_REG_A|OP_FLAG|OP_LOAD  ; $f9 SBC abs,y
.byte $00			; ---
.byte $00			; ---
.byte $00			; ---
.byte OP_LOAD|OP_FLAG|OP_REG_A	; $fd SBC abs,x
.byte OP_LOAD|OP_STORE|OP_FLAG	; $fe INC abs,x
.byte $00			; ---

;******************************************************************************
; corresponding masks for top 2 bits of opcode to flags in the status register
branch_masks:
.byte $80	; negative
.byte $40	; overflow
.byte $01	; carry
.byte $02	; zero

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
