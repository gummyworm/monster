.include "asm.inc"
.include "bitmap.inc"
.include "breakpoints.inc"
.include "config.inc"
.include "cursor.inc"
.include "edit.inc"
.include "errors.inc"
.include "fastcopy.inc"
.include "finalex.inc"
.include "labels.inc"
.include "irq.inc"
.include "key.inc"
.include "layout.inc"
.include "macros.inc"
.include "memory.inc"
.include "source.inc"
.include "string.inc"
.include "text.inc"
.include "util.inc"
.include "view.inc"
.include "zeropage.inc"

.import __DEBUGGER_LOAD__
.import __DEBUGGER_SIZE__
.import __BANKCODE_LOAD__
.import __BANKCODE_SIZE__

;******************************************************************************
; Debug info constants
MAX_FILES = 24		; max files that debug info may be generated for
MAX_SEGMENTS=32		; max segments that debug info may be generated for
MAX_BREAKPOINTS = 16	; max number of breakpoints that may be set
MAX_WATCHPOINTS = 8	; max number of watchpoints that may be set
MAX_LINES  = 8000	; max number of lines across all segments

SEG_START_ADDR = 0      ; offset in segment header for start address
SEG_STOP_ADDR = 2       ; offset in segment header for stop address
SEG_LINE_COUNT = 4	; offset in segment header for line count
DATA_FILE = 0		; offset of FILE ID in debug info
DATA_LINE = 1		; offset of line number in debug info
DATA_ADDR = 3		; offset of line address in debug info

AUX_MEM   = 1		; enables the memory viewer in the debug view
AUX_BRK   = 2		; enables the breakpoint view in the debug view
AUX_WATCH = 3		; enables the watchpoint view in the debug view

BREAKPOINT_ENABLED = 1

;******************************************************************************
; STEP constants
; There are some important differences between INTO and OVER
; If we step INTO the next instruction, we can know a lot about what is being
; done between our current instruction and the next one.
; Most crucially, we can know if the instruction is writing to or reading from
; memory.
; If we step OVER an instruction (this applies only to JSR), then we must guess
; if memory was written to by comparing its value when we return from the
; subroutine with its value before that call.
STEP_INTO = 1
STEP_OVER = 2

;******************************************************************************
; Debug info pointers
file = zp::debug       ; current file id being worked on
addr = zp::debug+1     ; address of next line/addr to store
seg  = zp::debug+3     ; address of current segment pointer
line = zp::debug+5
srcline = zp::debug+7
segstart = zp::debug+9
segstop = zp::debug+$b
break_after_sr = zp::debug+$d	; if !0, NEXT_INSTRUCTION will skip subroutines
debugtmp = zp::debug+$10

.export __debug_src_line
__debug_src_line = srcline ; the line # stored by dbg::storeline
.export __debug_file
__debug_file = file

;******************************************************************************
; Program state variables
.BSS
reg_a:  .byte 0
reg_x:  .byte 0
reg_y:  .byte 0
reg_p:  .byte 0
reg_sp: .byte 0
pc:     .word 0

step_mode: .byte 0	; which type of stepping we're doing (INTO, OVER)
lineset:   .byte 0	; not zero if we know the line number we're on

; backup of the memory value being affected by the current instruction
; if it is destructive
mem_save:     .byte 0	; byte that was clobbered
mem_saveaddr: .word 0	; addrses of affected byte

aux_mode:         .byte 0	; the active auxiliary view
auto_swap_memory: .byte 0	; if 1, ALL memory will be swapped on BRK
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
segmentnames: .byte 0

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
; TODO: support .SEG
; TODO: store more compactly? e.g. store offsets for line/addr from previous
;******************************************************************************

; NOTE: this data is stored in its own bank (FINAL_BANK_DEBUG)
; the per-file debug info as described in the above table
.export debuginfo
debuginfo = __BANKCODE_LOAD__+__BANKCODE_SIZE__	; start after shared bank code

; backup address (in bank FINAL_BANK_DEBUG)
debug_backup = $a000

;******************************************************************************
; WATCHES
;******************************************************************************
numwatches: .byte 0		   ; number of active watches
watches:    .res MAX_WATCHPOINTS*2 ; addresses of the set watchpoints

;******************************************************************************
; BREAKPOINTS
;******************************************************************************
.export __debug_numbreakpoints
.export __debug_breakpoints
__debug_numbreakpoints:
numbreakpoints: .byte 0 		; number of active break points
__debug_breakpoints:
breakpoints:    .res MAX_BREAKPOINTS*2  ; addresses of the break points
.export __debug_breakpoint_flags
__debug_breakpoint_flags:
breakpoint_flags: .res MAX_BREAKPOINTS  ; active state of breakpoints
breaksave: .res MAX_BREAKPOINTS      ; backup of the instructions under the BRK

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
	sta numwatches
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
; out:
;  - .C: set if an error occurred
.export __debug_setup
.proc __debug_setup
@numsegs=zp::tmp0
@lines=zp::tmp4
@tmp=zp::tmp6
@segend=zp::tmp8
@cnt=@tmp
	lda numsegments
	beq @done
	sta @numsegs

	; start of data is 6*numsegments
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

	lda #$01
	sta @cnt
	cmp numsegments
	beq @done	; if there's only 1 segment, we're done

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

	inc @cnt
	lda @cnt
	cmp numsegments
	bne @l0

@done:	RETURN_OK
.endproc

;******************************************************************************
; INITSEG
; Initializes a segment (as with .ORG)
; IN:
;  .XY: the start address of the segment
.export __debug_init_segment
.proc __debug_init_segment
@tmp=zp::tmp1
@addr=zp::tmp2
	stxy @addr

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

	clc
	adc seg
	tax
	lda seg+1
	adc #$00
	tay
	lda #FINAL_BANK_DEBUG
	jsr fe3::load ; read the byte

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

	tya
	clc
	adc seg
	tax
	lda seg+1
	adc #$00
	tay
	lda #FINAL_BANK_DEBUG
	jsr fe3::store	; write the byte

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

	tya
	clc
	adc line
	tax
	lda line+1
	adc #$00
	tay
	lda #FINAL_BANK_DEBUG
	jsr fe3::store	; write the byte
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

	tya
	clc
	adc line
	tax
	lda line+1
	adc #$00
	tay
	lda #FINAL_BANK_DEBUG
	jsr fe3::load ; read the byte

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

	tya
	clc
	adc addr
	tax
	lda addr+1
	adc #$00
	tay
	lda #FINAL_BANK_DEBUG
	jsr fe3::store	; write the byte

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

	tya
	clc
	adc addr
	tax
	lda addr+1
	adc #$00
	tay
	lda #FINAL_BANK_DEBUG
	jsr fe3::load ; read the byte

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
;  - .XY: name of the segment
; OUT:
;  - .C: set on error
.export __debug_startsegment_byname
.proc __debug_startsegment_byname
@name=zp::tmp0
	stxy @name

	; find the address of the segment from its name
	; TODO:
	RETURN_OK
.endproc


;******************************************************************************
; STARTSEGMENT_BYADDR
; Activates the initialized segment (see dbg::initseg) from the given address
; Lines stored after the call to this routine will be stored in the debug info
; for this segment.
; This is for use with the .ORG directive
; IN:
;  - .XY: name of the segment
; OUT:
;  - .C: set on error
.export __debug_startsegment_byaddr
.proc __debug_startsegment_byaddr
@addr=zp::tmp0
@seg=zp::tmp2
@cnt=zp::tmp4
	stxy @addr
	lda numsegments
	sta @cnt

	; find the segment with this start address
	lda #<debuginfo
	sta @seg
	lda #>debuginfo
	sta @seg+1

	; install dummy IRQ
	ldxy #$eb15	; ack interrupts and RTI
	stxy $0314

@l0:	ldy #SEG_START_ADDR

	ldxy @seg
	lda #FINAL_BANK_DEBUG
	jsr fe3::load

	cmp @addr
	bne @next
	iny

	incw @seg
	ldxy @seg
	lda #FINAL_BANK_DEBUG
	jsr fe3::load

	pha
	decw @seg
	pla

	cmp @addr+1
	beq @found

@next:	lda @seg
	clc
	adc #$06
	sta @seg
	bcc :+
	inc @seg+1
:	dec @cnt
	bne @l0

	sec
	rts		; error; not found

@found: lda @seg
	clc
	adc #$06	; sizeof(seg_header)
	sta addr
	lda @seg+1
	adc #$00
	sta addr+1
	RETURN_OK
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
	jsr write_to_seg
	rts
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
	adc #$01	; +1
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
	lda @addr
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
	lda numfiles
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
;  - .C: set if no filename could be resolved
.export __debug_get_filename
.proc __debug_get_filename
	cmp numfiles
	bcs @done
	asl
	asl
	asl
	asl
	adc #<filenames
	tax
	lda #>filenames
	adc #$00
	tay
@done:	rts
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
@src=zp::tmp0
@filename=zp::tmp2
	stxy @src

@getfiledst:
	; find the location to store the filename to
	lda numfiles
	asl		; *2
	asl		; *4
	asl		; *8
	asl		; *16
	adc #<filenames
	sta @filename
	lda #>filenames
	adc #$00
	sta @filename+1

	ldy #$00
@copyfilename:
	lda (@src),y
	sta (@filename),y
	beq :+
	iny
	bne @copyfilename
:	lda numfiles
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

	; install a breakpoint at the first address
	ldxy pc
	jsr __debug_setbreakpoint
	jsr install_breakpoints

; install BRK handler
@install_isr:
	ldxy #debug_brk
	jsr irq::break

	; initialize auxiliary views
	jsr brkpt::init
	lda #$00
	sta aux_mode

	jsr save_debug_state ; save the debug state
	jsr __debug_restore_progstate

; execute the user program until BRK
@runpc:
	CALL FINAL_BANK_USER, pc
	rts
.endproc

;******************************************************************************
; INSTALL_BREAKPOINTS
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

	bank_read_byte #FINAL_BANK_USER, @brkaddr

	ldx @cnt
	sta breaksave,x		; save the instruction under the new BRK
	lda #$00		; BRK

	bank_store_byte #FINAL_BANK_USER, @brkaddr

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
	bank_store_byte #FINAL_BANK_USER, @addr
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
@savezp=mem::debugsave+$10
@colorsave=mem::debugsave+$110
	ldx #$10
@savevic:
	lda $9000-1,x
	sta @vicsave-1,x
	dex
	bne @savevic
@save_zp:
	lda $00,x
	sta @savezp,x
	dex
	bne @save_zp

	ldx #$f0
; save $9400-$94f0
@savecolor:
	lda $9400-1,x
	sta @colorsave-1,x
	dex
	bne @savecolor

	; backup the screen
	CALL FINAL_BANK_FASTCOPY, #fcpy::save
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

@save_zp:
	lda $00,x
	sta @zpsave,x
	dex
	bne @save_zp

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
	CALL FINAL_BANK_FASTCOPY2, #fcpy::save
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

	; save the program state before we restore the debugger's
	jsr __debug_save_prog_state

; wait for key to re-activate debugger
@waitkey:
	jsr key::getch
	beq @waitkey

	; BRK pushes PC + 2, subtract 2 from PC
	lda pc
	sec
	sbc #$02
	sta pc
	lda pc+1
	sbc #$00
	sta pc+1

	jsr restore_debug_state	; restore debugger state
	jsr showregs		; display the contents of the registers
	jsr show_aux		; display the auxiliary mode

	; remove the breakpoint at the current PC (that caused this BRK)
	jsr uninstall_breakpoints
	ldxy pc
	jsr remove_breakpoint	  ; remove BRK @ the current PC

@showbrk:
	; get the address before the BRK and go to it
	lda #$00
	sta lineset		; flag that line # is (yet) unknown
	ldxy pc
	jsr __debug_addr2line	; get the line #
	sta file
	bcs @print
	inc lineset
	stxy line
; open the file of the line we BRK'd in
; if the buffer is already loaded switch to it. if not, load it into the
; DEBUG bank
@openfile:
	sta file
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
	; TODO: handle err

	; goto the line that the BRK happened on and highlight it
	ldxy line
	jsr edit::gotoline	; go to the line where the BRK happened
	lda zp::cury
	jsr bm::rvsline		; highlight the line of the BRK

@print:	jsr showbrk		; display the BRK message
	; fall through to debug loop
.endproc

;******************************************************************************
; main debug loop
.proc debugloop
	lda #$70
	cmp $9004
	bne *-3
	cli
	jsr text::update
	jsr key::getch
	beq debugloop

	pha
	jsr cur::off
	pla
	ldx #@num_commands
@getcmd:
	dex
	bmi @nocmd	; unrecognized key, give to editor
	cmp @commands,x
	bne @getcmd
	beq @runcmd

@nocmd:	jsr edit::handlekey
	jmp debugloop

@runcmd:
	sei
	txa
	asl
	tax
	lda @command_vectors,x	; vector LSB
	sta zp::jmpvec
	lda @command_vectors+1,x ; vector MSB
	sta zp::jmpvec+1
	jsr zp::jmpaddr

@done:	; unhighlight the BRK line if it's still visible
	ldxy line
	jsr edit::src2screen
	bcs :+
	jsr bm::rvsline

:	; swap debug state out for user's program
	jsr save_debug_state
	jsr __debug_restore_progstate

@restore_regs:
	; from top to bottom: [STATUS, <PC, >PC]
	lda pc+1
	pha
	lda pc		; restore PC
	pha
	lda reg_p	; restore processor status
	pha

	lda #FINAL_BANK_USER
	sta zp::bankval

	lda reg_a
	ldx reg_x
	ldy reg_y

	; return from the BRK
	jmp fe3::bank_rti


;******************************************************************************
@commands:
	.byte $5f	; <- (quit)
	.byte $b0	; C=+a (toggle auto swap memory)
	.byte $ad	; C=+z (step)
	.byte $ae	; C=+s (step over)
	.byte $a5	; C=+g (go)
	.byte $85	; F1 (turn off all views)
	.byte $86	; F3 (edit memory)
	.byte $87	; F5 (edit breakpoints)
	.byte $bf	; C=+b (set breakpoint)
@num_commands=*-@commands
@command_vectors:
	.word quit
	.word auto_toggle_memory_swap
	.word step
	.word step_over
	.word go
	.word edit_source
	.word edit_mem
	.word edit_breakpoints
	.word set_breakpoint
.endproc

;******************************************************************************
; RESTORE_DEBUG_STATE
; Restores the saved debugger state
.proc restore_debug_state
@vicsave=mem::debugsave	; $0-$10
@savezp=mem::debugsave+$10	 ; $10-$110
@colorsave=mem::debugsave+$110
	ldx #$10
@restorevic:
	lda @vicsave-1,x
	sta $9000-1,x
	dex
	bne @restorevic
@restore_zp:
	lda @savezp,x
	sta $00,x
	dex
	bne @restore_zp

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
	CALL FINAL_BANK_FASTCOPY, #fcpy::restore
	rts
.endproc

;******************************************************************************
; RESTORE_PROGSTATE
; restores the saved program state
.export __debug_restore_progstate
.proc __debug_restore_progstate
@savezp=mem::prog00
@vicsave=mem::prog9000
@internalmem=mem::prog1000
@colorsave=mem::prog9400
	ldx #$10
@restorevic:
	lda @vicsave-1,x
	sta $9000-1,x
	dex
	bne @restorevic
@restore_zp:
	lda @savezp,x
	sta $00,x
	dex
	bne @restore_zp

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
	CALL FINAL_BANK_FASTCOPY2, #fcpy::restore
	rts
.endproc

;******************************************************************************
.proc auto_toggle_memory_swap
	lda #$01
	eor auto_swap_memory
	sta auto_swap_memory
	rts
.endproc

;******************************************************************************
; GO
; Runs the user program until the next breakpoint or an NMI occurs
.proc go
	jsr uninstall_breakpoints
	jmp install_breakpoints	  ; reinstall rest of breakpoints and continue
.endproc

;******************************************************************************
; QUIT
; Exits the debugger
.proc quit
	ldxy #@stopdebugging_msg
	lda #REGISTERS_LINE
	jsr text::putz

:	jsr key::getch
	beq :-
	cmp #$59		; Y
	beq @quit
@return:
	pla
	pla
	jsr showstate	; restore the register display
	jmp debugloop

@quit:	jsr uninstall_breakpoints

	lda #$00
	pha
	plp
	sei

	pla
	pla
	pla
	pla
	rts

@stopdebugging_msg:
	.byte "stop debugging? (press 'y' to quit)",0
.endproc

;******************************************************************************
; EDIT_SOURCE
; Disable all views and reenable (almost) fullscreen editing
.proc edit_source
	lda #$00
	sta aux_mode
	lda #REGISTERS_LINE-1
	sta edit::height	; resize the editor
	jsr edit::refresh	; and refresh
	jsr showstate		; restore the state
	pla
	pla
	jmp debugloop
.endproc

;******************************************************************************
; EDIT_MEM
; Transfers control to the memory viewer/editor until the user exits it
.proc edit_mem
	lda #DEBUG_INFO_START_ROW-1
	sta edit::height
	jsr edit::refresh
	jsr showstate		; restore the state

	sta edit::height	; resize the editor
	lda #AUX_MEM
	sta aux_mode
	ldxy #$1000
	jsr view::edit
	pla
	pla
	jmp debugloop
.endproc

;******************************************************************************
; EDIT_BREAKPOINTS
; Transfers control to the breakpoint viewer/editor until the user exits it
.proc edit_breakpoints
	lda #DEBUG_INFO_START_ROW-1
	sta edit::height
	jsr edit::refresh
	jsr showstate		; restore the state

	lda #AUX_BRK
	sta aux_mode
	jsr brkpt::edit
	pla
	pla
	jmp debugloop
.endproc

;******************************************************************************
; SET_BREAKPOINT
; Sets a breakpoint at the current line selection
.proc set_breakpoint
	jsr edit::setbreakpoint		; set a breakpoint in the source
	ldxy src::line
	jsr __debug_line2addr
	jsr __debug_setbreakpoint	; add the breakpoint to the debugger
	pla
	pla
	jmp debugloop
.endproc

;******************************************************************************
; STEP
; Runs the next instruction from the .PC and returns to the debug prompt.
; This works by inserting a BRK instruction after
; the current instruction and RUNning.
.proc step
@addr=zp::tmp0
	; delete the last STEP breakpoint
	jsr uninstall_breakpoints

	ldxy #$100	; ROM (we don't need the string)
	stxy zp::tmp0	; TODO: make way to not disassemble to string
	ldxy pc		; get address of next instruction
	jsr asm::disassemble  ; disassemble it to get its size (next BRK offset)
	bcc @ok
	rts		; return error

@ok:	pha
	jsr is_destructive
	bcs @setbrk

@savemem:
	stxy mem_saveaddr
	ldxy mem_saveaddr
	stxy @addr
	ldy #$00
	lda (@addr),y
	sta mem_save

@setbrk:
	pla
	ldxy pc
	jsr next_instruction	  ; get the address of the next instruction
	jsr __debug_setbreakpoint ; add a breakpoint at the next instruction
	inc $900f
	jsr install_breakpoints	  ; update breakpoints with our new one

	lda #$00
	sta break_after_sr ; reset step over
	rts	; return to the debugger
.endproc

;******************************************************************************
; STEP_OVER
; Runs the next instruction from the .PC and returns to the debug prompt.
; This works by inserting a BRK instruction after
; the current instruction and RUNning.
; Unlike STEP, if the next procedure is a JSR, execution will continue
; at the line after the subroutine (after the subroutine has run)
.proc step_over
	lda #$01
	sta break_after_sr
	jmp step
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
	bank_read_byte #FINAL_BANK_USER, @op
	sta @opcode
	bank_read_byte_rel #FINAL_BANK_USER, @op, #1
	sta @operand
	bank_read_byte_rel #FINAL_BANK_USER, @op, #2
	sta @operand+1

	lda @opcode
	cmp #$20	; JSR?
	bne @notjsr
	jsr @jmpjsr
@jsr:
	cmpw #$c000	; is the target in ROM?
	bcs @nocontrol	; if so, set breakpoint to PC+3
	lda step_over	; are we stepping OVER?
	bne @nocontrol	; if yes, don't go into the subroutine
	rts
@notjsr:
	beq @jmpjsr
	cmp #$4c	; JMP?
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
	lda @branch_masks,x	; if bit y is 1, use the mask
:	sta @y

	lda @branch_masks,x
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

; corresponding masks for top 2 bits of opcode to flags in the status register
@branch_masks:
.byte $80	; negative
.byte $40	; overflow
.byte $01	; carry
.byte $02	; zero
.endproc

;******************************************************************************
; IS_DESTRUCTIVE
; Checks if the given instruction is destructive (writes to memory)
; If it does, returns the address that the instruction writes to
; IN:
;  - .XY: address of the binary instruction
;  - .A: the size of the instruction
; OUT:
;  - .C: clear if the instruction is desctructive
;  - .XY: the target of the instruction (if it is destructive)
.proc is_destructive
@instruction=zp::tmp0
@opsz=zp::tmp1
@target=zp::tmp2
	stxy @instruction
	sta @opsz

	ldy #$02

; set MSB to 0 if operand size < 3 (instruction size is 1-2 bytes)
	ldx #$00
	cmp #$03
	bne :+
	lda (@instruction),y
	tax
:	stx @target+1

	dey
	lda (@instruction),y
	sta @target
	dey
	lda (@instruction),y

	ldx #num_destructive_ops-1
@find:	cmp destructive_ops,x
	beq @found
	dex
	bpl @find
	sec		; opcode is not destructive
	rts

; the opcode is destructive; determine which address if affects
@found:
	cpx #destructive_rel_x
	bcc @check_rel_y
; add the value of .X to get the target address
@rel_x:
	lda reg_x
	clc
	adc @target
	sta @target
	bcc :+
	inc @target+1
:	jmp @done

; add the value of .Y to get the target address
@check_rel_y:
	cpx #destructive_rel_y
	bcc @check_rel_y_ind
@rel_y:
	lda @target
	lda reg_y
	clc
	adc @target
	sta @target
	bcc :+
	inc @target+1
:	jmp @done

; use the value at the target ZP adddress + .Y to get the target address
@check_rel_y_ind:
	cpx #destructive_rel_y_ind
	bne @check_rel_x_ind
@rel_y_ind:
	; get the value of the ZP location in the program's ZP
	ldy @target
	lda mem::prog00,y
	sta @target
	lda mem::prog00+1,y
	sta @target+1

	; add the .Y register value to the address from the ZP
	ldy reg_y
	adc @target
	sta @target
	bcc :+
	inc @target+1
:	jmp @done

@check_rel_x_ind:
	cpx #destructive_rel_x_ind
	bne @done
@rel_x_ind:
	; TODO:

; the operand value is the target address
@done:
	ldxy @target
	RETURN_OK
.endproc

;******************************************************************************
; ADDWATCH
; Adds a watch for the given memory location. If this location is written to,
; execution will return to the debugger
; in:
;  - .XY: the address to add a watch for
.export __debug_addwatch
.proc __debug_addwatch
	txa
	pha
	lda numwatches
	asl
	tax

	pla
	sta watches,x
	tya
	sta watches+1,x

	inc numwatches
	rts
.endproc

;******************************************************************************
; SETBREAKPOINT
; Sets a breakpoint at the address in .XY
; IN:
;  - .XY: the address of the breakpoint to set
.export __debug_setbreakpoint
.proc __debug_setbreakpoint
	; if this is a duplicate, remove the existing breakpoint
	jsr remove_breakpoint

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
	rts
.endproc

;******************************************************************************
; REMOVEBREAKPOINT
; Removes a breakpoint at the address in .XY
; IN:
;  - .XY: the address of the breakpoint to remove
.export __debug_remove_breakpoint
__debug_remove_breakpoint:
.proc remove_breakpoint
@addr=debugtmp
@end=debugtmp+2
	stxy @addr
	lda @addr

	lda numbreakpoints
	asl
	tax
	beq @done	; nothing to remove

@l0:	lda @addr
	cmp breakpoints,x
	bne @next
	lda @addr+1
	cmp breakpoints+1,x
	beq @found
@next:	dex
	dex
	bpl @l0
	bmi @done	; not found - nothing to remove

@found:	; shift breakpoints down
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

@done:	ldxy @addr
	rts
.endproc

;******************************************************************************
; SHOWSTATE
; Shows the current debug state (registers and BRK line)
.proc showstate
	jsr showregs
	jmp showbrk
.endproc

;******************************************************************************
; SHOWREGS
; prints the contents of the registers in the format
;  ADDR A  X  Y  SP NV-BDIZC
;  f59c 02 02 00 f7 00100000
.proc showregs
@addr=zp::asm+4
@tmp=zp::asm+6
@flag=zp::asm+7
	; display the register names
	ldxy #@regsline
	lda #REGISTERS_LINE
	jsr text::putz

	ldy #39
	lda #' '
:	sta mem::linebuffer,y
	dey
	bpl :-

	; draw .Y
	lda reg_y
	jsr util::hextostr
	sty mem::linebuffer+11
	stx mem::linebuffer+12

	; draw .X
	lda reg_x
	jsr util::hextostr
	sty mem::linebuffer+8
	stx mem::linebuffer+9

	; draw .A
	lda reg_a
	jsr util::hextostr
	sty mem::linebuffer+5
	stx mem::linebuffer+6

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
	sta mem::linebuffer+18,x
:	lsr @flag
	inx
	cpx #2
	beq :-
	lda @flag
	bne @getstatus

@getsp:
	lda reg_sp
	jsr util::hextostr
	sty mem::linebuffer+14
	stx mem::linebuffer+15

@getaddr:
	lda pc+1
	jsr util::hextostr
	sty mem::linebuffer
	stx mem::linebuffer+1

	lda pc
	jsr util::hextostr
	sty mem::linebuffer+2
	stx mem::linebuffer+3

	lda #$00
	sta mem::linebuffer+26

	ldxy #mem::linebuffer
	lda #REGISTERS_LINE+1
	jsr text::putz
	rts

@regsline: .byte "addr a  x  y  sp  nv-bdizc",0
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
	ldxy #@brk_message_addr
	jmp @print

@showline:
	; display the BRK message
	lda line
	pha
	lda line+1
	pha
	ldxy #@brk_message_line
@print:	lda #DEBUG_MESSAGE_LINE
	jsr text::print		; break in line <line #>
	lda #DEBUG_MESSAGE_LINE
	jmp bm::rvsline

@brk_message_line: .byte "brk in line ",$fd,0 ; when line number is resolved
@brk_message_addr: .byte "brk @ ", $fe, 0      ; when line is unresolvable
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
	jmp zp::jmpaddr
@done:	rts
.define auxtab view::mem, brkpt::view
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
;  .C: set if the segment doesn't exist, clear on success
.proc get_segment_by_id
@info=zp::tmp0
	cmp numsegments
	bcc :+
	rts		; err, segment doesn't exist

:	pha

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

	; get the start address of the segment
	ldy #SEG_START_ADDR
	jsr read_from_seg
	sta segstart
	iny
	jsr read_from_seg
	sta segstart+1

	; get the stop address of the segment
	iny
	jsr read_from_seg
	sta segstop
	iny
	jsr read_from_seg
	sta segstop+1

	; get the address of the segment data
	pla
	asl
	tax
	lda segaddresses,x
	sta line
	lda segaddresses+1,x
	sta line+1

	RETURN_OK
.endproc

;******************************************************************************
; NEXT_LINE
; Updates the line pointer to point to the next line
; OUT:
;  - .C: set if there are no lines left in the active segment (seg)
.proc nextline
	lda line
	adc #$05
	sta line
	bcc :+
	inc line+1
:	RETURN_OK
.endproc

;******************************************************************************
; End the .DEBUG segment
.CODE

;******************************************************************************
.DATA
;******************************************************************************
; DESTRUCTIVE_OPS
; This table contains all opcodes that may alter memory.
; When stepping through code, the targets of these must be saved/restored
; before and after executing them.
destructive_ops:
	.byte $84	; STY zpg
	.byte $85	; STA zpg
	.byte $06	; ASL zpg
	.byte $26	; ROL zpg
	.byte $46	; LSR zpg
	.byte $66	; ROR zpg
	.byte $86	; STX zpg
	.byte $c6	; DEC zpg
	.byte $e6	; INC zpg
	.byte $8c	; STY abs
	.byte $8d	; STA abs
	.byte $0e	; ASL abs
	.byte $2e	; ROL abs
	.byte $4e	; LSR abs
	.byte $6e	; ROR abs
	.byte $8e	; STX abs
	.byte $ce	; DEC abs
	.byte $ee	; INC abs

destructive_rel_x_ind=*-destructive_ops
	.byte $81	; STA ind,x

destructive_rel_y_ind=*-destructive_ops
	.byte $91	; STA ind,y

destructive_rel_y=*-destructive_ops
	.byte $96	; STX zpg,y
	.byte $99	; STA abs,y

destructive_rel_x=*-destructive_ops
	.byte $94	; STY zpg,x
	.byte $95	; STA zpg,x
	.byte $16	; ASL zpg,x
	.byte $36	; ROL zpg,x
	.byte $56	; LSR zpg,x
	.byte $76	; ROR zpg,x
	.byte $f6	; INC zpg,x
	.byte $d6	; DEC zpg,x
	.byte $9d	; STA abs,x
	.byte $1e	; ASL abs,x
	.byte $3e	; ROL abs,x
	.byte $5e	; LSR abs,x
	.byte $7e	; ROR abs,x
	.byte $de	; DEC abs,x
	.byte $fe	; INC abs,x
num_destructive_ops=*-destructive_ops
