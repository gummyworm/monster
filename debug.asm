.include "asm.inc"
.include "errors.inc"
.include "labels.inc"
.include "macros.inc"
.include "memory.inc"
.include "string.inc"
.include "text.inc"
.include "util.inc"
.include "zeropage.inc"

;******************************************************************************
MAX_FILES = 16	      ; max files that debug info may be generated for
MAX_BREAKPOINTS = 16  ; max number of breakpoints that may be set
MAX_WATCHPOINTS = 8   ; max number of watchpoints that may be set

;******************************************************************************
file = zp::debug     ; current file id being worked on
addr = zp::debug+1   ; address of next line/addr to store

;******************************************************************************
; Debug info is organized per file.  The format for a file's debug info is :
; stored in the following format:
;	 ---------------------------------------------
;	 | size     | description                    |
;	 |----------|--------------------------------|
;	 |    1     | number of segments             |
;	 |    2     | segment 1 start addr           |
;	 |    2     | segment 1 stop addr            |
;	 |   ...    |         ...                    |
;	 |    2     | segment n start addr           |
;	 |    2     | segment n stop addr            |
;	 |    2     | segment 1 instruction 1 line # |
;	 |    2     | segment 1 instruction 1 addr   |
;	 |   ...    |         ...                    |
;	 |    2     | segment 1 instruction n line # |
;	 |    2     | segment 1 instruction n addr   |
;	 |   ...    |         ...                    |
;	 |    2     | segment n instruction 1 line # |
;	 |    2     | segment n instruction 1 addr   |
;	 |   ...    |         ...                    |
;	 |    2     | segment n instruction m line # |
;	 |    2     | segment n instruction m addr   |
;	 |-------------------------------------------|
; In words: a file's debug info is organized as the filename, followed by the
; number of segments, followed by a _list_ of those segments (as start and stop
; addresses), and lastly a list of pairs of lines/addresses for each segment
;
; Clearly, this is a costly amount of memory, so it requires a Final Expansion
;
; A new "segment" begins with either a .ORG or a .INC (include) directive.
; Address/lines are stored sequentially within a segment until one of these
; is encountered at which point (because both are likely to lead to
; disconinuity) a new segment begins.
;
; Segments are not necessarily stored in order, so to see if an address
; exists within a file, we must traverse the structure as a linked-list to see
; if the given address is within the start/stop range for each segment
; This is likely to be a short traversal as it is fairly rare to change addresses
; within a source file (more than, say, 5 .ORG's would be a lot)
;******************************************************************************

.BSS
;******************************************************************************
; the per-file debug info as described in the above table
.export debuginfo
debuginfo: .res $100

; number of files that we have debug info for. The ID of a file is its index
.export numfiles
numfiles: .byte 0

; table of 0-terminated filenames
.export filenames
filenames: .res MAX_FILES * 16

; table of start addresses for each file (corresponds to filename)
.export fileaddresses
fileaddresses: .res MAX_FILES * 2

;******************************************************************************
; WATCHES
;******************************************************************************
numwatches: .byte 0		   ; number of active watches
watches:    .res MAX_WATCHPOINTS*2 ; addresses of the set watchpoints

;******************************************************************************
; BREAKPOINTS
;******************************************************************************
numbrkpoints: .byte 0		      ; number of active break points
brkpoints:    .res MAX_BREAKPOINTS*2  ; addresses of the break points
brkbackups:   .res MAX_BREAKPOINTS    ; backup of the instructions under the BRK

;******************************************************************************
; pointers used when building the debug info
; may be used for other purposes after debug info is generated
nextsegment: .res MAX_FILES ; offset to next free segment start/end addr in file

.CODE
;******************************************************************************
; INIT
; Clears any debug state that exists
.export __debug_init
.proc __debug_init
	lda #$00
	sta numfiles
	rts
.endproc

;******************************************************************************
; SET_FILE
; Sets the current file that we are storing symbols to. The number of lines and
; number of segments tell us how much space to allocate for this file
; in:
;  - .XY: address of the filename
;  - zp::tmp0: # of lines in file
;  - zp::tmp2: # of segments in file
; out:
;  - .C: set if an error occurred
.export __debug_set_file
.proc __debug_set_file
@numlines=zp::tmp0
@numlines4=zp::tmp0
@numsegments=zp::tmp2
@numsegments4=zp::tmp2
@filename=zp::tmp6
@addr=zp::str2
@f=zp::tmp8
@cnt=zp::tmp9
@filename_src=zp::str2	; use string ZP for strcmp
	stxy @filename_src

	; check if we've already allocated space for debug info for this file
	lda #$00
	sta @f
	lda numfiles
	sta @cnt
	bne @chkfile

; if this is the first init fileaddresses[0]
	lda #<debuginfo
	sta fileaddresses
	lda #>debuginfo
	sta fileaddresses+1
	bne @initsegs	; no files, skip check

@chkfile:
	ldx @f
	lda filenames,x
	sta zp::str0
	lda filenames+1,x
	sta zp::str0+1
	lda #$10	; fixed size of 16
	jsr str::compare
	bne @next
	RETURN_OK	; space for file is already allocated
@next:	lda @f
	adc #$10
	sta @f
	dec @cnt
	bne @chkfile

;------------------
; init segment count to 0
@initsegs:
	lda numfiles
	asl
	tax
	lda fileaddresses,x
	sta @addr
	lda fileaddresses+1,x
	sta @addr+1
	lda #$00
	tay
	sta (@addr),y

;------------------
; find the next open filename
@getfiledst:
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

;------------------
; copy the filename we're going to create debug info for
	ldy #$00
@copyfilename:
	lda (@filename_src),y
	sta (@filename),y
	beq @calcsize
	iny
	bne @copyfilename

;------------------
; calculate the end address of the info for this file
; filenameaddresses[numfiles] + 1 + 1 + (num_segments*4) + (num_lines*4)
; the +1's are taken care of by to SEC's before adding other values
@calcsize:
	lda numfiles
	cmp #MAX_FILES
	bcc @ok
	RETURN_ERR ERR_MAX_FILES_EXCEEDED
@ok:	asl
	tax
	bne @addsegments

; get numsegments*4
@addsegments:
	lda @numsegments
	asl
	rol @numsegments+1
	sec			; +1
	rol
	rol @numsegments+1

	adc fileaddresses,x
	sta @addr
	lda fileaddresses+1,x
	adc @numsegments+1
	sta @addr+1

;------------------
; get numlines*4
@addlines:
	lda @numlines
	asl
	rol @numlines+1
	sec			; +1
	rol
	rol @numlines+1

	adc @addr
	sta @addr
	lda @addr+1
	adc @numlines+1
	sta @addr+1

;------------------
; store the address that the next file's debug info will begin at
@storeaddr:
	ldx numfiles
	lda #$00
	sta nextsegment,x ; set next segment to 0

	inc numfiles
	lda numfiles
	asl
	tax
	lda @addr
	sta fileaddresses,x
	lda @addr+1
	sta fileaddresses+1,x
	RETURN_OK
.endproc

;******************************************************************************
; STARTSEGMENT
; Begins a new segment in the active file where debug info will be stored
; OUT:
;  - .C: set on error
.proc __debug_startsegment
@addr=zp::tmp0
	lda file
	asl
	tax
	lda fileaddresses,x
	sta @addr
	lda fileaddresses+1,x
	sta @addr+1
	ldy #$00
	lda (@addr),y	; get number of segments
	pha
	clc
	adc #$01
	sta (@addr),y	; store updated segment count
	pla
; get the stop address of the previous segment
	asl		; *2
	asl		; *4
	sec		; +1 (num_segments)
	adc @addr
	sta @addr
	bcc :+
	inc @addr+1

; @addr now points to the last segment's start address
:	ldy #$02	; offset to stop address
	lda (@addr),y
	pha
	iny
	lda (@addr),y

; store last segment's stop as new segment's start address
	iny
	iny
	sta (@addr),y
	dey
	pla
	sta (@addr),y

	RETURN_OK
.endproc

;******************************************************************************
; STORE_LINE
; Stores the given address and line number in the debug info for the current
; file
; in:
;  - .XY: the line number
;  - zp::tmp0: the address corresponding to the given line number
.export __debug_store_line
.proc __debug_store_line
	; store the line number
	tya
	ldy #$01
	sta (addr),y
	txa
	dey
	sta (addr),y

	; store the address
	ldy #$02
	lda zp::tmp0
	sta (addr),y
	iny
	lda zp::tmp0+1
	sta (addr),y

	; update pointer for line/addr
	lda addr
	clc
	adc #$02
	sta addr
	bcc :+
	inc addr+1
:	rts
.endproc

;******************************************************************************
; ADDR2LINE
; returns the filename and address that correspond
; to the given address
; in:
;  - .XY: the address to get the location of
; out:
;  - .XY: the address of the filename
;  - zp::tmp0: the line number (2 bytes)
;  - .C: set on error
.export __debug_addr2line
.proc __debug_addr2line
@info=zp::tmp0
@addr=zp::tmp2
@cnt=zp::tmp4
@segstart=zp::tmp5
@segstop=zp::tmp7
	stxy @addr

	lda #$00
	sta @cnt
@l0:	ldx @cnt
	lda fileaddresses,x
	sta @info
	lda fileaddresses+1,x
	sta @info+1

@l1:	ldy #$00
	lda (@info),y	; # of segments
	tax
	iny

@l2:	lda (@info),y	; get the start address for the segment
	sta @segstart
	iny
	lda (@info),y
	sta @segstart+1

	iny
	lda (@info),y	; get the stop address for the segment
	sta @segstop
	iny
	lda (@info),y
	sta @segstop+1

; is the address we're looking for is in the range [segstart, segstop]?
@checkstop:
	lda @addr+1
	cmp @segstop+1
	bcc @checkstart
	beq :+
	bcs @next
:	lda @addr
	cmp @segstop
	beq @segfound
	bcs @next

@checkstart:
	lda @addr+1
	cmp @segstart+1
	bcc @next
	lda @addr
	cmp @segstart
	bcs @segfound

@next:	lda @info
	clc
	adc #$04 ; sizeof(segstart)+sizeof(segstop)
	sta @info
	bcc :+
	inc @info+1
:	dex	 ; decrement segment counter
	bne @l2	 ; repeat until we've checked all segments
	RETURN_ERR ERR_LINE_NOT_FOUND

;------------------
; find the line that the address we were given is on
@segfound:
@findline:
	ldy #$02
	lda (@segstart),y
	cmp @addr
	bne @nextline
	iny
	lda (@segstart),y
	cmp @addr+1
	bcc @nextline

;------------------
; get the line corresponding to the address
@found: ldy #$00
	lda (@segstart),y
	tax
	iny
	lda (@segstart),y
	tay
	RETURN_OK   ; TODO: ensure address is still < segstop

@nextline:
	lda @segstart
	clc
	adc #$04 ; sizeof(line_num)+sizeof(line_addr)
	sta @segstart
	lda @segstart
	adc #$00
	sta @segstart
	jmp @findline	; TODO: ensure address is still < segstop
.endproc

;--------------------------------------
; START
; Begins debugging at the given address
; Execution will continue until a BRK instruction occurs at which point the
; debugger will take over and allow for interactive debugging from the user.
; in:
;  - .XY: the address to begin debugging at
.export __debug_start
.proc __debug_start
.endproc

;--------------------------------------
; SAVE_DEBUGSTATE
; saves memory likely to be clobbered by the user's
; program (namely the screen)
.proc save_debugstate
.endproc

;--------------------------------------
; SAVE_PROGSTATE
; saves the user program's state so that the debugger may use the memory for
; screen etc.
.proc save_progstate
.endproc

;--------------------------------------
; RESTORE_DEBUGSTATE
; restores the saved debugger state
.proc restore_debugstate
.endproc

;--------------------------------------
; RESTORE_PROGSTATE
; restores the saved program state
.proc restore_progstate
.endproc

;--------------------------------------
; RUN
; Runs unitl the next BRK instruction and returns to the debugger
.proc run

.endproc

;--------------------------------------
; STEP
; Runs the next instruction from the .PC and returns to the debug prompt.
; This works by inserting a BRK instruction after
; the current instruction and RUNning.
.proc step

.endproc

;--------------------------------------
; WATCH
; Adds a watch for the given memory location. If this location is written to,
; execution will return to the debugger
; in:
;  - .XY: the address to add a watch for
.proc watch

.endproc


;--------------------------------------
; SHOWREGS
; prints the contents of the registers in the format
;  ADDR A  X  Y  SP NV-BDIZC
;  f59c 02 02 00 f7 00100000
.proc showregs
@reg_a=zp::asm
@reg_x=zp::asm+1
@reg_y=zp::asm+2
@reg_sp=zp::asm+3
@addr=zp::asm+4
@tmp=zp::asm+6
@flag=zp::asm+7
	; save the registers
	php
	txs
	sta @reg_a
	stx @reg_x
	sty @reg_y
	txs
	stx @reg_sp

	; display the register names
	ldxy #@regsline
	lda zp::cury
	jsr text::puts

	; .Y
	pla
	sta @reg_y
	jsr util::hextostr
	tya
	sta mem::linebuffer+11,x
	txa
	sta mem::linebuffer+12,x

	; .X
	pla
	sta @reg_x
	jsr util::hextostr
	tya
	sta mem::linebuffer+8,x
	txa
	sta mem::linebuffer+9,x

	; draw .A
	pla
	sta @reg_a
	jsr util::hextostr
	tya
	sta mem::linebuffer+5,x
	txa
	sta mem::linebuffer+6,x

	; status
	pla
	sta @tmp
	lda #$80
	sta @flag
	ldx #$00

@getstatus:
	lda @flag
	and @tmp
	bne :+
	lda #'0'
	skw
:	lda #'1'
	sta mem::linebuffer,x
:	lsr @flag
	beq @getaddr
	inx
	cpx #2
	beq :-
	bne @getstatus

@getaddr:
	pla
	sta @addr
	pla
	sta @addr+1
	jsr util::hextostr
	tya
	sta mem::linebuffer,x
	txa
	sta mem::linebuffer+1,x
	lda @addr
	jsr util::hextostr
	tya
	sta mem::linebuffer+2,x
	txa
	sta mem::linebuffer+3,x
	rts

@regsline: .byte "addr a  x  y  sp  nv-bdizc",0
.endproc

;--------------------------------------
; GET_LINE_DATA_START
; Returns the start address of the debug line info.
; That is the data that is after the segments info
; in:
;  - .A the file to get the start of line data for
; out:
;  - .XY: the address of the start of line data
;  - .C: set if the given file is not found
.proc get_data_start
@tmp=zp::tmpa
@f=zp::tmpb
	asl
	tax
	lda fileaddresses,x
	lda (@f),y ; get # of segments
; *4 (num_segments * sizeof(start_addr) * sizeof(end_addr)
	asl
	rol @tmp
	asl
	rol @tmp
	adc @f
	tax
	lda @tmp
	adc @f+1
	tay
	RETURN_OK
.endproc
