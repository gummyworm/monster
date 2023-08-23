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
MAX_SEGMENTS=32	      ; max segments that debug info may be generated for
MAX_BREAKPOINTS = 16  ; max number of breakpoints that may be set
MAX_WATCHPOINTS = 8   ; max number of watchpoints that may be set

;******************************************************************************
file = zp::debug     ; current file id being worked on
addr = zp::debug+1   ; address of next line/addr to store
seg  = zp::debug+3   ; address of current segment pointer

;******************************************************************************
SEG_LINE_COUNT = 4 ; offset in segment header for line count

DATA_FILE = 0
DATA_LINE = 1
DATA_ADDR = 3

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

.BSS
;******************************************************************************
; the per-file debug info as described in the above table
.export debuginfo
segments:
debuginfo: .res $100

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
segaddresses: .res MAX_FILES * 2

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
; STOREFILE
; Copies the given filename thereby creating an ID for that file
; IN:
;  -.XY: address of 0-terminated filename
; OUT:
;  -.C: clear on success, set on error
.export __debug_storefile
.proc __debug_storefile
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
:	rts
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
@seg=zp::tmp2
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
	adc #<segments
	sta @segend
	sta segaddresses
	lda @segend+1
	adc #>segments
	sta @segend+1
	sta segaddresses+1

	; get the address of the start of the segments
	ldxy #segments
	stxy @seg

	lda #$01
	sta @cnt
	cmp numsegments
	beq @done	; if there's only 1 segment, we're done

@l0:	ldy #SEG_LINE_COUNT	; get the line count for the segment
	lda (@seg),y
	sta @lines
	iny
	lda (@seg),y
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
@tmp=zp::tmp0
	; get the address of the segment
	lda numsegments	; *6 to get offset for this segment
	asl		; *2
	sta @tmp
	asl		; *4
	adc @tmp	; *6
	adc #<segments
	sta seg
	lda #$00
	adc #>segments
	sta seg+1

	; store the start address of the segment
	tya
	ldy #$01
	sta (seg),y
	txa
	dey
	sta (seg),y

	; end address will be determined by dbg::endseg

	; initialize the line count to 0
	ldy #SEG_LINE_COUNT
	lda #$00
	sta (seg),y
	inc numsegments
	RETURN_OK
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
	lda #<segments
	sta @seg
	lda #>segments
	sta @seg+1

@l0:	ldy #$00
	lda (@seg),y
	cmp @addr
	bne @next
	iny
	lda (@seg),y
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
	ldy #$03
	sta (seg),y
	dey
	txa
	sta (seg),y
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

; pass 2- store the line number and corresponding address
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
	; update line count for this segment
	ldy #SEG_LINE_COUNT
	lda (seg),y
	clc
	adc #$01
	sta (seg),y
	bcc @done
	iny
	lda (seg),y
	adc #$00	; +1
	sta (seg),y

@done:	rts
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

	lda #<debuginfo
	sta @info
	lda #>debuginfo
	sta @info+1


	ldx #$00
@l0:	ldy #$00
	lda (@info),y	; get the start address for the segment
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
	adc #$06 ; sizeof(segstart)+sizeof(segstop)+sizeof(numlines)
	sta @info
	bcc :+
	inc @info+1
:	inx
	cpx numsegments
	bne @l0	 ; repeat until we've checked all segments
	RETURN_ERR ERR_LINE_NOT_FOUND

;------------------
; find the line that the address we were given is on
@segfound:
	txa
	asl
	tax
	lda segaddresses,x
	sta @info
	lda segaddresses+1,x
	adc #$00
	sta @info+1
@findline:
	ldy #DATA_ADDR
	lda (@info),y
	cmp @addr
	bne @nextline
	iny
	lda (@info),y
	cmp @addr+1
	bne @nextline

;------------------
; get the line corresponding to the address
@found: ldy #DATA_LINE
	lda (@info),y
	tax
	iny
	lda (@info),y
	tay
	RETURN_OK   ; TODO: ensure address is still < segstop

@nextline:
	lda @info
	clc
	adc #$05 ; sizeof(line_num)+sizeof(line_addr)+sizeof(file_id)
	sta @info
	bcc @findline
	inc @info+1
	bne @findline	; TODO: ensure address is still < segstop
	sec
	rts
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
; SETFILE
; IN:
;  - .XY: the 0-terminated file to set as the current file
.export __debug_set_file
.proc __debug_set_file
	jsr get_fileid
	sta file
	rts
.endproc


;--------------------------------------
; GET_FILEID
; Returns the ID for the given file name
; IN:
;  - .XY: the filename to return the id of
; OUT:
;  - .A: the file ID
;  - .C: set if there was no match
.proc get_fileid
@filename=zp::str2
@cnt=zp::tmp4
@other=zp::tmp5
@len=zp::tmp6
	stxy @filename
	jsr str::len
	sta @len
	lda numfiles
	beq @notfound
	lda #$00
	sta @other
	sta @cnt

@l0:	lda @other
	clc
	adc #<filenames
	tax
	lda @other+1
	adc #>filenames
	tay
	lda @len
	jsr str::compare
	bne @next
	RETURN_OK	; space for file is already allocated
@next:	lda @other
	adc #$10
	sta @other
	dec @cnt
	bne @l0
@notfound:
	sec		; not found
	rts
.endproc
