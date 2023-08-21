.include "asm.inc"
.include "errors.inc"
.include "labels.inc"
.include "macros.inc"
.include "memory.inc"
.include "string.inc"
.include "text.inc"
.include "util.inc"
.include "zeropage.inc"

;--------------------------------------
MAX_FILES = 16

;******************************************************************************
; Debug info is organized per file.  The format for a file's debug info is :
; stored in the following format:
; ---------------------------------------------
; | size     | description                    |
; |----------|--------------------------------|
; |    1     |   filename index               |
; |    1     | number of segments             |
; |    2     | segment 1 start addr           |
; |    2     | segment 1 stop addr            |
; |   ...    |         ...                    |
; |    2     | segment n start addr           |
; |    2     | segment n stop addr            |
; |    2     | segment 1 instruction 1 line # |
; |    2     | segment 1 instruction 1 addr   |
; |   ...    |         ...                    |
; |    2     | segment 1 instruction n line # |
; |    2     | segment 1 instruction n addr   |
; |   ...    |         ...                    |
; |    2     | segment n instruction 1 line # |
; |    2     | segment n instruction 1 addr   |
; |   ...    |         ...                    |
; |    2     | segment n instruction m line # |
; |    2     | segment n instruction m addr   |
; |-------------------------------------------|
; In words: a file's debug info is organized as the filename, followed by the
; number of segments, followed by a _list_ of those segments (as start and stop
; addresses), and lastly a list of the lines and addresses for each segment
;
; Clearly, this is a costly amount of memory, so it requires a Final Expansion
;
; Segments are not necessarily stored in order, so to see if an address
; exists within a file, we must traverse the structure as a linked-list to see
; if the given address is within the start/stop range for each segment
; This is likely to be a short traversal as it is fairly rare to change addresses
; within a source file (more than, say, 5 .ORG's would be a lot)
;******************************************************************************

.BSS
;--------------------------------------
; the per-file debug info as described in the above table
debuginfo:

; table of 0-terminated filenames
filenames: .res MAX_FILES * 16

; table of start addresses for each file (corresponds to filename)
fileaddresses: .res MAX_FILES * 2

; lengths (in bytes) for each file (corresponds to filename)
filelens: .res MAX_FILES * 2

; # of segments in a file (corresponds to filename)
files_num_segments: .res MAX_FILES

numfiles: .byte 0

.CODE
;--------------------------------------
; INIT
; Clears any debug state that exists
.export __debug_init
.proc __debug_init
	lda #$00
	sta numfiles
	rts
.endproc

;--------------------------------------
; SET_FILE
; sets the current file that we are storing symbols to. The number of lines is
; also given in order to allocate the appropriate amount of space
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

;------------------
; check if we've already allocated space for debug info for this file
	lda numfiles
	sta @cnt
	lda #$00
	sta @f
@chkfile:
	ldx @f
	lda filenames,x
	sta zp::str0
	lda filenames+1,x
	sta zp::str0+1
	lda #$10	; fixed size of 16
	jsr str::compare
	bne :+
	RETURN_OK	; space for file is already allocated
:	lda @f
	adc #$10
	sta @f
	dec @cnt
	bne @chkfile

;------------------
; find the next open filename
	lda numfiles
	asl
	asl
	asl
	asl
	tax
	adc #<filenames
	sta @filename
	lda #>filenames
	adc #$00
	sta @filename+1

;------------------
	ldy #$00
@copyfilename:
	lda (@filename_src),y
	sta (@filename),y
	beq @cont
	iny
	bne @copyfilename

;------------------
@cont:
; get numsegments*4
	asl @numsegments
	rol @numsegments+1
	asl @numsegments
	rol @numsegments+1

;------------------
; get numlines*4
	asl @numlines
	rol @numlines+1
	asl @numlines
	rol @numlines+1

;----------------------------
; calculate the end address of the info for this file
; filenameaddresses[numfiles] + 1 + 1 + (num_segments*4) + (num_lines*4)
; the +1's are taken care of by to SEC's before adding other values
;----------------------------
; addr = fileaddresses[numfiles]
	lda fileaddresses,x
	sta @addr
	lda fileaddresses+1,x
	sta @addr+1

;------------------
; addr += numsegments*4 + 1
	lda @addr
	sec	; +1
	adc @numsegments4
	bcc :+
	inc @addr+1

;------------------
; addr += numlines*4 + 1
:	lda @addr
	sec	; +1
	adc @numlines4
	sta @addr
	bcc @storeaddr
	inc @addr+1

;------------------
; store the address that the next file's debug info will begin at
@storeaddr:
	inc numfiles
	lda numfiles
	cmp #MAX_FILES
	bcc @ok
	RETURN_ERR ERR_MAX_FILES_EXCEEDED

@ok:	asl
	tax
	lda @addr
	sta fileaddresses,x
	lda @addr+1
	sta fileaddresses+1,x
	rts
.endproc

;--------------------------------------
; STORE_LINE
; Stores the given address and line number in the debug info for the current
; file
; in:
;  - .XY: the line number
;  - zp::tmp0: the address corresponding to the given line number
.export __debug_store_line
.proc __debug_store_line

.endproc

;--------------------------------------
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
