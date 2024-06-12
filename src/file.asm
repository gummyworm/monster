;******************************************************************************
; FILE.ASM
; This file contains procedures to read/write to files to/from source or binary.
; The pattern for file I/O is similar to the C standard or CBM DOS:
;
; ldx #<filename
; ldy #>filename
; jsr file::open
; lda #'3'
; jsr $ffd2
; ...
; jsr file::close
;
; We open a file, perform whatever reads/writes we wish, and then close it.
;
; File I/O behaves in two modes: binary and source. The mode is determined by
; the isbin flag, internal to this file, but which is set by the procedure
; that corresponds to the mode.  e.g. file::savebin writes using memory
; and file::savesrc writes the active source file.
;  - When reading binary files, the data goes directly to file::loadaddress
;  - When readings source files, the data is read into whatever the active
;    source file is (see source.asm)
;  - When writing binary files, data is written to the file at file::saveaddress
;  - When writing source files, data is written from the active source file.
;
; Note that the binary that gets written (file::writeb) comes from the virtual
; memory (vmem.asm) and not the main program memory, which is where the editor
; lives.
;******************************************************************************

.include "errors.inc"
.include "finalex.inc"
.include "io.inc"
.include "macros.inc"
.include "memory.inc"
.include "source.inc"
.include "string.inc"
.include "strings.inc"
.include "util.inc"
.include "vmem.inc"
.include "zeropage.inc"

;******************************************************************************
; CONSTANTS
MAX_OPEN_FILES = 10
FIRST_FILE_ID  = 2

files        = $259	; KERNAL open file table
file_devices = $261	; KERNAL device ID table
kernal_sas   = $26d	; KERNAL secondary address table

isbin        = zp::tmp17	; flag for binary save/load  to memory

;******************************************************************************
; The address to load from during a binary LOAD
; VOLATILE should be set immediately before calling
.export __file_load_address
__file_load_address = zp::tmpb

; The address to save to during a binary SAVE
; VOLATILE should be set immediately before calling
.export __file_save_address
__file_save_address     = zp::tmpb
.export __file_save_address_end
__file_save_address_end = zp::tmpd

;******************************************************************************
; LOADBIN
; loads the given file into the given memory address
; IN:
;  .A:                      the file handle to load from
;  file::file_load_address: the address to load the file to
; OUT:
;  - .C: set on error
.export __file_load_bin
.proc __file_load_bin
	ldx #$01
	stx isbin
	bne load
.endproc

;******************************************************************************
; LOAD SOURCE
; loads the given file into a new source buffer
; IN:
;  - .A: the file handle to load from
; OUT:
;  - .C: set on error
.export __file_load_src
__file_load_src:
	ldx #$00
	stx isbin		; not binary (load to source buff)
	sta secondaryaddr	; use handle as secondary address

	; fall through

;******************************************************************************
; LOAD
; loads the given file into memory or a source buffer
; IN:
;  - .A the file handle
;  zp::tmpb: the address to load the file to
; OUT:
;  .C: set on error, clear on success
.proc load
	tax
	jsr $ffc6     ; CHKIN (file in .X now used as input)

	; if .PRG, read load address
	lda secondaryaddr
	bne @l0
	jsr $ffb7	; READST
	bne @eof	; error/eof
	jsr $ffcf	; CHRIN, read 1st byte
	jsr $ffb7	; READST
	bne @eof	; error/eof
	jsr $ffcf	; CHRIN, read 2nd byte of load address

@l0: 	jsr $ffb7	; call READST (read status byte)
	cmp #$00
	bne @eof	; either EOF or read error
	jsr $ffcf	; call CHRIN (get a byte from file)
	ldy isbin	; skip conversions for binary LOAD
	bne @put
	cmp #$0a	; convert LF to CR
	bne @put
	lda #$0d
@put:	jsr putb	; write the byte to source/memory
	jmp @l0		; and continue

@eof:	eor #$40
	beq @noerr	; if EOF, return
@error:
	jsr io::readerr
	txa
	beq @noerr
	sec
	rts
@noerr: clc
	rts
.endproc

;******************************************************************************
; DOSAVE
; Saves the source buffer or binary block to the given file.
; This is called by __file_save and __file_savebin after those routines
; initialize the flags necessary to save source or memory respectively.
; IN:
;  - the file to wrtie out should be open (file::open_w)
; OUT:
;  - .C: set on error, clear on success
.proc dosave
	ldx isbin
	bne @save
	jsr src::rewind	; if saving source, go back to the start of it

@save:  jsr $ffb7       ; READST (read status byte)
	bne @error	; error
@chout: jsr getb
	php		; save EOF flag
	jsr $ffd2	; write to file
	plp		; get EOF flag
	bcc @save	; loop if !EOF
	lda #$00	; no error
	RETURN_OK	; done

@error:	jmp io::readerr
.endproc

;******************************************************************************
; SAVEBIN
; Saves the binary from the given address, to the given filename.
; NOTE: the address refers to the virtual memory address not the physical
; one.
; IN:
;  - .A:                      the file to save to
;  - .XY:                     the start address to write from
;  - __file_save_address_end: the end of the address range to save
; OUT:
;  .C: set on error, clear on success
.export __file_save_bin
.proc __file_save_bin
	stxy __file_save_address
	tax
	jsr $ffc9	; CHKOUT (file in .X now used as output)
	lda #$01
	sta isbin	; flag that we're saving binary
	jmp dosave
.endproc

;******************************************************************************
; SAVE SRC
; Saves the active source buffer to a file of the given name
; IN:
;  .XY: the 0-terminated filename to save the source buffer to
;  .A:  the length of the filename
; OUT:
;  .C: set on error, clear on success
.export __file_save_src
.proc __file_save_src
	tax
	jsr $ffc9	; CHKOUT (file in .X now used as output)
	ldx #$00
	stx isbin	; not binary
	jmp dosave
.endproc

;******************************************************************************
; READB
; Reads a byte from the open file
; OUT:
;  - .C: set on error or EOF
;  - .A: the byte that was read
.export __file_readb
.proc __file_readb
	jsr $ffb7     ; call READST (read status byte)
	cmp #$00
	bne @eof      ; either EOF or read error
	jsr $ffcf     ; call CHRIN (get a byte from file)
	RETURN_OK

; read drive err chan and translate CBM DOS error code to ours if possible
@eof:  	jsr io::readerr
	txa
	cmp #62		; FILE NOT FOUND
	bne :+
	lda #ERR_FILE_NOT_FOUND
:	sec
	rts
.endproc

;******************************************************************************
; GETLINE
; Reads the file handle given in .A until EOF or a newline ($0d or $0a) is
; encountered and stores the data at the address given in .XY
; will read a maximum of 255 bytes
; IN:
;  - .XY: the address to read the line into
;  - .A: the file ID
; OUT:
;  - .A: contains the # of bytes read OR the error code
;  - .C: is set if an error occurred
.export __file_getline
.proc __file_getline
	stxy __file_load_address
	tax
	jsr $ffc6     ; CHKIN (file in .X now used as input)

	ldy #$00
@l0:	jsr __file_readb
	bcs @ret	; return err
	cmp #$0d
	beq @done

	; convert tab characters
	cmp #$0a
	beq @done
	cmp #$09	; TAB
	bne :+
	lda #' '
:	sta (__file_load_address),y
	iny
	bne @l0

@done:  lda #$00
	sta (__file_load_address),y	; 0-terminate the string
	tya		; put # of bytes read in .A
	RETURN_OK	; no error
@ret:	cmp #$01	; set .C if error > 0
	rts
.endproc

;******************************************************************************
; SCRATCH
; Deletes the given file
; IN:
;  - .XY: the 0-terminated filename of the file to open for deletion
; OUT:
;   - .C: set on error
.export __file_scratch
.proc __file_scratch
	stx r0
	sty r0+1

	jsr init_drive

	ldx #<@s_colon
	ldy #>@s_colon
	jsr str::cat	; s@:<filename>
	lda #15		; SA (command channel)
	jsr __file_open
	bcs @err

@close: lda #15		; filenumber 15 (command channel)
	jsr $ffc3	; CLOSE 15
	jsr $ffcc	; CLRCHN
	lda #$00	; no error
	RETURN_OK

@err:   jsr io::readerr
	jsr @close
	RETURN_ERR ERR_IO_ERROR
@s_colon:
	.byte "s:",0
.endproc

;******************************************************************************
; OPEN_W
; Opens a file for writing
; IN:
;  - .XY: the 0-terminated filename to open for writing
; OUT:
;  - .A: the file handle
;  - .C: set on error
.export __file_open_w
.proc __file_open_w
	lda #<@p_w
	sta r0
	lda #>@p_w
	sta r0+1
	jsr str::cat	; filename + ",p,w"
	lda #$03	; SA
	jmp __file_open
@p_w:	.byte ",p,w",0
.endproc

;******************************************************************************
; OPEN_R
; Opens a file for reading
; IN:
;  - .XY: the 0-terminated filename to open for writing
; OUT:
;  - .A: the file handle
;  - .C: set on error
.export __file_open_r
.proc __file_open_r
	lda #$03	; SA
	bne __file_open
.endproc

;******************************************************************************
; OPEN_R_PRG
; Opens a file for reading as a .PRG
.export __file_open_r_prg
.proc __file_open_r_prg
	lda #$00
	; fall through
.endproc

;******************************************************************************
; OPEN
; Opens a file from disk, opens a channel and returns the handle to it.
; you may call the read and write operations with this handle
; IN:
;  - .XY: the 0-terminated filename of the file to open
;  - .A:  the secondary address
; OUT:
;  - .A: the file handle
;  - .C: set on error
.export __file_open
.proc __file_open
@file=r2
@filename=r3
	sta secondaryaddr
	lda zp::numfiles
	cmp #MAX_OPEN_FILES
	bcc :+
	lda #ERR_MAX_FILES_EXCEEDED
	rts

:	stxy @filename
	jsr str::len
	pha

	; find a free file ID in KERNAL's file table
	lda #FIRST_FILE_ID-1
	sta @file
@l0:	inc @file
	lda @file	; get an ID to try
	ldx zp::numfiles
@l1:	dex
	bmi @found	; file ID not found in table, it's free
	cmp files,x	; is file ID already in table?
	bne @l1		; check all entries
	beq @l0		; if file ID matches another entry, try a new ID

@found:
	pla		; get length of filename
	ldxy @filename
	jsr $ffbd	; SETNAM

	lda @file		; file handle
	ldx zp::device		; last used device number
	ldy secondaryaddr	; SA
	jsr $ffba 		; SETLFS
	jsr $ffc0 		; call OPEN
	bcs @getopenerr
	lda @file	; get file ID that we opened
	rts		; return it

@getopenerr:
	cmp #$01
	bne :+
	lda #ERR_TOO_MANY_OPEN_FILES
	rts
:	cmp #$02
	bne :+
	lda #ERR_LOGICAL_FILE_IN_USE
	rts
:	cmp #$05
	bne :+
	lda #ERR_DRIVE_DID_NOT_RESPOND
	rts

:	RETURN_ERR ERR_IO_ERROR		; unknown error
.endproc

;******************************************************************************
; CLOSE
; Closes the file with the given handle.
; IN:
;  - .A: the file handle to close
;  - .Z: clear on error
.export __file_close
.proc __file_close
	sei
	pha
	jsr $ffc3
	pla
	jmp $ffb7		; READST
.endproc

;******************************************************************************
; INIT DRIVE
; Initializes the drive
.proc init_drive
	ldxy #@i
	lda #1
	jsr $ffbd	; SETNAM
	lda #15
	ldx zp::device
	ldy #15
	jsr $ffba	; SETLFS
	jsr $ffc0	; OPEN
	lda #15
	jmp $ffc3	; CLOSE
@i:	.byte "I"
.endproc

;******************************************************************************
; GETB
; During SAVE: reads the next byte to be saved.
; OUT:
;  - .A: the character that was read
;  - .C: set if EOF
.proc getb
	lda isbin	; are we reading from memory or source
	bne @use_bin

@use_src:
	jsr src::next
	jsr src::end
	beq :+		; if EOF, return with .C set
	clc		; !EOF, return with .C clear
	rts
:	sec
	rts

@use_bin:
	ldxy __file_save_address
	jsr vmem::load
	incw __file_save_address
	ldxy __file_save_address
	cmpw __file_save_address_end ; set .C if src >= end address
@done:	rts
.endproc

;******************************************************************************
; PUTB
; Outputs a byte to the __file_load_address (if isbin is !0) or to the current
; source if not.
; IN:
;  - .A: the byte to output
.proc putb
	ldx isbin
	beq @src

	; if not loading to SOURCE, write to memory
	ldy #$00
	sta (__file_load_address),y
	incw __file_load_address
	rts
@src:	jmp src::insert
.endproc

.BSS
;******************************************************************************
secondaryaddr: .byte 0	; secondary address to use on file open
