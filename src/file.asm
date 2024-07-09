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

isvirtual = zp::tmp16 	; flag for binary load to VIRTUAL memory
isbin     = zp::tmp17	; flag for binary save/load to memory

;******************************************************************************
; The address to load from during a binary LOAD
; VOLATILE should be set immediately before calling
.export __file_load_address
__file_load_address = rb

; The address to save to during a binary SAVE
; VOLATILE should be set immediately before calling
.export __file_save_address
__file_save_address     = rb
.export __file_save_address_end
__file_save_address_end = rd

;******************************************************************************
; LOADBINV
; loads the given file into the given virtual memory address
; IN:
;  .A:             the file handle to load from
;  file::loadaddr: the address to load the file to
; OUT:
;  - .C: set on error
.export __file_load_binv
.proc __file_load_binv
	ldx #$01
	stx isvirtual
	stx isbin
	bne load
.endproc

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
	ldx #$00
	sta isvirtual
	inx
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
;  - .A  the file handle
;  - rb: the address to load the file to
; OUT:
;  - .C: set on error, clear on success
;  - .A: if .C is set, the error code
.proc load
	tax
	jsr $ffc6	; CHKIN (file in .X now used as input)

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
@error:	jmp geterr
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

@error:	jmp geterr
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
	jsr $ffc9	; CHKOUT (file in .X now uesd as output)
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
	jsr $ffc9	; CHKOUT (file in .X now uesd as output)
	ldx #$00
	stx isbin	; not binary
	jmp dosave
.endproc

;******************************************************************************
; READB
; Reads a byte from the open file
; OUT:
;  - .C:  set on error or EOF
;  - .A:  the byte that was read
;  - eof: set if READST returns eof
.export __file_readb
.proc __file_readb
	lda #$00
	sta __file_eof
	jsr $ffb7     ; call READST (read status byte)
	cmp #$00
	bne @eof      ; either EOF or read error
	jsr $ffcf     ; call CHRIN (get a byte from file)
	RETURN_OK

; read drive err chan and translate CBM DOS error code to ours if possible
@eof:  	and #$40
	beq @err
	inc __file_eof
	RETURN_OK

@err:	jmp geterr
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
	bcs @ret	; return err or check EOF

	cmp #$0d
	beq @done
	cmp #$0a
	beq @done	; end of line

	sta (__file_load_address),y
	iny
	bne @l0

@done:  lda #$00
	sta (__file_load_address),y	; 0-terminate the string
	tya		; put # of bytes read in .A
@ok:	clc		; no error
@ret:	rts
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

@err:   jsr @close
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
	jmp geterr
.endproc

;******************************************************************************
; EXISTS
; Opens, reads a byte, and closes a file of the given name to check for its
; existence.
; IN:
;  - .XY: the filename to check if exists
; OUT:
;  - .Z: set if the file exists; clear if it does
.export __file_exists
.proc __file_exists
@file=r0
	jsr __file_open_r
	ldx #$01	; failed to open file
	bcs @done
	sta @file
	tax
	jsr $ffc6	; CHKIN (file in .X now used as input)
	jsr __file_readb
	jsr $ffb7	; call READST (read status byte)
	pha
	lda @file
	jsr __file_close
	pla		; restore READST status flag
	beq @done
	lda #ERR_FILE_NOT_FOUND
@done:	rts
.endproc


;******************************************************************************
; CLOSE
; Closes the file with the given handle.
; IN:
;  - .A: the file handle to close
;  - .Z: clear on error
.export __file_close
.proc __file_close
	pha
	jsr $ffc3		; CLOSE
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
; GETERR
; Reads the error channel and maps the DOS error code to the internal one
; IN:
;  - .A: the error code to map e.g. #$3e (62)
; OUT:
;  - .A: the internal error code e.g. ERR_FILE_NOT_FOUND
.proc geterr
	jsr io::readerr
	cpx #$01
	bcs :+
@ok:	rts

:	cpx #$3e
	bne :+
	lda #ERR_FILE_NOT_FOUND
	rts
:	RETURN_ERR ERR_IO_ERROR		; unknown error
.endproc

;******************************************************************************
; PUTB
; Outputs a byte to the __file_load_address (if isbin is !0) or to the current
; source if not.
; If loading a binary file, isvirtual determines if it will be loaded into
; physical memory (isvirtual == 0) or virtual (isvirtual != 0)
; IN:
;  - .A: the byte to output
.proc putb
	ldx isbin
	beq @src

	; if not loading to SOURCE, write to memory
	ldy #$00
	ldx isvirtual
	beq :+
	ldxy __file_load_address
	jsr vmem::store
	skw
:	sta (__file_load_address),y
@done:	incw __file_load_address
	rts
@src:	jmp src::insert
.endproc

.BSS
;******************************************************************************
secondaryaddr:	.byte 0	; secondary address to use on file open

.export __file_eof
__file_eof:	.byte 0	; if !0, EOF; only valid after call to readb or getline
