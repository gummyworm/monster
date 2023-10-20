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

isbin        = zp::tmp17	; flag for binary save/load
				; to memory

; The address to load from during a binary LOAD
.export __file_load_address
__file_load_address = zp::tmpb

; The address to save to during a binary SAVE
.export __file_save_address
__file_save_address     = zp::tmpb
.export __file_save_address_end
__file_save_address_end = zp::tmpd

;******************************************************************************
; LOADDIR
; Loads the directory listing into mem::spare
; OUT:
;  - mem::spare: contains the directory listing
;  - .C: set on error, clear on success
.export __file_loaddir
.proc __file_loaddir
	lda #$00
	sta secondaryaddr
	ldxy #(mem::spare+40)
	stxy __file_load_address
	ldxy #strings::dir
	lda #$01
	sta isbin
	jmp load
.endproc

;******************************************************************************
; LOADBIN
; loads the given file into the given memory address
; IN:
;  .XY:                the filename to load
;  .A: 		       the length of the file
;  file::file_load_address: the address to load the file to
.export __file_load_bin
.proc __file_load_bin
	pha
	lda #$01
	sta isbin
	pla
	jmp doload
.endproc

;******************************************************************************
; LOAD SOURCE
; loads the given file into a new source buffer
; IN:
;  .XY: the filename to load
;  .A: the length of the file
.export __file_load_src
__file_load_src:
@filename=zp::tmp10
	stxy @filename
	pha

	jsr src::new
	ldxy @filename
	jsr src::name
	lda #$00
	sta isbin

	pla
	ldxy @filename

	; fall through

;******************************************************************************
; DOLOAD
; loads the given file into memory or a source buffer
; IN:
;  .XY: the filename to load
;  .A: the length of the file
;  zp::tmpb: the address to load the file to
; OUT:
;  .C: set on error, clear on success
.proc doload
@filename=zp::tmp10
@file=zp::tmp12
	pha
	stxy @filename

; try to read from the file to make sure it exists before we do anything else
	jsr __file_open
	sta @file
	bcc @ok
@err:	jsr __file_close
	tax
	pla
	txa
	pha
	jsr $ffe7	; CLALL
	pla
	sec
	rts

@ok:	ldxy #$120
	jsr __file_getline
	lda @file
	bcs @err
	jsr __file_close

	lda #$03
	sta secondaryaddr
	ldxy @filename
	pla
	; fallthrough
.endproc

;--------------------------------------
; LOAD
; This entrypoint loads the open file (in file 3)
.proc load
@errcode=zp::tmpb
@dev=$ba
	pha
	lda #$0a
	sta @dev
	pla
	jsr $ffbd		; SETNAM

	lda #$03		; file #3
	ldx @dev		; last used device number
	bne :+
	ldx #$0a 		; default to device 10
:	ldy secondaryaddr 	; SA
	jsr $ffba 		; SETLFS

	jsr $ffc0 		; call OPEN
	bcc @ok
	pha
	bcs @closepla

@ok:    ldx #$03      ; filenumber 3
	jsr $ffc6     ; CHKIN (file 2 now used as input)

	; if dir, read load address
	lda secondaryaddr
	bne @l0
	jsr $ffb7
	bne @error
	jsr $ffcf
	jsr $ffb7
	and #$40
	bne @error
	jsr $ffcf

@l0: 	jsr $ffb7     ; call READST (read status byte)
	cmp #$00
	bne @eof      ; either EOF or read error
	jsr $ffcf     ; call CHRIN (get a byte from file)
	ldy isbin     ; skip conversions for binary LOAD
	bne @put
	cmp #$0a      ; convert LF to CR
	bne :+
	lda #$0d
:	cmp #$09	; convert TAB to space
	bne @put
	lda #' '
@put:	jsr putb	; write the byte to source/memory
	jmp @l0		; and continue

@eof:	eor #$40
	beq @close	; if EOF, close with errcode 0

@error:	jsr io::readerr
	txa
@close: pha
@closepla:
	lda #$03      ; filenumber 3
	jsr $ffc3     ; call CLOSE
	jsr $ffcc     ; call CLRCHN
	pla
	cmp #$00
	beq @noerr
	pha
	jsr src::close
	pla
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
;  .XY: the 0-terminated filename to save the buffer to
; OUT:
;  .C: set on error, clear on success
.proc dosave
@name=zp::tmp8
@namelen=zp::tmpa
@end=zp::tmpc
@src=zp::tmpe
@dev=$ba
@namebuff=mem::spare
	sta @namelen
	stxy @name

	ldy #$00
@setname:
	lda (@name),y
	beq @add_p_w
	sta @namebuff,y
	iny
	bne @setname

@add_p_w:
	ldx #$00
:	lda @p_w,x
	sta @namebuff,y
	iny
	inx
	cpx #@p_w_len
	bcc :-
	tya		; .A = name length

@resave:
	ldxy #@namebuff
	jsr $ffbd 	; SETNAM
	lda #$03
	tay
	ldx #$0a	; device 10
	jsr $ffba	; SETLFS

	jsr $ffc0 	; call OPEN
	bcs @error 	; if carry set, the file could not be opened

	jsr io::readerr
	cpx #$00
	beq @drive_ok
	cpx #63		; FILE EXISTS
	beq @retry
	bne @error

@drive_ok:
	ldx #$03	; filenumber 3
	jsr $ffc9	; CHKOUT (file 3 now used as output)

	jsr src::rewind

@save:  jsr $ffb7       ; READST (read status byte)
	bne @error	; error or EOF
@chout: jsr getb
	php		; save EOF flag
	jsr $ffd2
	plp		; get EOF flag
	bcc @save	; loop if !EOF

@error:	jsr io::readerr
	lda #$03      ; filenumber 3
	jsr $ffc3     ; CLOSE
	jsr $ffcc     ; call CLRCHN
	RETURN_ERR ERR_IO_ERROR

; if errcode was 63, try deleting the file and doing the SAVE again
@retry: lda #$03	; filenumber 3
	jsr $ffc3	; CLOSE 3
	lda #$0f	; filenumber 15
	jsr $ffc3	; CLOSE 15
	jsr $ffcc	; CLRCHN

	; delete old file
	ldxy #@namebuff
	lda @namelen
	jsr __file_scratch
	beq :+
	sec
	rts		; scratch failed, leave error from io:readerr for user

: 	; retry the save
	lda @namelen
	clc
	adc #$04	; strlen(@p_w)
	jmp @resave
@p_w:	.byte ",p,w"
@p_w_len=*-@p_w
.endproc

;******************************************************************************
; SAVEBIN
; Saves the binary from the given address, to the given filename.
; NOTE: the address refers to the virtual memory address not the physical
; one.
; IN:
;  - .XY:                     the filename to save the memory range to
;  - __file_save_address:     the start of the address range to save
;  - __file_save_address_end: the end address to save
; OUT:
;  .C: set on error, clear on success
.export __file_save_bin
.proc __file_save_bin
	lda #$01
	sta isbin
	jmp dosave
.endproc

;******************************************************************************
; SAVE SRC
; Saves the active source buffer to a file of the given name
; IN:
;  .XY: the 0-terminated filename to save the source buffer to
; OUT:
;  .C: set on error, clear on success
.export __file_save_src
.proc __file_save_src
	lda #$00
	sta isbin
	jmp dosave
.endproc

;******************************************************************************
; SCRATCH
; Deletes the given filename
; IN:
;  .YX: the filename of the file to delete
;  .A: the length of the file to delete
.export __file_scratch
.proc __file_scratch
@sname=mem::linebuffer2
@name=zp::tmp0
	pha
	stxy @name
	lda #'s'
	sta @sname
	lda #':'
	sta @sname+1

	pla
	tay
	tax
	dey
:	lda (@name),y
	sta @sname+2,y
	dey
	bpl :-

	txa
	clc
	adc #$02
	ldxy #@sname
	jsr $ffbd 	; SETNAM
	lda #$0f
	ldy #$0f
	ldx #$0a	; device 10
	jsr $ffba	; SETLFS
	jsr $ffc0 	; OPEN 15,9,15 "S:FILE"
	bcs @err

@close: lda #15		; filenumber 3
	jsr $ffc3	; CLOSE 15
	jsr $ffcc	; CLRCHN
	lda #$00	; no error
	rts

@err:   jsr io::readerr
	jsr @close
	RETURN_ERR ERR_IO_ERROR
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
	jsr $ffc6     ; CHKIN (file in .A now used as input)

	ldy #$00
@l0:	jsr __file_readb
	bcs @ret	; return err
	cmp #$0d
	beq @done
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
; OPEN
; Opens a file from disk, opens a channel and returns the handle to it.
; you may call the read and write operations withe returned handle to interact
; with it.
; IN:
;  - .XY: the 0-terminated filename of the file to open
; OUT:
;  - .A: Containing the file handle
;  - .C: Set on error
.export __file_open
.proc __file_open
@file=zp::tmp2
@filename=zp::tmp3
	lda zp::numfiles
	cmp #MAX_OPEN_FILES
	bcc :+
	lda #ERR_MAX_FILES_EXCEEDED
	rts

:	stxy @filename
	jsr str::len
	pha

	; find a free file ID
	lda #FIRST_FILE_ID-1
	sta @file
@l0:	inc @file
	lda @file
	ldx zp::numfiles
@l1:	dex
	bmi @found
	cmp files,x
	bne @l1
	beq @l0

@found: ; store entry in files table
	pla		; get length of filename
	ldxy @filename
	jsr $ffbd	; SETNAM

	lda @file	; file handle
	ldx zp::device	; last used device number
	bne :+
	ldx #$0a 	; default to device 10
:	ldx #$0a
	ldy @file	; file #
	jsr $ffba 	; SETLFS
	jsr $ffc0 	; call OPEN
	bcs @getopenerr
	lda @file	; get file ID
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
.export __file_close
.proc __file_close
@file=zp::tmp0
	sei
	jsr $ffc3
	jmp $ffb7
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
	jmp src::end
	;cmp #$80
	;bcs @done	; skip non-printable chars (e.g. breakpoints)
	;jsr $ffd2	; CHROUT (write byte to file)

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
	sta (__file_load_address),y
	incw __file_load_address
	rts
@src:	jmp src::insert
.endproc

.BSS
;******************************************************************************
secondaryaddr: .byte 0	; secondary address to use on file open
