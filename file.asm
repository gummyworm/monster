.include "errors.inc"
.include "finalex.inc"
.include "io.inc"
.include "macros.inc"
.include "memory.inc"
.include "source.inc"
.include "string.inc"
.include "util.inc"
.include "zeropage.inc"

MAX_OPEN_FILES = 10
FIRST_FILE_ID = 2

;******************************************************************************
; LOADDIR
; Loads the directory listing into mem::spare
; OUT:
;  - mem::spare: contains the directory listing
;  - .C: set on error, clear on success
.export __file_loaddir
.proc __file_loaddir
@dst=zp::tmpb
	lda #$00
	sta secondaryaddr
	ldx #<mem::spare
	ldy #>mem::spare
	stx @dst
	sty @dst+1
	ldxy #@dir
	lda #$01
	jmp load
@dir:  .byte "$"
.endproc

;******************************************************************************
; LOAD
; loads the given file into a new source buffer
; IN:
;  .XY: the filename to load
;  .A: the length of the file
;  zp::tmpb: the address to load the file to
; OUT:
;  .C: set on error, clear on success
.export __file_load
.proc __file_load
@filename=zp::tmp10
@file=zp::tmp12
	pha
	stxy @filename

; try to read from the file to make sure it exists before we do anything else
	jsr __file_open
	bcc @ok
@err:	jmp __file_close
@ok:	sta @file
	ldxy #$120
	jsr __file_getline
	bcs @err
	lda @file
	jsr __file_close

	jsr src::new
	ldxy @filename
	jsr src::name

	lda #$03
	sta secondaryaddr
	ldxy @filename
	pla
	; fallthrough
.endproc
;--------------------------------------
; load loads the filename given in (YX) (of length given in .A)
; into the address contained by zp::tmp0
.proc load
@dst=zp::tmpb
@errcode=zp::tmpb
@dev=$ba
	pha
	lda #$0a
	sta @dev
	pla
	jsr $ffbd	; SETNAM

	lda #$03	; file #3
	ldx @dev	; last used device number
	bne :+
	ldx #$0a 	; default to device 10
:	ldy secondaryaddr ; SA
	jsr $ffba 	; SETLFS

	jsr $ffc0 	; call OPEN
	bcs @error 	; if carry set, the file could not be opened

	ldx #$03      ; filenumber 3
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
	ldy #$00
	cmp #$0a 	; convert LF to CR
	bne :+
	lda #$0d
:	cmp #$09      ; convert TAB to space
	bne :+
	lda #' '
:	ldx secondaryaddr
	bne @src
@dir:	; if loading directory, write to memory
	sta (@dst),y
	incw @dst
	bne @l0
@src:	jsr src::insert
	jmp @l0

@eof:	eor #$40
	beq @close	; if EOF, close with errcode 0

@error:	jsr io::readerr
	txa
@close: pha
	lda #$03      ; filenumber 3
	jsr $ffc3     ; call CLOSE
	jsr $ffcc     ; call CLRCHN
	pla
	rts
.endproc

;******************************************************************************
; SAVE
; Saves the source buffer to the given file.
; IN:
;  .XY: the 0-terminated filename to save the buffer to
; OUT:
;  .C: set on error, clear on success
.export __file_save
.proc __file_save
@name=zp::tmp4
@dev=$ba
	sta namelen
	stxy @name
	ldy #$00

@setname:
	lda (@name),y
	beq @add_p_w
	sta name,y
	iny
	bne @setname

@add_p_w:
	ldx #$00
:	lda @p_w,x
	sta name,y
	iny
	inx
	cpx #@p_w_len
	bcc :-
	tya		; .A = name length
	ldxy #name
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
	bne @done

@chout: jsr src::next
	jsr $ffd2	; CHROUT (write byte to file)
	jsr src::end	; done yet?
	bne @save

@done:
@error: jsr io::readerr
	lda #$03      ; filenumber 3
	jsr $ffc3     ; CLOSE
	jsr $ffcc     ; call CLRCHN
	RETURN_ERR ERR_IO_ERROR

@retry: lda #$03	; filenumber 3
	jsr $ffc3	; CLOSE 3
	lda #$0f	; filenumber 15
	jsr $ffc3	; CLOSE 15
	jsr $ffcc	; CLRCHN

	; delete old file
	ldxy @name
	lda namelen
	jsr __file_scratch
	beq :+
	sec
	rts		; scratch failed, leave error from io:readerr for user

: 	; retry the save
	ldxy @name
	lda namelen
	jmp __file_save

;------------------
@p_w:	.byte ",p,w"
@p_w_len=*-@p_w
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
; GETLINE
; Reads the file handle given in .A until EOF or a newline ($0d or $0a) is
; encountered and stores the data at the address given in .XY
; will read a maximum of 255 bytes
; IN:
;  - .XY: the address to read the line into
;  - .A: the file ID
; OUT:
;  - .A: contains the # of bytes read
;  - .C: is set if an error occurred
.export __file_getline
.proc __file_getline
@dst=zp::tmp0
	stxy @dst
	tax
	jsr $ffc6     ; CHKIN (file in .A now used as input)

	ldy #$00
@l0:	jsr $ffb7     ; call READST (read status byte)
	cmp #$00
	bne @eof      ; either EOF or read error
	jsr $ffcf     ; call CHRIN (get a byte from file)
	cmp #$0d
	beq @done
	cmp #$0a
	beq @done
	cmp #$09	; TAB
	bne :+
	lda #' '
:	sta (@dst),y
	iny
	bne @l0

@done:  lda #$00
	sta (@dst),y	; 0-terminate the string
	tya		; put # of bytes read in .A
	RETURN_OK

; read drive err chan and translate CBM DOS error code to ours if possible
@eof:  	jsr io::readerr
	txa
	cmp #62		; FILE NOT FOUND
	bne :+
	lda #ERR_FILE_NOT_FOUND
:	cmp #$01	; set .C if error > 0
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
	stxy @filename
	jsr str::len
	pha

	ldx #FIRST_FILE_ID
:	lda files-FIRST_FILE_ID,x
	beq @found
	inx
	cpx #MAX_OPEN_FILES+FIRST_FILE_ID
	bcc :-
	RETURN_ERR ERR_MAX_FILES_EXCEEDED

@found: ; store entry in files table
	txa
	sta @file
	sta files-FIRST_FILE_ID,x

	pla		; get length of filename
	ldxy @filename
	jsr $ffbd	; SETNAM

	lda @file	; file handle
	ldx zp::device	; last used device number
	bne :+
	ldx #$0a 	; default to device 10
:	ldx #$0a
	ldy #$03	; SA
	jsr $ffba 	; SETLFS
	jsr $ffc0 	; call OPEN
	bcs @getopenerr
	lda @file	; get file ID
	rts		; return it

@getopenerr:
	cmp #$01
	bne :+
	RETURN_ERR ERR_TOO_MANY_OPEN_FILES
:	cmp #$02
	bne :+
	RETURN_ERR ERR_LOGICAL_FILE_IN_USE
:	cmp #$05
	bne :+
	RETURN_ERR ERR_DRIVE_DID_NOT_RESPOND
:	RETURN_ERR ERR_IO_ERROR	; unknown error
.endproc

;******************************************************************************
; CLOSE
; Closes the file with the given handle.
; IN:
;  - .A: the file handle to close
.export __file_close
.proc __file_close
@file=zp::tmp0
	tax
	lda #$00
	sta files-FIRST_FILE_ID,x	; clear entry in files table
	txa
	sei
	jsr $ffc3
	jmp $ffb7
.endproc

.BSS
;******************************************************************************
.export __file_name
__file_name:
name:  .res 16		; file name
namelen: .byte 0	; length of name sent for SETNAM
secondaryaddr: .byte 0	; secondary address to use on file open
files: .res 10 		; handles to all open files (1=open, 0=available)
