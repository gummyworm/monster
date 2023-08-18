.include "io.inc"
.include "macros.inc"
.include "memory.inc"
.include "source.inc"
.include "string.inc"
.include "util.inc"
.include "zeropage.inc"

MAX_OPEN_FILES = 10
FIRST_FILE_ID = 3

;--------------------------------------
; loaddir loads the directory listing into mem::spare
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
@dir: .byte "$"
.endproc

;--------------------------------------
; load loads the file given in .YX length in .A
.export __file_load
.proc __file_load
	pha
	txa
	pha
	tya
	pha

	jsr src::new

	lda #$03
	sta secondaryaddr
	pla
	tay
	pla
	tax
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
	lda #$09
	sta @dev
	pla
	jsr $ffbd	; SETNAM

	lda #$03	; file #3
	ldx @dev	; last used device number
	bne :+
	ldx #$09 	; default to device 9
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
	cmp #$0a	; convert LF to CR
	bne :+
	lda #$0d
:	sta (@dst),y  ; write byte to memory

	ldx secondaryaddr ; if loading directory, don't update source pointers
	beq @filedone
	jsr src::insert
@filedone:
	incw @dst
	jmp @l0
@eof:
	pha
	and #$40      ; end of file?
	beq @error
	pla
	lda #$00
	skb
@error:
	pla
	sta @errcode
	lda #$03      ; filenumber 3
	jsr $ffc3     ; call CLOSE
	jsr $ffcc     ; call CLRCHN
	lda @errcode
	rts
.endproc

;--------------------------------------
; save saves the buffer to the given file.
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
	ldx #$09
	jsr $ffba	; SETLFS

	jsr $ffc0 	; call OPEN
	bcs @error 	; if carry set, the file could not be opened

	jsr @chk_drive_err
	cpx #$00
	beq @drive_ok
	cpx #63		; FILE EXISTS
	beq @retry
	bne @error

@drive_ok:
	ldx #$03	; filenumber 3
	jsr $ffc9	; CHKOUT (file 3 now used as output)

	jsr src::rewind
@save:
	jsr $ffb7     ; READST (read status byte)
	bne @done
@chout:
	jsr src::next
	jsr $ffd2	; CHROUT (write byte to file)
	jsr src::end	; done yet?
	bne @save

@done:
@error:
	jsr @chk_drive_err
	lda #$03      ; filenumber 3
	jsr $ffc3     ; CLOSE
	jsr $ffcc     ; call CLRCHN
	lda #$00
	rts

@retry:
	lda #$03	; filenumber 3
	jsr $ffc3	; CLOSE 3
	lda #$0f	; filenumber 15
	jsr $ffc3	; CLOSE 15
	jsr $ffcc	; CLRCHN

	; delete old file
	ldxy @name
	lda namelen
	jsr __file_scratch
	beq :+
	rts		; scratch failed, leave error from io:readerr for user

	; retry the save
:	ldxy @name
	lda namelen
	jmp __file_save

;------------------
@chk_drive_err:
	jsr io::readerr
	ldxy #$0100
	jmp atoi

;------------------
@p_w:	.byte ",p,w"
@p_w_len=*-@p_w
.endproc

;--------------------------------------
; scratch deletes the filename given in .YX, length in .A
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
	ldx #$09
	jsr $ffba	; SETLFS
	jsr $ffc0 	; OPEN 15,9,15 "S:FILE"
	bcs @err

@close:
	lda #15		; filenumber 3
	jsr $ffc3	; CLOSE 15
	jsr $ffcc	; CLRCHN
	lda #$00	; no error
	rts
@err:
	jsr io::readerr
	inc $900f
	ldxy #$0100
	jmp *-3
	jmp @close
.endproc

;--------------------------------------
; getline reads the file handle given in .A until EOF or a newline ($0d or $0a) is
; encountered and stores the data at the address given in .XY
; will read a maximum of 255 bytes
; returns:
;  .A contains the # of bytes read
;  .C is set if an error occurred
.export __file_getline
.proc __file_getline
@dst=zp::tmp0
	stxy @dst
	tax
	jsr $ffc6     ; CHKIN (file in .A now used as input)
	ldy #$00
:	jsr $ffb7     ; call READST (read status byte)
	cmp #$00
	bne @eof      ; either EOF or read error
	jsr $ffcf     ; call CHRIN (get a byte from file)
	cmp #$0d
	beq @done
	cmp #$0a
	beq @done
	sta (@dst),y
	iny
	bne :-
@done:
	lda #$00
	sta (@dst),y
	tya
	clc
	rts
@eof:
	and #$20	; EOF?
	bne @done	; yes, return okay
	sec		; error
	rts
.endproc

;--------------------------------------
; open opens a file from disk, opens a channel and returns the handle to it.
; you may call the read and write operations withe returned handle to interact
; with it.
; returns:
;  .A containing the file handle
;  .C set on error
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
	cpx #MAX_OPEN_FILES
	bcc :-
	sec	; no available files
	rts

@found:
	txa
	sta @file
	pla
	ldxy @filename
	jsr $ffbd	; SETNAM

	lda @file
	ldx zp::device	; last used device number
	bne :+
	ldx #$09 	; default to device 9
:	ldy #$03	; SA
	jsr $ffba 	; SETLFS
	jsr $ffc0 	; call OPEN
	lda @file
	rts
.endproc

;--------------------------------------
; close closes the file with the given handle.
.export __file_close
.proc __file_close
@file=zp::tmp0
	tax
	lda #$00
	sta files-FIRST_FILE_ID,x
	txa
	jmp $ffc3
.endproc

.BSS
;--------------------------------------
.export __file_name
__file_name:
name:  .res 16
namelen: .byte 0
secondaryaddr: .byte 0
files: .res 10 ; handles to all open files (1 indicates open, 0 indicates available)
