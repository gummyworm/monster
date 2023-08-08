.include "io.inc"
.include "macros.inc"
.include "memory.inc"
.include "source.inc"
.include "util.inc"
.include "zeropage.inc"

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
	sta (@dst),y  ; write byte to memory

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
.DATA

;--------------------------------------
.export __file_name
__file_name:
name:      .byte "test.s" ; the name of the active procedure
.res 13
namelen: .byte 6
secondaryaddr: .byte 3
