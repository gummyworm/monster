.include "io.inc"
.include "irq.inc"
.include "zeropage.inc"
.include "macros.inc"
.include "memory.inc"
.include "util.inc"

GAPSIZE = 20	; size of gap in gap buffer

.segment "DATA"
.export src_debug

;--------------------------------------
data_start:

sp: .byte 0
stack: .res 32


src_debug:
pre:  .word 0       ; # of bytes before the gap
post: .word 0       ; # of bytes after the gap

.export __src_line
__src_line:
line: .word 0       ; the current line # of the cursor

data_end:

len:  .word GAPSIZE ; size of the buffer (pre+post+gap)
;--------------------------------------

.export __src_buffer
__src_buffer:
buffer:
data:
.res 1024*4 ; buffer of the active procedure's tokens

.CODE
;--------------------------------------
;  new initializes the source buffer
.export __src_new
.proc __src_new
	ldx #data_end-data_start-1
	lda #$00
:	sta data_start,x
	dex
	bpl :-
	lda #GAPSIZE
	sta len
	rts
.endproc

;--------------------------------------
; pushp pushes the current source position to an internal stack.
.export __src_pushp
.proc __src_pushp
	lda pre
	pha
	lda pre+1
	tay

	lda sp
	asl
	tax
	inc sp

	pla
	sta stack,x
	tya
	sta stack+1,x
	rts
.endproc

;--------------------------------------
; popp returns the the most recent source position pushed in .YX
.export __src_popp
.proc __src_popp
	lda sp
	beq :+

	dec sp
	lda sp
	asl
	tax
	lda stack+1,x
	tay
	lda stack,x
	tax
:	rts
.endproc

;--------------------------------------
;  end returns .Z set if the cursor is at the end of the buffer.
.export __src_end
.proc __src_end
	ldxy post
	cmpw #$0000
	rts
.endproc

;--------------------------------------
;  start returns .Z set if the cursor is at the start of the buffer.
.export __src_start
.proc __src_start
	ldxy pre
	cmpw #$0000
	rts
.endproc

;--------------------------------------
; loaddir loads the directory listing into mem::spare
.export __src_loaddir
.proc __src_loaddir
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
; loadfile loads the file given in .YX length in .A
.export __src_loadfile
.proc __src_loadfile
	pha
	txa
	pha
	tya
	pha

	jsr __src_new

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
__src_load:
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
	jsr __src_insert
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
.export __src_save
.proc __src_save
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

	jsr __src_rewind
@save:
	jsr $ffb7     ; READST (read status byte)
	bne @done
@chout:
	jsr __src_next
	jsr $ffd2	; CHROUT (write byte to file)
	jsr __src_end	; done yet?
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
	jsr __src_scratchfile
	beq :+
	rts		; scratch failed, leave error from io:readerr for user

	; retry the save
:	ldxy @name
	lda namelen
	jmp __src_save

;------------------
@chk_drive_err:
	jsr io::readerr
	ldxy #$0100
	jmp util::atoi

;------------------
@p_w:	.byte ",p,w"
@p_w_len=*-@p_w
.endproc

;--------------------------------------
; scratch deletes the filename given in .YX, length in .A
.export __src_scratchfile
.proc __src_scratchfile
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
; backspace deletes the character immediately before the current cursor position.
.export __src_backspace
.proc __src_backspace
	jsr __src_start
	beq @skip
	jsr atcursor
	cmp #$0d
	bne :+
	decw line
:	decw pre
@skip:	rts
.endproc

;--------------------------------------
; delete deletes the character at the current cursor position.
.export __src_delete
.proc __src_delete
	ldxy post
	cmpw #$0000
	beq @skip
	decw post
@skip:	rts
.endproc

;--------------------------------------
; next moves the cursor up one character in the gap buffer.
; Returns the character at the new cursor position in .A
.export __src_next
.proc __src_next
@src=zp::tmp0
@dst=zp::tmp2
	ldxy post
	cmpw #$0000
	beq @skip

	; move char from start of gap to the end of the gap
	jsr cursor
	stx @dst
	sty @dst+1
	jsr poststart
	stx @src
	sty @src+1

	ldy #$00
	lda (@src),y
	sta (@dst),y

	cmp #$0d
	bne :+
	incw line

:	incw pre
	decw post
 @skip:	jmp atcursor
.endproc

;--------------------------------------
; right moves to the next character unless it is a newline
; .C is set if the cursor was moved, clear if not
.export __src_right
.proc __src_right
	ldxy post
	cmpw #0
	bne :+
	clc
	rts

:	jsr __src_next
	cmp #$0d
	bne :+
	jsr __src_prev
	clc
	rts

:	sec
	rts
.endproc

;--------------------------------------
; prev moves the cursor back one character in the gap buffer.
; The character at the new position is returned in .A
.export __src_prev
.proc __src_prev
@src=zp::tmp0
@dst=zp::tmp2
	ldxy pre
	cmpw #0
	beq @skip

	; move char from start of gap to the end of the gap
	jsr cursor
	stxy @src
	jsr poststart
	stxy @dst

	decw @src
	decw @dst
	ldy #$00
	lda (@src),y
	sta (@dst),y

	cmp #$0d
	bne :+
	decw line

:	decw pre
	incw post
 @skip:	jmp atcursor
.endproc

;--------------------------------------
; up moves the cursor back one line or to the start of the buffer if it is
; already on the first line
; this will leave the cursor on the first newline character encountered while
; going backwards through the source.
; .C is set if cursor is at the start of the buffer
; .A contains the character at the cursor position
.export __src_up
.proc __src_up
	ldxy pre
	cmpw #0
	bne @l0
	sec
	rts

@l0:	jsr __src_prev
	cmp #$0d
	beq :+
	ldxy pre
	cmpw #0
	bne @l0
	sec
	rts
:	clc
	rts
.endproc

;--------------------------------------
; down moves the cursor beyond the next RETURN character (or to the end of
; the buffer if there is no such character
; .C is set if the end of the buffer was reached (cannot move "down")
.export __src_down
.proc __src_down
	ldxy post
	cmpw #0
	bne @l0
	clc
	rts

@l0:	jsr __src_next
	cmp #$0d
	beq :+
	ldxy post
	cmpw #0
	bne @l0
	clc	; end of the buffer
:	rts
.endproc

;--------------------------------------
; insert adds the character in .A to the buffer at the gap position (gap).
.export __src_insert
.proc __src_insert
@len=zp::tmp0
@src=zp::tmp2
@dst=zp::tmp4
	pha
	jsr gaplen
	cmpw #0		; is gap closed?
	bne @ins	; no, insert as usual

	; gap is closed, create a new one
	; copy data[poststart] to data[poststart + len]
	jsr poststart
	stxy @src
	add16 len
	stxy @dst

	; get size to copy (len)
	copy @dst, @src, len

	; double size of buffer (new gap size is the size of the old buffer)
	asl len
	rol len+1

@ins:	jsr cursor
	stxy @dst
	ldy #$00
	pla
	sta (@dst),y
	cmp #$0d
	bne :+
	incw line
:	incw pre
	rts
.endproc

;--------------------------------------
; replace adds the character in .A to the buffer at the cursor position,
; replacing the character that currently resides there
.export __src_replace
.proc __src_replace
	pha
	jsr __src_delete
	pla
	jmp __src_insert
.endproc

;--------------------------------------
; gaplen returns the length of the gap buffer in (>Y,<X)
.proc gaplen
	ldxy len
	sub16 post
	sub16 pre
	rts
.endproc

;--------------------------------------
; cursor returns the address of the current cursor position within (data).
.proc cursor
	ldxy #data
	add16 pre
	rts
.endproc

;--------------------------------------
; atcursor returns the character at the cursor position.
.export __src_atcursor
__src_atcursor:
.proc atcursor
	jsr cursor
	sub16 #1
	stxy zp::tmp0
	ldy #$00
	lda (zp::tmp0),y
	rts
.endproc

;--------------------------------------
;poststart returns the address of the post-start section of the gap buffer
.proc poststart
	ldxy #data
	add16 len
	sub16 post
	rts
.endproc

;--------------------------------------
; rename names the buffer to the string in (YX)
.export __src_rename
.proc __src_rename
@src=zp::tmp0
	stxy @src
	ldy #$ff
:	iny
	lda (@src),y
	beq @done
	sta name,y
	cpy #(40-23)
	bcc :-
@done:	sta name,y
	rts
.endproc

;--------------------------------------
; getall sets mem::spare to the entire buffer (without gap).
; YX contains the length of the buffer upon return.
.export __src_getall
.proc __src_getall
@len=zp::tmp0
@src=zp::tmp2
@dst=zp::tmp4
	; copy "pre" section of gap buffer
	copy #mem::spare, #data, pre

	; copy "post" section
	jsr poststart
	stxy @src
	ldxy mem::spare
	add16 pre
	stxy @dst
	ldxy post
	stxy @len
	jsr util::memcpy

	ldxy post
	add16 pre
	rts
.endproc

;--------------------------------------
; rewind moves the cursor back to the start of the buffer
.export __src_rewind
.proc __src_rewind
@l0:	jsr __src_prev
	ldxy pre
	cmpw #0
	bne @l0
@done:
	rts
.endproc

;--------------------------------------
; readb reads one byte at the cursor positon and advances the cursor
; Out:
;  .A: the byte that was read
;
.export __src_readb
.proc __src_readb
	jsr atcursor
	pha
	jsr __src_next
	pla
	rts
.endproc

;--------------------------------------
; readline reads one line at the cursor positon and advances the cursor
; Out:
;  mem::linebuffer: the line that was read will be 0-terminated
;  .A is $0d if the last character read was a RETURN ($0d)
;
.export __src_readline
.proc __src_readline
@cnt=zp::tmp4
	lda #$00
	sta @cnt
@l0:	jsr __src_readb
	ldx @cnt
	cmp #$0d
	bne :+
	lda #$00
:	sta mem::linebuffer,x
	beq @done
	inc @cnt
	ldxy post
	cmpw #0
	bne @l0
	; null terminate if end of source
	lda #$00
	ldx @cnt
	sta mem::linebuffer,x
@done:	rts
.endproc

;--------------------------------------
; goto goes to the source position given in .YX
.export __src_goto
.proc __src_goto
@dest=zp::tmp4
	stxy @dest
	cmpw pre
	beq @done
	bcc @backwards
@forwards:
	jsr __src_next
	ldxy pre
	cmpw @dest
	bne @forwards
	rts
@backwards:
	jsr __src_prev
	ldxy pre
	cmpw @dest
	bne @backwards
@done:
	rts
.endproc

;--------------------------------------
; gotoend goes to the last line, last character in the buffer
.export __src_gotoend
.proc __src_gotoend
	; TODO:
.endproc

;--------------------------------------
; gotolastline goes to the last line, first character in the buffer
.export __src_gotolastline
.proc __src_gotolastline
	; TODO:
.endproc

;--------------------------------------
; get returns the text at the current cursor position in mem::linebuffer
.export __src_get
.proc __src_get
@cnt=zp::tmp1
@src=zp::tmp3
	jsr gaplen
	add16 pre
	add16 #data
	stxy @src

	ldxy post
	cmpw #$00
	beq @done
	stxy @cnt
	incw @cnt

	ldy #$00
@l0:	lda (@src),y
	beq @done
	cmp #$0d
	beq @done
	sta mem::linebuffer,y
	decw @cnt
	lda @cnt+1
	bne :+
	lda @cnt
	beq @done
:	iny
	cpy #39
	bcc @l0

@done:	lda #$00
	sta mem::linebuffer,y
	rts
.endproc

;--------------------------------------
.export __src_name
__src_name:
name:      .byte "test.s" ; the name of the active procedure
.res 13
namelen: .byte 6
secondaryaddr: .byte 3
