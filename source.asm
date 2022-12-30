.include "zeropage.inc"
.include "macros.inc"
.include "memory.inc"
.include "util.inc"

GAPSIZE = 20	; size of gap in gap buffer

.segment "DATA"
.export src_debug
src_debug:
pre:  .word 0       ; # of bytes before the gap
post: .word 0       ; # of bytes after the gap
len:  .word GAPSIZE ; size of the buffer (pre+post+gap)

.export __src_line
__src_line:
line: .word 0       ; the current line # of the cursor

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
	rts
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
@dst=zp::tmp0
	lda #$00
	sta secondaryaddr
	ldxy #mem::spare
	stxy @dst
	ldxy #@dir
	lda #$01
	jmp load
@dir: .byte "$"
.endproc

;--------------------------------------
; load loads the given file into the buffer.
.export __src_load
.proc __src_load
@dst=zp::tmp0
	lda #$02
	sta secondaryaddr
	ldxy #__src_buffer
	stxy @dst

	ldy #$00
	sty len
	sty len+1
	sty post
	sty post+1
	sty pre
	sty pre+1

	lda #namelen
	ldxy #name
	jsr load
	jmp __src_rewind
.endproc

;--------------------------------------
; load loads the filename given in (YX) (of length given in .A)
; into the address contained by zp::tmp0
.proc load
@dst=zp::tmp0
@dev=$ba
	pha
	lda #$09
	sta @dev
	pla
	jsr $ffbd	; SETNAM

	lda #$02	; file #2
	ldx @dev	; last used device number
	bne :+
	ldx #$0a 	; default to device 10
:	ldy secondaryaddr ; SA 2
	jsr $ffba 	; SETLFS

	jsr $ffc0 	; call OPEN
	bcs @error 	; if carry set, the file could not be opened

	ldx #$02      ; filenumber 2
	jsr $ffc6     ; CHKIN (file 2 now used as input)

	; read load address
	jsr $ffb7
	bne @error
	jsr $ffcf
	jsr $ffb7
	bne @error
	jsr $ffcf

@l0: 	jsr $ffb7     ; call READST (read status byte)
	cmp #$00
	bne @eof      ; either EOF or read error
	jsr $ffcf     ; call CHRIN (get a byte from file)
	ldy #$00
	sta (@dst),y  ; write byte to memory

	lda secondaryaddr ; if loading directory, don't updated cursor
	beq :+
	incw pre
	incw len
:	incw @dst
	jmp @l0
@eof:
	and #$40      ; end of file?
	beq @error
@close:
	lda #$02      ; filenumber 2
	jsr $ffc3     ; call CLOSE
	jsr $ffcc     ; call CLRCHN
	rts
@error:
	jsr @close
	inc $900f
	jmp *-3
.endproc

;--------------------------------------
; save saves the buffer to the given file.
.export __src_save
.proc __src_save
@tmp=zp::tmp0
@sz=zp::tmp2
@dev=$ba
	lda #$09
	sta @dev
	jsr __src_getall
	stxy @sz

	lda #namelen
	ldxy #name
	jsr $ffbd 	; SETNAM
	lda #$00
	ldx @dev 	; last used device number
	bne :+
	ldx #$08	; default to device 8
:	ldy #$00
	jsr $ffba	; SETLFS

	ldxy #mem::spare
	stxy @tmp
	add16 @sz
	lda #@tmp
	jsr $ffd8	; SAVE
	bcs @err	; if carry set, a load error has happened
	rts

@err:	inc $900f
	jmp @err
	rts
.endproc

;--------------------------------------
; backspace deletes the character immediately before the current cursor position.
.export __src_backspace
.proc __src_backspace
	ldxy pre
	cmpw #$0000
	beq @skip
	decw pre
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
; .C is set if cursor is at the start of the buffer
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
	jsr __src_insert
	rts
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
; puts adds the string in (YX) of length .A to the buffer at the cursor's
; position
.export __src_puts
.proc __src_puts
	sta @cnt
	stxy @src
@src=*+1
@l0:	lda $ffff
	jsr __src_insert
	incw @src
	dec @cnt
	bne @l0
	rts
@cnt: .byte 0
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
	cmp #$0d
	beq @done
	sta name,y
	cpy #(40-23)
	bcc :-
@done:	lda #$00
	sta name,y
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
	cmpw #1
	bne @l0
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
	cmp #$0d
	bne :+
	incw line
:	jsr __src_next
	pla
	rts
.endproc

;--------------------------------------
; readline reads one line (including $0d) at the cursor positon and advances
; the cursor
; Out:
;  mem::linebuffer: the line that was read
;  .A is $0d if the last character read was a RETURN ($0d)
;
.export __src_readline
.proc __src_readline
@cnt=zp::tmp4
	lda #$00
	sta @cnt
@l0:	jsr __src_readb
	ldx @cnt
	sta mem::linebuffer,x
	cmp #$0d
	beq @done
	inc @cnt
	ldxy post
	cmpw #0
	bne @l0
@done:	rts
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
name:      .byte "test.s",0 ; the name of the active procedure
namelen=*-name
secondaryaddr: .byte 0
