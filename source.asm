.include "zeropage.inc"
.include "macros.inc"
.include "test.inc"
.include "test_macros.inc"
.include "util.inc"

GAPSIZE = 20	; size of gap in gap buffer

;--------------------------------------
; load loads the given file into the buffer.
.export __src_load
.proc __src_load
.endproc

;--------------------------------------
; save saves the buffer to the given file.
.export __src_save
.proc __src_save
.endproc

;--------------------------------------
;  new initializes the source buffer
.export __src_new
.proc __src_new
	lda #<__src_buffer
	sta zp::gap

	lda #GAPSIZE
	sta zp::gapsize
	clc
	adc #<__src_buffer
	sta buffend

	lda #>__src_buffer
	sta zp::gap+1
	adc #$00
	sta buffend+1

	lda #$00
	ldy #GAPSIZE-1
@l0:	sta __src_buffer,y
	dey
	bpl @l0
	rts
.endproc


;--------------------------------------
; line returns the address of the line whose number is given in (YX) in (YX).
.export __src_line
.proc __src_line
@src=zp::tmp0
@cnt=zp::tmp2
	stx @cnt
	sty @cnt+1

	ldx #<buffer
	ldy #>buffer
	stx @src
	sty @src+1

	ldy #$00
@l0:	lda (@src),y
	incw @src
	cmp #$0d
	bne @l0
	lda @cnt
	bne :++
	lda @cnt+1
	bne :+
	ldx @src
	ldy @src+1
	rts

:	dec @cnt+1
	jmp @l0
:	dec @cnt
	jmp @l0
.endproc

;--------------------------------------
; opengap opens a gap GAPSIZE bytes in size at the position in cur.
; This procedure should only be called when a new gap is needed (when
; cur==gapend).
.proc opengap
@src=zp::tmp0
@dst=zp::tmp2
@cnt=zp::tmp4
	; move all the data from the cursor position up GAPSIZE bytes
	; copy from memmove(zp::gap+GAPSIZE, zp::gap, buffend - zp::gap)
	lda buffend
	sta @src
	clc
	adc #GAPSIZE
	sta @dst
	sta buffend

	lda buffend+1
	sta @src+1
	adc #$00
	sta @dst+1
	sta buffend+1

@l0:	; move one byte from (src) to (dst) until src == zp::gap
	ldy #$00
	lda (@src),y
	sta (@dst),y

	ldx @src
	ldy @src+1
	decw @src
	decw @dst
	cmpw zp::gap
	bne @l0

@done:	lda #GAPSIZE
	sta zp::gapsize
	rts
.endproc

;--------------------------------------
; next moves the cursor up one character in the gap buffer.
.export __src_next
.proc __src_next
	lda #$00
	sta zp::err

	ldx #<buffend
	ldy #>buffend
	cmpw zp::gap
	bne @ok

@err:	dec zp::err	; already at end of buffer
	rts

@ok:	; move character from end of gap to the start
	ldy zp::gapsize
	lda (zp::gap),y
	ldy #$00
	sta (zp::gap),y
	incw zp::gap
	rts
.endproc

;--------------------------------------
; prev moves the cursor up one character back in the gap buffer.
.export __src_prev
.proc __src_prev
	lda #$00
	sta zp::err

	ldx #<buffer
	ldy #>buffer
	cmpw zp::gap
	bne @ok

@err:	dec zp::err	; already at start of buffer
	rts

@ok:	; move char from start of gap to the end of the gap
	decw zp::gap
	ldy #$00
	lda (zp::gap),y
	ldy zp::gapsize
	sta (zp::gap),y
 	rts
.endproc

;--------------------------------------
; lineup updates the gap buffer by moving it from the current position to the
; start of the previous line plus the offset given in .A.
.export __src_lineup
.proc __src_lineup
@cur=zp::tmp0
@cnt=zp::tmp2
@stop=zp::tmp3
	sta @cnt
	lda #$02	; # of $0d characters to read past
	sta @stop

	; find start of previous line (2 $0d bytes back)+1
@l0:	jsr __src_prev
	lda zp::err
	beq @cont
	rts

@cont:	ldy #$00
	lda (zp::gap),y
	cmp #$0d
	bne @l0
	dec @stop
	bne @l0

@l1:	dec @cnt
	bmi @done
	jsr __src_next
	lda zp::err
	bne @l1
@done:	jsr __src_next
	rts
.endproc

;--------------------------------------
; puts adds the string in (YX) of length .A to the buffer at the cursor's
; position
.export __src_puts
.proc __src_puts
	sta @cnt
	stx @src
	sty @src+1
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
; rewind sets cur to the beginning of buffer.
.export __src_rewind
.proc __src_rewind
	lda #<buffer
	sta zp::gap
	lda #>buffer
	sta zp::gap+1
	rts
.endproc

;--------------------------------------
; getline updates cur to point past the end of the current line.
; $ff is returned if there is no more data to get.
.export __src_getline
.proc __src_getline
@l0:	lda (zp::gap),y
	cmp #$0d
	beq @done
	jsr __src_next
	lda zp::err
	beq @l0
	rts
@done:	lda #$00
	rts
.endproc

;--------------------------------------
; getrow returns the text at the row number given in (YX). zp::err is non-zero
; if the row does not exist
.export __src_getrow
.proc __src_getrow
@src=zp::tmp0
@cnt=zp::tmp2
	stx @cnt
	sty @cnt+1

	lda #<buffer
	sta @src
	lda #>buffer
	sta @src+1

	lda #$00
	sta zp::err

@l0:	; if we've reached or passed the end of the buffer, err
	lda @src+1
	cmp buffend+1
	bcc :+
	lda @src
	cmp buffend
	bcc :+
	dec zp::err
	rts

	; if we've reached the gap, skip over it
:	ldx @src
	ldy @src+1
	cmpw zp::gap
	bne :+
	lda @src
	clc
	adc zp::gapsize
	sta @src
	lda @src+1
	adc #$00
	sta @src+1
	jmp @l0

:	; if the count has reached zero, we're done
	lda @cnt
	bne :+
	lda @cnt+1
	bne :+
	ldx @src
	ldy @src+1
	rts

:	; check if we're at the end of a line
	ldy #$00
	lda (@src),y
	cmp #$0d
	bne :+
	decw @cnt

:	incw @src
	jmp @l0
.endproc

;--------------------------------------
; insert adds the character in .A to the buffer at the gap position (zp::gap).
.export __src_insert
.proc __src_insert
	pha
	lda zp::gapsize
	bne @ins
	jsr opengap ; cursor is at end of gap, open a new gap
@ins:	ldy #$00
	pla
	sta (zp::gap),y
	incw zp::gap
	dec zp::gapsize
	bne :+
	jsr opengap
:	rts
.endproc

;--------------------------------------
; delete deletes the character immediately after the current cursor position.
.export __src_delete
.proc __src_delete
	; if (gap + gapsize) == buffend, nothing to delete (at end of buffer)
	lda zp::gap
	clc
	adc zp::gapsize
	tax
	lda zp::gap+1
	adc #$00
	tay
	cmpw buffend
	beq @skip

	inc zp::gapsize
@skip:	rts
.endproc

;--------------------------------------
; backspace deletes the character immediately before the current cursor position.
.export __src_backspace
.proc __src_backspace
	; if gap == buffer, nothing to delete (at the start of the buffer)
	ldx zp::gap
	ldy zp::gap+1
	cmpw buffer
	beq @skip

	decw zp::gap
	inc zp::gapsize
@skip:	rts
.endproc

;--------------------------------------
; replaceline replaces the contents at (cur) with the line given in (YX).
; Once a newline is encountered in the buffer, the contents of the given line
; are inserted into the buffer.
.export __src_replaceline
.proc __src_replaceline
@line=zp::tmp0
@buff=zp::gap
	stx @line
	sty @line

	ldy #$00
@l0:	lda (@buff),y
	cmp #$0d
	beq @ins
	lda (@line),y
	sta (@buff),y
	incw @line
	incw @buff
	cmp #$0d
	bne @l0
@done:	rts

@ins:	ldy #$00
	lda (@line),y
	cmp #$0d
	beq @done
	jsr __src_insert
	jmp @ins
.endproc

;--------------------------------------
; pos returns the position of the cursor in the source buffer in (YX)
.export __src_pos
.proc __src_pos
	lda zp::gap
	sec
	sbc #<buffer
	tax
	lda zp::gap+1
	sbc #>buffer
	tay
	rts
.endproc

;--------------------------------------
; size returns the size of the file in (YX)
.export __src_size
.proc __src_size
	lda zp::gapsize
	adc #<buffer
	sta @lsb
	lda #>buffer
	adc #$00
	sta @msb

	lda buffend
	sec
@lsb=*+1
	sbc #$00
	tax
	lda buffend+1
@msb=*+1
	sbc #$00
	tay
	rts
.endproc

;--------------------------------------
name:      .res 16  ; the name of the active procedure

.export __src_buffer
__src_buffer:
buffer:	.res 1024*4 ; buffer of the active procedure's tokens
buffend: .word buffer+GAPSIZE


;--------------------------------------
.ifdef TEST
testtext:
line1: .byte "hello world",$0d
line1_len = *-line1
line2: .byte "line 2",$0d
line2_len = *-line2
line3: .byte "line 3",$0d
line3_len = *-line3
testtextlen = *-testtext

testinsert: .byte "insert",$0d
testinsertlen = *-testinsert
testinsert2: .byte "wwwutttt",$0d
testinsert2len = *-testinsert2

.export __src_test
.proc __src_test
	ldx #<testtext
	ldy #>testtext
	lda #testtextlen
	jsr __src_puts

test_getrow:
	ldx #0
	ldy #0
	jsr __src_getrow
	streq line1, line1_len
	assertz

	ldx #1
	ldy #0
	jsr __src_getrow
	streq line2, line2_len
	assertz

	ldx #2
	ldy #0
	jsr __src_getrow
	streq line3, line3_len
	assertz

test_lineup:
	jsr __src_lineup
	; verify the gap is correctly positioned after moving up a line
	ldx zp::gap
	ldy zp::gap+1
	streq line3, line3_len
	assertz

test_insert_after_lineup:
	ldx #<testinsert
	ldy #>testinsert
	lda #testinsertlen
	jsr __src_puts

	; the newly inserted row should appear as row 2
	ldx #2
	ldy #0
	jsr __src_getrow
	streq testinsert, testinsertlen
	assertz

	; line3 should now be in row 3.
	ldx #3
	ldy #0
	jsr __src_getrow
	streq line3, line3_len
	assertz

	; getrow should fail when line 4 is requested
	ldx #4
	ldy #0
	jsr __src_getrow
	lda zp::err
	assertnz

	; do another insert (it should be placed between lines 2 and 3)
	ldx #<testinsert2
	ldy #>testinsert2
	lda #testinsert2len
	jsr __src_puts
	ldx #3
	ldy #0
	jsr __src_getrow
	streq testinsert2, testinsert2len
	assertz

	ldx #4
	ldy #0
	jsr __src_getrow
	streq line3, line3_len
	assertz

ok:	rts
.endproc
.endif
