.include "zeropage.inc"
.include "macros.inc"
.include "memory.inc"
.include "test.inc"
.include "test_macros.inc"
.include "util.inc"

GAPSIZE = 20	; size of gap in gap buffer

.segment "DATA"
.export src_debug
src_debug:
pre:  .word 0       ; # of bytes before the gap
post: .word 0       ; # of bytes after the gap
len:  .word GAPSIZE ; size of the buffer (pre+post+gap)

.export __src_buffer
__src_buffer:
buffer:
data:
.res 1024*4 ; buffer of the active procedure's tokens

.segment "CODE"
;--------------------------------------
;  new initializes the source buffer
.export __src_new
.proc __src_new
	rts
.endproc

;--------------------------------------
; load loads the given file into the buffer.
.export __src_load
.proc __src_load
	rts
.endproc

;--------------------------------------
; save saves the buffer to the given file.
.export __src_save
.proc __src_save
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
.export __src_next
.proc __src_next
src=zp::tmp0
dst=zp::tmp2
	ldxy post
	cmpw #$0000
	beq @skip

	; move char from start of gap to the end of the gap
	jsr cursor
	stx dst
	sty dst+1
	jsr poststart
	stx src
	sty src+1

	ldy #$00
	lda (src),y
	sta (dst),y

	incw pre
	decw post
 @skip:	rts
.endproc

;--------------------------------------
; prev moves the cursor back one character in the gap buffer.
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

	decw pre
	incw post
 @skip:	rts
.endproc

;--------------------------------------
; up moves the cursor back one line or to the start of the buffer if it is
; already on the first line
.export __src_up
.proc __src_up
@cnt=zp::tmp4
	lda #$02
	sta @cnt
@l0:	jsr __src_prev
	jsr atcursor
	cmp #$0d
	beq :+
	jsr cursor
	cmpw #data
	bne @l0
	rts
:	dec @cnt
	bne @l0
	jsr __src_next
	rts
.endproc

;--------------------------------------
; down moves the cursor beyond the next RETURN character (or to the end of
; the buffer if there is no such character
; .C is set if the end of the buffer was reached (cannot move "down")
.export __src_down
.proc __src_down
@l0:	jsr atcursor
	pha
	jsr __src_next
	pla
	cmp #$0d
	beq :+
	ldxy post
	cmpw #0
	bne @l0
	clc
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
	cmpw #$0000
	bne @ins

	; copy data[poststart] to data[poststart + len]
	jsr poststart
	stx @src
	sty @src+1
	txa
	clc
	adc len
	sta @dst
	tya
	adc len+1
	sta @dst+1

	; get size to copy (len)
	ldxy len
	stx @len
	sty @len+1

	jsr util::memcpy
	asl len
	rol len+1

@ins:	jsr cursor
	stx @dst
	sty @dst+1

	ldy #$00
	pla
	sta (@dst),y
	incw pre
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
	lda len
	sec
	sbc post
	tax
	lda len+1
	sbc post+1
	tay

	txa
	sec
	sbc pre
	tax
	tya
	sbc pre+1
	tay
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

	txa
	sec
	sbc post
	tax
	tya
	sbc post+1
	tay
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
	ldy #-1
:	iny
	lda (@src),y
	beq @done
	cmp #$0d
	beq @done
	sta name,y
	cpy #(40-23)
	bcc :-
@done:	lda #' '
:	sta name,y
	iny
	cpy #(40-23)
	bcc :-
	rts
.endproc

;--------------------------------------
; cursor returns the text at the current cursor position in mem::linebuffer
.export __src_get
.proc __src_get
@pre=zp::tmp1
@src=zp::tmp3
@buffend=zp::tmp7
	jsr gaplen
	add16 pre
	add16 #data
	stxy @src
	ldy #$00
@l0:
	lda (@src),y
	beq @done
	cmp #$0d
	beq @done
	sta mem::linebuffer,y
	iny
	cpy #39
	bcc @l0

@done:	lda #$00
	sta mem::linebuffer,y
	rts
.endproc

;--------------------------------------
.export __src_name
__src_name:
name:      .byte "hello world!     " ; the name of the active procedure

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
.endproc
.endif
