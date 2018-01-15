.include "zeropage.inc"
.include "macros.inc"
.include "test.inc"

GAPSIZE = 40	; size of gap in gap buffer

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
@stop=zp::tmp4
	; get copy src/destination
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

	; get address to stop copy
	lda gapend
	sta @stop
	lda gapend+1
	sta @stop+1

	; copy (buffer) to (buffer+GAPSIZE)
@l0:    ldx @src
	ldy @src+1
	cmpw @stop
	beq @done

	ldy #$00
	lda (@src),y
	sta (@dst),y
	decw @src
	decw @dst
	jmp @l0

@done:	; update gap end pointer
	lda #GAPSIZE
	clc
	adc gapend
	sta gapend 
	lda gapend+1
	adc #$00
	sta gapend+1
	rts
.endproc

;--------------------------------------
; moveb moves the cursor .A bytes backward moving the text between cur and
; the new location to the second buffer.
.export __src_moveb
.proc __src_moveb
@src=zp::tmp0
@dst=zp::tmp2
@sz=zp::tmp4
	tay
	sta @sz

	lda gapend
	sec
	sbc @sz
	sta @dst
	sta gapend
	lda gapend+1
	sbc #$00
	sta @dst+1
	sta gapend+1

	lda cur
	sbc @sz
	sta @src
	sta cur
	lda cur+1
	sbc #$00
	sta @dst+1
	sta cur+1

	dey
@l0:	lda (@src),y
	sta (@dst),y
	dey
	bpl @l0
	rts
.endproc

;--------------------------------------
; movef moves the cursor .A bytes forward moving the text between cur and 
; the new location to the first buffer.
.export __src_movef
.proc __src_movef
@src=zp::tmp0
@dst=zp::tmp2
@sz=zp::tmp4
	tay
	sta @sz

	lda gapend
	clc
	adc @sz
	sta @dst
	lda gapend+1
	adc #$00
	sta @dst+1

	lda cur
	adc @sz
	sta @src
	lda cur+1
	adc #$00
	sta @dst+1

	dey
@l0:	lda (@src),y
	sta (@dst),y
	dey
	bpl @l0
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
	lda #$00
	sec
	sbc @cnt
	sta @cnt

	lda #$02
	sta @stop

	lda cur
	sta @cur
	lda cur+1
	sta @cur+1

	; find start of previous line (2 $0d bytes back)+1
@l0:	ldy #$00
	lda (@cur),y
	cmp #$0d
	bne :+
	dec @stop
	beq @done
:	inc @cnt
	decw @cur
	ldx #<buffer
	ldy #>buffer
	cmpw @cur
	bne @l0
	inc $900f
	rts ; reached the beginning of the text buffer (cannot lineup)

@done:	lda @cnt
	jsr __src_moveb
	rts
.endproc

;--------------------------------------
; puts adds the string in (YX) of length .A to the buffer. A newline is
; also appended.
.export __src_puts
.proc __src_puts
@cnt=zp::tmp2
	sta @cnt
	stx @src
	sty @src+1
@src=*+1
@l0:	lda $ffff
	jsr __src_insert
	incw @src
	dec @cnt
	bne @l0
	lda #$0d
	jsr __src_insert
	rts
.endproc

;--------------------------------------
; rewind sets cur to the beginning of buffer.
.export __src_rewind
.proc __src_rewind
	lda #<buffer
	sta cur
	lda #>buffer
	sta cur+1
	rts
.endproc

;--------------------------------------
; getline updates cur to point past the end of the current line.
; $ff is returned if there is no more data to get.
.export __src_getline
.proc __src_getline
@src=zp::tmp0
	lda cur
	sta @src
	lda cur+1
	sta @src+1

	ldy #$ff
	sty @eof
	iny
@l0:	lda (@src),y
	incw @src
	ldx buffend
	ldy buffend+1	; EOF
	cmpw @src
	beq @done
	cmp #$0d
	bne @l0
	inc @eof
	
@done:	lda @src
	sta cur
	lda @src+1
	sta cur+1
@eof=*+1
	lda #$00
	rts
.endproc

;--------------------------------------
; insert adds the character in .A to the buffer
.export __src_insert
.proc __src_insert
	pha
	lda cur
	cmp gapend
	bne @ins
	lda cur+1
	cmp gapend+1
	bne @ins

	; cursor is at end of gap, open a new gap
	jsr opengap

@ins:	pla
	ldx cur
	ldy cur+1
	stx zp::tmp0
	sty zp::tmp0+1
	ldy #$00
	sta (zp::tmp0),y

	inc cur
	bne :+
	inc cur+1
:	rts
.endproc

;--------------------------------------
; replaceline replaces the contents at (cur) with the line given in (YX).
; Once a newline is encountered in the buffer, the contents of the given line
; are inserted into the buffer.
.export __src_replaceline
.proc __src_replaceline
@line=zp::tmp0
@buff=zp::tmp2
	stx @line
	sty @line
	lda cur
	sta @buff
	lda cur+1
	sta @buff+1

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
name:      .res 16  ; the name of the active procedure

.export __src_cur
__src_cur:
cur:       .word buffer  ; current position to insert tokens at

gapend:	   .word buffer+GAPSIZE  ; end pointer of the gap buffer

.export __src_buffer
__src_buffer:
buffer:    
	.byte $0d
	.res 1024*4 ; buffer of the active procedure's tokens

buffend:   .word buffer+GAPSIZE

;--------------------------------------
.ifdef TEST
.export __src_test
.proc __src_test
	jsr testaddtxt
	jsr testmovecur	; move cursor back 4 chars
	jsr testaddtxt	; more text (make new gap)
	rts
.endproc

.proc testaddtxt
	; insert test text
	lda #$00
	sta @cnt
@l0:	ldx @cnt
	lda @text,x
	jsr __src_insert
	inc @cnt
	lda @cnt
	cmp #@textlen
	bcc @l0
	rts
@text: .byte "hello world",$0d,"line 2",$0d,"line 3",$0d
@textlen=*-@text
@cnt: .byte 0
.endproc

.proc testmovecur
	lda #$04
	jsr __src_moveb

	ldx gapend
	ldy gapend+1
	stx zp::tmp0
	sty zp::tmp0+1

	ldy #$00
	lda (zp::tmp0),y
	cmp #'e'
	bne @fail
	iny
	lda (zp::tmp0),y
	cmp #' '
	bne @fail
	iny
	lda (zp::tmp0),y
	cmp #'3'
	bne @fail
	iny
	lda (zp::tmp0),y
	cmp #$0d
	bne @fail
	rts

@fail:	inc $900f
	jmp *-3
.endproc

.proc testmoveu
.endproc
.endif
