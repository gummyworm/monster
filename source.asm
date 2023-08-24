.include "irq.inc"
.include "zeropage.inc"
.include "macros.inc"
.include "memory.inc"
.include "finalex.inc"
.include "util.inc"

GAPSIZE = 20	; size of gap in gap buffer

.segment "SOURCE"
;******************************************************************************
data_start:
sp: .byte 0
stack: .res 32

.export src_debug
src_debug:
pre:  .word 0       ; # of bytes before the gap
post: .word 0       ; # of bytes after the gap

.export __src_line
__src_line:
line: .word 0       ; the current line # of the cursor
.export __src_lines
__src_lines:
lines: .word 0      ; total lines of source
data_end:

len:  .word 0	; size of the buffer (pre+post+gap)
bank: .byte 0

;******************************************************************************
; DATA
.export __src_buffer
__src_buffer:
data:
.res 1024*4

.CODE
;******************************************************************************
; NEW
; Initializes the source buffer
.export __src_new
.proc __src_new
	ldx #data_end-data_start-1
	lda #$00
:	sta data_start,x
	dex
	bpl :-
	lda #GAPSIZE
	sta len
	inc line
	rts
.endproc

;******************************************************************************
; PUSHP
; Pushes the current source position to an internal stack.
.export __src_pushp
.proc __src_pushp
	lda sp
	asl
	tax
	inc sp

	lda pre
	sta stack,x
	lda pre+1
	sta stack+1,x
	rts
.endproc

;******************************************************************************
; POPP
; Returns the the most recent source position pushed in .YX
; OUT:
;  - .XY: the most recently pushed source position
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

;******************************************************************************
; END
; Returns .Z set if the cursor is at the end of the buffer.
; OUT:
;  - .Z: set if the cursor is at the end of the buffer
.export __src_end
.proc __src_end
	ldxy post
	cmpw #$0000
	rts
.endproc

;******************************************************************************
; START
; Returns .Z set if the cursor is at the start of the buffer.
; OUT:
;  - .Z: set if the cursor is at the start of the buffer
.export __src_start
.proc __src_start
	ldxy pre
	cmpw #$0000
	rts
.endproc

;******************************************************************************
; BACKSPACE
; Deletes the character immediately before the current cursor position.
.export __src_backspace
.proc __src_backspace
	jsr __src_start
	beq @skip
	jsr atcursor
	cmp #$0d
	bne :+
	decw line
	decw lines
:	decw pre
@skip:	rts
.endproc

;******************************************************************************
; DELETE
; Deletes the character at the current cursor position.
.export __src_delete
.proc __src_delete
	ldxy post
	cmpw #$0000
	beq @skip
	decw post
@skip:	rts
.endproc

;******************************************************************************
; NEXT
; Moves the cursor up one character in the gap buffer.
; OUT:
;  - .A: the character at the new cursor position in .A
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
.IFDEF USE_FINAL
	READ_BANKED @src
	WRITE_BANKED @dst
.ELSE
	lda (@src),y
	sta (@dst),y
.ENDIF

	cmp #$0d
	bne :+
	incw line

:	incw pre
	decw post
 @skip:	jmp atcursor
.endproc

;******************************************************************************
; RIGHT
; Moves to the next character unless it is a newline
; OUT:
;  - .C: set if the cursor was moved, clear if not
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

;******************************************************************************
; PREV
; Moves the cursor back one character in the gap buffer.
; OUT:
;  - .A: the character at the new position
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
.IFDEF USE_FINAL
	READ_BANKED @src
	WRITE_BANKED @dst
.ELSE
	ldy #$00
	lda (@src),y
	sta (@dst),y
.ENDIF
	cmp #$0d
	bne :+
	decw line

:	decw pre
	incw post
 @skip:	jmp atcursor
.endproc

;******************************************************************************
; UP
; Moves the cursor back one line or to the start of the buffer if it is
; already on the first line
; this will leave the cursor on the first newline character encountered while
; going backwards through the source.
; OUT:
;  - .A: the character at the cursor position
;  - .C: set if cursor is at the start of the buffer
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

;******************************************************************************
; DOWN
; Moves the cursor beyond the next RETURN character (or to the end of
; the buffer if there is no such character
; OUT:
;  - .C: set if the end of the buffer was reached (cannot move "down")
.export __src_down
.proc __src_down
	ldxy post
	cmpw #0
	bne @l0
	sec
	rts

@l0:	jsr __src_next
	cmp #$0d
	beq :+
	ldxy post
	cmpw #0
	bne @l0
	sec	; end of the buffer
	rts
:	clc
	rts
.endproc

;******************************************************************************
; INSERT
; Adds the character in .A to the buffer at the gap position (gap).
; IN:
;  - .A: the character to insert
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
	pla
.IFDEF USE_FINAL
	WRITE_BANKED @dst
.ELSE
	ldy #$00
	sta (@dst),y
.ENDIF
	cmp #$0d
	bne :+
	incw line
	incw lines
:	incw pre
	rts
.endproc

;******************************************************************************
; REPLACE
; Adds the character in .A to the buffer at the cursor position,
; replacing the character that currently resides there
; IN:
;  - .A: the character to replace the existing one with
.export __src_replace
.proc __src_replace
	pha
	jsr __src_delete
	pla
	jmp __src_insert
.endproc

;******************************************************************************
; GAPLEN
; Returns the length of the gap
; OUT:
;  - .XY: the length of the gap (len-post-pre)
.proc gaplen
	ldxy len
	sub16 post
	sub16 pre
	rts
.endproc

;******************************************************************************
; CURSOR
; Returns the address of the current cursor position within (data).
; OUT:
;  - .XY: the address of the cursor position
.proc cursor
	ldxy #data
	add16 pre
	rts
.endproc

;******************************************************************************
; ATCURSOR
; Returns the character at the cursor position.
; OUT:
;  - .A: the character at the current cursor position
.export __src_atcursor
__src_atcursor:
.proc atcursor
	jsr cursor
	sub16 #1
	stxy zp::tmp0
.IFDEF USE_FINAL
	READ_BANKED zp::tmp0
.ELSE
	ldy #$00
	lda (zp::tmp0),y
.ENDIF
	rts
.endproc

;******************************************************************************
; POSTSTART
; Returns the address of the post-start section of the gap buffer
; OUT:
;  - .XY: the address of the post-start section
.proc poststart
	ldxy #data
	add16 len
	sub16 post
	rts
.endproc

;******************************************************************************
; REWIND
; Moves the cursor back to the start of the buffer
.export __src_rewind
.proc __src_rewind
@l0:	jsr __src_prev
	ldxy pre
	cmpw #0
	bne @l0
@done:	rts
.endproc

;******************************************************************************
; READB
; Reads one byte at the cursor positon and advances the cursor
; OUT:
;  - .A: the byte that was read
.export __src_readb
.proc __src_readb
	jsr atcursor
	pha
	jsr __src_next
	pla
	rts
.endproc

;******************************************************************************
; READLINE
; Reads one line at the cursor positon and advances the cursor
; OUT:
;  - mem::linebuffer: the line that was read will be 0-terminated
;  - .C: set if the end of the source was reached
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
	sec
	rts
@done:	clc
	rts
.endproc


;******************************************************************************
; GOTO
; Goes to the source position given
; IN:
;  - .XY: the line to go to
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
@done:  rts
.endproc

;--------------------------------------
; get returns the text at the current cursor position in mem::linebuffer
; .C is set if the end of the buffer was reached as we were reading
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
	beq @eof
	stxy @cnt
	incw @cnt

	ldy #$00
@l0:
.IFDEF USE_FINAL
	READ_BANKED_VEC_Y @src
.ELSE
	lda (@src),y
.ENDIF
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
@eof:
	sec
	skb
@done:	clc
	lda #$00
	sta mem::linebuffer,y
	rts
.endproc

;******************************************************************************
; DOWNN
; Advances the source by the number of lines in .YX
;  - .YX: the number of lines that were not read
;  - .C: set if the end was reached before the total lines requested could be reached
.export __src_downn
.proc __src_downn
@cnt=zp::tmp4
	stxy @cnt
@loop:	ldxy @cnt
	decw @cnt
	cmpw #$0000
	beq @done
	jsr __src_down
	bcc @loop
@done:	ldxy @cnt
	rts
.endproc

;******************************************************************************
; UPN
; Advances the source by the number of lines in .YX
; IN:
;  - .XY: the number of lines to move "up"
; OUT:
;  - .YX: contains the number of lines that were not read
;  - .C: set if the beginning was reached before the total lines requested could be reached
.export __src_upn
.proc __src_upn
@cnt=zp::tmp4
	stxy @cnt
@loop:	ldxy @cnt
	decw @cnt
	cmpw #$0000
	beq @done
	jsr __src_up
	bcc @loop
@done:	ldxy @cnt
	rts
.endproc
