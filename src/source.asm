.include "cursor.inc"
.include "errors.inc"
.include "finalex.inc"
.include "irq.inc"
.include "macros.inc"
.include "memory.inc"
.include "string.inc"
.include "util.inc"
.include "zeropage.inc"

.ifdef USE_FINAL
	.import __BANKCODE_LOAD__
	.import __BANKCODE_SIZE__
	MAX_SOURCES=8
.else
	MAX_SOURCES=1
.endif

;******************************************************************************
; CONSTANTS
GAPSIZE = 20		; size of gap in gap buffer
POS_STACK_SIZE = 32 	; size of source position stack
MAX_BUFFER_NAME_LEN=16  ; max name for each buffer

;******************************************************************************
; FLAGS
FLAG_DIRTY = 1

.segment "SOURCE"
;******************************************************************************
data_start:
sp: 	   .byte 0
stack:     .res POS_STACK_SIZE

.export src_debug
src_debug:
pre:      .word 0		; # of bytes before the gap
post:     .word 0		; # of bytes after the gap
.export __src_line
__src_line:
line:     .word 0       	; the current line # for the cursor
.export __src_lines
__src_lines:
lines:    .word 0		; number of lines in the source
len:  	  .word 0		; size of the buffer (pre+post+gap)
data_end:

pres:     .res MAX_SOURCES*2    ; buffers for each source
posts:    .res MAX_SOURCES*2    ; buffers for each source
linenums: .res MAX_SOURCES*2 	; line #'s for each source
linecnts: .res MAX_SOURCES*2	; number of lines in each source
lens:	  .res MAX_SOURCES*2	; buffers for each source

.export __src_names
__src_names:
names:	   .res MAX_SOURCES*MAX_BUFFER_NAME_LEN

.BSS
;******************************************************************************
.export __src_numbuffers
__src_numbuffers:
numsrcs:    .byte 0		; number of buffers
.export __src_activebuff
__src_activebuff:
activesrc:  .byte 0		; index of active buffer (also bank offset)
.ifdef USE_FINAL
bank:	    .byte 0
buffs_curx: .res MAX_SOURCES	; cursor X positions for each inactive buffer
buffs_cury: .res MAX_SOURCES	; cursor Y positions for each inactive buffer
.endif
flags:	.res MAX_SOURCES	; flags for each source buffer
banks:  .res MAX_SOURCES	; the corresponding bank for each buffer
;******************************************************************************

;******************************************************************************
; DATA
.export __src_buffer
__src_buffer:
.ifndef USE_FINAL
data:
.res 1024*4
.else
data = __BANKCODE_LOAD__ + __BANKCODE_SIZE__
.endif

.CODE
;******************************************************************************
; SAVE
; Backs up the pointers for the active source so that they may be set for
; another source
.export __src_save
.proc __src_save
	; save the cursor position in the current buffer
	ldx activesrc
	lda zp::curx
	sta buffs_curx,x
	lda zp::cury
	sta buffs_cury,x

	; save the bank
	lda bank
	sta banks,x

	; save the data for the source we're switching from
	txa
	asl
	tax

	lda pre
	sta pres,x
	lda pre+1
	sta pres+1,x

	lda post
	sta posts,x
	lda post+1
	sta posts+1,x

	lda len
	sta lens,x
	lda len+1
	sta lens+1,x

	lda lines
	sta linecnts,x
	lda lines+1
	sta linecnts+1,x

	lda line
	sta linenums,x
	lda line+1
	sta linenums+1,x

	rts
.endproc

;******************************************************************************
; SET
; Sets the active source to the source in the given ID.
; IN:
;  - .A: the source we're switching to
; OUT:
;  - .C: set if the buffer could not be switched to
.export __src_set
.proc __src_set
	; set the pointers to those of the source we're switching to
	cmp numsrcs
	bcc @ok
	rts		; buffer doesn't exist; return with .C set

@ok:	sta activesrc
	asl
	tax

	lda pres,x
	sta pre
	lda pres+1,x
	sta pre+1

	lda posts,x
	sta post
	lda posts+1,x
	sta post+1

	lda lens,x
	sta len
	lda lens+1,x
	sta len+1

	lda linecnts,x
	sta lines
	lda linecnts+1,x
	sta lines+1

	lda linenums,x
	sta line
	lda linenums+1,x
	sta line+1

	ldx activesrc

	; restore bank
	lda banks,x
	sta bank

	; restore cursor position in the new source
	ldy buffs_cury,x
	lda buffs_curx,x
	tax
	jsr cur::set

	RETURN_OK
.endproc

;******************************************************************************
; FIND_BANK
; Returns the ID of a free bank to store source in
; OUT:
;  - .A: the next available ID
;  - .C: set if no open bank was found
.proc find_bank
@free=zp::tmp0
	lda #FINAL_BANK_SOURCE0
@l0:	ldx #$00
	stx @free
@l1:	cmp banks,x
	bne :+
	inc @free		; flag NOT free
:	inx
	cpx #MAX_SOURCES
	bne @l1
	ldx @free
	beq @found
	clc
	adc #$01
	cmp #FINAL_BANK_SOURCE0+MAX_SOURCES
	bcc @l0
	sec		; no open banks
	rts

@found:	RETURN_OK
.endproc

;******************************************************************************
; NEW
; Initializes a new source buffer and sets it as the current buffer
; OUT:
;  - .C: set if the source could not be initialized (e.g. too many open
;        sources)
.export __src_new
.proc __src_new
	ldx numsrcs
	beq @init
	inx
	cpx #MAX_SOURCES
	bcc @saveold
	rts		; err, too many sources

@saveold:
	lda numsrcs
	jsr __src_save	; save current source data
@init:
	; clear the state for the new buffer
	ldx #data_end-data_start
	lda #$00
:	sta data_start-1,x
	dex
	bne :-
	lda #GAPSIZE
	sta len

	; init line and lines to 1
	inc line
	inc lines

	lda numsrcs
	sta activesrc
	inc numsrcs

	; mark the buffer as clean
	tax
	lda #$00
	sta flags,x

	jsr find_bank
	ldx activesrc
	sta bank

	rts
.endproc

;******************************************************************************
; BUFFER_BY_NAME
; Returns the ID of the buffer associated with the given filename. The carry
; is set if no buffer by the given name exists.
; IN:
;  - .XY: the name of the buffer to search for
; OUT:
;  - .A: the ID of the buffer; use src::set to make it the active buffer
;  - .C: set if no buffer was found by the given name
.export __src_buffer_by_name
.proc __src_buffer_by_name
@name=zp::str0
@other=zp::str2
@names=zp::tmp0
@cnt=zp::tmp2
@len=zp::tmp3
	jsr str::len
	sta @len

	ldxy #names
	stxy @names
	lda #$ff
	sta @cnt

@l0:	inc @cnt
	lda @cnt
	cmp numsrcs
	bcs @notfound
	ldxy @names
	stxy @other
	lda @len
	jsr str::compare
	php
	lda @names
	clc
	adc #16
	sta @names
	bcc :+
	inc @names+1
:	plp
	bne @l0
	lda @cnt
	clc		; flag as FOUND
@notfound:
	rts
.endproc

;******************************************************************************
; GET_FILENAME
; Returns the filename for the active buffer
.export __src_get_filename
.proc __src_get_filename
	lda __src_activebuff
	asl
	asl
	asl
	asl
	adc #<__src_names
	tax
	lda #$00
	adc #>__src_names
	tay
	rts
.endproc

;******************************************************************************
; ISDIRTY
; Returns .Z clear if the active buffer is dirty (has changed since it was last
; marked !DIRTY)
; OUT:
;  - .Z: clear if the buffer has changed since last flagged clean
.export __src_isdirty
.proc __src_isdirty
	ldy activesrc
	lda flags,y
	and #FLAG_DIRTY
	rts
.endproc

;******************************************************************************
; GET_FLAGS
; Returns the flags for the requested buffer
; IN:
;  - .A: the buffer to get flags for
; OUT:
;  - .A: the flags for the active buffer
.export __src_getflags
.proc __src_getflags
	tay
	lda flags,y
	rts
.endproc

;******************************************************************************
; SET_FLAGS
; Sets the flags for the active source buffer
; IN:
;  - .A: the flags to set on the active source buffer
.export __src_setflags
.proc __src_setflags
	ldy activesrc
	sta flags,y
	rts
.endproc

;******************************************************************************
; CLOSE
; Closes the active buffer
.export __src_close
.proc __src_close
@end=zp::tmp0
; get number of last byte to move (num_buffers)*2
	lda numsrcs
	bne :+
	rts		; no buffer to close
:	cmp #$01
	bne :+
; only one buffer open; re-initialize it
	dec numsrcs	; set num sources down to 0
	jsr __src_new	; and initialize the buffer
	ldxy #@new
	jmp __src_name	; set name to NEW

:	asl
	sta @end

; get first byte to shift down (active_buffer+1)*2
	ldx activesrc
	inx
	cpx numsrcs
	bcs @cont	; if this is last buffer, no need to shift

	txa
	asl
	tax
	pha

@l0:	; copy all the buffers' data down
	lda pres,x
	sta pres-2,x

	lda posts,x
	sta posts-2,x

	lda linenums,x
	sta linenums-2,x

	lda linecnts,x
	lda linecnts-2,x

	lda lens,x
	sta lens-2,x
	inx
	cpx @end
	bne @l0

	; copy cursor data and banks down
	ldx activesrc
	inx
:	lda banks,x
	sta banks-1,x
	lda buffs_curx,x
	sta buffs_curx-1,x
	lda buffs_cury,x
	sta buffs_cury-1,x
	inx
	cpx numsrcs
	bne :-


	; copy the names down
	pla
	asl
	asl
	asl
	tax

	lda numsrcs
	asl
	asl
	asl
	asl
	sta @end

:	lda names,x
	sta names-16,x
	inx
	cpx @end
	bne :-

@cont:	; if there is no next buffer, open the previous
	dec numsrcs
	lda activesrc
	cmp numsrcs
	bcc :+
	dec activesrc
:	lda activesrc
	jsr __src_set
@done:	rts
@new: .byte "new",0
.endproc

;******************************************************************************
; NAME
; Sets the name for the active buffer
; IN:
;  - .XY: the 0-terminated name to set for the active buffer
.export __src_name
.proc __src_name
@name=zp::tmp0
	stxy @name
	lda activesrc
	asl
	asl
	asl
	asl
	tax
	ldy #$00
:	lda (@name),y
	sta names,x
	beq @done
	inx
	iny
	cpy #MAX_BUFFER_NAME_LEN
	bne :-
@done:	rts
.endproc

;******************************************************************************
; PUSHP
; Pushes the current source position to an internal stack.
.export __src_pushp
.proc __src_pushp
	lda sp
	cmp #POS_STACK_SIZE-1
	bcc :+
	RETURN_ERR ERR_STACK_OVERFLOW

:	asl
	tax
	inc sp

	lda pre
	sta stack,x
	lda pre+1
	sta stack+1,x
	RETURN_OK
.endproc

;******************************************************************************
; POPP
; Returns the the most recent source position pushed in .YX
; OUT:
;  - .XY: the most recently pushed source position
.export __src_popp
.proc __src_popp
	lda sp
	bne :+
	RETURN_ERR ERR_STACK_UNDERFLOW

:	dec sp
	lda sp
	asl
	tax
	lda stack+1,x
	tay
	lda stack,x
	tax
	RETURN_OK
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
; OUT:
;  - .C: set if the backspace failed (we're at the START of the source)
.export __src_backspace
.proc __src_backspace
	jsr mark_dirty
	jsr __src_start
	beq @skip
	jsr atcursor
	cmp #$0d
	bne :+
	decw line
	decw lines
:	decw pre
	clc
	rts
@skip:	sec
	rts
.endproc

;******************************************************************************
; DELETE
; Deletes the character at the current cursor position.
; OUT:
;  - .C: set if there is nothing to delete (cursor is at END)
.export __src_delete
.proc __src_delete
	jsr mark_dirty
	jsr __src_end
	beq @skip
	jsr __src_after_cursor
	cmp #$0d
	bne :+
	decw lines
:	decw post
	clc
	rts
@skip:	sec
	rts
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
	jsr __src_end
	beq @skip

	; move char from start of gap to the end of the gap
	jsr cursor
	stx @dst
	sty @dst+1
	jsr poststart
	stx @src
	sty @src+1

.IFDEF USE_FINAL
	bank_read_byte bank, @src
	bank_store_byte bank, @dst
	lda zp::bankval
.ELSE
	ldy #$00
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
;  - .C: set if the cursor wasn't moved, clear if it was
.export __src_right
.proc __src_right
	jsr __src_end
	beq @endofline

	jsr __src_next
	cmp #$0d
	bne @done
	jsr __src_prev
@endofline:
	sec
	rts
@done:	clc
	rts
.endproc

;******************************************************************************
; RIGHT_REP
; Moves to the next character unless a newline exists AFTER the destination
; OUT:
;  - .C: set if the cursor wasn't moved, clear if it was
.export __src_right_rep
.proc __src_right_rep
	jsr __src_end
	beq @endofline

	jsr __src_next
	jsr __src_after_cursor
	cmp #$0d
	bne @done
	jsr __src_prev
@endofline:
	sec
	rts
@done:	clc
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
	bank_read_byte bank, @src
	bank_store_byte bank, @dst
	lda zp::bankval
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
	jsr __src_start
	beq @beginning

@l0:	jsr __src_prev
	cmp #$0d
	beq @done
	jsr __src_start
	bne @l0
	jsr __src_next
@beginning:
	sec
	rts
@done:	clc
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
	jsr __src_end
	beq @eof
@l0:	jsr __src_next
	cmp #$0d
	beq :+
	jsr __src_end
	bne @l0
@eof:	sec	; end of the buffer
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
	jsr mark_dirty
	jsr gaplen
	cmpw #0		; is gap closed?
	bne @ins	; no, insert as usual

	; gap is closed, create a new one
	; copy data[poststart] to data[poststart + len]
	jsr poststart
	stxy @src
	add16 len
	stxy @dst

.IFDEF USE_FINAL
	ldxy len
	lda bank
	jsr fe3::copy
.ELSE
	copy @dst, @src, len
.ENDIF

	; double size of buffer (new gap size is the size of the old buffer)
	asl len
	rol len+1

@ins:	jsr cursor
	stxy @dst
	pla
.IFDEF USE_FINAL
	bank_store_byte bank, @dst
	lda zp::bankval
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
	jsr mark_dirty
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
	bank_read_byte bank, zp::tmp0
.ELSE
	ldy #$00
	lda (zp::tmp0),y
.ENDIF
	rts
.endproc

;******************************************************************************
; ATCURSOR
; Returns the character AFTER the cursor position.
; OUT:
;  - .A: the character after the current cursor position
;  - .C: set if there is no character after the cursor
.export __src_after_cursor
.proc __src_after_cursor
	jsr __src_end
	beq @end
	jsr __src_next
	pha
	jsr __src_prev
	pla
	RETURN_OK
@end:	sec		; end of buffer
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
	jsr __src_start
	bne @l0
	rts
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
	sta mem::linebuffer
	sta @cnt

	jsr __src_end
	beq @eofdone

@l0:	jsr __src_readb
	ldx @cnt
	cmp #$0d
	bne :+
	lda #$00
:	sta mem::linebuffer,x
	beq @done
	inc @cnt
	jsr __src_end
	bne @l0
@eof:	; go back a character; read last byte and null terminate if end of source
	jsr atcursor
	ldx @cnt
	cmp #$0d
	bne :+
	lda #$00
:	sta mem::linebuffer,x
	lda #$00
	sta mem::linebuffer+1,x
@eofdone:
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

;******************************************************************************
; GET
; Returns the text at the current cursor position in mem::linebuffer
; OUT:
;  - mem::linebuffer: a line of text from the cursor position
;  - .C: set if the end of the buffer was reached as we were reading
.export __src_get
.proc __src_get
@cnt=zp::tmp1
@src=zp::tmp3
	jsr gaplen
	add16 pre
	add16 #data
	stxy @src

	jsr __src_end
	bne :+
	lda #$00
	sta mem::linebuffer	; init buffer
	sec
	rts

:	stxy @cnt
	incw @cnt

	ldy #$00
@l0:
.IFDEF USE_FINAL
	sty zp::bankval
	ldxy @src
	lda bank
	jsr fe3::load_off
	ldy zp::bankval
	cmp #$00
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
	cpy #79
	bcc @l0

@eof:	sec
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

;******************************************************************************
.proc mark_dirty
	lda #FLAG_DIRTY
	ldx activesrc
	sta flags,x
	rts
.endproc
