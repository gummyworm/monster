.include "cursor.inc"
.include "errors.inc"
.include "finalex.inc"
.include "irq.inc"
.include "macros.inc"
.include "memory.inc"
.include "string.inc"
.include "strings.inc"
.include "util.inc"
.include "zeropage.inc"

.import __BANKCODE_RUN__
.import __BANKCODE_SIZE__
MAX_SOURCES=8

;******************************************************************************
; CONSTANTS
GAPSIZE = $100		; size of gap in gap buffer
POS_STACK_SIZE = 32 	; size of source position stack
MAX_BUFFER_NAME_LEN=16  ; max name for each buffer

;******************************************************************************
; FLAGS
FLAG_DIRTY = 1

.BSS

;******************************************************************************
data_start:
sp: 	   .byte 0
stack:     .res POS_STACK_SIZE

.export buffstate
buffstate:
pre:      .word 0		; # of bytes before the gap
post:     .word 0		; # of bytes after the gap
.export __src_line
__src_line:
line:     .word 0       	; the current line # for the cursor
.export __src_lines
__src_lines:
lines:    .word 0		; number of lines in the source
len:  	  .word 0		; size of the buffer (pre+post+gap)
SAVESTATE_SIZE = *-buffstate

savestate:  .res MAX_SOURCES*10	; 10 bytes: pre, post, line, lines, len

.export __src_names
__src_names:
names:	   .res MAX_SOURCES*MAX_BUFFER_NAME_LEN

;******************************************************************************
.export __src_numbuffers
__src_numbuffers:
numsrcs:    .byte 0		; number of buffers
.export __src_activebuff
__src_activebuff:
activesrc:  .byte 0		; index of active buffer (also bank offset)

bank:	    .byte 0
buffs_curx: .res MAX_SOURCES	; cursor X positions for each inactive buffer
buffs_cury: .res MAX_SOURCES	; cursor Y positions for each inactive buffer
banks:      .res MAX_SOURCES	; the corresponding bank for each buffer

flags:	.res MAX_SOURCES	; flags for each source buffer
;******************************************************************************

;******************************************************************************
; DATA
data = __BANKCODE_RUN__ + __BANKCODE_SIZE__

.CODE
;******************************************************************************
; SAVE
; Backs up the pointers for the active source so that they may be set for
; another source
.export __src_save
.proc __src_save
@save=r0
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
	asl		; *2
	asl		; *4
	adc activesrc	; *5
	asl		; *10
	tay

	; save the buffer state
	ldx #$00
@l0:	lda buffstate,x
	sta savestate,y
	iny
	inx
	cpx #SAVESTATE_SIZE
	bne @l0

	rts
.endproc

;******************************************************************************
; SET
; Sets the active source to the source in the given ID.
; IN:
;  - .A: the source buffer we're switching to
; OUT:
;  - .C: set if the buffer could not be switched to
.export __src_set
.proc __src_set
@save=r0
	; set the pointers to those of the source we're switching to
	cmp numsrcs
	bcc @ok
	rts		; buffer doesn't exist; return with .C set

@ok:	tax

	sta activesrc
	asl		; *2
	asl		; *4
	adc activesrc	; *5
	asl		; *10
	tay

	; set the active bank ID
	lda banks,x
	sta bank

	; set the cursor position in the new source
	lda buffs_cury,x
	sta zp::cury
	lda buffs_curx,x
	sta zp::curx

	; restore the state for this buffer
	ldx #$00
@l0:	lda savestate,y
	sta buffstate,x
	iny
	inx
	cpx #SAVESTATE_SIZE
	bne @l0

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
	ldx #SAVESTATE_SIZE-1
	lda #$00
:	sta buffstate-1,x
	dex
	bne :-
	lda #<GAPSIZE
	sta len
	lda #>GAPSIZE
	sta len+1

	; init line and lines to 1
	inc line
	inc lines

	lda numsrcs
	sta activesrc
	inc numsrcs
	pha

	; set name to 0 (unnamed)
	asl
	asl
	asl
	asl
	tax
	lda #$00
	sta names,x

	; mark the buffer as clean
	pla
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
	jsr str::len	; sets @name (str0) to .XY
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
; IN:
;  - .A: the id of the buffer to get the name of
; OUT:
;  - .XY: the filename of the buffer or [NO NAME] if it has no name
; CLOBBERS:
;  - .C:  set if the file has no name ([NO NAME])
;  - zp:tmp0-zp::tmp1
.export __src_get_filename
.proc __src_get_filename
	asl			; * 16
	asl
	asl
	asl
	tax
	lda __src_names,x
	bne @named

@noname:
	ldxy #strings::noname
	RETURN_ERR ERR_UNNAMED_BUFFER

@named:	txa
	adc #<__src_names
	tax
	lda #>__src_names
	adc #$00
	tay
	RETURN_OK
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
@cnt=r0
@end=r0
; get number of last byte to move (num_buffers)*2
	lda numsrcs
	bne :+
	rts		; no buffer to close
:	cmp #$01
	bne @close

	; only one buffer open; re-initialize it to "close" it
	dec numsrcs	; set num sources back to 0
	jmp __src_new	; and initialize the buffer

@close:
	; get the number of buffers to shift (numsrcs - activesrc - 1)
	lda numsrcs
	sbc #$01	; .C is set
	sbc activesrc
	beq @cont	; if this was the last buffer, skip shifting
	sta @cnt
	pha		; save this count for moving names

	; get offset to start shifting at
	lda activesrc
	asl		; *2
	asl		; *4
	adc activesrc	; *5
	asl		; *10
	tax

@l0:	; copy all the buffers' data down
	ldy #SAVESTATE_SIZE
:	lda savestate+SAVESTATE_SIZE,x
	sta savestate,x
	inx
	dey
	bne :-
	dec @cnt
	bne @l0

	; copy cursor data and banks down
	ldx activesrc
	inx
@l1:	lda banks,x
	sta banks-1,x
	lda buffs_curx,x
	sta buffs_curx-1,x
	lda buffs_cury,x
	sta buffs_cury-1,x
	inx
	cpx numsrcs
	bne @l1

	; copy the names down
	lda activesrc
	asl		; *2
	asl		; *4
	asl		; *8
	asl		; *16
	tax

	pla
	sta @cnt

@l2:	ldy #MAX_BUFFER_NAME_LEN
:	lda names+MAX_BUFFER_NAME_LEN,x
	sta names,x
	inx
	dey
	bne :-
	dec @cnt
	bne @l2

@cont:	; if there is no next buffer, open the previous
	dec numsrcs
	lda activesrc
	cmp numsrcs
	bcc :+
	dec activesrc
:	lda activesrc
	jmp __src_set
.endproc

;******************************************************************************
; NAME
; Sets the name for the active buffer
; IN:
;  - .XY: the 0-terminated name to set for the active buffer
.export __src_name
.proc __src_name
@name=r0
	stxy @name
	lda activesrc
	asl
	asl
	asl
	asl			; *16
	tax
	ldy #$00
@l0:	lda (@name),y
	sta names,x
	beq @done
	inx
	iny
	cpy #MAX_BUFFER_NAME_LEN
	bne @l0
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
; Returns the most recent source position pushed in .YX
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
	ldy stack+1,x
	lda stack,x
	tax
	RETURN_OK
.endproc

;******************************************************************************
; POS
; Returns the current source position.  You may go to this position with the
; src::goto routine.  Note that if the source changes since this procedure is
; called, this may not be the same (or expected) position
; OUT:
;  - .XY: the current source position
.export __src_pos
__src_pos = __src_start	 ; start implements the same behavior

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
;  - .A: the character that was deleted
;  - .C: set if the backspace failed (we're at the START of the source)
.export __src_backspace
.proc __src_backspace
	jsr mark_dirty
	jsr __src_start
	beq @skip
	jsr atcursor
	pha
	cmp #$0d
	bne :+
	decw line
	decw lines
:	decw pre
	pla
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

	bank_read_byte bank, @src
	bank_store_byte bank, @dst

	cmp #$0d
	bne :+
	incw line

:	incw pre
	decw post
 @skip:	jmp atcursor
.endproc

;******************************************************************************
; HOME
; Moves left until at the start of the source buffer or the start of the line
.export __src_home
.proc __src_home
:	jsr __src_left
	bcc :-
	rts
.endproc

;******************************************************************************
; LEFT
; Moves left to the previous character unless it is a newline
; OUT:
;  - .C: set if the source cursor was unmoved
;  - .A: the character at the new source cursor position
.export __src_left
.proc __src_left
	jsr __src_prev
	bcs @nomove
	cmp #$0d
	bne @done
	jsr __src_next
@nomove:
	sec
	rts
@done:	RETURN_OK
.endproc

;******************************************************************************
; RIGHT
; Moves to the next character unless it is a newline
; OUT:
;  - .C: set if the cursor wasn't moved, clear if it was
;  - .A: the character at the position that was moved to
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

	jsr __src_after_cursor	; if we're at end of line, don't move
	cmp #$0d
	beq @endofline
	jsr __src_next
	jsr __src_after_cursor	;if moving would put us at end of line, stay
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
;  - .C: set if we're at the start of the buffer and couldn't move back
.export __src_prev
.proc __src_prev
@src=zp::tmp0
@dst=zp::tmp2
	jsr __src_start
	bne :+
	jsr atcursor
	sec
	rts

:	; move char from start of gap to the end of the gap
	jsr cursor
	stxy @src
	jsr poststart
	stxy @dst

	decw @src
	decw @dst

	bank_read_byte bank, @src
	bank_store_byte bank, @dst

	cmp #$0d
	bne :+
	decw line

:	decw pre
	incw post
 	jsr atcursor
	RETURN_OK
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
	bcs @beginning
	cmp #$0d
	bne @l0
	clc
@beginning:
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
	; copy data[poststart] to data[poststart + GAPSIZE]
	jsr poststart
	stxy @src
	stx @dst
	iny
	sty @dst+1

	ldxy post
	lda bank
	jsr fe3::copy

	inc len+1	; increase size by $100

@ins:	jsr cursor
	pla
	cpy #$80
	bcs @done	; out of range
	stxy @dst

	bank_store_byte bank, @dst

	cmp #$0d
	bne :+
	incw line
	incw lines
:	incw pre
@done:	rts
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
	lda bank
	jmp fe3::load
.endproc

;******************************************************************************
; BEFORE_NEWL
; Checks if src::aftercursor is a newline ($0d)
; OUT:
;  - .Z: set if src::aftercursor == $0d
.export __src_before_newl
.proc __src_before_newl
	jsr __src_after_cursor
	cmp #$0d
	rts
.endproc

;******************************************************************************
; AFTER CURSOR
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
; READLINE
; Reads one line at the cursor position and advances the cursor
; OUT:
;  - .A: the length of the line
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

@l0:	jsr __src_next
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
	ldx @cnt
	lda #$00
	sta mem::linebuffer,x
@eofdone:
	lda @cnt
	sec
	rts
@done:	txa
	clc
	rts
.endproc

;******************************************************************************
; GOTO
; Goes to the source position given
; IN:
;  - .XY: the source position to go to (see src::pos, src::pushp, src::popp)
.export __src_goto
.proc __src_goto
@dest=zp::tmp4
	stxy @dest
	cmpw pre
	beq @done
	bcc @backwards
@forwards:
	jsr __src_end
	beq @done
	jsr __src_next
	ldxy pre
	cmpw @dest
	bne @forwards
	rts
@backwards:
	jsr __src_start
	beq @done
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
	stxy zp::bankaddr0

	jsr __src_end
	bne :+
	lda #$00
	sta mem::linebuffer	; init buffer
	sec
	rts

:	ldxy #mem::linebuffer
	stxy zp::bankaddr1

	ldxy line
	cmpw lines		; on last line already?
	bne :+
	ldy post		; bytes to copy
	dey
	lda bank
	jsr fe3::fcopy
	ldy post
	jmp @done

:	ldxy #mem::linebuffer
	stxy zp::bankaddr1
	lda bank
	jsr fe3::copyline

@done:	lda #$00
	sta mem::linebuffer,y
	RETURN_OK
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
; MARK DIRTY
; Marks the given buffer as "dirty" by setting its appropriate flag.
; IN:
;  - .A: the buffer ID to flag as DIRTY
.proc mark_dirty
	lda #FLAG_DIRTY
	ldx activesrc
	sta flags,x
	rts
.endproc
