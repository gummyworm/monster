.include "ram.inc"
.include "reu.inc"
.include "../config.inc"
.include "../debug.inc"
.include "../debuginfo.inc"
.include "../edit.inc"
.include "../errors.inc"
.include "../macros.inc"
.include "../zeropage.inc"

.import __src_bank
.import __src_get_filename
.import __src_mark_dirty
.import __src_on_last_line

buffstate   = zp::srccur
cursorzp    = zp::srccur
poststartzp = zp::srccur2
line        = zp::srcline
lines       = zp::srclines
end         = zp::srcend

; TODO:
BUFFER_SIZE = $8000	; max size of buffer
GAPSIZE     = $100	; size of gap in gap buffer
PAGESIZE    = $100	; size of data "page" (amount stored in c64 RAM)

.CODE
;*******************************************************************************
; INIT BUFF
; Initializes a new source buffer by setting its pointers to the
; start/end of the gap
.export __src_init_buff
.proc __src_init_buff
	lda #$00
	sta cursorzp
	sta cursorzp+1

	lda #<GAPSIZE
	sta end
	sta poststartzp
	lda #>GAPSIZE
	sta end+1
	sta poststartzp+1
	rts
.endproc

;*******************************************************************************
; INSERT
; Adds the character in .A to the buffer at the gap position (gap).
; If the character is not valid, it is not inserted, but the operation is
; still considereed a success (.C is returned clear)
; IN:
;  - .A: the character to insert
; OUT:
;  - .C: set if the character could not be inserted (buffer full)
; CLOBBERS:
;  - $120-$130: may be clobbered if newline is inserted
.export __src_insert
.proc __src_insert
@skip_insert_logic=r0
@src=r2
@dst=r4
	ldx #$00
	stx @skip_insert_logic

	cmp #$0d
	beq :+
	cmp #$0a
	beq :+
	cmp #$09
	beq :+
	cmp #$20
	bcs :+
	cmp #$80
	bcc :+
	jmp @done	; not displayable

:	pha
	jsr __src_mark_dirty
	jsr gaplen
	cmpw #0		; is gap closed?
	bne @ins	; no, insert as usual

	; check if there is room to expand the gap
	lda poststartzp+1
	cmp #>BUFFER_SIZE-1	; -1 to save space for a $100 byte gap
	bcc @ok

@err:	; buffer overflow, cannot insert character
	pla				; clean stack
	lda #ERR_BUFFER_FULL
	rts

@ok:	; gap is closed, create a new one
	; copy data[poststart] to data[poststart + GAPSIZE]
	; TODO:
	jmp *
	lda __src_bank
	sta reu::reuaddr
	ldxy cursorzp
	stxy reu::reuaddr+1

	; set the C64 address where the RAM will be intermediately
	; stored
	ldxy #@done		; address of the end of this procedure
	stxy reu::c64addr

	; get number of bytes to copy
	ldxy end
	sub16 poststartzp
	stxy reu::txlen

	; save the C64 RAM that will be clobbered by the copy
	lda #^REU_TMP_ADDR
	sta reu::reuaddr+2
	lda #>REU_TMP_ADDR
	sta reu::reuaddr+1
	lda #<REU_TMP_ADDR
	sta reu::reuaddr
	jsr reu::swap

	; calculate the new destination in the REU to store the data
	inc poststartzp+1
	inc end+1		; increase size by $100
	lda __src_bank
	sta reu::reuaddr+2
	ldxy poststartzp
	stxy reu::reuaddr	; set REU destination

	; store the copied data to its new location in the REU
	jsr reu::store

	; restore the C64 RAM that was saved to make room for the copy buff
	lda #^REU_TMP_ADDR
	sta reu::reuaddr+2
	lda #>REU_TMP_ADDR
	sta reu::reuaddr+1
	lda #<REU_TMP_ADDR
	sta reu::reuaddr
	jsr reu::swap

@ins:	pla
	ldy cursorzp+1
	bmi @done	; out of range

	; write the character to insert
	sta24 __src_bank, cursorzp

	cmp #$0d
	bne @insdone
	incw line
	lda @skip_insert_logic
	bne :+
	jsr on_line_inserted
:	incw lines
@insdone:
	incw cursorzp
@done:	RETURN_OK
.endproc

;*******************************************************************************
; COPY LINE
; Returns the text at the current cursor position and stores it to the given
; target location
; IN:
;  - r1:  the destination to copy to
; OUT:
;  - (.XY): a line of text from the cursor position
;  - .C: set if the end of the buffer was reached as we were reading
.export __src_copy_line
.proc __src_copy_line
@target=r1
	stxy reu::c64addr
	stxy @target

	; initialize transfer length to max line size
	lda #LINESIZE
	sta reu::txlen
	lda #$00
	sta reu::txlen+1

	lda __src_bank
	sta reu::reuaddr+2

	jsr __src_on_last_line	; on last line already?
	bne @load		; if yes, skip

	; on last line, there will be no newline to mark end of line,
	; calculate exact # of bytes to copy
	ldxy end
	sub16 poststartzp	; bytes to copy
	txa
	tay			; .Y = bytes to copy
	lda #$00
	sta (@target),y		; terminate line
	dey
	cpy #$ff
	bne :+
	rts			; nothing to copy
:	sty reu::txlen

@load:	jsr reu::load
	; look for a newline and replace it with a 0 if found
	ldy #$00
:	lda (@target),y
	cmp #$0d
	beq @done
	iny
	cpy #LINESIZE
	bne :-
@done:	lda #$00
	sta (@target),y
	rts
.endproc

;******************************************************************************
; NEXT
; Moves the cursor up one character in the gap buffer
; OUT:
;  - .A: the character at the new cursor position in .A
;  - .C: clear on success (always clear)
.export __src_next
.proc __src_next
	; do __src_end inline to save the cycles from JSR and RTS
	ldx poststartzp
	cpx end
	bne @cont
	ldx poststartzp+1
	cpx end+1
	beq @done

@cont: ; move one byte from the end of the gap to the start
	lda24 __src_bank, poststartzp
	sta24 __src_bank, cursorzp

	incw cursorzp
	incw poststartzp

	cmp #$0d
	bne @done
	incw line
@done:	RETURN_OK
.endproc

;*******************************************************************************
; PREV
; Moves the cursor back one character in the gap buffer.
; OUT:
;  - .A: the character at the new cursor position (if not at the start of buff)
;  - .C: set if we're at the start of the buffer and couldn't move back
.export __src_prev
.proc __src_prev
	jsr __src_start
	bne :+
	jsr __src_atcursor
	sec
	rts

:	; move char from start of gap to the end of the gap
	decw cursorzp
	decw poststartzp

	; move one byte from the start of the gap to the end
	lda24 __src_bank, cursorzp
	sta24 __src_bank, poststartzp

	cmp #$0d
	bne :+
	decw line

:	; get the character at the new cursor position
	decw cursorzp
	lda24 __src_bank, cursorzp
	incw cursorzp
	RETURN_OK
.endproc

;*******************************************************************************
; START
; Returns .Z set if the cursor is at the start of the buffer.
; OUT:
;  - .Z: set if the cursor is at the start of the buffer
.export __src_start
.proc __src_start
	ldx cursorzp
	bne @done	; if LSB is !0, not the start
	ldx cursorzp+1	; set .Z if MSB is 0
@done:	rts
.endproc

;*******************************************************************************
; GAPLEN
; Returns the length of the gap
; OUT:
;  - .XY: the length of the gap
.proc gaplen
	ldxy poststartzp
	sub16 cursorzp
	rts
.endproc

;*******************************************************************************
; ON LINE INSERTED
; Callback to handle a line insertion. Various state needs to be shifted when
; this occurs (breakpoints, etc.)
.proc on_line_inserted
	; update debug info: find all line programs in the current file with
	; start lines greater than the current line and increment those
	jsr __src_get_filename
	jsr dbgi::getfileid

	; shift breakpoints
	jsr edit::currentfile
	sta r0
	lda #$01
	jmp dbg::shift_breakpointsd
.endproc

;*******************************************************************************
; ATCURSOR
; Returns the character at the cursor position.
; OUT:
;  - .A: the character at the current cursor position
.export __src_atcursor
.proc __src_atcursor
	decw cursorzp
	lda24 __src_bank, cursorzp
	incw cursorzp
	rts
.endproc
