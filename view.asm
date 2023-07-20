.include "bitmap.inc"
.include "cursor.inc"
.include "draw.inc"
.include "key.inc"
.include "macros.inc"
.include "memory.inc"
.include "text.inc"
.include "util.inc"
.include "zeropage.inc"
.CODE

ROW_START=15
ROW_STOP=23
BYTES_TO_DISPLAY=8

COL_START=7
COL_STOP=COL_START+(3*BYTES_TO_DISPLAY)-1

;--------------------------------------
; off turns off the memory view window
.export __view_off
.proc __view_off
	jmp bm::restore
.endproc

;--------------------------------------
; edit starts the memory editor at the address given in .YX
.export __view_edit
.proc __view_edit
@byte_offset=zp::tmp0
@src=zp::tmpb
@dst=zp::tmp4
	stx @src
	sty @src+1

	jsr bm::save
	pushcur

	ldx #COL_START
	ldy #ROW_START+1
	jsr cur::setmin

	ldy #ROW_STOP
	ldx #COL_STOP
	jsr cur::setmax

	ldy #ROW_START+1
	ldx #COL_START
	jsr cur::set

	lda #$00	; REPLACE mode
	sta text::insertmode

	ldx @src
	ldy @src+1
	jsr __view_mem

; until user exits (<- or RETURN), get input and update memory
@edit:
	jsr key::getch
	beq @edit

	cmp #$5f	; <- (done)
	beq @done
	cmp #$0d	; RETURN (done)
	beq @done

	cmp #$91	; up arrow
	bne :+
	ldy #$ff
	ldx #0
	jsr cur::move
	jsr cur::on
	jmp @edit

:	cmp #$11
	bne :+
	ldy #1
	ldx #0
	jsr cur::move
	jsr cur::on
	jmp @edit

:	cmp #$14	; delete
	beq @retreat
	cmp #$9d	; left
	bne :+
@retreat:
	jsr @prev_x
	jsr cur::on
	jmp @edit

:	cmp #$1d	; right
	bne :+
	jsr @next_x
	jsr cur::on
	jmp @edit

:	jsr key::ishex
	bcs @replace_val
	bcc @edit
@done:
	jsr cur::off
	popcur
	jmp bm::restore

@replace_val:
	jsr @set_nybble	; replace the nybble under cursor
	jsr @next_x	; advance the cursor (if we can)
	ldx @src
	ldy @src+1
	jsr __view_mem	; update the display
	jsr cur::on
	jmp @edit

; get the address of the memory at the cursor position
@set_nybble:
	jsr util::chtohex
	pha
	lda zp::cury
	sec
	sbc #ROW_START+1
	asl
	asl
	asl
	adc @src
	sta @dst
	lda @src+1
	adc #$00
	sta @dst+1

	ldy #$ff
	lda zp::curx
	sec
	sbc #COL_START
:	iny
	sbc #$03
	bpl :-

	; get odd/even cursor column
	lda zp::curx
	and #$01
	sta zp::tmp0
	; bytes alternate odd/even columns for hi/lo nybble
	tya
	and #$01
	eor zp::tmp0
	beq @lownybble

@hinybble:
	lda (@dst),y
	and #$0f
	sta zp::tmp0
	pla
	asl
	asl
	asl
	asl
	ora zp::tmp0
	sta (@dst),y
	rts
@lownybble:
	lda (@dst),y
	and #$f0
	sta zp::tmp0
	pla
	ora zp::tmp0
	sta (@dst),y
	rts

; move cursor to the next x-position
@next_x:
	jsr cur::off
	ldx zp::curx
@next_x2:
	inx
	txa
	ldy #@num_x_skips-1
:	cmp @x_skips,y
	beq @next_x2
	dey
	bpl :-
	ldy zp::cury
	jmp cur::set

; move cursor to the previous x-position
@prev_x:
	jsr cur::off
	ldx zp::curx
@prev_x2:
	dex
	txa
	ldy #@num_x_skips-1
:	cmp @x_skips,y
	beq @prev_x2
	dey
	bpl :-
	ldy zp::cury
	jmp cur::set

; table of columns to skip in cursor movement
@x_skips:
	.byte COL_START+2
	.byte COL_START+5
	.byte COL_START+8
	.byte COL_START+11
	.byte COL_START+14
	.byte COL_START+17
	.byte COL_START+20
@num_x_skips=*-@x_skips
.endproc

;--------------------------------------
; mem displays the contents of memory in a large block beginning with the
; address in (YX).
.export __view_mem
.proc __view_mem
@src=zp::tmp7
@col=zp::tmp9
@row=zp::tmpa
	stx @src
	sty @src+1

	lda #ROW_START
	jsr draw::hline

	lda #40
	sta zp::tmp0
	lda #' '
	ldx #<mem::spare
	ldy #>mem::spare
	jsr util::memset

	lda #ROW_START+1
	sta @row
@l0:	; draw the address of this line
	lda @src+1
	jsr util::hextostr
	sty mem::spare
	stx mem::spare+1
	lda @src
	jsr util::hextostr
	sty mem::spare+2
	stx mem::spare+3
	lda #':'
	sta mem::spare+4

	ldx #$00
@l1:	stx @col
	; get a byte to display
	ldy #$00
	lda (@src),y
	incw @src

	sta mem::spare+31,x
	jsr util::hextostr
	txa
	pha
	lda @col
	asl
	adc @col
	tax
	pla
	sta mem::spare+8,x
	tya
	sta mem::spare+7,x
	ldx @col
	inx
	cpx #BYTES_TO_DISPLAY
	bcc @l1

	ldx #<mem::spare
	ldy #>mem::spare
	lda @row
	jsr text::puts
	inc @row
	lda @row
	cmp #ROW_STOP
	bcc @l0
	rts
.endproc
