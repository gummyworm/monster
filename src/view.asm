.include "bitmap.inc"
.include "cursor.inc"
.include "draw.inc"
.include "finalex.inc"
.include "key.inc"
.include "layout.inc"
.include "macros.inc"
.include "memory.inc"
.include "text.inc"
.include "util.inc"
.include "zeropage.inc"
.CODE

BYTES_TO_DISPLAY=8

COL_START=7
COL_STOP=COL_START+(3*BYTES_TO_DISPLAY)-1

TOTAL_BYTES=BYTES_TO_DISPLAY*(MEMVIEW_STOP-MEMVIEW_START)

.BSS
;******************************************************************************
dirtybuff: .res TOTAL_BYTES
memaddr:   .word 0

.CODE
;******************************************************************************
; OFF
; Turns off the memory view window by restoring the screen
.export __view_off
.proc __view_off
	jmp bm::restore
.endproc

;******************************************************************************
; EDIT
; Starts the memory editor at the address given in .YX
; IN:
;  - .XY: the address to edit memory at
.export __view_edit
.proc __view_edit
@odd=zp::tmp0
@dst=zp::tmp4
@offset=zp::tmp6
@src=zp::tmp8
	stxy @src
	stxy memaddr

	pushcur

	ldx #COL_START
	ldy #MEMVIEW_START+1
	jsr cur::setmin

	ldy #MEMVIEW_STOP
	ldx #COL_STOP
	jsr cur::setmax

	ldy #MEMVIEW_START+1
	ldx #COL_START
	jsr cur::set

	lda #$00	; REPLACE mode
	sta text::insertmode

	ldxy @src
	jsr __view_mem

	jsr cur::on

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

@done:	jsr cur::unlimit
	jsr cur::off
	popcur
	rts

@replace_val:
	jsr @set_nybble	; replace the nybble under cursor
	jsr @next_x	; advance the cursor (if we can)
	ldxy @src
	jsr __view_mem	; update the display
	jsr cur::on
	jmp @edit

; get the address of the memory at the cursor position
@set_nybble:
	jsr util::chtohex
	pha
	lda zp::cury
	sec
	sbc #MEMVIEW_START+1
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
	sta @odd
	; bytes alternate odd/even columns for hi/lo nybble
	tya
	and #$01
	eor @odd
	beq @lownybble

@hinybble:
.ifdef USE_FINAL
	ldxa @dst
	jsr get_byte
.else
	lda (@dst),y
.endif
	and #$0f
	sta @odd
	pla
	asl
	asl
	asl
	asl
	ora @odd
	jmp @store

@lownybble:
.ifdef USE_FINAL
	ldxa @dst
	jsr get_byte
.else
	lda (@dst),y
.endif
	and #$f0
	sta @odd
	pla
	ora @odd
@store:
	sta zp::bankval
.ifdef USE_FINAL
	ldxa @dst
	jsr store_byte
.else
	sta (@dst),y
.endif
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

;******************************************************************************
; MEM
; Displays the contents of memory in a large block beginning with the
; address in (YX).
; The address is that which was set with the most recent call to mem::edit
.export __view_mem
.proc __view_mem
@src=zp::tmpa
@col=zp::tmpc
@row=zp::tmpd
	ldxy memaddr
	stxy @src

	ldxy #@title
	lda #MEMVIEW_START
	jsr text::print
	lda #MEMVIEW_START
	jsr bm::rvsline

	lda #40
	sta zp::tmp0
	lda #' '
	ldx #<mem::spare
	ldy #>mem::spare
	jsr util::memset

	lda #MEMVIEW_START+1
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
.ifdef USE_FINAL
	ldy #$00
	ldxa @src
	jsr get_byte
.else
	ldy #$00
	lda (@src),y
.endif
	incw @src
	pha
	jsr val2ch
	ldx @col
	sta mem::spare+31,x	; write the character representation
	pla
	jsr util::hextostr
	txa
	pha
	lda @col
	asl
	adc @col
	tax
	pla
	sta mem::spare+8,x	; LSB
	tya
	sta mem::spare+7,x	; MSB
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
	cmp #MEMVIEW_STOP
	bcc @l0
	rts

@title: .byte ESCAPE_SPACING,16, "memory",0
.endproc

;******************************************************************************
; Get the character representation of the given byte value
.proc val2ch
	cmp #$20
	bcc :+
	cmp #$80
	bcs :+
	rts
:	lda #'.'	; use '.' for undisplayable chars
	rts
.endproc

;******************************************************************************
; DIRTY
; Returns .Z set if the memory in the viewer is dirty (has changed since the
; last render)
; OUT:
;  - .Z: set if the display is dirty
.export __view_dirty
.proc __view_dirty
@cnt=zp::tmp2
	ldx #TOTAL_BYTES-1
	stx @cnt

:	ldxa memaddr
	ldy @cnt
	jsr get_byte

	ldx @cnt
	cmp dirtybuff,x
	bne @dirty
	dec @cnt
	bpl :-
@clean: lda #$ff
	rts
@dirty: lda #$00
	rts
.endproc

.ifdef USE_FINAL
;******************************************************************************
; GET_REAL_ADDRESS
; Transforms the given address into the actual address used to store the user's
; program data for that address.  Some ranges are stored in special buffers
; to keep them safe while debugging.
; These ranges are:
;  - $00-$100
;  - $1000-$1100
;  - $1100-$2000
;  - $9400-$9500
; Other ranges are read directly from memory or from the user's memory bank
; IN:
;  - .AX: the address to read
;  - .Y: the offset from the address to read
; OUT:
;  - .XY: the address to read from
;  - .A: the bank to read from
.proc get_real_address
@addr=zp::tmp0
	; get the base address + the offset in .YX
	stx @addr
	sta @addr+1
	tya
	clc
	adc @addr
	tax
	lda @addr+1
	adc #$00
	tay

	lda #FINAL_BANK_USER	; default to user's bank
	cmpw #$100
	bcs :+

@00:	add16 #(mem::prog00-$00)
	lda #FINAL_BANK_MAIN
	rts

:	cmpw #$1000
	bcc @done
	cmpw #$1100
	bcs :+

@1000:	add16 #(mem::prog1000-$1000)
	lda #FINAL_BANK_MAIN
	rts

:	cmpw #$2000
	bcs :+

@1100:	; read from the screen buffer bank (stored at $a000)
	add16 #($a000-$1100)
	lda #FINAL_BANK_FASTCOPY
	rts

:	cmpw #$9000
	bcc @done
	cmpw #$9010
	bcs :+

@9000:	add16 #(mem::prog9000-$9000)
	lda #FINAL_BANK_MAIN
	rts

:	cmpw #$9400
	bcc @done
	cmpw #$9500
	bcs @done

@9400:	add16 #(mem::prog9400-$9400)
	lda #FINAL_BANK_MAIN
	rts

@done:	ldxy @addr
	lda #FINAL_BANK_USER
	rts
.endproc

;******************************************************************************
; GET_BYTE
; Reads a byte from the given address. If the address is in a range internal to
; the Vic or zeropage, it is read from a buffer that stores the user data.
; IN:
;  - .AX: the address to read
;  - .Y: the offset from the address to read
; OUT:
;  - .A: the byte at the given address+offset
.proc get_byte
@addr=zp::tmpe
@offset=zp::tmp10
@bank=zp::tmp11
@ysave=zp::tmp12
	sty @ysave
	jsr get_real_address
	sta @bank
	stxy @addr
	bank_read_byte @bank, @addr
	ldy @ysave
	rts
.endproc

;******************************************************************************
; STORE_BYTE
; Writes a byte to the given address. If the address is in a range internal to
; the Vic or zeropage, it is read from a buffer that stores the user data.
; IN:
;  - .AX: the address to write
;  - .Y: the offset from the address to read
;  - zp::bankval: the value to store
; OUT:
;  - .A: the byte at the given address+offset
.proc store_byte
@addr=zp::tmpe
@offset=zp::tmp10
@bank=zp::tmp11
@ysave=zp::tmp12
	stx @addr
	sta @addr+1
	sty @ysave

	jsr get_real_address	; .XY has address, .A has bank
	jsr fe3::store

	ldy @ysave
	rts
.endproc
.endif
