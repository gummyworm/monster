.include "beep.inc"
.include "bitmap.inc"
.include "config.inc"
.include "cursor.inc"
.include "debug.inc"
.include "draw.inc"
.include "edit.inc"
.include "errors.inc"
.include "expr.inc"
.include "finalex.inc"
.include "flags.inc"
.include "key.inc"
.include "keycodes.inc"
.include "layout.inc"
.include "macros.inc"
.include "memory.inc"
.include "strings.inc"
.include "text.inc"
.include "util.inc"
.include "watches.inc"
.include "vmem.inc"
.include "zeropage.inc"

;******************************************************************************
; CONSTANTS
BYTES_TO_DISPLAY=8

COL_START = 7
COL_STOP  = COL_START+(3*BYTES_TO_DISPLAY)-1

TOTAL_BYTES = BYTES_TO_DISPLAY*(MEMVIEW_STOP-MEMVIEW_START)

.BSS
;******************************************************************************
dirtybuff: .res TOTAL_BYTES

.export __view_addr
__view_addr:
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
; Starts the memory editor
.export __view_edit
.proc __view_edit
@dst=r0
@odd=r4
@dstoffset=r6
@src=r8
	ldx #COL_START
	ldy #MEMVIEW_START+1
	jsr cur::setmin

	ldy #MEMVIEW_STOP
	ldx #COL_STOP
	jsr cur::setmax

	ldy #MEMVIEW_START+1
	ldx #COL_START
	jsr cur::set

	lda #TEXT_REPLACE
	sta text::insertmode

	jsr __view_mem
	jsr cur::on

; until user exits (<- or RETURN), get input and update memory
@edit:
	ldxy memaddr
	stxy @src

:	jsr key::getch
	beq :-

	cmp #K_UP_ARROW
	bne :+
	jsr getset_addr
	jmp __view_edit	; reactivate editor at new address
:	cmp #K_QUIT	; <- (done)
	beq @done

	jsr key::isup
	bne :+
@up:	jsr up
	jmp @edit

:	jsr key::isdown
	bne :+
@down:	jsr down
	jmp @edit

:	cmp #K_DEL
	beq @retreat
	cmp #K_LEFT
	beq @retreat
	cmp #$68	; h (also left)
	bne :+
@retreat:
	jsr @prev_x
	jsr cur::on
	jmp @edit

:	cmp #K_RIGHT
	beq @right
	cmp #$6c	; l (also right)
	bne :+
@right: jsr @next_x
	jsr cur::on
	jmp @edit

:	cmp #K_FIND
	bne :+
	jmp @find

:	jsr key::ishex
	bcs @replace_val
	cmp #K_SET_WATCH
	bcc @edit

@setwatch:
	jsr get_addr	; get the address of the byte under the cursor
	stxy r0		; also set as STOP address to this address
	txa
	pha
	tya
	pha
	lda #WATCH_STORE
	jsr watch::add
	ldxy #strings::watch_added
	jsr text::info
	jsr beep::short	; beep to confirm add
	jmp @edit

@done:	jsr cur::unlimit
	jmp cur::off

@replace_val:
	jsr @set_nybble	; replace the nybble under cursor
	jsr @next_x	; advance the cursor (if we can)
	ldxy @src
	jsr __view_mem	; update the display
	jsr cur::on
	jmp @edit

;--------------------------------------
; get the address of the memory at the cursor position
@set_nybble:
	jsr util::chtohex
	pha

	; get the base address for the row that the cursor is on
	lda zp::cury
	sec
	sbc #MEMVIEW_START+1
	asl		; *8 (each row is 8 bytes)
	asl
	asl
	adc @src
	sta @dst
	lda @src+1
	adc #$00
	sta @dst+1

	; get the offset from the row's base address using the curor's x pos
	; the offset is calcuated by: (zp::curx - COL_START) / 3
	ldy #$ff
	lda zp::curx
	sec
	sbc #COL_START
:	iny
	sbc #$03	; -3 (bytes are 3 cursor positions apart)
	bpl :-
	sty @dstoffset

	; get odd/even cursor column
	lda zp::curx
	and #$01
	sta @odd
	; bytes alternate odd/even columns for hi/lo nybble
	tya
	and #$01
	eor @odd
	beq @lownybble

;--------------------------------------
@hinybble:
	ldxy @dst
	lda @dstoffset
	jsr vmem::load_off

	and #$0f
	sta @odd
	pla
	asl
	asl
	asl
	asl
	ora @odd
	jmp @store

;--------------------------------------
@lownybble:
	ldxy @dst
	lda @dstoffset
	jsr vmem::load_off

	and #$f0
	sta @odd
	pla
	ora @odd
@store:
	sta zp::bankval
	ldxy @dst
	lda @dstoffset
	jsr vmem::store_off
	rts

;--------------------------------------
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

;--------------------------------------
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

;--------------------------------------
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

;--------------------------------------
@find:	pushcur
@len=r0
	lda #$00
	sta cur::minx
	sta zp::curx
	lda #CUR_NORMAL
	sta cur::mode
	lda #TEXT_INSERT
	sta text::insertmode

	ldxy #key::getch
	jsr edit::gets		; get the string to parse
	sta @len		; save the string len; 1-2: byte, >2: word

	popcur
	lda #CUR_SELECT
	sta cur::mode
	lda #TEXT_REPLACE
	sta text::insertmode

	jsr util::parsehex	; parse the user's given hex string
	bcs @find		; if invalid hex, retry
	lda @len
	cmp #$03
	bcs @word			; 3-4 characters -> find a word
	txa
	ldxy memaddr
	jsr find_byte		; find byte
	jmp @cont
@word:	jsr find_word		; find the word we're looking for
@cont:	bcs @reset
	stxy memaddr		; set address of word to memaddr
@reset: jmp __view_edit		; restart the viewer at the word's address
.endproc

;******************************************************************************
; UP
; Handles the Up key, moving the cursor or scrolling if needed
.proc up
	jsr cur::off

	; are we at the top of the editor?
	lda zp::cury
	cmp #MEMVIEW_START+1
	bne :+

	; we're at the top, scroll
	lda memaddr
	sbc #$08	; # of bytes per row (.C is always set)
	sta memaddr
	bcs @done
	dec memaddr+1
@done:	sta memaddr
	jmp __view_mem	; refresh the display

:	ldy #$ff
	ldx #0
	jsr cur::move
	jmp cur::on
.endproc

;******************************************************************************
; DOWN
; Handles the Down key, moving the cursor or scrolling if needed
.proc down
	jsr cur::off

	; are we at the bottom of the editor?
	lda zp::cury
	cmp #MEMVIEW_STOP-1
	bcc :+

	; we're at the bottom, scroll
	lda memaddr
	adc #$07	; # of bytes per row - 1 (.C is always set)
	sta memaddr
	bcc @done
	inc memaddr+1
@done:	sta memaddr
	jmp __view_mem	; refresh the display

:	ldy #1
	ldx #0
	jsr cur::move
	jmp cur::on
.endproc

;******************************************************************************
; GETSET ADDR
; Gets an address from the user (as input in the memory title area) and updates
; the memory view to render that area of memory.
.proc getset_addr
	pushcur
	jsr cur::off

	lda #DEFAULT_900F^$08
	ldx #MEMVIEW_START
	jsr draw::hline

	; copy title to linebuffer
	ldx #25-1
:	lda strings::memview_title,x
	sta mem::linebuffer,x
	dex
	bpl :-

	; set bounds for the input
	lda #18
	sta cur::minx
	sta zp::curx
	lda #18+4
	sta cur::maxx

	lda #MEMVIEW_START
	sta zp::cury

	ldxy #key::gethex
	jsr edit::gets

	ldxy #mem::linebuffer+17
	stxy zp::line
	jsr expr::eval
	bcs :+		; couldn't convert (TODO: should be impossible)
	stxy memaddr
:	popcur
	rts
.endproc

;******************************************************************************
; MEM
; Displays the contents of memory in a large block beginning with the
; address in memaddr
; The address is that which was set with the most recent call to mem::edit
.export __view_mem
.proc __view_mem
@src=ra
@col=rc
@row=rd
	lda memaddr
	sta @src
	jsr util::hextostr
	stx strings::memview_title+21
	sty strings::memview_title+20

	lda memaddr+1
	sta @src+1
	jsr util::hextostr
	stx strings::memview_title+19
	sty strings::memview_title+18

	; draw the title for the memory display
	ldxy #strings::memview_title
	lda #MEMVIEW_START
	jsr text::print
	lda #MEMVIEW_START
	jsr bm::rvsline

	; initialize line to empty (all spaces)
	lda #40
	sta r0
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
	ldy #$00
	ldxy @src
	jsr vmem::load
	pha			; save the byte

	incw @src		; update @src to the next byte
	jsr val2ch
	ldx @col
	sta mem::spare+31,x	; write the character representation
	pla			; get the byte we're rendering
	jsr util::hextostr	; convert to hex characters
	txa			; get LSB char
	pha			; and save temporarily
	lda @col		; get col*3 (column to draw byte)
	asl
	adc @col
	tax
	pla			; restore LSB char to render
	sta mem::spare+8,x	; store to text buffer
	tya			; get MSB
	sta mem::spare+7,x	; store to text buffer
	ldx @col
	inx
	cpx #BYTES_TO_DISPLAY	; have we drawn all columns?
	bcc @l1			; repeat until we have

	ldx #<mem::spare
	ldy #>mem::spare
	lda @row
	jsr text::puts		; draw the row of rendered bytes
	inc @row
	lda @row
	cmp #MEMVIEW_STOP	; have we drawn all rows?
	bcc @l0			; repeat til we have
	rts
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
@cnt=r2
	ldx #TOTAL_BYTES-1
	stx @cnt

:	ldxy memaddr
	lda @cnt
	jsr vmem::load_off

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

;******************************************************************************
; GET_ADDR
; Gets the address of the byte under the cursor when editing memory
; IN:
;  - memaddr: the base address of the current view
; OUT:
;  - r0: the address under the cursor
.proc get_addr
@dst=r0
	lda zp::cury
	sec
	sbc #MEMVIEW_START+1
	asl		; *8 (each row is 8 bytes)
	asl
	asl
	adc memaddr
	sta @dst
	lda memaddr+1
	adc #$00
	sta @dst+1

	ldy #$ff
	lda zp::curx
	sec
	sbc #COL_START
:	iny
	sbc #$03
	bpl :-

	tya
	clc
	adc @dst
	tax
	bcc :+
	inc @dst+1
:	ldy @dst+1
	rts
.endproc

;******************************************************************************
; FIND WORD
; Seeks forward from the address in memaddr for the given WORD value.
; IN:
;  - .XY: the word to seek for
; OUT:
;  - .XY: the address of the first occurrence of the value
;  - .C:  set if the value was not found
.proc find_word
@val=r0
@addr=r2
	stxy @val
	ldxy memaddr
	stxy @addr

@l0:	lda @val
	ldxy @addr
	jsr find_byte		; look for the LSB
	bcs @done		; return with .C set

	stxy @addr		; store the address of the LSB
	lda #$01		; next byte
	jsr vmem::load_off
	cmp @val+1		; is the MSB a match our value's?
	beq @found		; if so, we found our word
@next:	incw @addr		; try from the next address
	cmpw memaddr
	bne @l0
@notfound:
	rts			; return with .C set

@found:	ldxy @addr
	clc
@done:	rts
.endproc

;******************************************************************************
; FIND BYTE
; Searches for the given byte value starting at the given address and ending at
; the given address (wrapping around if needed)
; IN:
;  - .A:  the byte value to look for
;  - .XY: the start address
; OUT:
;  - .XY: the address of the first occurrence of the value
;  - .C:  set if the value was not found
.proc find_byte
@addr=r4
@val=r6
@start=r7
	stxy @start
	stxy @addr
	sta @val

@l0:	ldxy @addr	; get current address to seek at
	jsr vmem::load	; load the value at the next address
	cmp @val	; == val we're looking for?
	beq @found	; if so, we're done
	incw @addr	; move to the next address
	ldxy @addr	; get current address
	cmpw @start	; are we back at the start address?
	bne @l0
	sec		; not found
	rts

@found:	ldxy @addr
	RETURN_OK
.endproc
