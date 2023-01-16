.include "bitmap.inc"
.include "cursor.inc"
.include "text.inc"
.include "zeropage.inc"
.CODE

;--------------------------------------
.export __scr_h
.proc __scr_h
	lda #24
	rts
.endproc

;--------------------------------------
.export __scr_w
.proc __scr_w
	lda #40
	rts
.endproc

;--------------------------------------
; getc returns the character at row .X col .Y
.export __scr_getc
.proc __scr_getc
	lda #$00
.endproc
;
; putc sets the character at row .X, col .Y to .A
; if .A is 0, fetches the character instead
.export __scr_putc
.proc __scr_putc
@dst=zp::tmp1
	pha
	lda #$00
	sta @dst+1
	txa
	asl ; *40
	sta zp::tmp0
	asl
	asl
	adc zp::tmp0
	asl
	rol @dst+1
	asl
	rol @dst+1
	stx zp::tmp0
	adc zp::tmp0
	sta @dst+1
	pla
	beq :+
	sta (@dst),y
	sta dirty,y
:	lda (@dst),y
	rts
.endproc

;--------------------------------------
; refresh redraws the entire display from the screen buffer.
.export __scr_refresh
.proc __scr_refresh
	ldx #23
	lda #$ff
@l0:	sta dirty,x
	dex
	bpl @l0
	rts
; redraw redraws the lines that have been marked dirty
.export __scr_redraw
__scr_redraw:
@row=zp::tmp0
@src=zp::tmp1
	lda #$00
	sta @row
	ldx #<buffer
	ldy #>buffer
	stx @src
	sty @src+1
@l0:	ldx @row
	lda dirty,x
	beq @next
	lda #$00
	sta dirty,x
	ldx @src
	ldy @src+1
	txa
	jsr text::puts
@next:  inc @row
	lda @row
	cmp #24
	bcc @l0
	rts
.endproc

;--------------------------------------
.export __scr_update
.proc __scr_update
	jsr text::update
	jsr text::status
	lda #23
	jmp bm::rvsline
.endproc

;--------------------------------------
dirty: .res 24	; 1 byte per line; 0=not dirty, non-zero=dirty
buffer: .res 40*24 ; character representation of the screen bitmap
