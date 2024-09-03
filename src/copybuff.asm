.include "errors.inc"
.include "finalex.inc"
.include "macros.inc"
.include "zeropage.inc"

.import __COPYBUFF_BSS_SIZE__

MAX_COPY_SIZE = __COPYBUFF_BSS_SIZE__

.CODE
.export __buff_putch
__buff_putch: JUMP FINAL_BANK_BUFF, #putch

.export __buff_getch
__buff_getch: JUMP FINAL_BANK_BUFF, #putch

.export __buff_getline
__buff_getline: JUMP FINAL_BANK_BUFF, #getline

.export __buff_clear
__buff_clear: JUMP FINAL_BANK_BUFF, #clear

.export __buff_lines_copied
__buff_lines_copied: JUMP FINAL_BANK_BUFF, #lines_copied

.export __buff_push
__buff_push: JUMP FINAL_BANK_BUFF, #push

.export __buff_pop
__buff_pop: JUMP FINAL_BANK_BUFF, #pop

.segment "COPYBUFF_BSS"
buffptr:  .word 0 		; copy buffer pointer
buffsave: .word 0		; backup buffer pointer (see buff::push)
visual_lines_copied:	.byte 0	; the number of lines copied in VISUAL modes
copybuff: .res $1e00		; buffer for copy data

.segment "COPYBUFF"

;******************************************************************************
; PUTCH
; Pushes the given character onto the copy/paste buffer
; IN:
;  - .A: the character to put into the buffer
; OUT:
;  - .A: the same as was passed in
;  - .C: set if the buffer is full (couldn't add char)
.proc putch
@buff=r0
	ldxy buffptr
	stxy @buff
	cmpw #copybuff+MAX_COPY_SIZE	; buffer is full
	bcs @done
	ldy #$00
	sta (@buff),y
	cmp #$0d
	bne :+
	inc visual_lines_copied
	bne :+
	RETURN_ERR ERR_COPY_TOO_BIG	; > 255 lines, error

:	incw buffptr
	clc
@done:	rts
.endproc

;******************************************************************************
; GETCH
; Gets the last character that was PUT to the buffer
; OUT:
;  - .A: the last character PUT into the buffer (0 if none)
;  - .C: set if the buffer is empty
.proc getch
@buff=rb
	ldxy buffptr
	stxy @buff
	cmpw #copybuff
	beq @done		; buffer empty

	ldy #$00
	decw buffptr
	decw @buff
	lda (@buff),y
	clc
@done:	rts
.endproc

;******************************************************************************
; GETLINE
; Gets the last line that was PUT to the buffer
; IN:
;  - .XY: the address to store the line to
; OUT:
;  - (.XY): the last line PUT to the buffer
;  - .A:    $0d if last character is a newline
;  - .C:    set if the buffer is empty
.proc getline
@dst=r9
@buff=rb
@i=r4
	stxy @dst
	lda #$00
	tay
	sta (@dst),y
	ldxy buffptr
	cmpw #copybuff
	beq @done		; buffer empty

	lda #$00
	sta @i
@l0:	jsr getch
	bcs @empty
	cmp #$0d
	beq @ok
	ldy @i
	sta (@dst),y
	inc @i
	bne @l0

@empty: lda #$00
@ok:	pha
	ldxy @buff
	stxy buffptr
	lda #$00
	ldy @i
	sta (@dst),y	; terminate the line
	pla
	clc
@done:	rts
.endproc

;******************************************************************************
; CLEAR
; Initializes the copy buffer by clearing it
.proc clear
	ldx #$00
	stx visual_lines_copied
	ldxy #copybuff
	stxy buffptr
	rts
.endproc

;******************************************************************************
; LINES COPIED
; Returns the # of lines in the copy buffer
; OUT:
;   - .A: the number of lines in the copy bufferj
.proc lines_copied
	lda lines_copied
	rts
.endproc

;******************************************************************************
; PUSH
; Saves the current location of the buffer pointer. Call buff::pop to restore
; it
.proc push 
	lda buffptr
	sta buffsave
	lda buffptr+1
	sta buffsave+1
	rts
.endproc

;******************************************************************************
; POP
; Pops the buffer pointer that was saved by calling buff::push
.proc pop
	lda buffsave+1
	sta buffptr+1
	lda buffsave
	sta buffptr
	rts
.endproc
