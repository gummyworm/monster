.include "reu.inc"
.include "../errors.inc"
.include "../macros.inc"

;*******************************************************************************
MAX_COPY_SIZE = $ffff
SAVESTACK_DEPTH = 4

.export copybuff
copybuff = $0000	; copybuff starts at $0000 in its REU bank

;*******************************************************************************
; VARS
.segment "COPYBUFF_VARS"

.export buffptr
buffptr:  		.word 0 	; buffer pointer

; backup buffer pointer stack (see buff::push)
buffsavelo: 		.res SAVESTACK_DEPTH
buffsavehi: 		.res SAVESTACK_DEPTH
buffsave_sp:		.byte 0

; number of lines copied in VISUAL modes
.export __buff_num_lines_copied
__buff_num_lines_copied:	.byte 0


.segment "COPYBUFF"

;*******************************************************************************
; PUTCH
; Pushes the given character onto the copy/paste buffer
; IN:
;  - .A: the character to put into the buffer
; OUT:
;  - .A: the same as was passed in
;  - .C: set if the buffer is full (couldn't add char)
.export __buff_putch
.proc __buff_putch
@char=r0
	sta @char
	ldxy buffptr
	cmpw #copybuff+MAX_COPY_SIZE	; buffer is full
	bcs @done

	STORE16 #^REU_COPYBUFF_ADDR, buffptr, #@char, #1

	lda @char
	cmp #$0d
	bne :+
	inc __buff_num_lines_copied
	bne :+
	RETURN_ERR ERR_COPY_TOO_BIG	; > 255 lines, error

:	incw buffptr
	clc
@done:	rts
.endproc

;*******************************************************************************
; GETCH
; Gets the last character that was PUT to the buffer
; OUT:
;  - .A: the last character PUT into the buffer (0 if none)
;  - .C: set if the buffer is empty
.export __buff_getch
.proc __buff_getch
@buff=rb
	ldxy buffptr
	stxy @buff
	cmpw #copybuff
	beq @done		; buffer empty

	decw buffptr
	decw @buff
	ldy #$00
	lda (@buff),y
	clc
@done:	rts
.endproc

;*******************************************************************************
; LASTLINE
; Returns the contents of the oldest line in the copy buffer.
; This is useful because we may need to know that this line plus the contents
; of a line that will be joined with it are oversized.
; Middle lines are implicitly correctly sized because they will not be joined
; with anything. This procedure does not modify the buffer pointers (it only
; "peeks" at the data)
; IN:
;  - .XY: the address to store the line to
; OUT:
;  - .Y: the number of characters that were read to the buffer
;  - .C: set if the buffer is empty
.export __buff_lastline
.proc __buff_lastline
@buff=r9
@dst=rb
	stxy @dst

	jsr __buff_push	; save the buffer pointers
	ldxy #copybuff
	stxy @buff

	; make sure buffer is not empty
	lda (@buff),y
	beq @done	; buffer is empty

	; seek for the start of the oldest line
:	lda (@buff),y
	cmp #$0d
	beq @found
	iny
	bne :-

@found:	dey
	tya
	clc
	adc @dst
	sta buffptr
	bcc @done
	inc buffptr+1

@done:	ldxy @dst
	jsr __buff_getline

	php
	jsr __buff_pop	; restore the buffer
	plp
	rts
.endproc

;*******************************************************************************
; GETLINE
; Gets the last line that was PUT to the buffer
; IN:
;  - .XY: the address to store the line to
; OUT:
;  - .A: $0d if last character is a newline
;  - .Y: the number of characters that were read to the buffer
;  - .C: set if the buffer is empty
;  - r9: the address of the string that was read (same as given)
.export __buff_getline
.proc __buff_getline
@dst=r9
@buff=rb
@i=r4
	stxy @dst
	lda #$00
	tay
	sta (@dst),y		; init buffer
	ldxy buffptr
	cmpw #copybuff
	beq @done		; buffer empty

	lda #$00
	sta @i
@l0:	jsr __buff_getch
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

;*******************************************************************************
; CLEAR
; Initializes the copy buffer by clearing it
.export __buff_clear
.proc __buff_clear
	ldx #$00
	stx buffsave_sp
	stx __buff_num_lines_copied
	ldxy #copybuff
	stxy buffptr
	rts
.endproc

;*******************************************************************************
; LINES COPIED
; Returns the # of lines in the copy buffer
; OUT:
;   - .A: the number of lines in the copy buffer
;   - .C: clear if there are no lines copied
.export __buff_lines_copied
.proc __buff_lines_copied
	lda __buff_lines_copied
	cmp #$01
	rts
.endproc

;*******************************************************************************
; PUSH
; Saves the current location of the buffer pointer. Call buff::pop to restore
; it
; OUT:
;   - .C: set on overflow
.export __buff_push
.proc __buff_push
	ldx buffsave_sp
	cpx #SAVESTACK_DEPTH-1
	bcc :+
	RETURN_ERR ERR_STACK_OVERFLOW

:	lda buffptr
	sta buffsavelo,x
	lda buffptr+1
	sta buffsavehi,x
	inc buffsave_sp
	rts
.endproc

;*******************************************************************************
; POP
; Pops the buffer pointer that was saved by calling buff::push
; OUT:
;   - .C: set on underflow
.export __buff_pop
.proc __buff_pop
	dec buffsave_sp
	bpl :+
	RETURN_ERR ERR_STACK_UNDERFLOW

:	ldx buffsave_sp
	lda buffsavehi,x
	sta buffptr+1
	lda buffsavelo,x
	sta buffptr
	RETURN_OK
.endproc

;*******************************************************************************
; LEN
; Returns the length of the buffer
; OUT:
;   - .XY: the number of characters in the buffer
.export __buff_len
.proc __buff_len
	ldxy buffptr
	sub16 #copybuff
	rts
.endproc
