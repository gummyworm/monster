.include "finalex.inc"
.include "../errors.inc"
.include "../macros.inc"

;*******************************************************************************
.import __COPYBUFF_BSS_SIZE__
MAX_COPY_SIZE   = __COPYBUFF_BSS_SIZE__
SAVESTACK_DEPTH = 4

.export copybuff

.export __buff_clear
.export __buff_putch
.export __buff_getch
.export __buff_getline
.export __buff_lastline
.export __buff_lines_copied
.export __buff_push
.export __buff_pop
.export __buff_len
.export __buff_reverse

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

.CODE

;*******************************************************************************
.linecont +
.macro COPYBUFFJUMP proc_id
	pha
	lda #proc_id
	bpl do_buff_proc
.endmacro

.enum buff_proc_ids
PUTCH
GETCH
GETLINE
CLEAR
LASTLINE
LINES_COPIED
PUSH
POP
LEN
REVERSE
.endenum

.RODATA
.define buff_procs putch, getch, getline, clear, lastline, lines_copied, push,\
	pop, len, reverse
buff_procs_lo: .lobytes buff_procs
buff_procs_hi: .hibytes buff_procs

.CODE
;*******************************************************************************
; Copy Buff JUMP table
__buff_putch:        COPYBUFFJUMP buff_proc_ids::PUTCH
__buff_getch:        COPYBUFFJUMP buff_proc_ids::GETCH
__buff_getline:      COPYBUFFJUMP buff_proc_ids::GETLINE
__buff_clear:        COPYBUFFJUMP buff_proc_ids::CLEAR
__buff_lastline:     COPYBUFFJUMP buff_proc_ids::LASTLINE
__buff_lines_copied: COPYBUFFJUMP buff_proc_ids::LINES_COPIED
__buff_push:         COPYBUFFJUMP buff_proc_ids::PUSH
__buff_pop:          COPYBUFFJUMP buff_proc_ids::POP
__buff_len:          COPYBUFFJUMP buff_proc_ids::LEN
__buff_reverse:      COPYBUFFJUMP buff_proc_ids::REVERSE

;*******************************************************************************
; Entrypoint for copy buffer routines
.proc do_buff_proc
	stx @savex
	tax
	lda buff_procs_lo,x
	sta zp::bankjmpvec
	lda buff_procs_hi,x
	sta zp::bankjmpvec+1
	lda #FINAL_BANK_BUFF
	sta zp::banktmp
@savex=*+1
	ldx #$00
	pla
	jmp __ram_call
.endproc
.linecont -

.segment "COPYBUFF_BSS"

;*******************************************************************************
.export copybuff
copybuff:
	.res $1e00	; buffer for copy data

.segment "COPYBUFF"

;*******************************************************************************
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
.proc getch
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
.proc lastline
@buff=r9
@dst=rb
	stxy @dst

	jsr push	; save the buffer pointers
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
	jsr getline

	php
	jsr pop		; restore the buffer
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
.proc getline
@dst=r9
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
.proc clear
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
.proc lines_copied
	lda __buff_num_lines_copied
	cmp #$01
	rts
.endproc

;*******************************************************************************
; PUSH
; Saves the current location of the buffer pointer. Call buff::pop to restore
; it
; OUT:
;   - .C: set on overflow
.proc push
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
.proc pop
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
.proc len
	ldxy buffptr
	sub16 #copybuff
	rts
.endproc

;*******************************************************************************
; REVERSE
; Reverses the contents of the copy buffer
.proc reverse
@left=r0
@right=r2
	ldxy buffptr
	stxy @right
	decw @right
	ldxy #copybuff
	stxy @left
	cmpw @right
	bcs @done

@l0:	ldy #$00
	lda (@left),y
	tax
	lda (@right),y
	sta (@left),y
	txa
	sta (@right),y
	incw @left
	decw @right
	ldxy @left
	cmpw @right
	bcc @l0

@done:	rts
.endproc
