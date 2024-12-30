;*******************************************************************************
; COPYBUFF.ASM
; This file contains the procedures to read and write from/to the copy buffer.
; This is used by the editor to store data that was copied in VISUAL mode
; or deleted.
;*******************************************************************************

.include "errors.inc"
.include "finalex.inc"
.include "macros.inc"
.include "zeropage.inc"

.import __COPYBUFF_BSS_SIZE__

MAX_COPY_SIZE = __COPYBUFF_BSS_SIZE__

.if FINAL_BANK_MAIN=FINAL_BANK_BUFF
;******************************************************************************
; Flat memory procedure mappings
__buff_putch        = putch
__buff_getch        = getch
__buff_getline      = getline
__buff_clear        = clear
__buff_lines_copied = lines_copied
__buff_push         = push
__buff_pop          = pop
__buff_len          = len

.else
;******************************************************************************
; Copy Buff JUMP table
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
LINES_COPIED
PUSH
POP
LEN
.endenum

.RODATA
.linecont +
.define buff_procs putch, getch, getline, clear, lines_copied, push, pop, len
.linecont -
buff_procs_lo: .lobytes buff_procs
buff_procs_hi: .hibytes buff_procs

.CODE
.export __buff_putch
__buff_putch: COPYBUFFJUMP buff_proc_ids::PUTCH

.export __buff_getch
__buff_getch: COPYBUFFJUMP buff_proc_ids::GETCH

.export __buff_getline
__buff_getline: COPYBUFFJUMP buff_proc_ids::GETLINE

.export __buff_clear
__buff_clear: COPYBUFFJUMP buff_proc_ids::CLEAR

.export __buff_lines_copied
__buff_lines_copied: COPYBUFFJUMP buff_proc_ids::LINES_COPIED

.export __buff_push
__buff_push: COPYBUFFJUMP buff_proc_ids::PUSH

.export __buff_pop
__buff_pop: COPYBUFFJUMP buff_proc_ids::POP

.export __buff_len
__buff_len: COPYBUFFJUMP buff_proc_ids::LEN

;******************************************************************************
; Entrypoint for label routines
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
	jmp __final_call
.endproc
.endif

.segment "COPYBUFF_BSS"
buffptr:  		.word 0 	; buffer pointer
buffsave: 		.word 0		; backup buffer pointer (see buff::push)
visual_lines_copied:	.byte 0		; number of lines copied in VISUAL modes
copybuff: 		.res $1e00	; buffer for copy data

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

	decw buffptr
	decw @buff
	ldy #$00
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
;  - .A: $0d if last character is a newline
;  - .Y: the number of characters that were read to the buffer
;  - .C: set if the buffer is empty
.proc getline
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
;   - .A: the number of lines in the copy buffer
;   - .C: clear if there are no lines copied
.proc lines_copied
	lda visual_lines_copied
	cmp #$01
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

;******************************************************************************
; LEN
; Returns the length of the buffer
; OUT:
;   - .XY: the number of characters in the buffer
.export len
.proc len
	ldxy buffptr
	sub16 #copybuff
	rts
.endproc
