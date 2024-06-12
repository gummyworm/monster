.include "errors.inc"
.include "macros.inc"
.include "memory.inc"
.include "zeropage.inc"

;******************************************************************************
; CONSTANTS

; constants for special symbols in 5-bit compressed strings
SPECIAL_CHARS_START = 'z'-'a'+2

SPACE = SPECIAL_CHARS_START
DOT   = SPECIAL_CHARS_START+1
SLASH = SPECIAL_CHARS_START+2

.CODE

;******************************************************************************
; LEN
; Returns the length of the string in
; IN:
;  - .YX: the string to get the length of
; OUT:
;  - .A: the length of the string
;  - .Z: set if the string is empty (length 0)
.export __str_len
.proc __str_len
@str=zp::str0
	stx @str
	sty @str+1
	ldy #$00
@l0:	lda (@str),y
	beq @done
	cmp #$0d
	beq @done
	iny
	bne @l0
@done:	tya
	rts
.endproc

;******************************************************************************
; COMPARE
; Compares the strings in (str0) and (str2) up to a length of .A
; IN:
;  zp::str0: one of the strings to compare
;  zp::str2: the other string to compare
;  .A:       the max length to compare
; OUT:
;  .Z: set if the strings are equal
.export __str_compare
.proc __str_compare
	tay		; is length 0?
	dey
	bmi @match	; if 0-length comparison, it's a match by default

@l0:	lda (zp::str0),y
	cmp (zp::str2),y
	beq :+
	rts
:	dey
	bpl @l0
@match:	lda #$00
	rts
.endproc

;******************************************************************************
; COMPAREZ
; Compares the 0-terminated string in (str0) with (str2).
; If str0 matches just the first part of str2, will return a match but not
; vise-versa.
; IN:
;  zp::str0: one of the strings to compare
;  zp::str2: the other string to compare
; OUT:
;  .Z: set if the strings are equal
.export __str_comparez
.proc __str_comparez
	ldy #$00
@l0:	lda (zp::str0),y
	beq @match
	cmp (zp::str2),y
	beq :+
	rts			; not equal
:	iny
	bpl @l0
	rts

@match:	lda #$00
	rts
.endproc


;******************************************************************************
; CAT
; concatenates the two provided strings.
; IN:
;  - .XY: the string to concatentate to
;  - r0:  the string to add to the end of the first
; OUT:
;  - .XY:         the address to a buffer containing the combined string
;  - .C:          set if the string is too large (>40 chars)
;  - linebuffer2: contains the result of the concatenation
.export __str_cat
.proc __str_cat
@buff=mem::spare
@str1=r2
@str2=r0
	; copy the first string to the buffer
	stxy @str1
	ldy #$00
:	lda (@str1),y
	sta @buff,y
	beq @cat
	iny
	cpy #40
	bne :-
@toolong:
	RETURN_ERR ERR_LINE_TOO_LONG

@cat:
	tya
	clc
	adc #<@buff
	sta @str1
	lda #>@buff
	adc #$00
	sta @str1+1

	ldy #$00
:	lda (@str2),y
	sta (@str1),y
	beq @done
	iny
	cpy #40
	bcs @toolong
	bcc :-
@done:
	ldxy #@buff
	RETURN_OK
.endproc

;******************************************************************************
; COPY
; copies one 0-terminated string to another
; IN:
;  - .XY: the source string to copy
;  - zp::tmp0: the destination string to copy to
.export __str_copy
.proc __str_copy
@src=zp::str0
@dst=zp::tmp0
	stxy @src
	ldy #$00
:	lda (@src),y
	sta (@dst),y
	beq @done
	iny
	bne :-
@done:	RETURN_OK
.endproc

;******************************************************************************
; TOUPPER
; Replaces all lowercase characters in the given string ($61-$7a) with uppercase
; ($41-$5a) ones.
; IN:
;  - .XY: the address of the string to convert to uppercase
.export __str_toupper
.proc __str_toupper
@str=zp::str0
	stxy @str
	ldy #$00
@l0:	lda (@str),y
	beq @done
	cmp #$61
	bcc @next
	cmp #$7a+1
	bcs @next
	eor #$20	; to upper
	sta (@str),y
@next:	iny
	bne @l0
@done:	rts
.endproc

;******************************************************************************
; UNCOMPRESS
; Uncompresses the 5-bit compressed string
;
; The bytes are arranged in the following format (3 characters = 2 bytes)
; byte 0
;  char 0[4:0] | 5 bits
;  char 1[4:1] | 3 bits
; byte 1
;  char 1[1:0] | 2 bits
;  x           | 1 bit (unused)
;  char 2[4:0] | 5 bits
;
; IN:
;  - .XY: the string to uncompress
; OUT:
;  - mem::spare: the uncompressed string
;  - .XY:        the address of the uncompressed string
.export __str_uncompress
.proc __str_uncompress
@rptr=zp::str0
@wptr=zp::str2
@tmp=zp::tmp0
@tmp2=zp::tmp1
@chars=zp::tmp2		; 3 bytes
@dst=mem::spare
	stxy @rptr
	ldxy #@dst
	stxy @wptr

@l0:	ldy #$00
	lda (@rptr),y		; get byte containing chars 0 and 1[4:1]
	sta @tmp
	lsr
	lsr
	lsr
	sta @chars		; store char 0
	iny
	lda (@rptr),y
	sta @tmp2

	; build char 1
	; get top 2 bits of byte 1 (bottom bits of char 1)
	asl
	rol
	rol
	and #$03
	sta @chars+1
	; combine with bottom 3 bytes of byte 0
	lda @tmp
	and #$07
	asl
	asl
	ora @chars+1
	sta @chars+1

	; build char 2
	lda @tmp2
	and #$1f
	sta @chars+2

	; update pointer
	lda @rptr
	clc
	adc #$02
	sta @rptr
	bcc :+
	inc @rptr+1

:	; write the 5 extracted bytes
	ldy #$00
@l1:	lda @chars,y
	beq @done	; if we found the terminating 0, we're done
	cmp #SPECIAL_CHARS_START
	bcc :+
	tax
	lda special_chars-SPECIAL_CHARS_START,x
	skw
:	adc #'a'-1	; a is actually character 1 (0 is for terminating NULL)
	sta (@wptr),y
	iny
	cpy #$03
	bne @l1

	lda @wptr
	clc
	adc #$03
	sta @wptr
	bcc @l0
	inc @wptr+1
	bne @l0

@done:	sta (@wptr),y	; write the 0
	ldxy #@dst
	rts

.endproc

.RODATA

special_chars:
	.byte ' ', '.', '/'
