.include "errors.inc"
.include "macros.inc"
.include "zeropage.inc"

.CODE

;******************************************************************************
; FIND
; returns the address to the first occurrence of the text in .YX in the string
; in zp::str0
; IN:
;  - .XY: the string to find the substring within
;  - zp::tmp0: the substring to search for
; OUT:
;  - .A: the index of the first occurrence
;  - .C: set if not found
.export __str_find
.proc __str_find
@str=zp::str0
@lookfor=zp::str2
	stxy @str
@l0:
	ldy #$00
	lda (@str),y
	bne @l1
	sec		; not found
	rts
@l1:	lda (@lookfor),y
	beq @done
	cmp (@str),y
	bne :+
	iny
	bne @l1
:	incw @str
	bne @l0
@done:
	tya
	clc
	rts
.endproc

;******************************************************************************
; REPLACE
; Replaces all occurrences of the string in .XY in the string in zp::str0
; with the string in zp::str2
; in zp::str0
; IN:
;  - .XY - the address of the string to replace in
;  - zp::srt0 - the string to replace
;  - zp::str2 - the string to replace with
; OUT:
;  - .A - the index of the first occurrence
;  - .C - set if not found
; SIDE-EFFECTS:
;  the string that the replace was done on is modified in place. It MUST be in
;  a buffer big enough to accomodate this.
.export __str_replace
.proc __str_replace
@str=zp::str0	; string to replace
@replace=zp::str2
@replacewith=zp::str4
@len1=zp::str6	; the length of 'replace'
@len2=zp::str7	; the length of 'replacewith'
@index=zp::str8	; index of string to replace in str
@len=zp::str9
	jsr __str_len
	jsr __str_find
	sta @index
	bcc :+
	rts	; nothing to replace
:	tay

	ldxy @replace
	jsr __str_len	; compare the lengths of the strings
	sta @len1
	ldxy @replacewith
	jsr __str_len
	sta @len2

	; if len2 < len1, we need to shift chars down by len2-len1 chars
	cmp @len1
	bcs :+
	lda @len1
	sec
	sbc @len2
	tax
@shiftback:
	ldy @index
:	lda (@str),y
	dey
	sta (@str),y
	bpl :-
	dex
	bpl @shiftback

	; if len2 > len1, we need to shift chars up
	lda @len2
	sec
	sbc @len1
	tax
@shiftup:
	ldy @index
:	lda (@str),y
	iny
	sta (@str),y
	cpy @len
	bne :-
	inx
	dex
	bpl @shiftup

@doreplace:
	lda @str
	clc
	adc @index
	sta @str
	lda @str+1
	adc #$00
	sta @str+1

	ldy @len2
:	lda (@replacewith),y
	sta (@str),y
	dey
	bpl :-
	rts
.endproc

;******************************************************************************
; LEN
; Returns the length of the string in
; IN:
;  - .YX: the string to get the length of
; OUT:
;  - .A: the length of the string
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
; OUT:
;  .Z: set if the strings are equal
.export __str_compare
.proc __str_compare
	tay
	dey
	bmi @match

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
; CAT
; concatentates the two provided strings.
; IN:
;  - .XY: the string to concatentate to
;  - zp::tmp0: the string to add to the end of the first
; OUT:
;  - .XY: the address to a buffer containing the combined string
;  - .C: set if the string is too large (>40 chars)
.export __str_cat
.proc __str_cat
@buff=$100
@str1=zp::tmp2
@str2=zp::tmp0
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
