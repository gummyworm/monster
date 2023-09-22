.include "errors.inc"
.include "memory.inc"
.include "text.inc"
.include "zeropage.inc"
.include "macros.inc"
.CODE

;******************************************************************************
; MEMSET
; Sets zp::tmp0 bytes of the memory at (YX) to .A.
; IN:
;  - zp::tmp0: the memory address to set
;  - .A: the value to set the memory to
.export __util_memset
.proc __util_memset
	stxy zp::util
	ldy zp::tmp0
@l0:	sta (zp::util),y
	dey
	bpl @l0
	rts
.endproc

;******************************************************************************
; MEMCPY
; Moves zp::tmp0 bytes from (zp::tmp2) to (zp::tmp4).
; IN:
;  - zp::tmp0: the number of bytes to move
;  - zp::tmp2: the source address
;  - zp::tmp4: the destination address
.export __util_memcpy
.proc __util_memcpy
@src=zp::tmp2
@dst=zp::tmp4
@len=zp::tmp0
	ldxy @len
	cmpw #$0000
	beq @done

@l0:	ldy #$00
	lda (@src),y
	sta (@dst),y
	incw @src
	incw @dst
	decw @len
	ldxy @len
	cmpw #$0000
	bne @l0
@done:  rts
.endproc

;******************************************************************************
; CHTOHEX
; Returns the binary representation of the character given in .A
; IN:
;  - .A: the character to get the hex representation of
; OUT:
;  - .X: the hex representation of the least significant nybble
;  - .Y: the hex representation of the most significant nybble
.export __util_chtohex
.proc __util_chtohex
	cmp #'f'+1
	bcs @done
	cmp #'a'
	bcc @numeric
	sbc #'a'-$a
	rts
@numeric:
	cmp #'9'+1
	bcs @done
	cmp #'0'
	bcc @done
	sbc #'0'
@done:
	rts
.endproc


;******************************************************************************
; HEXTOSTR
; Returns the string representation of the value in .A
; IN:
;  - .A: the value to get the string representation of
; OUT:
;  - .X: the character representation of the low nybble
;  - .Y: the character representation of the  high nybble
;
.export __util_hextostr
.proc __util_hextostr
	pha
	lsr
	lsr
	lsr
	lsr
	cmp #$0a
	bcs :+
	adc #'0'
	bcc :++
:	adc #'a'-$a-1
:	tay

	pla
	and #$0f
	cmp #$0a
	bcs :+
	adc #'0'
	bcc :++
:	adc #'a'-$a-1
:	tax
	rts
.endproc

;******************************************************************************
; ATOI
; Returns the value of the decimal string given in .XY.
; The string must be terminated by a \0, $0d (newline), or ','.
; IN:
;  - .XY: the address of the string to get the value of
; OUT:
;  - .A: the index after the last character
;  - .C: clear on success
;  - .XY: the 16 bit value of the string
; used in the given string
.export atoi
.proc atoi
@tmp=zp::tmp10
@str=zp::tmp12
@scale=zp::tmp14
@val=zp::tmp15
@tmp2=zp::tmp17
	stxy @str
	lda #$00
	sta @val
	sta @val+1

	; find terminating char
	ldy #$ff
:	iny
	lda (@str),y
	jsr __util_is_separator
	bne :-
@endfound:
	sty @offset
	dey

	ldx #$ff
	stx @scale
@l0:	inc @scale
	lda (@str),y
	cmp #'9'+1
	bcs @unexpectedchar
	cmp #'0'
	bcc @unexpectedchar
	sec
	sbc #'0'
	sta @tmp
	lda #$00
	sta @tmp+1
	ldx @scale
	beq @muldone
@l1:	jsr @mul10
	bcs @err	; oversized value
	dex
	bne @l1
@muldone:
	clc
	lda @tmp
	adc @val	; add the digit*(10**x)
	sta @val
	lda @val+1
	adc @tmp+1
	sta @val+1
	bcs @err	; oversized value
	dey
	bpl @l0

	ldx @val
	ldy @val+1
@offset=*+1
	lda #$00
	RETURN_OK
@unexpectedchar:
	lda #ERR_UNEXPECTED_CHAR
@err:	rts

;------------------
@mul10:
; multiply (in place) word in @tmp by 10
; .C is clear on success, set on failure
	lda @tmp+1
	sta @tmp2+1
	lda @tmp
	sta @tmp2

	asl		; *2
	rol @tmp+1
	bcs @mulerr
	asl		; *4
	rol @tmp+1
	bcs @mulerr
	asl		; *8
	rol @tmp+1
	bcs @mulerr

	adc @tmp2	; *9
	sta @tmp
	lda @tmp+1
	adc @tmp2+1
	bcs @mulerr
	sta @tmp+1

	lda @tmp	; *10
	adc @tmp2
	sta @tmp
	lda @tmp+1
	adc @tmp2+1
	bcs @mulerr
	sta @tmp+1
	RETURN_OK
@mulerr:
	RETURN_ERR ERR_OVERSIZED_OPERAND
.endproc

;******************************************************************************
; TODEC
; Gets the decimal representation of the value given.
; IN:
;  - .XY: the value to get the decimal representation of
; OUT:
;  - mem::spare: contains the decimal representation fo the value.
;   The returned string is 5 bytes in length (not 0 terminated).
;   Leading zeros are present if the value is < 10000
.export __util_todec
.proc __util_todec
result=mem::spare
	lda #'0'
	sta result
	sta result+1
	sta result+2
	sta result+3
	sta result+4
@l1000s:
	sub16 #1000
	bcc @do100s
	inc result+1
	lda result+1
	cmp #'9'+1
	bcc @l1000s
	lda #'0'
	sta result+1
	inc result
	bne @l1000s

@do100s:
	add16 #1000
@l100s:	sub16 #100
	bcc @do10s
	inc result+2
	bne @l100s

@do10s:	add16 #100
@l10s:	sub16 #10
	bcc @do1s
	inc result+3
	bne @l10s

@do1s:	txa
	clc
	adc #10+'0'
	sta result+4
	rts
.endproc

;******************************************************************************
; IS_WHITESPACE
; Checks if the given character is a whitespace character
; IN:
;  - .A: the character to test
; OUT:
;  - .Z: set if if the character in .A is whitespace
.export __util_is_whitespace
.proc __util_is_whitespace
	cmp #$0d
	beq :+
	cmp #' '
:	rts
.endproc

;******************************************************************************
; is_null_space_comma_closingparen
; IN:
;  - .A: the character to test
; OUT:
;  - .Z: set if the char in .A is: 0,$0d,' ', ',', or ')'
.export __util_is_null_return_space_comma_closingparen_newline
.proc __util_is_null_return_space_comma_closingparen_newline
	cmp #$00
	beq @done
	jsr __util_is_whitespace
	beq @done
	cmp #','
	beq @done
	cmp #')'
@done:	rts
.endproc

;******************************************************************************
; IS_OPERATOR
; IN:
;  - .A: the character to test
; OUT:
;  - .Z: set if the char in .A is an operator ('+', '-', etc.)
.export __util_isoperator
.proc __util_isoperator
	cmp #'('
	beq :+
	cmp #')'
	beq :+
	cmp #'+'
	beq :+
	cmp #'-'
	beq :+
	cmp #'*'
	beq :+
	cmp #'/'
:	rts
.endproc

;******************************************************************************
; IS_SEPARATOR
.export __util_is_separator
.proc __util_is_separator
	cmp #':'
	beq @yes
	jsr __util_is_null_return_space_comma_closingparen_newline
	bne :+
@yes:	rts
:	jmp __util_isoperator
.endproc

;******************************************************************************
; IS_ALPHANUM
; IN:
;  .A: the character to test for alphanumeric
; OUT:
;  - .Z: set if the given character is alpha ('A'-'z', or '0'-'9')
.export __util_is_alphanum
.proc __util_is_alphanum
	cmp #'0'
	bcc @no
	cmp #'9'+1
	bcc @yes
	cmp #$41
	bcc @no
	cmp #$7a+1
	bcs @no
@yes:	lda #$00
	rts
@no:	lda #$ff
	rts
.endproc

;******************************************************************************
; PARSE_ENQUOTED_STRING
; Parses an enquoted text string in returns the text that is within the quotes
; at the given destination address
; returns the length in .A ($ff if no string was found)
; IN:
;  - .XY: the address of the string to parse
;  - zp::tmp0: the address to store the parsed string
; OUT:
;  - zp::tmp0: the text within the quotes (")
;  - .C: set if the given string was not valid
.export __util_parse_enquoted_string
.proc __util_parse_enquoted_string
@src=zp::tmp2
@dst=zp::tmp0
	stxy @src
	ldy #$00
@eatws:	lda (@src),y
	cmp #' '
	bne :+
	incw @src
	bne @eatws
:	cmp #'"'
	bne @err

@l0:	incw @src
	lda (@src),y
	beq @err	; no closing quote
	cmp #'"'
	beq @done
	sta (@dst),y
	incw @dst
	bne @l0

@done:	lda #$00
	sta (@dst),y	; 0-terminate the string
	RETURN_OK
@err:	RETURN_ERR ERR_SYNTAX_ERROR
.endproc
