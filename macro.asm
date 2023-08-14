.include "asm.inc"
.include "errors.inc"
.include "labels.inc"
.include "macros.inc"
.include "string.inc"
.include "zeropage.inc"

.segment "SOURCE"
;--------------------------------------
nummacros: .byte 0
macro_addresses: .res 256
macros: .res 1024

;--------------------------------------
; the format of a macro is:
; -------------------------------------
; | size  |  description              |
; |-----------------------------------|
; |  0-16 | macro name                |
; |   1   | number of parameters      |
; |  0-16 | parameter 0 name          |
; |  ...  | parameter n name          |
; | 0-255 | macro definition          |
; |   1   | terminating 0             |
; -------------------------------------

.CODE
;--------------------------------------
; MAC_INIT
; initializes the macro state by removing all existing macros
.export __mac_init
.proc __mac_init
	lda #$00
	sta nummacros
	ldxy #macros
	stxy macro_addresses
	rts
.endproc

;--------------------------------------
; MAC_ADD
; adds the macro to the internal macro state.
; in:
;  - .XY: pointer to the macro definition (start with .MAC & ends with .ENDMAC)
;  - .A: number of parameters
;  -  zp::tmp0: pointer to parameters as a sequence of length prefixed strings
.export __mac_add
.proc __mac_add
@src=zp::macros
@dst=zp::macros+2
@addr=zp::macros+4
@params=zp::tmp0
@numparams=zp::tmp2
	sta @numparams
	stxy @src
	lda nummacros
	asl
	adc #<macro_addresses
	sta @addr
	lda #>macro_addresses
	adc #$00
	sta @addr+1

	; get the address to write the macro to
	ldy #$00
	lda (@addr),y
	sta @dst
	iny
	lda (@addr),y
	sta @dst+1

; copy the name of the macro (parameter 0)
	ldy #$00
@copyname:
	lda (@params),y
	sta (@dst),y
	php
	incw @dst
	incw @params
	plp
	bne @copyname

	; store the number of parameters
	dec @numparams	; decrement to get the # without the macro name
	lda @numparams
	sta (@dst),y
	incw @dst

	; store the parameters if there are any
	lda @numparams
	beq @paramsdone
@copyparams:
	lda (@params),y
	sta (@dst),y
	php
	incw @dst
	incw @params
	plp
	bne @copyparams
	dec @numparams
	bne @copyparams

@paramsdone:
@l0:	ldy #$00
	lda (@src),y
	cmp #'.'
	bne @next
	ldxy @src
	streq @endmac, 7	; are we at .endrep?
	beq @done

@next:	sta (@dst),y
	incw @src
	incw @dst
	bne @l0

@done:
	; 0-terminate the macro definition
	lda #$00
	tay
	sta (@dst),y
	incw @dst
	sta (@dst),y

	; done copying use the end address as the start address for the next macro
	inc nummacros
	lda nummacros
	asl
	adc #<macro_addresses
	sta @addr
	lda #>macro_addresses
	adc #$00
	sta @addr+1
	ldy #$00
	lda @dst
	sta (@addr),y
	iny
	lda @dst+1
	sta (@addr),y
	RETURN_OK

@endmac: .byte ".endmac"
.endproc

;--------------------------------------
; ASM
; expands the given macro using the provided parameters and assembles it
; in:
;  - zp::mac0-zp::mac4: the macro parameters
;  - .A: the id of the macro
;  - .XY: pointer to the name of the macro (terminated by ' ' or 0)
.export __mac_asm
.proc __mac_asm
@params=zp::macros
@cnt=zp::macros+$0c
@macro=zp::macros+$0d
@numparams=zp::macros+$0f
	asl
	tax
	lda macro_addresses,x
	sta @macro
	lda macro_addresses+1,x
	sta @macro+1

	; read past the macro name
	ldy #$00
:	incw @macro
	lda (@macro),y
	bne :-

	; define the macro params
	incw @macro
	lda (@macro),y
	sta @numparams
	incw @macro

	lda #$00
	sta @cnt
@setparams:
	lda @cnt
	cmp @numparams
	beq @paramsdone
	asl
	tax
	; get the value to set the parameter to
	lda @params,x
	sta zp::label_value
	lda @params+1,x
	sta zp::label_value+1

	; get the name of the parameter to set the value for
	ldy #$00
	ldx @macro
	ldy @macro+1
	; save the label name for later removal
	txa
	pha
	tya
	pha
	jsr lbl::add	; set the parameter to its value

	; read past the param name
	ldy #$00
:	incw @macro
	lda (@macro),y
	bne :-
	incw @macro

	inc @cnt
	jmp @setparams	; repeat for all params

@paramsdone:
	; assemble the macro line by line
@asm:	ldxy @macro
	jsr asm::tokenize

	; move to the next line
	ldy #$00
:	incw @macro
	lda (@macro),y
	bne :-

	incw @macro
	lda (@macro),y		; at the end?
	bne @asm		; no, continue
@cleanup:
	pla
	tay
	pla
	tax
	jsr lbl::del
	dec @cnt
	bne @cleanup

@done:	rts
.endproc

;--------------------------------------
; GET
; returns the id of the macro corresponding to the given text
; in:
;  - .XY: pointer to the text
; out:
;  - .A: the id of the macro (if any)
;  - .C: set if there is no macro for the given text, clear if there is
.export __mac_get
.proc __mac_get
@tofind=zp::tmp0
@addr=zp::tmp2
@name=zp::tmp4
@cnt=zp::tmp6
	stxy @tofind
	lda #<macro_addresses
	sta @addr
	lda #>macro_addresses
	sta @addr+1
	lda #$00
	sta @cnt
	cmp nummacros
	beq @notfound
@find:
	ldy #$00
	lda (@addr),y
	sta @name
	iny
	lda (@addr),y
	sta @name+1
	dey
@compare:
	lda (@tofind),y
	beq :+		; end of the string we're trying to find
	cmp #' '
	beq :+
	cmp (@name),y
	bne @next
	iny
	bne @compare

:	lda (@name),y	; make sure the name length matches
	beq @found
@next:
	incw @addr
	incw @addr
	inc @cnt
	ldx @cnt
	cpx nummacros
	bne @find
@notfound:
	sec		; not found
	rts
@found:
	ldy #$00
	lda @cnt
	RETURN_OK
.endproc
