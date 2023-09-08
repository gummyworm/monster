.include "asm.inc"
.include "errors.inc"
.include "labels.inc"
.include "macros.inc"
.include "string.inc"
.include "zeropage.inc"

.segment "SOURCE"
;******************************************************************************
.export macro_addresses
.export macros
nummacros: .byte 0
macro_addresses: .res 256
macros: .res 512

;******************************************************************************
; MACRO FORMAT
;    -------------------------------------
;    | size  |  description              |
;    |-----------------------------------|
;    |  0-16 | macro name                |
;    |   1   | number of parameters      |
;    |  0-16 | parameter 0 name          |
;    |  ...  | parameter n name          |
;    | 0-255 | macro definition          |
;    |   1   | terminating 0             |
;    -------------------------------------

.CODE
;******************************************************************************
; MAC_INIT
; Initializes the macro state by removing all existing macros
.export __mac_init
.proc __mac_init
	lda #$00
	sta nummacros
	ldxy #macros
	stxy macro_addresses
	rts
.endproc

;******************************************************************************
; MAC_ADD
; Adds the macro to the internal macro state.
; IN:
;  - .XY: pointer to the macro definition
;     This will not contain the .MAC but does end with .ENDMAC)
;  - .A: number of parameters
;  -  zp::tmp0: pointer to parameters as a sequence of 0-terminated strings
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

; copy the macro definition byte-by-byte til we get to .endmac
@paramsdone:
@l0:	ldy #$00
	lda (@src),y
	cmp #'.'
	bne @next
	ldxy @src
	streq @endmac, 7	; are we at .endmac?
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

;******************************************************************************
; ASM
; Expands the given macro using the provided parameters and assembles it.
; IN:
;  - zp::mac0-zp::mac4: the macro parameters
;  - .A: the id of the macro
.export __mac_asm
.proc __mac_asm
@params=zp::macros
@err=zp::macros+$0b
@errcode=zp::macros+$0c
@cnt=zp::macros+$0d
@macro=zp::macros+$0e
@numparams=zp::macros+$10
	asl
	tax
	lda macro_addresses,x
	sta @macro
	lda macro_addresses+1,x
	sta @macro+1

	; read past the macro name
	ldy #$00
	sty @err	; init err to none
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
	inc @cnt
	jsr lbl::add	; set the parameter to its value
	bcc :+
	bcs @cleanup

:	; read past the param name
	ldy #$00
:	incw @macro
	lda (@macro),y
	bne :-
	incw @macro

	jmp @setparams	; repeat for all params

@paramsdone:
	; assemble the macro line by line
@asm:	ldxy @macro
	; save state that may be clobbered if we assemble another macro
	txa
	pha
	tya
	pha
	lda @cnt
	pha

	jsr asm::tokenize
	sta @errcode
	lda #$00
	adc #$00
	sta @err

	; restore state
	pla
	sta @cnt
	pla
	sta @macro+1
	pla
	sta @macro

@chkerr:
	bcc @ok
	bne @cleanuploop

@ok:	; move to the next line
	ldy #$00
:	incw @macro
	lda (@macro),y
	bne :-

	incw @macro
	lda (@macro),y		; at the end?
	bne @asm		; no, continue

@cleanup:
	lda @cnt
	beq @done
@cleanuploop:
	pla
	tay
	pla
	tax
	jsr lbl::del
	dec @cnt
	bne @cleanuploop

@done:	lsr @err	; set .C if error
	lda @errcode
	rts
.endproc

;******************************************************************************
; GET
; Returns the id of the macro corresponding to the given text
; IN:
;  - .XY: pointer to the text
; OUT:
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
