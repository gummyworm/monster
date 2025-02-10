.ifndef VIC20_RAM_INC
VIC20_RAM_INC = 1

.include "finalex.inc"
.include "../zeropage.inc"

;******************************************************************************
; LDA24
; Loads the given 24 bit address into .A
.macro lda24 bank, addr, offset
.ifdef vic20
.ifnblank offset
	lda offset
	sta zp::bankval
.endif
.ifnblank addr
	ldxy addr
.endif
	lda bank

.ifblank offset
	jsr __ram_load_byte
.else
	lda bank
	jsr __ram_load_byte_off
.endif
.endif
.endmacro

;******************************************************************************
; STA24
; Stores the value in .A to the given 24 bit address
.macro sta24 bank, addr
.ifdef vic20
	sta zp::bankval
	ldxy addr
	lda bank
	jsr __ram_store_byte
.endif
.endmacro

;******************************************************************************
; MOV
; Moves the value from the given 24-bit source to the given 24-bit destination
.macro mov dst24, src24
	.if (.match (.left (1, {src24}), #))
		; immediate mode, just load .A with the immediate value
		lda src24
	.else
		lda24 src
	.endif
	sta24 dst24
.endmacro
.endif
