.include "zeropage.inc"
.include "macros.inc"

; where the bank-switch code lives
BANK_CODE_ADDRESS=$2000

.import __SOURCE_START__
.import __SOURCE_SIZE__

;--------------------------------------
; init inializes the Final Expansion memory by writing the code needed to
; switch banks regardless of which bank we are in
.export __final_init
__final_init:

	ldx #bank_code_size-1
	; store to zeropage
@l0:	lda __final_bank,x
	sta zp::tmp0,x
	dex
	bpl @l0

	; store code from ZP to all banks
@l1:
	ldy #%10100000
	sty $9c02
	lda bank_code,x
	sta BANK_CODE_ADDRESS,x

	; copy the source-editing code to all source buffers

	rts

;--------------------------------------
; the below code is written to all banks at BANK_CODE_ADDRESS
bank_code:
.CODE

;--------------------------------------
; memcpy writes the memory from (tmp0) to (tmp2)
; The number of bytes is given in .YX
; the block # to write to is given in .A
memcpy:
@cnt=zp::tmp4
@srcbank=zp::tmp5
@dstbank=zp::tmp6
	sta @dstbank
	lda $9c02
	sta @srcbank

	ldy #$00
@l0:	lda @srcbank
	sta $9c02
	lda (zp::tmp0),y
	pha
	lda @dstbank
	sta $9c02
	pla
	sta (zp::tmp2),y
	incw zp::tmp0
	incw zp::tmp2
	dex
	bne @l0
	dey
	bne @l0
	rts

;--------------------------------------
; store_byte stores the byte given in zp::tmp0 toa address .YX in bank .A
store_byte:
	stxy zp::tmp1
	ora #%10100000
	sta $9c02
	lda zp::tmp0
	ldy #$00
	sta (zp::tmp1),y
	rts

;--------------------------------------
; read the byte in bank .A at address .YX
read_byte:
	stxy zp::tmp0
	ora #%10100000
	sta $9c02
	ldy #$00
	lda (zp::tmp0),y
	rts

;--------------------------------------
; bank sets the bank of RAM (0-15) given in .A
.export __final_bank
__final_bank:
	ora #%10100000	; enable "super RAM" mode
	sta $9c02
	rts

bank_code_size=*-__final_bank
