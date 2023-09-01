.include "zeropage.inc"
.include "macros.inc"

; where the bank-switch code lives
BANK_CODE_ADDRESS=$2000

.import __SOURCE_START__
.import __SOURCE_SIZE__
.import __BANKCODE_SIZE__
.import __BANKCODE_LOAD__

;******************************************************************************
; BANK CODE
; The following procedures have stable positions in every bank.
; This means that any bank may call them without worrying about the instruction
; after a bank switch changing mid-procedure.
;******************************************************************************

.segment "BANKCODE"

bankcode:
;******************************************************************************
; STORE_BYTE
; stores the byte given in zp::bankval to address .YX in bank .A
; IN:
;  - .XY: the address to store to
;  - .A: the bank to store to
;  - zp::bankval: the byte to write
; CLOBBERS:
;  - .A
.export __final_store_byte
.proc __final_store_byte
@bank=zp::banktmp
@dst=zp::banktmp+1
	sta @bank
	tya
	pha
	lda $9c02
	pha		; save current bank
	stxy @dst
	lda @bank
	ora #%10100000	; SUPERRAM mode in final expansion
	sta $9c02
	lda zp::bankval
	ldy #$00
	sta (@dst),y
	pla
	sta $9c02	; restore bank
	pla
	tay
	rts
.endproc

;******************************************************************************
; READ_BYTE
; Returns the byte in bank .A at address .YX
; IN:
;  - .XY: the address to read from
;  - .A: the bank to read from
; OUT:
;  - .A: the byte that was read
.export __final_load_byte
.proc __final_load_byte
@src=zp::banktmp
@bank=zp::banktmp+2
@oldbank=zp::banktmp+3
	sta @bank
	lda $9c02
	sta @oldbank	; save current bank
	stxy @src
	lda @bank
	ora #%10100000	; SUPERRAM mode in final expansion
	sta $9c02
	ldy #$00
	lda (@src),y
	pha
	lda @oldbank
	sta $9c02	; restore bank
	pla
	rts
.endproc

;******************************************************************************
; bank sets the bank of RAM (0-15) given in .A
.export __final_bank
__final_bank:
	ora #%10100000	; enable "super RAM" mode
	sta $9c02
	rts

bankcode_size = *-bankcode
;******************************************************************************
; END OF BANK CODE
;******************************************************************************

.CODE
;******************************************************************************
; INIT
; Inializes the Final Expansion memory by writing the code needed to
; switch banks regardless of which bank we are in
.export __final_init
.proc __final_init
@src=zp::tmp1
@dst=zp::tmp3
@cnt=zp::tmp5
@bank=zp::tmp6
	; copy the bank code that we wish to copy to ZP
	ldx #bankcode_size-1
@l0:	lda bankcode,x
	sta $30,x
	dex
	bpl @l0

	lda #$02	; skip bank 1 (main bank)
	sta @bank
@copybank:
; copy the code from ZP to all banks
	lda #bankcode_size
 	sta @cnt

	ldxy #__BANKCODE_LOAD__
	stxy @dst
	ldxy #$30
	stxy @src

@l1:	ldy #$00
	lda (@src),y	; get a byte to write to the bank
	sta zp::bankval	; byte to write
	ldxy @dst	; destination address
	lda @bank	; bank to copy to
	jsr $30		; call the zeropage code

	incw @src
	incw @dst
	dec @cnt
	bne @l1
	inc @bank
	lda @bank
	cmp #$0f
	bne @copybank
	rts
.endproc

;******************************************************************************
; COPY
; Writes the memory from (tmp0) to (tmp2)
; The number of bytes is given in .YX and the block # to write to is given in .A
; IN:
;  - .A: the destination block
;  - .XY: the number of bytes to copy
;  - zp::tmp1: the source address
;  - zp::tmp3: the destination address
.export __final_memcpy
.proc __final_memcpy
@src=zp::tmp1
@dst=zp::tmp3
@cnt=zp::tmp5
@srcbank=zp::tmp6
@dstbank=zp::tmp8
@size=zp::tmp9
	stxy @size
	sta @dstbank
	lda #$01
	sta @srcbank

@l0:	; read a byte from the source bank/addr
	lda @srcbank
	ldxy @src
	jsr __final_load_byte

	; write the byte to the dest bank/addr
	sta zp::bankval
	ldxy @dst
	lda @dstbank
	jsr __final_store_byte

	; move to the next location
	incw @src
	incw @dst

	decw @size
	ldxy @size
	cmpw #0
	bne @l0
	rts
.endproc
