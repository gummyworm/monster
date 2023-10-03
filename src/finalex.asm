.include "zeropage.inc"
.include "macros.inc"

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
	sei
	pha
	lda #$00
	sta zp::bankoffset
	pla
	; fall through
.endproc

;******************************************************************************
; STORE_BYTE_REL
; stores the byte given in zp::bankval to the address in .XA in bank .A
; IN:
;  - .XY: the base address
;  - .A: the bank to store to
;  - zp::bankoffset: the offset from the base address
;  - zp::bankval: the byte to write
.export __final_bank_store_rel
.proc __final_bank_store_rel
@dst=zp::banktmp
	sei
	stxy @dst

	sta $9c02
	lda zp::bankval
	ldy zp::bankoffset
	sta (@dst),y

	ldx #$80
	stx $9c02	; restore bank
	ldxy @dst
	cli
	rts
.endproc
final_store_size=*-__final_store_byte

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
	sei
	pha
	lda #$00
	sta zp::bankval
	pla
	; fall through
.endproc

;******************************************************************************
; LOAD_BYTE_OFF
; Returns the byte in bank .A at address .YX plus a given offset
; IN:
;  - .XY: the address to read from
;  - .A: the bank to read from
;  - zp::bankval: the offset to read from
; OUT:
;  - .A: the byte that was read
;  - .Y: contains the offset (same that was given as zp::bankval)
.export __final_load_byte_off
.proc __final_load_byte_off
@src=zp::banktmp
	sei
	stxy @src
	sta $9c02	; set bank
	ldy zp::bankval
	lda (@src),y
	ldx #$80
	stx $9c02	; restore bank
	ldx @src
	cli
	rts
.endproc

;******************************************************************************
; COPY
; Copies up to 256 bytes from zp::bankaddr0 to zp::bankaddr1
; IN:
;  - .A:            the bank to perform the copy within
;  - .Y:            the number of bytes to copy
;  - zp::bankaddr0: the source address to copy from
;  - zp::bankaddr1: the destination address to copy to
; DESTROYS:
;  .A, .Y, .X
.export __final_copy
.proc __final_copy
	sei
	sta $9c02
:	lda (zp::bankaddr0),y
	sta (zp::bankaddr1),y
	dey
	bpl :-
	ldx #$80
	stx $9c02	; restore bank
	cli
	rts
.endproc
final_copy_end=*-__final_copy

;******************************************************************************
; CALL
; Performs a JSR to the target address at the given bank. When the routine is
; done, returns to the caller's bank.
; IN:
;  - .XY: the address of the procedure to call
;  - .A: the bank of the target procedure
.export __final_call
.proc __final_call
@src=zp::banktmp
@bank=zp::banktmp+2
@oldbank=zp::banktmp+3
	sei
	stxy zp::jmpvec
	ldx #$4c
	stx zp::jmpaddr	; write the JMP instruction
	ldx $9c02
	stx @oldbank	; save current bank

	sta $9c02

	jsr zp::jmpaddr

	lda @oldbank
	sta $9c02	; restore bank
	cli
	rts
.endproc

;******************************************************************************
; BRK
; Handles the BRK interrupt by returning control to the main bank
; and continuing execution there.
.export __final_brk
.proc __final_brk
	pha
	lda #$80
	sta $9c02
	pla
	jmp ($0334)		; execute the MAIN BRK hanlder
.endproc

;******************************************************************************
; BANK_RTI
; Returns to the given bank and then RTI's
; IN:
;  - zp::bankval: the bank to return to
.export __final_rti
.proc __final_rti
	pha
	lda zp::bankval
	sta $9c02
	pla
	rti
.endproc

.export bankcode_size
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
@copyaddr=$33c
	sei

	; copy the bank code that we wish to copy to ZP
	ldx #bankcode_size
@l0:	lda bankcode-1,x
	sta @copyaddr-1,x
	dex
	bne @l0

	; disable the CLI at the end of store_byte for init
	lda #$ea	; NOP
	sta @copyaddr + final_store_size - 2

	lda #$a2	; skip bank 1 (main bank)
	sta @bank
@copybank:
; copy the code from ZP to all banks
	lda #bankcode_size
 	sta @cnt

	ldxy #__BANKCODE_LOAD__
	stxy @dst
	ldxy #@copyaddr
	stxy @src

@l1:	ldy #$00
	lda (@src),y	; get a byte to write to the bank
	sta zp::bankval	; byte to write
	ldxy @dst	; destination address
	lda @bank	; bank to copy to
	jsr @copyaddr	; call the zeropage code (STORE byte)

	incw @src
	incw @dst
	dec @cnt
	bne @l1
	inc @bank
	lda @bank
	cmp #$b0
	bne @copybank
	cli
	rts
.endproc

;******************************************************************************
; COPY
; Writes the memory from (tmp0) to (tmp2)
; The number of bytes is given in .YX and the block # to write to is given in .A
; IN:
;  - .A: the source/destination block
;  - .XY: the number of bytes to copy
;  - zp::tmp2: the source address
;  - zp::tmp4: the destination address
.export __final_memcpy
.proc __final_memcpy
@src=zp::tmp2
@dst=zp::tmp4
@bank=zp::tmp6
@size=zp::tmp0
	cmpw #$00
	beq @done

	stxy @size
	sta @bank

@l0:	; read a byte from the source bank/addr
	ldxy @src
	lda @bank
	jsr __final_load_byte

	; write the byte to the dest bank/addr
	sta zp::bankval
	ldxy @dst
	lda @bank
	jsr __final_store_byte

	; move to the next location
	incw @src
	incw @dst

	decw @size
	ldxy @size
	cmpw #0
	bne @l0
@done:	rts
.endproc
