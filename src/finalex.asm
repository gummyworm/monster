.include "zeropage.inc"
.include "macros.inc"

.import __BANKCODE_SIZE__
.import __BANKCODE_LOAD__

.BSS
.export __final_rti_bank
__final_rti_bank:	.byte 0	; NOTE: only available in the main bank

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

;******************************************************************************
; COPY LINE
; Copies up to 256 bytes from zp::bankaddr0 to zp::bankaddr1 stopping at the
; first $0d or $00
; IN:
;  - .A:            the bank to perform the copy within
;  - zp::bankaddr0: the source address to copy from
;  - zp::bankaddr1: the destination address to copy to
;  OUT:
;   - .Y: the number of bytes copied
;   - .A: the last byte copied
.export __final_copy_line
.proc __final_copy_line
	sei
	sta $9c02
	ldy #$00
:	lda (zp::bankaddr0),y
	sta (zp::bankaddr1),y
	beq @done
	cmp #$0d
	beq @done
	iny
	bne :-
@done:	ldx #$80
	stx $9c02	; restore bank
	cli
	rts
.endproc

;******************************************************************************
; CALL
; Performs a JSR to the target address at the given bank. When the routine is
; done, returns to the caller's bank.
; IN:
;  - zp::bankjmpaddr: the procedure address
;  - zp::banktmp: the destination bank address
.export __final_call
.proc __final_call
@a=zp::banktmp+1
@bank=zp::banktmp
	sei
	sta @a

	lda #$4c
	sta zp::bankjmpaddr	; write the JMP instruction
	lda $9c02
	pha			; save current bank

	lda @bank
	sta $9c02
	lda @a
	jsr zp::bankjmpaddr
	sta @a			; save .A

	pla			; get the caller's bank
	sta $9c02		; restore bank

	lda @a			; restore .A
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
	jmp ($0334)		; execute the MAIN BRK handler
.endproc

;******************************************************************************
; BANK_RTI
; Returns to the given bank and then RTI's
; MUST be called from the main bank (0)
; IN:
;  - zp::bankval: the bank to return to
.export __final_rti
.proc __final_rti
	pha
	lda __final_rti_bank
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
; Initializes the Final Expansion memory by writing the code needed to
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
; This routine assumes that IF the memory overlaps, that it will do so from
; the TOP. (dst > src)
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

	decw @size

	; we need to copy from top to bottom- add @size-1 to the dst and src
	ldxy @src
	add16 @size
	stxy @src

	ldxy @dst
	add16 @size
	stxy @dst

	incw @size

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
	decw @src
	decw @dst

	decw @size
	lda @size
	bne @l0
	lda @size+1
	bne @l0

@done:	rts
.endproc
