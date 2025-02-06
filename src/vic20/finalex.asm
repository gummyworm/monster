;******************************************************************************
; FINALEX.ASM
; This file contains routines for reading, writing, and executing code in
; different banks.
; The bank code itself resides in low RAM, where it is visible regardless of
; the active bank.
;******************************************************************************

.include "../config.inc"
.include "../macros.inc"
.include "../zeropage.inc"

.segment "BANKCODE"
.export banksp

bankcode:
;*******************************************************************************
; STORE_BYTE
; stores the byte given in zp::bankval to address .YX in bank .A
; IN:
;  - .XY:         the address to store to
;  - .A:          the bank to store to
;  - zp::bankval: the byte to write
; CLOBBERS:
;  - .A
.export __final_store_byte
.proc __final_store_byte
	pha
	lda #$00
	sta zp::bankoffset
	pla
	; fall through
.endproc

;*******************************************************************************
; STORE_BYTE_REL
; stores the byte given in zp::bankval to the address in .XA in bank .A
; IN:
;  - .XY:            the base address
;  - .A:             the bank to store to
;  - zp::bankoffset: the offset from the base address
;  - zp::bankval:    the byte to write
.export __final_bank_store_rel
.proc __final_bank_store_rel
@dst=zp::banktmp
	stxy @dst

	sta $9c02
	lda zp::bankval
	ldy zp::bankoffset
	sta (@dst),y

	ldx #$80
	stx $9c02	; restore bank
	ldxy @dst
	rts
.endproc
final_store_size=*-__final_store_byte

;*******************************************************************************
; READ_BYTE
; Returns the byte in bank .A at address .YX
; IN:
;  - .XY: the address to read from
;  - .A:  the bank to read from
; OUT:
;  - .A: the byte that was read
.export __final_load_byte
.proc __final_load_byte
	pha
	lda #$00
	sta zp::bankval
	pla
	; fall through
.endproc

;*******************************************************************************
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
	stxy @src
	sta $9c02	; set bank
	ldy zp::bankval
	lda (@src),y
	ldx #$80
	stx $9c02	; restore bank
	ldx @src
	rts
.endproc

;*******************************************************************************
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
	sta $9c02
:	lda (zp::bankaddr0),y
	sta (zp::bankaddr1),y
	dey
	bpl :-
	ldx #$80

	; fall through to return_to_x
.endproc

;*******************************************************************************
; RETURN TO X
; Sets the bank to the given bank and returns (RTS)
; IN:
;  - .X: the bank to return to
.proc return_to_x
@done:	stx $9c02	; restore bank
	rts
.endproc

;*******************************************************************************
; COPY LINE
; Copies up to LINESIZE bytes from zp::bankaddr0 to zp::bankaddr1 stopping at
; the first $0d or $00
; IN:
;  - .A:            the bank to perform the copy within
;  - zp::bankaddr0: the source address to copy from
;  - zp::bankaddr1: the destination address to copy to
;  OUT:
;   - .Y: the number of bytes copied
;   - .A: the last byte copied
.export __final_copy_line
.proc __final_copy_line
	ldx $9c02	; get current bank
	sta $9c02	; set bank to copy within
	ldy #$00
:	lda (zp::bankaddr0),y
	sta (zp::bankaddr1),y
	beq @done
	cmp #$0d
	beq @done
	iny
	cpy #LINESIZE
	bne :-
@done:	beq return_to_x	; restore bank
.endproc

;*******************************************************************************
; CALL
; Performs a JSR to the target address at the given bank. When the routine is
; done, returns to the caller's bank.
; IN:
;  - zp::bank:        the bank of the procedure to call
;  - zp::bankjmpaddr: the procedure address
;  - zp::banktmp:     the destination bank address
.export __final_call
.proc __final_call
@a=zp::banktmp+1
@x=zp::banktmp+2
@bank=zp::banktmp
	stx @x
	sta @a

	lda #$4c
	sta zp::bankjmpaddr	; write the JMP instruction
	lda $9c02
	ldx banksp
	inc banksp
	sta zp::bankstack,x

	lda @bank
	sta $9c02		; swap in the target bank
	lda @a			; restore .A
	ldx @x			; restore .X
	jsr zp::bankjmpaddr	; call the target routine
	sta @a			; save .A
	stx @x			; save .X

	dec banksp
	ldx banksp
	lda zp::bankstack,x	; get the caller's bank
	sta $9c02		; restore bank

	lda @a			; restore .A
	ldx @x			; restore .X
	rts
.endproc

banksp:    .byte 0

.CODE

;*******************************************************************************
; GET BYTE
; When called from a non-MAIN bank, returns the value from the MAIN bank for the
; the given address.
; IN:
;   - .XY: the address to get the value of
; OUT:
;   - .A: the value for the given address in the MAIN bank
; CLOBBERS:
;   - .A, .Y, r0-r1
.export __final_get_byte
.proc __final_get_byte
	stxy r0
	ldy #$00
	lda (r0),y
	rts
.endproc

;*******************************************************************************
; COPY BANK 2 BANK
; Entrypoint to copy from one bank to another
; IN:
;  - .A:  the source bank
;  - .XY: the number of bytes to copy
;  - r2:  the source address
;  - r4:  the destination address
;  - r7:  the destination bank
.export __final_memcpy_bank
__final_memcpy_bank:
	skw	; don't overwrite destination bank

;*******************************************************************************
; COPY
; Writes the memory from (tmp0) to (tmp2)
; The number of bytes is given in .YX and the block # to write to is given in .A
; This routine assumes that IF the memory overlaps, that it will do so from
; the TOP. (dst > src)
; IN:
;  - .A:  the source/destination bank
;  - .XY: the number of bytes to copy
;  - r2:  the source address
;  - r4:  the destination address
.export __final_memcpy
.proc __final_memcpy
@size=r0
@src=r2
@dst=r4
@bank=r6
@bankdst=r7
	sta @bankdst	; use source bank as dest as well
	sta @bank

	cmpw #$00
	beq @done
	stxy @size

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
	lda @bankdst
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
