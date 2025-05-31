.include "reu.inc"
.include "../config.inc"
.include "../macros.inc"
.include "../zeropage.inc"

.export __ram_get_byte
.proc __ram_get_byte
	; TODO:
.endproc

.export	__ram_call
.proc __ram_call
	; TODO:
.endproc

.export	__ram_copy
.proc __ram_copy
	; TODO:
.endproc

.export	__ram_memcpy
.proc __ram_memcpy
	; TODO:
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
.export	__ram_copy_line
.proc __ram_copy_line
	ldy #$00
:	lda (zp::bankaddr0),y
	sta (zp::bankaddr1),y
	beq @done
	cmp #$0d
	beq @done
	iny
	cpy #LINESIZE
	bcc :-
@done:	rts
.endproc

;*******************************************************************************
; STORE_BYTE
; stores the byte given in zp::bankval to address .YX in bank .A
; IN:
;  - .XY:         the address to store to
;  - .A:          the bank to store to
;  - zp::bankval: the byte to write
; CLOBBERS:
;  - .A
.export	__ram_store_byte
.proc __ram_store_byte
@dst=zp::banktmp
	stxy @dst
	ldy #$00
	sta (@dst),y
	ldy @dst+1
	rts
.endproc

;*******************************************************************************
; STORE_BYTE_REL
; stores the byte given in zp::bankval to the address in .XA in bank .A
; IN:
;  - .XY:            the base address
;  - .A:             the bank to store to
;  - zp::bankoffset: the offset from the base address
;  - zp::bankval:    the byte to write
.export	__ram_bank_store_rel
.proc __ram_bank_store_rel
	; TODO:
.endproc

.export	__ram_load_byte
.proc	__ram_load_byte
	; TODO:
.endproc

.export	__ram_load_byte_off
.proc __ram_load_byte_off
	; TODO:
.endproc

.export	__ram_copy_banked
.proc __ram_copy_banked
	; TODO:
	rts
.endproc
