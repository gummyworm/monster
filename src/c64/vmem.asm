.include "reu.inc"
.include "../errors.inc"
.include "../macros.inc"
.include "../memory.inc"

.BSS
.export prog00
prog00: .res $400

.export STEP_EXEC_BUFFER
.export STEP_HANDLER_ADDR
STEP_HANDLER_ADDR:
STEP_EXEC_BUFFER: .res 10

.BSS

;*******************************************************************************
savexy: .word 0

.CODE

;*******************************************************************************
; LOAD
; Reads a byte from the physical address associated with the given virtual
; address
; IN:
;  - .XY: the virtual address
; OUT:
;  - .A: the byte at the physical address
.export __vmem_load
.proc __vmem_load
@tmp=zp::banktmp
	stxy reu::reuaddr
	ldx #^REU_VMEM_ADDR
	stx reu::reuaddr+2
	ldxy #@tmp
	stxy reu::c64addr
	jsr reu::load
	lda @tmp
	ldxy reu::reuaddr
	rts
.endproc

;*******************************************************************************
; LOAD OFF
; Reads a byte from the physical address associated with the given virtual
; address
; IN:
;  - .XY: the virtual address
;  - .A: the offset of the virtual address to load
; OUT:
;  - .A: the byte at the physical address
.export __vmem_load_off
.proc __vmem_load_off
@tmp=zp::banktmp
	stxy savexy
	sta @tmp
	txa
	clc
	adc @tmp
	tax
	bcc :+
	iny
:	jsr __vmem_load
	ldxy savexy
	rts
.endproc

;*******************************************************************************
; STORE
; Stores a byte at the physical address associated with the given virtual
; address
; IN:
;  - .XY: the virtual address
;  - .A:  the byte to store
.export __vmem_store
.proc __vmem_store
@tmp=zp::banktmp
	sta @tmp
	stxy reu::reuaddr
	ldx #^REU_VMEM_ADDR
	stx reu::reuaddr+2
	ldxy #@tmp
	stxy reu::c64addr
	jsr reu::store
	lda @tmp
	ldxy reu::reuaddr
	rts
.endproc

;*******************************************************************************
; STORE OFF
; Stores a byte at the physical address associated with the given virtual
; address offset by the given offset.
; IN:
;  - .XY:         the virtual address
;  - .A:          the offset from the base address
;  - zp::bankval: the value to store
.export __vmem_store_off
.proc __vmem_store_off
@tmp=zp::banktmp
	stxy savexy
	sta @tmp
	txa
	clc
	adc @tmp
	tax
	bcc :+
	iny
:	jsr __vmem_store
	ldxy savexy
	rts
.endproc

;*******************************************************************************
; TRANSLATE
; Returns the physical address associated with the given virtual address
; IN:
;  - .XY: the virtual address
; OUT:
;  - .XY: the physical address
;  - .A:  the bank number of the physical address
.export __vmem_translate
.proc __vmem_translate
	lda #^REU_VMEM_ADDR
	rts
.endproc

;*******************************************************************************
; WRITABLE
; Checks if the given address is within the valid writable range.
; This includes the addresses [$00, $8000) and [$a000, $c000)
; IN:
;   - .XY: the address to check for writability
; OUT:
;   - .C: set if the address is NOT writable
.export __vmem_writable
.proc __vmem_writable
	; all RAM is writable
	; TODO: check the bank register?
	RETURN_OK
.endproc
