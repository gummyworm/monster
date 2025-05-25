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
@tmp=r0
	stxy reu::c64addr
	ldxy #@tmp
	stxy reu::c64addr
	jsr reu::load
	lda @tmp
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
@tmp=r0
	sta @tmp
	txa
	clc
	adc @tmp
	tax
	bcc :+
	iny
:	jmp __vmem_load
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
@tmp=r0
	sta @tmp
	stxy reu::reuaddr
	ldxy #@tmp
	stxy reu::c64addr
	jsr reu::store
	lda @tmp
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
@tmp=r0
	sta @tmp
	txa
	clc
	adc @tmp
	tax
	bcc :+
	iny
:	jmp __vmem_store
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
