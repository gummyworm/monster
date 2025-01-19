;*******************************************************************************
; RAM.ASM
; This file contains the interface functions for reading and writiing to/from
; 24 bit addresses
;*******************************************************************************

.include "zeropage.inc"

;*******************************************************************************
; CALL
; Executes the code at the given 24 bit address and returns to the caller.
; IN:
.export __ram_call
.proc __ram_call
.endproc

;*******************************************************************************
; STORE_BYTE
.export __ram_store_byte
.proc __ram_store_byte
.endproc

;*******************************************************************************
; LOAD_BYTE
.export __ram_load_byte
.proc __ram_load_byte
.endproc

;*******************************************************************************
; LOAD_BYTE_OFF
.export __ram_load_byte_off
.proc __ram_load_byte_off
.endproc

;*******************************************************************************
; MEMCPY
.export __ram_memcpy
.proc __ram_memcpy
.endproc

;*******************************************************************************
; COPY
.export __ram_copy
.proc __ram_copy
.endproc

;*******************************************************************************
; COPY_LINE
.export __ram_copy_line
.proc __ram_copy_line
.endproc

;*******************************************************************************
; STORE_REL
; Stores to the given 24 bit address + a given offset
.export __ram_bank_store_rel
.proc __ram_bank_store_rel
.ifdef VIC20
.endif
.endproc

;*******************************************************************************
; RUN100
; Relocates the given block of code to address $100 and executes it there.
; This is used to run code that is stored in one bank but which needs to
; interact (read/write) with another bank.
; Care must be taken to ensure the code will not overwrite the stack
; IN:
;  - r0: size of code to relocate (must be > 0)
;  - r1: address of code to relocate/execute
.export __ram_run100
.proc __ram_run100
@size=r0
@addr=r1
	php
	pha
	tya
	pha
	ldy @size
	dey
@l0:	lda (@addr),y
	sta $110,y
	dey
	bpl @l0
	pla
	tay
	pla
	plp
	jmp $110
.endproc
