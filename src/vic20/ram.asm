;*******************************************************************************
; RAM.ASM
; This file contains the interface functions for reading and writiing to/from
; 24 bit addresses
;*******************************************************************************

.include "../zeropage.inc"

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
