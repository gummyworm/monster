.include "zeropage.inc"

;############################################################################
; Final Expansion overview
;  The final expansion enables a full 32k of RAM to be usable for a source file
;  With the final expansion, the 16 available 32k blocks are divided as follows:
;    block 0: Monster (editor/assembler)
;    block 1: assembled program
;    block 2-15: source files (1 per block)
;  Monster installs an NMI prior to executing the user's program that allows 
;  them to return to the editor (and it's bank) via the RESTORE key
;############################################################################

.import __final_bank
.import __final_store_byte
.import __final_load_byte
.import __final_memcpy
.import __final_init

; USE_FINAL = 1	; enable final expansion

;******************************************************************************
; BANK LAYOUT
; These definitions describe which banks store the code/data for their
; corresponding item.
FINAL_BANK_MAIN = 1	; where the user program is stored
FINAL_BANK_DEBUG = 2	; where debug symbols are stored

;******************************************************************************
; MEMORY BANKS
.scope bank
	monster = 0
	program = 1
	buff0   = 2
	buff1   = 3
	buff2   = 4
	buff3   = 5
	buff4   = 6
	buff5   = 7
	buff6   = 8
	buff7   = 9
	buff8   = 10
	buff9   = 11
	buffa   = 12
	buffb   = 13
	buffc   = 14
	buffd   = 15
.endscope

;--------------------------------------
; Routines
.scope fe3
	bank = __final_bank
	copy = __final_memcpy
	init = __final_init
	store = __final_store_byte
	load = __final_load_byte
.endscope

;--------------------------------------
; Macros
.macro bank_store_byte bank,val,addr
	ldxy addr
	lda val
	sta zp::tmp0
	lda bank
	jsr __final_store_byte
.endmacro

.macro bank_read_byte bank,addr
	ldxy addr
	lda bank
	jsr __final_get_byte
.endmacro

; writes the byte in .A to addr in zp::bank
.macro BANK_WRITE addr
	pha
	lda bank
	ora #%10100000
	sta $9c02
	sta 

	pla
	.if .match( .left( 1, {val} ), # )
		lda #val
	.endif
.endmacro

; pushes the current RAM bank 
.macro PUSH_BANK
	sta zp::final
	lda $9c02
	pha
	lda zp::final
.endmacro

; pops/restores the last RAM bank 
; NOTE: switches banks; make sure code using this macro exists in the bank
; it switches to
.macro POP_BANK
	sta zp::final
	pla
	sta $9c02
	lda zp::final
.endmacro
