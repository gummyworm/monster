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

USE_FINAL = 1	; enable final expansion

;--------------------------------------
; memory banks
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
.scope final
	block = __final_bank
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

.macro bank_read_word bank, addr
	ldxy addr
	lda bank
	jsr __final_get_byte
	pha
	ldxy addr+1
	jsr __final_get_byte
	tay
	pla
	tax
.endmacro

.macro bank_read_word_yx bank, addr
	lda bank
	jsr __final_get_byte
	pha
	ldxy addr+1
	jsr __final_get_byte
	tay
	pla
	tax
.endmacro
