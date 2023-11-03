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

.import __final_call
.import __final_store_byte
.import __final_load_byte
.import __final_load_byte_off
.import __final_memcpy
.import __final_copy
.import __final_copy_line
.import __final_init
.import __final_brk
.import __final_rti
.import __final_rti_bank
.import __final_bank_store_rel

;******************************************************************************
; BANK LAYOUT
; These definitions describe which banks store the code/data for their
; corresponding item.
FINAL_BANK_MAIN    = $a0|1	; where the editor/assembler/etc code is stored
FINAL_BANK_DEBUG   = $a0|2	; where debug symbols are stored
FINAL_BANK_USER    = $a0|3	; where the user's program is stored

FINAL_BANK_SOURCE0 = $a0|4	; source bank
FINAL_BANK_SOURCE1 = $a0|5	; source bank
FINAL_BANK_SOURCE2 = $a0|6	; source bank
FINAL_BANK_SOURCE3 = $a0|7	; source bank
FINAL_BANK_SOURCE4 = $a0|8	; source bank
FINAL_BANK_SOURCE5 = $a0|9	; source bank
FINAL_BANK_SOURCE6 = $a0|$a	; source bank
FINAL_BANK_SOURCE7 = $a0|$b	; source bank

; used for unrolled loops to make stuff fast
FINAL_BANK_FAST = $a0|$0c
			
FINAL_BANK_SYMBOLS = $a0|$d	; symbols bank (label names and their addresses)

; these are used to store the debuggger screen and the
; user program data ($1100-$2000) respectively
FINAL_BANK_FASTCOPY2 = $a0|$e
FINAL_BANK_FASTCOPY  = $a0|$f

; FASTTEXT lives at the top 4k of the FASTCOPY2 bank
FINAL_BANK_FASTTEXT  = $a0|$e

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

;******************************************************************************
.scope fe3
	call     = __final_call
	fcopy    = __final_copy
	copy     = __final_memcpy
	copyline = __final_copy_line
	init     = __final_init
	store    = __final_store_byte
	store_off= __final_bank_store_rel
	load     = __final_load_byte
	load_off = __final_load_byte_off
	break    = __final_brk
	bank_rti = __final_rti
	rti_bank = __final_rti_bank
.endscope

;******************************************************************************
; Macros
.macro bank_store_byte bank, addr, val
.ifnblank val
	lda val
.endif
	sta zp::bankval
	ldxy addr
	lda bank
	jsr __final_store_byte
.endmacro

.macro bank_store_byte_rel bank, addr, offset, val
.ifnblank val
	lda val
.endif
	sta zp::bankval
	lda offset
	sta zp::bankoffset
	ldxy addr
	lda bank
	jsr __final_bank_store_rel
.endmacro

.macro bank_read_byte bank,addr
	ldxy addr
	lda bank
	jsr __final_load_byte
.endmacro

.macro bank_read_byte_rel bank,addr,offset
	lda offset
	sta zp::bankval
	ldxy addr
	lda bank
	jsr __final_load_byte_off
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

; calls the banked routine
; params:
;  bank: the bank of the procedure to call
;  proc: the procedure to call
; 
.macro CALL bank, proc
	lda #bank
	ldxy proc
	jsr __final_call
.endmacro
