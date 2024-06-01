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

;******************************************************************************
; This is the default bank while the editor has control of the CPU
; MAIN bank ($01) 
FINAL_BANK_MAIN    = $a0|1	; where the editor/assembler/etc code is stored

;******************************************************************************
; DEBUG bank ($02)
; This bank stores debug information (file ID's, line numbers and addresses
; for the user program (which is stored in the USER bank). This is stored in
; the address range $2000-$8000. 
;
; $a000-$c000 is free
FINAL_BANK_DEBUG   = $a0|2	; where debug symbols are stored

;******************************************************************************
; USER bank ($03)
; The assembled user program is stored in this bank. When debugging the program
; this bank is activated.
FINAL_BANK_USER    = $a0|3	; where the user's program is stored

;******************************************************************************
; SOURCE banks ($04-$05)
; There are 8 source banks. Each stores the text for one buffer of source.
; The text is stored in $2000-$8000.
; $a000-$c000 is free
FINAL_BANK_SOURCE0 = $a0|4	; source bank
FINAL_BANK_SOURCE1 = $a0|5	; source bank
FINAL_BANK_SOURCE2 = $a0|6	; source bank
FINAL_BANK_SOURCE3 = $a0|7	; source bank
FINAL_BANK_SOURCE4 = $a0|8	; source bank
FINAL_BANK_SOURCE5 = $a0|9	; source bank
FINAL_BANK_SOURCE6 = $a0|$a	; source bank
FINAL_BANK_SOURCE7 = $a0|$b	; source bank

; The following banks use the upper part of the 
; source banks ($a000-$c000)
; $a000-$c000 
FINAL_BANK_MACROS       = $a0|4	; macro code & macro data ($a000-$c000)
FINAL_BANK_UDGEDIT = $a0|5 ; UDG editor ($a000-$c000)
FINAL_BANK_LINKER  = $a0|6	; linker ($a000-$c000)

;******************************************************************************
; FAST bank ($0C)
; Used for the unrolled bitmap clear routine and backups for bitmap columns
;  $2100-$727f: bitmap save/restore column routines and backup buffers
;  $727f-$8000: 2nd part of bitmap clear routine
;  $a000-$c000: 1st part of bitmap clear routine 
FINAL_BANK_SAVESCR = $a0|$0c
FINAL_BANK_FAST    = $a0|$0c
			
;******************************************************************************
; SYMBOLS bank ($0D)
; The SYMBOLS bank stores the label names for the program's labels in $2000-$8000
; $a000-$b000 is used to store the addresses that these names correspond to.
; $b000-$b800 is FREE
; $b800-$c000 stores the linker code (code to create, remove, find, etc. labels)
FINAL_BANK_SYMBOLS = $a0|$d	; symbols bank (label names and their addresses)

;******************************************************************************
; FASTCOPY blocks ($0E-$0F)
; In these blocks, $2000-$8000 is used to store undrolled loops that copy data
; to the upper RAM ($a000-$c000).  The hi RAM ($a000-$b000) stores the data 
; for the debugger (FASTCOPY2) and user program (in FASTCOPY).
FINAL_BANK_FASTCOPY2 = $a0|$e
FINAL_BANK_FASTCOPY  = $a0|$f

; FASTTEXT lives at the top 4k ($b000-$c000) of the FASTCOPY2 bank
FINAL_BANK_FASTTEXT  = $a0|$e

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
.macro CALL bank, proc
	pha
	.if .match( .left( 1, {proc} ), # )
		lda #<(.right( .tcount( {proc} )-1, {proc} ))
		sta zp::bankjmpvec
		lda #>(.right( .tcount( {proc} )-1, {proc} ))
		sta zp::bankjmpvec+1
	.else
		lda proc
		sta zp::bankjmpvec
		lda proc+1
		sta zp::bankjmpvec+1
	.endif

	lda #bank
	sta zp::banktmp
	pla
	jsr __final_call
.endmacro

; jumps to the banked routine
; params:
;  bank: the bank of the procedure to jump to
;  proc: the procedure to jump to
.macro JUMP bank, proc
	pha
	.if .match( .left( 1, {proc} ), # )
		lda #<(.right( .tcount( {proc} )-1, {proc} ))
		sta zp::bankjmpvec
		lda #>(.right( .tcount( {proc} )-1, {proc} ))
		sta zp::bankjmpvec+1
	.else
		lda proc
		sta zp::bankjmpvec
		lda proc+1
		sta zp::bankjmpvec+1
	.endif

	lda #bank
	sta zp::banktmp
	pla
	jmp __final_call
.endmacro
