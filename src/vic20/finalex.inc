.ifndef VIC20_FINALEX_INC
VIC20_FINALEX_INC=1

.import __ram_call
.import __ram_load_byte
.import __ram_load_byte_off
.import __ram_memcpy
.import __ram_copy_line
.import __ram_bank_store_rel
.import __ram_memcpy_bank
.import __ram_get_byte
.import __ram_store_byte

;******************************************************************************
; BANK LAYOUT
; These definitions describe which banks store the code/data for their
; corresponding item.

FINAL_BANK_ROM0	= $00		; ROM address $000000

;******************************************************************************
; This is the default bank while the editor has control of the CPU
; MAIN bank ($01)
FINAL_BANK_MAIN    = $a0|1	; where the editor/assembler/etc code is stored

;******************************************************************************
; USER bank ($02)
; The assembled user program is stored in this bank. When debugging the program
; this bank is activated.
FINAL_BANK_USER    = $a0|2	; where the user's program is stored

;******************************************************************************
; DEBUG bank ($03)
; This bank stores debug information (file ID's, line numbers and addresses
; as well as the code to access them
FINAL_BANK_DEBUG = $a0|3	; where debug symbols are stored

;******************************************************************************
; SOURCE banks ($04-$0B)
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
FINAL_BANK_MACROS  = $a0|4	; macro code & macro data ($a000-$c000)
FINAL_BANK_UDGEDIT = $a0|5	; UDG editor ($a000-$c000)
FINAL_BANK_LINKER  = $a0|$e	; linker ($2000-$8000)
FINAL_BANK_MONITOR = $a0|7	; monitor ($a000-$c000)
FINAL_BANK_BUFF    = $a0|8	; copy buffer ($a000-$c000)
FINAL_BANK_SYMVIEW = $a0|9	; symbol viewer ($a000-$c000)

;******************************************************************************
; FAST bank ($0C)
; Used for the unrolled bitmap clear routine and backups for bitmap columns
;  $2000-$727f: screen shift routines and backup buffers (screen.asm)
;
;  $727f-$8000: 2nd part of bitmap clear routine
;  $a000-$c000: 1st part of bitmap clear routine
FINAL_BANK_VSCREEN = $a0|$0c
FINAL_BANK_FAST    = $a0|$0c

;******************************************************************************
; SYMBOLS bank ($0D)
; The SYMBOLS bank stores the label names for the progam's labels in $2000-$8000
; $a000-$b000 is used to store the addresses that these names correspond to.
; $b000-$b800 is used for anonymous labels (up to 1024)
; $b800-$c000 stores the linker code (code to create, remove, find, etc. labels)
FINAL_BANK_SYMBOLS = $a0|$d	; symbols bank (label names and their addresses)

;******************************************************************************
; FASTCOPY ($0F)
; This holds routines/data for saving machine state as well
; as the virtual memory for the user program that resides within
; shared locations (e.g. $1000-$2000)
FINAL_BANK_FASTCOPY  = $a0|$f

; FASTTEXT lives at the top 4k ($b000-$c000) of the FASTCOPY2 bank
FINAL_BANK_FASTTEXT  = $a0|$e

;******************************************************************************
.scope fe3
	call       = __ram_call
	copy       = __ram_memcpy
	copyline   = __ram_copy_line
	store      = __ram_store_byte
	store_off  = __ram_bank_store_rel
	load       = __ram_load_byte
	load_off   = __ram_load_byte_off
	get_byte   = __ram_get_byte
.endscope

;******************************************************************************
; CALL
; calls the banked routine
; params:
;  bank: the bank of the procedure to call
;  proc: the procedure to call
.macro CALL bank, proc
	pha
	lda #<(proc)
	sta zp::bankjmpvec
	lda #>(proc)
	sta zp::bankjmpvec+1

	lda #bank
	sta zp::banktmp
	pla
	jsr __ram_call
.endmacro

;******************************************************************************
; JUMP
; jumps to the banked routine
; params:
;  bank: the bank of the procedure to jump to
;  proc: the procedure to jump to
.macro JUMP bank, proc
	pha
	lda #<(proc)
	sta zp::bankjmpvec
	lda #>(proc)
	sta zp::bankjmpvec+1

	lda #bank
	sta zp::banktmp
	pla
	jmp __ram_call
.endmacro

.endif
