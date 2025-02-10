.ifndef C64_RAM_INC
C64_RAM_INC = 1

.import __ram_copyline

FINAL_BANK_ROM0	   = $00
FINAL_BANK_MAIN    = $00
FINAL_BANK_USER    = $00
FINAL_BANK_DEBUG   = $00
FINAL_BANK_SOURCE0 = $00
FINAL_BANK_SOURCE1 = $00
FINAL_BANK_SOURCE2 = $00
FINAL_BANK_SOURCE3 = $00
FINAL_BANK_SOURCE4 = $00
FINAL_BANK_SOURCE5 = $00
FINAL_BANK_SOURCE6 = $00
FINAL_BANK_SOURCE7 = $00
FINAL_BANK_MACROS  = $00
FINAL_BANK_UDGEDIT = $00
FINAL_BANK_LINKER  = $00
FINAL_BANK_CONSOLE = $00
FINAL_BANK_BUFF    = $00
FINAL_BANK_SYMBOLS = $00
FINAL_BANK_SYMVIEW = $00

.macro CALL bank, proc
	jsr proc
.endmacro

.macro JUMP bank, proc
	jmp proc
.endmacro

;******************************************************************************
; LDA24
; Stores the value in .A to the given 24 bit address
.macro lda24 bank, addr
.ifdef vic20
	sta zp::bankval
	ldxy addr
	lda bank
	jsr __final_store_byte
.endif
.endmacro


;******************************************************************************
; STA24
; Stores the value in .A to the given 24 bit address
.macro sta24 bank, addr
.ifdef vic20
	sta zp::bankval
	ldxy addr
	lda bank
	jsr __final_store_byte
.endif
.endmacro

;******************************************************************************
; MOV
; Moves the value from the given 24-bit source to the given 24-bit destination
.macro mov dst24, src24
	.if (.match (.left (1, {src24}), #))
		; immediate mode, just load .A with the immediate value
		lda src24
	.else
		lda24 src
	.endif
	sta24 dst24
.endmacro

.endif
