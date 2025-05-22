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
; Loads the value from the given 24 bit address and returns it in .A
.macro lda24 bank, addr
	; set REU (load) address
	lda bank
	sta $df06	; MSB of REU address
	lda addr+1
	sta $df05	; middle byte of REU address
	lda addr
	sta $df04	; LSB of REU address

	ldx #$00

	; set c64 (target) address
	lda #zp::banktmp
	sta $df02
	stx $df03

	; set transfer size to 1 byte
	stx $df07
	inx
	stx $df08

	; execute the REU command
	lda #$91	; load 1 byte from REU -> C64
	sta $d0f1

	; read the loaded data
	lda zp::banktmp
.endmacro


;******************************************************************************
; STA24
; Stores the value in .A to the given 24 bit address
; CLOBBERS:
;  zp::banktmp
.macro sta24 bank, addr
	sta zp::banktmp

	; set REU (load) address
	lda bank
	sta $df06	; MSB of REU address
	lda addr+1
	sta $df05	; middle byte of REU address
	lda addr
	sta $df04	; LSB of REU address

	ldx #$00

	; set c64 (target) address
	lda #r0
	sta $df02
	stx $df03

	; set transfer size to 1 byte
	stx $df07
	inx
	stx $df08

	; execute the REU command
	lda #$91	; load 1 byte from REU -> C64
	sta $d0f1

	; read the loaded data
	lda zp::banktmp
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
