.include "zeropage.inc"

.import __final_bank
.import __final_ro
.import __final_store_byte

.scope final
	block = __final_bank
	ro = __final_ro
.endscope

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
