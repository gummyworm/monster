.ifndef RAM_INC
RAM_INC = 1

.include "zeropage.inc"

.ifdef vic20
.include "vic20/ram.inc"
.endif

.ifdef c64
.include "c64/ram.inc"
.endif

.import __ram_call
.import __ram_copy
.import __ram_memcpy
.import __ram_copy_banked
.import __ram_copy_line
.import __ram_store_byte
.import __ram_bank_store_rel
.import __ram_get_byte
.import __ram_load_byte
.import __ram_load_byte_off

.scope ram
	call      = __ram_call
	copy      = __ram_memcpy
	copybanked= __ram_copy_banked
	copyline  = __ram_copy_line
	store     = __ram_store_byte
	store_off = __ram_bank_store_rel
	get_byte  = __ram_get_byte
	load      = __ram_load_byte
	load_off  = __ram_load_byte_off

	load_byte_off  = __ram_load_byte_off
.endscope

.endif
