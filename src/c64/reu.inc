.import __reu_dbg
.import __reu_move
.import __reu_compare
.import __reu_c64_addr
.import __reu_find
.import __reu_init
.import __reu_load
.import __reu_reu_addr
.import __reu_swap
.import __reu_store
.import __reu_txlen
.import __reu_tabsetup
.import __reu_tabfind

; address in the REU to save/restore C64 data to
REU_TMP_ADDR            = $ff0000
REU_VMEM_ADDR           = $fe0000
REU_SYMTABLE_ADDRS_ADDR = $fd0000
REU_SYMTABLE_NAMES_ADDR = $fc0000
REU_SYMTABLE_ANONS_ADDR = $fb0000

.scope reu
	c64addr  = __reu_c64_addr
	compare  = __reu_compare
	dbg      = __reu_dbg
	find     = __reu_find
	load     = __reu_load
	init     = __reu_init
	reuaddr  = __reu_reu_addr
	store    = __reu_store
	swap     = __reu_swap
	tabsetup = __reu_tabsetup
	tabfind  = __reu_tabfind
	txlen    = __reu_txlen
.endscope
