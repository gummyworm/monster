.import __reu_dbg
.import __reu_move
.import __reu_compare
.import __reu_c64_addr
.import __reu_find
.import __reu_init
.import __reu_load
.import __reu_move
.import __reu_reu_addr
.import __reu_swap
.import __reu_store
.import __reu_txlen
.import __reu_tabsetup
.import __reu_tabfind
.import __reu_tabfind_sorted
.import __reu_zero

.importzp __reu_move_src
.importzp __reu_move_dst
.importzp __reu_move_size

; address in the REU to save/restore C64 data to
REU_TMP_ADDR            = $ff0000
REU_VMEM_ADDR           = $fe0000
REU_SYMTABLE_ADDRS_ADDR = $fd0000	; label addresses
REU_SYMTABLE_NAMES_ADDR = $fc0000	; label names
REU_SYMTABLE_ANONS_ADDR = $fb0000	; anonymous label addresses
REU_DBGINFO_PROG        = $fa0000
REU_DBGINFO_HEADERS     = $f90000
REU_COPYBUFF_ADDR       = $f80000

.scope reu
	c64addr        = __reu_c64_addr
	compare        = __reu_compare
	dbg            = __reu_dbg
	find           = __reu_find
	load           = __reu_load
	init           = __reu_init
	move           = __reu_move
	reuaddr        = __reu_reu_addr
	store          = __reu_store
	swap           = __reu_swap
	tabsetup       = __reu_tabsetup
	tabfind        = __reu_tabfind
	tabfind_sorted = __reu_tabfind_sorted
	txlen          = __reu_txlen
	move_src       = __reu_move_src
	move_dst       = __reu_move_dst
	move_size      = __reu_move_size
	zero           = __reu_zero
.endscope

;*******************************************************************************
; STORE16
; Copies the data at the given source address to the destination
; one in the REU.
; The most-significant byte of the REU address is given as the "bank"
.macro STORE16 bank, dst16, src16, size16
	.if .match( .left( 1, {bank} ), # )
		lda #^(.right( .tcount( {bank} )-1, {bank} ))
		sta __reu_reu_addr+2
	.else
		lda bank
		sta __reu_reu_addr+2
	.endif

	.if .match( .left( 1, {dst16} ), # )
		lda #<(.right( .tcount( {dst16} )-1, {dst16} ))
		sta __reu_reu_addr
		lda #>(.right( .tcount( {dst16} )-1, {dst16} ))
		sta __reu_reu_addr+1
	.else
		lda dst16
		sta __reu_reu_addr
		lda dst16+1
		sta __reu_reu_addr+1
	.endif

	.if .match( .left( 1, {src16} ), # )
		lda #<(.right( .tcount( {src16} )-1, {src16} ))
		sta __reu_c64_addr
		lda #>(.right( .tcount( {src16} )-1, {src16} ))
		sta __reu_c64_addr+1
	.else
		lda src16
		sta __reu_c64_addr
		lda src16+1
		sta __reu_c64_addr+1
	.endif

	.if .match( .left( 1, {txlen} ), # )
		lda #<(.right( .tcount( {size16} )-1, {size16} ))
		sta __reu_txlen
		lda #>(.right( .tcount( {size16} )-1, {size16} ))
		sta __reu_txlen+1
	.else
		lda size16
		sta __reu_txlen
		lda size16+1
		sta __reu_txlen+1
	.endif
.endmacro

;*******************************************************************************
; STORE
; Copies the data at the given source address to the destination one.
.macro STORE dst24, src16, size16
	.if .match( .left( 1, {dst24} ), # )
		COPY16 #^(.right( .tcount( {dst24} )-1, {dst24} )), #(.right( .tcount( {dst24} )-1, {dst24} ) & $ffff), src16, size16
	.else
		COPY16 ^dst24, dst24 & $ffff, src16, size16
	.endif
.endmacro

;*******************************************************************************
; LOAD16
; Copies the data from the source REU address to the C64 destination one
; The most-significant byte of the REU address is given as the "bank"
.macro LOAD16 bank, dst16, src16, size16
	.if .match( .left( 1, {bank} ), # )
		lda #^(.right( .tcount( {bank} )-1, {bank} ))
		sta __reu_reu_addr+2
	.else
		lda bank
		sta __reu_reu_addr+2
	.endif

	.if .match( .left( 1, {dst16} ), # )
		lda #<(.right( .tcount( {dst16} )-1, {dst16} ))
		sta __reu_c64_addr
		lda #>(.right( .tcount( {dst16} )-1, {dst16} ))
		sta __reu_c64_addr+1
	.else
		lda dst16
		sta __reu_c64_addr
		lda dst16+1
		sta __reu_c64_addr+1
	.endif

	.if .match( .left( 1, {src16} ), # )
		lda #<(.right( .tcount( {src16} )-1, {src16} ))
		sta __reu_reu_addr
		lda #>(.right( .tcount( {src16} )-1, {src16} ))
		sta __reu_reu_addr+1
	.else
		lda src16
		sta __reu_reu_addr
		lda src16+1
		sta __reu_reu_addr+1
	.endif

	.if .match( .left( 1, {txlen} ), # )
		lda #<(.right( .tcount( {size16} )-1, {size16} ))
		sta __reu_txlen
		lda #>(.right( .tcount( {size16} )-1, {size16} ))
		sta __reu_txlen+1
	.else
		lda size16
		sta __reu_txlen
		lda size16+1
		sta __reu_txlen+1
	.endif
.endmacro

;*******************************************************************************
; LOAD
; Loads size16 bytes from the REU to the C64
.macro LOAD dst24, src16, size16
	.if .match( .left( 1, {dst24} ), # )
		COPY16 #^(.right( .tcount( {dst24} )-1, {dst24} )), #(.right( .tcount( {dst24} )-1, {dst24} ) & $ffff), src16, size16
	.else
		COPY16 ^dst24, dst24 & $ffff, src16, size16
	.endif
.endmacro
