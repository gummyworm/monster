.importzp __debug_src_line
.importzp __debug_file

.import __debug_addr2line
.import __debug_end_block
.import __debug_get_filename
.import __debug_line2addr
.import __debuginfo_init
.import __debuginfo_initonce
.import __debug_new_block
.import __debug_set_file
.import __debug_store_line
.import __debug_push_block
.import __debug_pop_block

.import __debuginfo_get_fileid

.scope dbgi
	addr2line    = __debug_addr2line
	endblock     = __debug_end_block
	file         = __debug_file
	get_filename = __debug_get_filename
	getfileid    = __debuginfo_get_fileid
	line2addr    = __debug_line2addr
	init         = __debuginfo_init
	initonce     = __debuginfo_initonce
	newblock     = __debug_new_block
	setfile      = __debug_set_file
	srcline      = __debug_src_line
	storeline    = __debug_store_line
	pop_block    = __debug_pop_block
	push_block   = __debug_push_block
.endscope
