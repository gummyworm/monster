.importzp __debug_src_line
.importzp __debug_file

.import __debug_addr2line
.import __debug_end_block
.import __debug_filenames
.import __debug_get_filename
.import __debuginfo_get_fileid
.import __debug_line2addr
.import __debug_init
.import __debug_new_block
.import __debug_set_file
.import __debug_setline
.import __debug_store_line
.import __debug_set_addr

.scope dbgi
	addr2line         = __debug_addr2line
	endblock          = __debug_end_block
	file              = __debug_file
	filenames         = __debug_filenames
	get_filename      = __debug_get_filename
	getfileid         = __debuginfo_get_fileid
	line2addr         = __debug_line2addr
	init              = __debug_init
	newblock          = __debug_new_block
	setfile           = __debug_set_file
	setline           = __debug_setline
	srcline           = __debug_src_line
	setaddr           = __debug_set_addr
	storeline         = __debug_store_line
.endscope
