.importzp __debug_src_line
.importzp __debug_file

.import __debug_addr2line
.import __debug_end_segment
.import __debug_filenames
.import __debug_getline
.import __debug_get_filename
.import __debuginfo_get_fileid
.import __debug_line2addr
.import __debug_init
.import __debug_init_segment
.import __debug_numbreakpoints
.import __debug_set_file
.import __debug_setline
.import __debug_setup
.import __debug_store_line
.import __debug_startsegment_byaddr

.scope dbgi
	addr2line         = __debug_addr2line
	endseg            = __debug_end_segment
	file              = __debug_file
	filenames         = __debug_filenames
	getline           = __debug_getline
	get_filename      = __debug_get_filename
	getfileid         = __debuginfo_get_fileid
	numbreakpoints    = __debug_numbreakpoints
	line2addr         = __debug_line2addr
	init              = __debug_init
	initseg           = __debug_init_segment
	setfile           = __debug_set_file
	setline           = __debug_setline
	setup             = __debug_setup
	srcline           = __debug_src_line
	startseg_addr     = __debug_startsegment_byaddr
	storeline         = __debug_store_line
.endscope
