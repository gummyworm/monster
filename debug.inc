.import __debug_addr2line
.import __debug_init
.import __debug_set_file
.import __debug_start
.import __debug_store_line

.scope dbg
	addr2line = __debug_addr2line
	init = __debug_init
	setfile = __debug_set_file
	storeline = __debug_store_line
	start = __debug_start
.endscope
