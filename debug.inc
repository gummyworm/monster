.import __debug_addr2line
.import __debug_end_segment
.import __debug_init
.import __debug_init_segment
.import __debug_file
.import __debug_set_file
.import __debug_setup
.import __debug_start
.import __debug_store_line
.import __debug_startsegment_byaddr

.scope dbg
	addr2line = __debug_addr2line
	endseg = __debug_end_segment
	file = __debug_file
	init = __debug_init
	initseg = __debug_init_segment
	setfile = __debug_set_file
	setup = __debug_setup
	storeline = __debug_store_line
	start = __debug_start
	startseg_addr = __debug_startsegment_byaddr
.endscope
