.importzp __debug_file
.importzp __debug_src_line

.import __debug_addr2line
.import __debug_breakpoints
.import __debug_breakpoint_flags
.import __debug_end_segment
.import __debug_getline
.import __debug_get_filename
.import __debug_init
.import __debug_init_segment
.import __debug_numbreakpoints
.import __debug_numwatches
.import __debug_restore_progstate
.import __debug_remove_breakpoint
.import __debug_setbreakpoint
.import __debug_toggle_breakpoint
.import __debug_save_prog_state
.import __debug_set_file
.import __debug_setup
.import __debug_setline
.import __debug_start
.import __debug_store_line
.import __debug_startsegment_byaddr
.import __debug_watches
.import __debug_watches_stop
.import __debug_watch_prevs
.import __debug_watch_flags
.import __debug_watch_vals

.scope dbg
	addr2line         = __debug_addr2line
	breakpoints       = __debug_breakpoints
	breakpoint_flags  = __debug_breakpoint_flags
	endseg            = __debug_end_segment
	file              = __debug_file
	getline           = __debug_getline
	get_filename      = __debug_get_filename
	init              = __debug_init
	initseg           = __debug_init_segment
	numbreakpoints    = __debug_numbreakpoints
	numwatches        = __debug_numwatches
	removebreakpoint  = __debug_remove_breakpoint
	save_progstate    = __debug_save_prog_state
	setfile           = __debug_set_file
	setline           = __debug_setline
	setup             = __debug_setup
	srcline           = __debug_src_line
	storeline         = __debug_store_line
	start             = __debug_start
	startseg_addr     = __debug_startsegment_byaddr
	toggle_breakpoint = __debug_toggle_breakpoint
	watches           = __debug_watches
	watches_stop	  = __debug_watches_stop
	watch_flags       = __debug_watch_flags
	watch_prevs       = __debug_watch_prevs
	watch_vals        = __debug_watch_vals
.endscope
