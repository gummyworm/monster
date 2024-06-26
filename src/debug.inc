.import __debug_breakpointslo
.import __debug_breakpointshi
.import __debug_breakpoint_flags
.import __debug_gotoaddr
.import __debug_load_file
.import __debug_init
.import __debug_numbreakpoints
.import __debug_numwatches
.import __debug_restore_progstate
.import __debug_remove_breakpoint
.import __debug_setbreakpoint
.import __debug_toggle_breakpoint
.import __debug_save_prog_state
.import __debug_start
.import __debug_watcheslo
.import __debug_watcheshi
.import __debug_watches_stoplo
.import __debug_watches_stophi
.import __debug_watch_prevs
.import __debug_watch_flags
.import __debug_watch_vals

.scope dbg
	breakpointslo     = __debug_breakpointslo
	breakpointshi     = __debug_breakpointshi
	breakpoint_flags  = __debug_breakpoint_flags
	gotoaddr          = __debug_gotoaddr
	loadfile          = __debug_load_file
	init              = __debug_init
	numbreakpoints    = __debug_numbreakpoints
	numwatches        = __debug_numwatches
	removebreakpoint  = __debug_remove_breakpoint
	save_progstate    = __debug_save_prog_state
	start             = __debug_start
	toggle_breakpoint = __debug_toggle_breakpoint
	watcheslo         = __debug_watcheslo
	watcheshi         = __debug_watcheshi
	watches_stoplo	  = __debug_watches_stoplo
	watches_stophi	  = __debug_watches_stophi
	watch_flags       = __debug_watch_flags
	watch_prevs       = __debug_watch_prevs
	watch_vals        = __debug_watch_vals
.endscope
