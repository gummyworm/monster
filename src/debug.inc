.import __debug_breakpointslo
.import __debug_breakpointshi
.import __debug_breakpoint_lineslo
.import __debug_breakpoint_lineshi
.import __debug_breakpoint_fileids
.import __debug_breakpoint_flags
.import __debug_brksetaddr
.import __debug_done
.import __debug_setbrkatline
.import __debug_edit_watches
.import __debug_gotoaddr
.import __debug_load_file
.import __debug_init
.import __debug_interface
.import __debug_numbreakpoints
.import __debug_numwatches
.import __debug_regs_contents
.import __debug_remove_breakpoint
.import __debug_removebreakpointbyid
.import __debug_setbreakpoint
.import __debug_save_prog_state
.import __debug_shift_breakpointsd
.import __debug_shift_breakpointsu
.import __debug_start
.import __debug_step
.import __debug_swap_user_mem
.import __debug_watcheslo
.import __debug_watcheshi
.import __debug_watches_stoplo
.import __debug_watches_stophi
.import __debug_watch_prevs
.import __debug_watch_flags
.import __debug_watch_vals

.scope dbg
	breakpointslo      = __debug_breakpointslo
	breakpointshi      = __debug_breakpointshi
	breakpoint_lineslo = __debug_breakpoint_lineslo
	breakpoint_lineshi = __debug_breakpoint_lineshi
	breakpoint_fileids = __debug_breakpoint_fileids
	breakpoint_flags   = __debug_breakpoint_flags
	brksetaddr         = __debug_brksetaddr
	done               = __debug_done
	setbrkatline       = __debug_setbrkatline
	edit_watches       = __debug_edit_watches
	gotoaddr           = __debug_gotoaddr
	loadfile           = __debug_load_file
	init               = __debug_init
	interface          = __debug_interface
	numbreakpoints     = __debug_numbreakpoints
	numwatches         = __debug_numwatches
	regs_contents      = __debug_regs_contents
	removebreakpoint   = __debug_remove_breakpoint
	removebreakpointbyid = __debug_removebreakpointbyid
	save_progstate     = __debug_save_prog_state
	shift_breakpointsd = __debug_shift_breakpointsd
	shift_breakpointsu = __debug_shift_breakpointsu
	start              = __debug_start
	step               = __debug_step
	swapusermem        = __debug_swap_user_mem
	watcheslo          = __debug_watcheslo
	watcheshi          = __debug_watcheshi
	watches_stoplo	   = __debug_watches_stoplo
	watches_stophi	   = __debug_watches_stophi
	watch_flags        = __debug_watch_flags
	watch_prevs        = __debug_watch_prevs
	watch_vals         = __debug_watch_vals
.endscope
