.import __str_assemble_prompt
.import __str_assembling
.import __str_breakpoints_title
.import __str_breakpoints_line
.import __str_debug_brk_line
.import __str_debug_brk_addr
.import __str_debug_registers
.import __str_debug_stop_debugging
.import __str_buffers
.import __str_deleting
.import __str_dir
.import __str_edit_file_delete_failed
.import __str_edit_file_load_failed
.import __str_edit_file_save_failed
.import __str_edit_line_err
.import __str_endrep
.import __str_errors
.import __str_file_open_failed
.import __str_invalid_command
.import __str_jam_detected
.import __str_load
.import __str_loading
.import __str_noname
.import __str_no_file
.import __str_null
.import __str_memory
.import __str_memview_title
.import __str_question_marks
.import __str_run
.import __str_saveall
.import __str_saving
.import __str_segments
.import __str_symview_title
.import __str_tracing
.import __str_watch_added
.import __str_watches_changed_line
.import __str_watches_line
.import __str_watches_line_end
.import __str_watches_range_line
.import __str_watches_title
.import __str_watch_triggered

.scope strings
	assemble_prompt         = __str_assemble_prompt
	assembling              = __str_assembling
	buffers                 = __str_buffers
	breakpoints_title       = __str_breakpoints_title
	breakpoints_line        = __str_breakpoints_line
	question_marks          = __str_question_marks
	debug_brk_line          = __str_debug_brk_line
	debug_brk_addr          = __str_debug_brk_addr
	debug_registers         = __str_debug_registers
	debug_stop_debugging    = __str_debug_stop_debugging
	deleting                = __str_deleting
	dir                     = __str_dir
	edit_file_load_failed   = __str_edit_file_load_failed
	edit_file_delete_failed = __str_edit_file_delete_failed
	edit_file_save_failed   = __str_edit_file_save_failed
	edit_line_err           = __str_edit_line_err
	endrep                  = __str_endrep
	errors                  = __str_errors
	invalid_command         = __str_invalid_command
	file_open_failed        = __str_file_open_failed
	loading                 = __str_loading
	memview_title           = __str_memview_title
	nofile                  = __str_no_file
	noname                  = __str_noname
	null                    = __str_null
	saving                  = __str_saving
	saveall			= __str_saveall
	symview_title           = __str_symview_title
	tracing                 = __str_tracing
	jam_detected            = __str_jam_detected
	watch_added             = __str_watch_added
	watches_line            = __str_watches_line
	watches_changed_line    = __str_watches_changed_line
	watches_range_line      = __str_watches_range_line
	watches_title           = __str_watches_title
	watch_triggered		= __str_watch_triggered
.endscope
