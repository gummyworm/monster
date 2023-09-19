.import __edit_gotoline
.import __edit_handle_key
.import __edit_init
.import __edit_load
.import __edit_refresh
.import __edit_run

.scope edit
	gotoline  = __edit_gotoline
	handlekey = __edit_handle_key
	init      = __edit_init
	load      = __edit_load
	refresh   =__edit_refresh
	run       = __edit_run
.endscope

;******************************************************************************
; CONSTANTS
MODE_COMMAND = 1
MODE_INSERT  = 2
