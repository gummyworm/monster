.import __edit_handle_key
.import __edit_init
.import __edit_run
.import __edit_gotoline
.import __edit_load

.scope edit
	handlekey = __edit_handle_key
	init = __edit_init
	load = __edit_load
	run  = __edit_run
	gotoline = __edit_gotoline
.endscope

;******************************************************************************
; CONSTANTS
MODE_COMMAND = 1
MODE_INSERT  = 2
