.import __breakpoint_edit
.import __brkpt_getbyline
.import __brkpt_num

BREAKPOINT_ENABLED = 1

.scope brkpt
	edit      = __breakpoint_edit
	getbyline = __brkpt_getbyline
	num       = __brkpt_num
.endscope
