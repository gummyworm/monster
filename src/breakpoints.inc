.import __breakpoint_edit
.import __brkpt_getbyline

BREAKPOINT_ENABLED = 1

.scope brkpt
	edit      = __breakpoint_edit
	getbyline = __brkpt_getbyline
.endscope
