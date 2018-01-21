.import __mem_init
.import __mem_spare
.import __linebuffer
.import __statusline

.scope mem
	init 		= __mem_init
        spare           = __mem_spare
	linebuffer      = __linebuffer
	statusline	= __statusline
.endscope
