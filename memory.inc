.import __mem_init
.import __mem_spare
.import __linebuffer
.import __linebuffer2
.import __statusline

.scope mem
	init 		= __mem_init
        spare           = __mem_spare
	linebuffer      = __linebuffer
	linebuffer2     = __linebuffer2
	statusline	= __statusline
.endscope
