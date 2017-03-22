.import __mem_init
.import __mem_spare
.import __linebuffer
.import __statusline
.import __mem_filebuffer1
.import __mem_filebuffer2

.scope mem
	init 		= __mem_init
        spare           = __mem_spare
	linebuffer      = __linebuffer
	statusline	= __statusline
	filebuffer1 	= __mem_filebuffer1
	filebuffer2	= __mem_filebuffer2
.endscope
