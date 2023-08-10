.import __mem_asmctx
.import __mem_init
.import __mem_spare
.import __mem_backbuff
.import __linebuffer
.import __linebuffer2
.import __mem_program
.import __statusline

.scope mem
	init 		= __mem_init
        spare           = __mem_spare
	backbuff        = __mem_backbuff
	linebuffer      = __linebuffer
	linebuffer2     = __linebuffer2
	program		= __mem_program
	statusline	= __statusline
	asmctx 		= __mem_asmctx
.endscope
