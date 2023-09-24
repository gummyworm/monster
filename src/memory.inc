.import __mem_asmctx
.import __mem_ctxbuffer
.import __mem_spare
.import __mem_spareend
.import __mem_backbuff
.import __linebuffer
.import __linebuffer2
.import __mem_progsave
.import __statusline
.import __mem_debugsave
.import __mem_prog00
.import __mem_prog1000
.import __mem_prog9000
.import __mem_prog9400

.scope mem
	ctxbuffer       = __mem_ctxbuffer
        spare           = __mem_spare
	spareend	= __mem_spareend
	backbuff        = __mem_backbuff
	linebuffer      = __linebuffer
	linebuffer2     = __linebuffer2
	statusline	= __statusline

	progsave	= __mem_progsave
	prog00		= __mem_prog00
	prog1000	= __mem_prog1000
	prog9000	= __mem_prog9000
	prog9400	= __mem_prog9400

	debugsave	= __mem_debugsave
.endscope
