.import __linesave
.import __mem_asmctx
.import __mem_copybuff
.import __mem_ctxbuffer
.import __mem_spare
.import __mem_spareend
.import __mem_backbuff
.import __mem_drive_err
.import __linebuffer
.import __linebuffer2
.import __mem_asmbuffer
.import __mem_progsave
.import __statusline
.import __mem_debugsave
.import __mem_dbg00
.import __mem_dbg9000
.import __mem_dbg9400
.import __mem_prog00
.import __mem_prog1000
.import __mem_prog9000
.import __mem_prog9400

.scope mem
	asmbuffer       = __mem_asmbuffer
	ctxbuffer       = __mem_ctxbuffer
        spare           = __mem_spare
	spareend	= __mem_spareend
	backbuff        = __mem_backbuff
	linebuffer      = __linebuffer
	linebuffer2     = __linebuffer2
	linesave        = __linesave
	statusline	= __statusline
	copybuff        = __mem_copybuff
	drive_err	= __mem_drive_err

	progsave	= __mem_progsave
	prog00		= __mem_prog00
	prog1000	= __mem_prog1000
	prog9000	= __mem_prog9000
	prog9400	= __mem_prog9400

	debugsave	= __mem_debugsave
	dbg00           = __mem_dbg00
	dbg9000         = __mem_dbg9000
	dbg9400         = __mem_dbg9400

.endscope
