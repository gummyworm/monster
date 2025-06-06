.import __mem_asmbuffer
.import __mem_asmctx
.import __mem_ctxbuffer
.import __mem_debugsave
.import __mem_drive_err
.import __mem_findbuff
.import __mem_spare
.import __mem_spareend

.import __statusinfo
.import __statusline

.import __linebuffer
.import __linebuffer2
.import __linesave

.import __mem_coloron
.import __mem_rowcolors
.import __mem_rowcolors_save

.scope mem
	asmbuffer       = __mem_asmbuffer
	ctxbuffer       = __mem_ctxbuffer
        spare           = __mem_spare
	spareend	= __mem_spareend
	findbuff        = __mem_findbuff
	linebuffer      = __linebuffer
	linebuffer2     = __linebuffer2
	linesave        = __linesave
	statusinfo      = __statusinfo
	statusline	= __statusline
	drive_err	= __mem_drive_err
	coloron         = __mem_coloron
	rowcolors       = __mem_rowcolors
	rowcolors_save  = __mem_rowcolors_save
.endscope
