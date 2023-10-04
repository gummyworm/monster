BITMAP_ADDR = $1100
COLMEM_ADDR = $9400

.import __bm_init
.import __bm_clr
.import __bm_clrcolor
.import __bm_clr_part
.import __bm_clrline
.import __bm_columns
.import __bm_rvsline
.import __bm_rvsline_part
.import __bm_save
.import __bm_shr
.import __bm_restore

.scope bm
        init     = __bm_init
        clr      = __bm_clr
	clrcolor = __bm_clrcolor
	clrline  = __bm_clrline
	clrpart  = __bm_clr_part
        columns  = __bm_columns
	rvsline  = __bm_rvsline
	rvsline_part = __bm_rvsline_part
	save     = __bm_save
	shr      = __bm_shr
	restore  = __bm_restore
.endscope

