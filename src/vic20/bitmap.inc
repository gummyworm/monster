BITMAP_ADDR   = $1100
COLMEM_ADDR   = $9400
BITMAP_HEIGHT = 192
BITMAP_WIDTH  = 160

.import __bm_init
.import __bm_char_addr
.import __bm_clr
.import __bm_clrcolor
.import __bm_clr_part
.import __bm_clrline
.import __bm_columnslo
.import __bm_columnshi
.import __bm_rvsline
.import __bm_rvsline_part

.scope bm
	init         = __bm_init
	charaddr     = __bm_char_addr
	clr          = __bm_clr
	clrcolor     = __bm_clrcolor
	clrline      = __bm_clrline
	clrpart      = __bm_clr_part
	columnslo    = __bm_columnslo
	columnshi    = __bm_columnshi
	rvsline      = __bm_rvsline
	rvsline_part = __bm_rvsline_part
.endscope

