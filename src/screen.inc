.import __scr_shr
.import __scr_shl

.import __scr_reset
.import __scr_restore

.import __scr_popcol
.import __scr_dropcol
.import __scr_pushcol

.scope scr
	reset   = __scr_reset
	restore = __scr_restore
	shr     = __scr_shr
	shl     = __scr_shl

	pushcol = __scr_pushcol
	popcol  = __scr_popcol
.endscope
