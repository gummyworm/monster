.import __scr_shl
.import __scr_shr
.import __scr_reset
.import __scr_restore

.scope scr
	reset   = __scr_reset
	restore = __scr_restore
	shl     = __scr_shl
	shr     = __scr_shr
.endscope
