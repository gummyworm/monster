.import __cur_left
.import __cur_mode
.import __cur_move
.import __cur_on
.import __cur_off
.import __cur_maxx
.import __cur_maxy
.import __cur_minx
.import __cur_miny
.import __cur_right
.import __cur_set
.import __cur_setmin
.import __cur_setmax
.import __cur_status
.import __cur_toggle
.import __cur_unlimit

CUR_NORMAL = 0
CUR_SELECT = 1

CUR_OFF = 0
CUR_ON  = 1

.scope cur
	on       = __cur_on
	off      = __cur_off
	maxx     = __cur_maxx
	minx     = __cur_minx
	maxy     = __cur_maxy
	miny     = __cur_miny
	mode     = __cur_mode
	move     = __cur_move
	set      = __cur_set
	right    = __cur_right
	left     = __cur_left
	toggle   = __cur_toggle
	setmin   = __cur_setmin
	setmax   = __cur_setmax
	status   = __cur_status
	unlimit  = __cur_unlimit
.endscope

.macro pushcur
	lda zp::curx
	pha
	lda zp::cury
	pha
.endmacro

.macro popcur
	pla
	sta zp::cury
	pla
	sta zp::curx
.endmacro
