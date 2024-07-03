.import __key_getch
.import __key_gethex
.import __key_isdown
.import __key_ishex
.import __key_isdec
.import __key_isleft
.import __key_isright
.import __key_isup
.import __key_is_printing

.scope key
	getch      = __key_getch
	gethex     = __key_gethex
	isdown     = __key_isdown
	isdec      = __key_isdec
	isleft     = __key_isleft
	ishex      = __key_ishex
	isright    = __key_isright
	isup       = __key_isup
	isprinting = __key_is_printing
.endscope
