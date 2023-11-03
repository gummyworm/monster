.import __key_getch
.import __key_gethex
.import __key_ishex
.import __key_isdec
.import __key_is_printing

.scope key
	getch      = __key_getch
	gethex     = __key_gethex
	ishex      = __key_ishex
	isdec      = __key_isdec
	isprinting = __key_is_printing
.endscope
