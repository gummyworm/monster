.import __src_backspace
.import __src_delete
.import __src_insert
.import __src_replace
.import __src_puts
.import __src_test
.import __src_buffer
.import __src_next
.import __src_new
.import __src_prev
.import __src_up
.import __src_down
.import __src_get
.import __src_load
.import __src_loaddir
.import __src_save
.import __src_rename
.import __src_name
.import __src_rewind
.import __src_readb
.import __src_readline

GAPSIZE = 20	; size of gap in gap buffer

.scope src
	backspace = __src_backspace
	delete = __src_delete
	insert = __src_insert
	replace = __src_replace
	puts = __src_puts
	test = __src_test
	buffer = __src_buffer
	next = __src_next
	new = __src_new
	prev = __src_prev
	up = __src_up
	down = __src_down
	get = __src_get
	save = __src_save
	load = __src_load
	loaddir = __src_loaddir
	rename = __src_rename
	name = __src_name
	rewind = __src_rewind
	readb = __src_readb
	readline = __src_readline
.endscope
