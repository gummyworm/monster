.import __src_atcursor
.import __src_backspace
.import __src_buffer
.import __src_delete
.import __src_down
.import __src_end
.import __src_get
.import __src_goto
.import __src_insert
.import __src_line
.import __src_name
.import __src_next
.import __src_new
.import __src_up
.import __src_loaddir
.import __src_loadfile
.import __src_popp
.import __src_prev
.import __src_pushp
.import __src_rename
.import __src_replace
.import __src_right
.import __src_rewind
.import __src_readb
.import __src_readline
.import __src_save
.import __src_start

GAPSIZE = 20	; size of gap in gap buffer

.scope src
	atcursor = __src_atcursor
	backspace = __src_backspace
	buffer = __src_buffer
	delete = __src_delete
	down = __src_down
	end = __src_end
	get = __src_get
	goto = __src_goto
	insert = __src_insert
	loaddir = __src_loaddir
	loadfile = __src_loadfile
	line = __src_line
	new = __src_new
	next = __src_next
	name = __src_name
	replace = __src_replace
	popp = __src_popp
	pushp = __src_pushp
	prev = __src_prev
	rename = __src_rename
	rewind = __src_rewind
	readb = __src_readb
	readline = __src_readline
	right = __src_right
	save = __src_save
	start = __src_start
	up = __src_up
.endscope
