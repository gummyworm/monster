.import __src_insert
.import __src_lineup
.import __src_getline
.import __src_puts
.import __src_replaceline
.import __src_test
.import __src_buffer
.import __src_next
.import __src_prev
.import __src_size
.import __src_pos
.import __src_getrow

GAPSIZE = 40	; size of gap in gap buffer

.scope src
	getline = __src_getline
	insert = __src_insert
	lineup = __src_lineup
	replaceline = __src_replaceline
	puts = __src_puts
	test = __src_test
	buffer = __src_buffer
	next = __src_next
	prev = __src_prev
	size = __src_size
	pos = __src_pos
	getrow = __src_getrow
.endscope
