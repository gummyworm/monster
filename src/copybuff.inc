.import __buff_clear
.import __buff_len
.import __buff_lastline
.import __buff_lines_copied
.import __buff_num_lines_copied
.import __buff_putch
.import __buff_getch
.import __buff_getline
.import __buff_pop
.import __buff_push
.import __buff_reverse

.scope buff
	clear            = __buff_clear
	getline          = __buff_getline
	lastline         = __buff_lastline
	lines_copied     = __buff_lines_copied
	num_lines_copied = __buff_num_lines_copied
	putch            = __buff_putch
	getch            = __buff_getch
	pop              = __buff_pop
	push             = __buff_push
	len              = __buff_len
	reverse          = __buff_reverse
.endscope
