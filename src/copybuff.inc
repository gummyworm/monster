.import __buff_clear
.import __buff_lines_copied
.import __buff_putch
.import __buff_getch
.import __buff_getline
.import __buff_pop
.import __buff_push

.scope buff
	clear        = __buff_clear
	getline      = __buff_getline
	lines_copied = __buff_lines_copied
	putch        = __buff_putch
	getch        = __buff_getch
	pop          = __buff_pop
	push         = __buff_push
.endscope
