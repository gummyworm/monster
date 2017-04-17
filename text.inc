.import __text_len
.import __text_clrline
.import __text_colstart
.import __text_puts
.import __text_charmap
.import __text_putch
.import __text_status
.import __text_update
.import __text_print
.import __text_puterr
.import __text_hiline
.import __text_curon
.import __text_curoff

.scope text
        len      = __text_len
	clrline  = __text_clrline
        colstart = __text_colstart
        puts     = __text_puts
	putch    = __text_putch
	print    = __text_print
        charmap  = __text_charmap
	status   = __text_status
	update   = __text_update
	hiline = __text_hiline
	curon = __text_curon
	curoff = __text_curoff
.endscope
