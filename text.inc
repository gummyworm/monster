.import __text_len
.import __text_colstart
.import __text_puts
.import __text_charmap
.import __text_putch
.import __text_status
.import __text_update
.import __text_print

.scope text
        len      = __text_len
        colstart = __text_colstart
        puts     = __text_puts
	putch    = __text_putch
	print    = __text_print
        charmap  = __text_charmap
	status   = __text_status
	update   = __text_update
.endscope

