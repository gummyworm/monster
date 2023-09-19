.import __text_len
.import __text_clrline
.import __text_puts
.import __text_putz
.import __text_charmap
.import __text_plot
.import __text_putch
.import __text_status 
.import __text_update
.import __text_print
.import __text_puterr
.import __text_hiline
.import __text_hioff
.import __text_drawline
.import __text_insertmode
.import __text_scrolldown
.import __text_scrollup
.import __text_linelen
.import __text_savebuff
.import __text_restorebuff

ESCAPE_STRING = $ff
ESCAPE_VALUE = $fe
ESCAPE_VALUE_DEC = $fd
ESCAPE_RVS_ON = $01
ESCAPE_RVS_OFF = $02

TEXT_INSERT = 1
TEXT_REPLACE = 0

BREAKPOINT_CHAR = 133
BREAKPOINT_OFF_CHAR = 134

.scope text
        len      = __text_len
	clrline  = __text_clrline
        puts     = __text_puts
	putz   	 = __text_putz
	plot     = __text_plot
	putch    = __text_putch
	print    = __text_print
        charmap  = __text_charmap
	status   = __text_status
	update   = __text_update
	hiline   = __text_hiline
	hioff    = __text_hioff
	drawline = __text_drawline
	scrolldown = __text_scrolldown
	scrollup = __text_scrollup
	insertmode = __text_insertmode
	linelen  = __text_linelen
	savebuff = __text_savebuff
	restorebuff = __text_restorebuff
.endscope
