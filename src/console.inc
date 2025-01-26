.import __console_enter
.import __console_getch
.import __console_init
.import __console_line
.import __console_outfile
.import __console_puts
.import __console_reenter
.import __console_quit

.scope con
	enter   = __console_enter
	getch   = __console_getch
	init	= __console_init
	line    = __console_line
	outfile = __console_outfile
	puts    = __console_puts
	reenter = __console_reenter
	quit    = __console_quit
.endscope
