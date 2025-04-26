.import __monitor_clear
.import __monitor_enter
.import __monitor_getch
.import __monitor_init
.import __monitor_int
.import __monitor_line
.import __monitor_outfile
.import __monitor_puts
.import __monitor_reenter
.import __monitor_quit

.scope mon
	clear   = __monitor_clear
	enter   = __monitor_enter
	getch   = __monitor_getch
	init	= __monitor_init
	int	= __monitor_int
	line    = __monitor_line
	outfile = __monitor_outfile
	puts    = __monitor_puts
	reenter = __monitor_reenter
	quit    = __monitor_quit
.endscope
