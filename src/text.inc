.import __text_charmap
.import __text_clrline
.import __text_drawline
.import __text_hiline
.import __text_hioff
.import __text_insertmode
.import __text_len
.import __text_linelen
.import __text_puts
.import __text_putz
.import __text_plot
.import __text_print
.import __text_putch
.import __text_puterr
.import __text_restorebuff
.import __text_savebuff
.import __text_scrolldown
.import __text_scrollup
.import __text_status 
.import __text_update

; ESCAPE CODES
ESCAPE_STRING    = $ff	; 2 byte: address of string
ESCAPE_VALUE     = $fe	; 2 byte: value to print in hex
ESCAPE_VALUE_DEC = $fd	; 2 byte: value to print in decimal
ESCAPE_SPACING   = $fc	; 1 byte: number of spaces to insert
ESCAPE_BYTE      = $fb  ; 1 byte: the value to print in hex
ESCAPE_RVS_ON    = $01
ESCAPE_RVS_OFF   = $02

TEXT_INSERT  = 1
TEXT_REPLACE = 0

BREAKPOINT_CHAR     = 133
BREAKPOINT_OFF_CHAR = 134
CH_R_ARROW	    = 135 

.scope text
        charmap       = __text_charmap
	clrline       = __text_clrline
	drawline      = __text_drawline
	hiline        = __text_hiline
	hioff         = __text_hioff
	insertmode    = __text_insertmode
        len           = __text_len
	linelen       = __text_linelen
	plot          = __text_plot
	print         = __text_print
	putch         = __text_putch
        puts          = __text_puts
	putz   	      = __text_putz
	savebuff      = __text_savebuff
	scrolldown    = __text_scrolldown
	scrollup      = __text_scrollup
	status        = __text_status
	restorebuff   = __text_restorebuff
	update        = __text_update
.endscope
