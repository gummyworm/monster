.import __text_char_index
.import __text_buffer
.import __text_bufferon
.import __text_bufferoff
.import __text_clrline
.import __text_drawline
.import __text_insertmode
.import __text_len
.import __text_linelen
.import __text_puts
.import __text_print
.import __text_info
.import __text_printstatus
.import __text_putch
.import __text_puterr
.import __text_rendered_line_len
.import __text_rvs
.import __text_restorebuff
.import __text_savebuff
.import __text_scrolldown
.import __text_scrollup
.import __text_status 
.import __text_status_mode
.import __text_tabl_dist
.import __text_tabr_dist
.import __text_update

; ESCAPE CODES
ESCAPE_STRING    = $ff	; 2 byte: address of string
ESCAPE_VALUE     = $fe	; 2 byte: value to print in hex
ESCAPE_VALUE_DEC = $fd	; 2 byte: value to print in decimal
ESCAPE_SPACING   = $fc	; 1 byte: number of spaces to insert
ESCAPE_BYTE      = $fb  ; 1 byte: the value to print in hex
ESCAPE_CHAR      = $fa	; 1 byte: the literal char to print
ESCAPE_RVS_ON    = $12
ESCAPE_RVS_OFF   = $92

TEXT_INSERT  = 1
TEXT_REPLACE = 0

RVS_ON  = $ff
RVS_OFF = $00

BREAKPOINT_CHAR     = 133
BREAKPOINT_OFF_CHAR = 134
CH_R_ARROW	    = 135 

.scope text
	buffer           = __text_buffer
	bufferon         = __text_bufferon
	bufferoff        = __text_bufferoff
	char_index	 = __text_char_index
	clrline          = __text_clrline
	drawline         = __text_drawline
	info             = __text_info
	insertmode       = __text_insertmode
	len              = __text_len
	linelen          = __text_linelen
	print            = __text_print
	printstatus      = __text_printstatus
	putch            = __text_putch
	puts             = __text_puts
	rendered_line_len= __text_rendered_line_len
	rvs              = __text_rvs
	savebuff         = __text_savebuff
	scrolldown       = __text_scrolldown
	scrollup         = __text_scrollup
	statusmode       = __text_status_mode
	status           = __text_status
	restorebuff      = __text_restorebuff
	tabl_dist        = __text_tabl_dist
	tabr_dist        = __text_tabr_dist
	update           = __text_update
.endscope
