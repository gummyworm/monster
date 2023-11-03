.import __src_activebuff
.import __src_after_cursor
.import __src_atcursor
.import __src_backspace
.import __src_buffer_by_name
.import __src_close
.import __src_delete
.import __src_down
.import __src_downn
.import __src_end
.import __src_get
.import __src_getflags
.import __src_goto
.import __src_get_filename
.import __src_insert
.import __src_isdirty
.import __src_line
.import __src_lines
.import __src_next
.import __src_name
.import __src_names
.import __src_new
.import __src_numbuffers
.import __src_up
.import __src_upn
.import __src_popp
.import __src_pos
.import __src_prev
.import __src_pushp
.import __src_replace
.import __src_right
.import __src_right_rep
.import __src_rewind
.import __src_readline
.import __src_save
.import __src_set
.import __src_setflags
.import __src_start

;******************************************************************************
; CONSTANTS
MAX_SOURCES = 8	; maximum number of source banks/buffers

FLAG_DIRTY = 1	; flag that source buffer has been changed since last save

GAPSIZE = 20	; size of gap in gap buffer

.scope src
	activebuff     = __src_activebuff
	after_cursor   = __src_after_cursor
	atcursor       = __src_atcursor
	backspace      = __src_backspace
	buffer_by_name = __src_buffer_by_name
	close          = __src_close
	delete         = __src_delete
	down           = __src_down
	downn          = __src_downn
	names          = __src_names
	numbuffers     = __src_numbuffers
	end            = __src_end
	filename       = __src_get_filename
	get            = __src_get
	getflags       = __src_getflags
	goto           = __src_goto
	insert         = __src_insert
	isdirty        = __src_isdirty
	line           = __src_line
	lines          = __src_lines
	name           = __src_name
	new            = __src_new
	next           = __src_next
	replace        = __src_replace
	popp           = __src_popp
	pos            = __src_pos
	pushp          = __src_pushp
	prev           = __src_prev
	rewind         = __src_rewind
	readline       = __src_readline
	right          = __src_right
	right_rep      = __src_right_rep
	save           = __src_save
	setbuff        = __src_set
	setflags       = __src_setflags
	start          = __src_start
	up             = __src_up
	upn            = __src_upn
.endscope
