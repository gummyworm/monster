.import __view_mem
.import __view_edit
.import __view_dirty
.import get_real_address

.scope view
	dirty = __view_dirty
	mem   = __view_mem
	edit  = __view_edit

	realaddr = get_real_address
.endscope
