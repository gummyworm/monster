.import __obj_add_reloc
.import __obj_add_section
.import __obj_add_symbol_info
.import __obj_dump
.import __obj_get_symbol_info

.scope obj
	addreloc	= __obj_add_reloc
	add_section     = __obj_add_section
	add_symbol_info = __obj_add_symbol_info
	get_symbol_info = __obj_get_symbol_info
	dump            = __obj_dump
.endscope
