.import __obj_add_export
.import __obj_add_reloc
.import __obj_add_section
.import __obj_dump
.import __obj_init

.scope obj
	add_export      = __obj_add_export
	addreloc	= __obj_add_reloc
	add_section     = __obj_add_section
	dump            = __obj_dump
	init            = __obj_init
.endscope
