.import __obj_add_export
.import __obj_add_import
.import __obj_add_reloc
.import __obj_add_section
.import __obj_dump
.import __obj_init

.scope obj
	add_export      = __obj_add_export
	add_import      = __obj_add_import
	addreloc	= __obj_add_reloc
	add_section     = __obj_add_section
	dump            = __obj_dump
	init            = __obj_init
.endscope
