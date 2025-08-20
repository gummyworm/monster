.import __obj_add_export
.import __obj_add_reloc
.import __obj_add_section
.import __obj_apply_relocation
.import __obj_close_section
.import __obj_dump
.import __obj_init
.import __obj_load
.import __obj_numsections

.scope obj
	add_export       = __obj_add_export
	addreloc         = __obj_add_reloc
	add_section      = __obj_add_section
	apply_relocation = __obj_apply_relocation
	close_section    = __obj_close_section
	dump             = __obj_dump
	init             = __obj_init
	load             = __obj_load
	numsections      = __obj_numsections
.endscope
