.import __link_add_export
.import __link_init
.import __link_link
.import __link_objfiles
.import __link_parse

.scope link
	add_export = __link_add_export
	init       = __link_init
	link       = __link_link
	objfiles   = __link_objfiles
	parse      = __link_parse
.endscope
