.import __link_add_section
.import __link_init
.import __link_link
.import __link_objfiles
.import __link_parse

.scope link
	init       = __link_init
	link       = __link_link
	objfiles   = __link_objfiles
	parse      = __link_parse
	addsection = __link_add_section
.endscope
