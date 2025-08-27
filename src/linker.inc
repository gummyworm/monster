.import __link_init
.import __link_link
.import __link_objfiles
.import __link_outfile
.import __link_parse

.scope link
	init       = __link_init
	link       = __link_link
	objfiles   = __link_objfiles
	outfile    = __link_outfile
	parse      = __link_parse
.endscope
