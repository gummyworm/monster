.import __link_add_section
.import __link_add_segment
.import __link_link
.import __link_objfiles
.import __link_parse

.scope link
	link       = __link_link
	objfiles   = __link_objfiles
	parse      = __link_parse
	addsection = __link_add_section
	addsegment = __link_add_segment
.endscope
