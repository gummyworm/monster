.import __link_get_segment_by_name
.import __link_init
.import __link_link
.import __link_objfiles
.import __link_outfile
.import __link_parse
.import __link_segaddr_for_file

.scope link
	init             = __link_init
	link             = __link_link
	objfiles         = __link_objfiles
	outfile          = __link_outfile
	parse            = __link_parse
	segaddr_for_file = __link_segaddr_for_file
	segid_by_name    = __link_get_segment_by_name
.endscope
