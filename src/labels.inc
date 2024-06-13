.import __label_add
.import __label_addanon
.import __label_by_id
.import __label_by_addr
.import __label_clr
.import __label_del
.import __label_find
.import __label_get_banon
.import __label_get_fanon
.import __label_get_name
.import __label_get_addr
.import __label_is_local
.import __label_isvalid
.import __label_address
.import __label_num
.import __label_name_by_id
.import __label_set
.import __label_setscope
.import __label_set24

.scope lbl
	add        = __label_add
	addanon    = __label_addanon
	set24      = __label_set24
	addr       = __label_address
	by_id      = __label_by_id
	by_addr    = __label_by_addr
	clr        = __label_clr
	del        = __label_del
	find       = __label_find
	get_banon  = __label_get_banon
	get_fanon  = __label_get_fanon
	getname    = __label_get_name
	getaddr    = __label_get_addr
	islocal    = __label_is_local
	isvalid    = __label_isvalid
	name_by_id = __label_name_by_id
	num        = __label_num
	set        = __label_set
	setscope   = __label_setscope
.endscope
