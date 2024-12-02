.import __watches_add
.import __watches_init
.import __watches_edit
.import __watches_getdata
.import __watches_mark
.import __watches_remove
.import __watches_update

.scope watch
	add     = __watches_add
	edit    = __watches_edit
	getdata = __watches_getdata
	init    = __watches_init
	mark    = __watches_mark
	remove  = __watches_remove
	update  = __watches_update
.endscope
