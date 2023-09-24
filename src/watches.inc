.import __watches_init
.import __watches_edit
.import __watches_mark
.import __watches_view
.import __watches_update

.scope watch
	edit   = __watches_edit
	init   = __watches_init
	mark   = __watches_mark
	update = __watches_update
	view   = __watches_view
.endscope
