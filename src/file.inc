.import __file_close
.import __file_getline
.import __file_load
.import __file_loaddir
.import __file_open
.import __file_save
.import __file_scratch


.scope file
	close   = __file_close
	getline = __file_getline
	load    = __file_load
	loaddir = __file_loaddir
	open    = __file_open
	save    = __file_save
	scratch = __file_scratch
.endscope
