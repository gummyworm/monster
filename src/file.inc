.import __file_close
.import __file_getline
.import __file_load_bin
.importzp __file_load_address
.import __file_loaddir
.import __file_load_src
.import __file_open
.import __file_readb
.import __file_save_bin
.import __file_save_src
.import __file_scratch

.scope file
	close    = __file_close
	getline  = __file_getline
	loadbin  = __file_load_bin
	loadaddr = __file_load_address
	loaddir  = __file_loaddir
	loadsrc  = __file_load_src
	open     = __file_open
	readb    = __file_readb
	savebin  = __file_save_bin
	savesrc  = __file_save_src
	scratch  = __file_scratch
.endscope
