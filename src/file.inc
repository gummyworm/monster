.import __file_close
.import __file_getline
.import __file_load_bin
.importzp __file_load_address
.import __file_load_src
.import __file_open
.import __file_open_r
.import __file_open_r_prg
.import __file_open_w
.import __file_readb
.import __file_save_bin
.importzp __file_save_address
.importzp __file_save_address_end
.import __file_save_src
.import __file_scratch
.import __file_write
.import __file_writeb

.scope file
	close            = __file_close
	getline          = __file_getline
	loadbin          = __file_load_bin
	loadaddr         = __file_load_address
	loadsrc          = __file_load_src
	open_r           = __file_open_r
	open_r_prg       = __file_open_r_prg
	open_w           = __file_open_w
	open             = __file_open
	readb            = __file_readb
	save_address     = __file_save_address
	save_address_end = __file_save_address_end
	savebin          = __file_save_bin
	savesrc          = __file_save_src
	scratch          = __file_scratch
.endscope
