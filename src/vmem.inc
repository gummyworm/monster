.import __vmem_load
.import __vmem_load_off
.import __vmem_store
.import __vmem_store_off
.import __vmem_translate
.import __vmem_writable

.scope vmem
	load      = __vmem_load
	load_off  = __vmem_load_off
	store     = __vmem_store
	store_off = __vmem_store_off
	translate = __vmem_translate
	writable  = __vmem_writable
.endscope
