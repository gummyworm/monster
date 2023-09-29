.import __vmem_load
.import __vmem_load_off
.import __vmem_store
.import __vmem_translate

.scope vmem
	load      = __vmem_load
	load_off  = __vmem_load_off
	store     = __vmem_store
	translate = __vmem_translate
.endscope
