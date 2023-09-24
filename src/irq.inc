.import __irq_brk
.import __irq_raster
.import __irq_sys_update

.scope irq
        raster     = __irq_raster
	break      = __irq_brk
	sys_update = __irq_sys_update
.endscope
