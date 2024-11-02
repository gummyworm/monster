.import __irq_raster
.import __irq_disable

.scope irq
        raster  = __irq_raster
	disable = __irq_disable
.endscope
