;sets up a raster interrupt at the line given in .A to the vector given in
;(<.X, >.Y)
.import __irq_raster
.import __irq_brk

.scope irq
        raster  = __irq_raster
	break = __irq_brk
.endscope
