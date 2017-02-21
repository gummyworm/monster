;sets up a raster interrupt at the line given in .A to the vector given in
;(<.X, >.Y)
.import __irq_raster

.scope irq
        raster  = __irq_raster 
.endscope
