BITMAP_ADDR = $1100
COLMEM_ADDR = $9400

;inializes the screen to a MINIGRAFIK bitmap
.import __bm_init

;clears the bitmap
.import __bm_clr

;contains the address of each column within the bitmap
.import __bm_columns

.import __bm_rvsline
.import __bm_save
.import __bm_restore


.scope bm
        init        = __bm_init
        clr         = __bm_clr
        columns     = __bm_columns
	rvsline     = __bm_rvsline
	save        = __bm_save
	restore     = __bm_restore
.endscope

