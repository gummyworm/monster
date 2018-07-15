.CODE
.scope mem

;--------------------------------------
.export __mem_init
.proc __mem_init
	rts
.endproc

.align 256
.export __mem_spare
__mem_spare:
        .res 1024

.export __linebuffer2
__linebuffer2:
	.res 40
.export __linebuffer
__linebuffer:
	.res 40

.export __statusline
__statusline:
	.res 40,' '

.endscope

