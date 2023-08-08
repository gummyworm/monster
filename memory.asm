.CODE
.scope mem

.DATA
;--------------------------------------
.export __mem_init
.proc __mem_init
	rts
.endproc

.align 256
.export __mem_spare
__mem_spare=$0400

.export __mem_backbuff
__mem_backbuff:
        .res $f00

.export __mem_program
__mem_program:
	.res $400

.export __linebuffer2
__linebuffer2:
	.res 40

.export __linebuffer
__linebuffer=$110

.export __statusline
__statusline:
	.res 40,' '
.endscope

