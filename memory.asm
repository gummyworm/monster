.CODE
.scope mem

.CODE
;--------------------------------------
.export __mem_init
.proc __mem_init
	rts
.endproc

.BSS
;--------------------------------------
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
__linebuffer=$140

.export __statusline
__statusline:
	.res 40

.export __mem_asmctx
__mem_asmctx:
	.res 16*40+22	; see asm_ctx.inc
.endscope
