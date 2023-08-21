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
.export __mem_spareend
__mem_spare=$0400
__mem_spareend = $1000

.export __mem_backbuff
__mem_backbuff = $c000-$f00 	; backup for the screen bitmap

.export __mem_program
__mem_program:
	.res $400	; buffer for the assembled program

.export __linebuffer2
__linebuffer2:
	.res 40		; backup buffer for when the linebuffer must be saved

.export __linebuffer
__linebuffer:
	.res 40
;__linebuffer=$150	; the buffer for the line being edited

.export __mem_ctxbuffer
__mem_ctxbuffer=$140+40	; the buffer for the context during assembly

.export __statusline
__statusline:
	.res 40
.endscope
