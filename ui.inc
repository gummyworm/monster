.include "zeropage.inc"

.import __ui_msgbox

.scope ui
	msgbox = __ui_msgbox
.endscope

.macro msgbox w,h,row,col,rows
	ldxy rows
	stxy zp::tmp0
	ldx col
	ldy row
	lda h
	jmp __ui_msgbox
.endmacro
