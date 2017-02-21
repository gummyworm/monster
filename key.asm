.include "zeropage.inc"

.CODE

;--------------------------------------
.export __key_getch
.proc __key_getch
	jsr $ffe4
@done: 	rts
.endproc

