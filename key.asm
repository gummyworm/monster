.include "zeropage.inc"
.CODE

;--------------------------------------
.export __key_getch
.proc __key_getch
	jmp $ffe4
.endproc

