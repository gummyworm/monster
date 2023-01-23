.include "zeropage.inc"
.CODE

;--------------------------------------
.export __key_getch
.proc __key_getch
	jmp $ffe4
.endproc

;--------------------------------------
.export __key_gethex
.proc __key_gethex
	jsr $ffe4
	jsr __key_ishex
	bcs @done
	lda #$00
@done:  rts
.endproc

;--------------------------------------
; ishex returns .C set if the given key is 0-9 or A-F
.export __key_ishex
.proc __key_ishex
	cmp #'0'
	bcc @nothex
	cmp #'f'+1
	bcs @nothex
	cmp #'a'
	bcs @done
	cmp #'9'+1
	bcs @nothex
	sec
	rts
@nothex:
	clc
@done:	rts
.endproc
