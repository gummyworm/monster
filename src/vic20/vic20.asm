.import __text_init

.segment "SETUP"

;*******************************************************************************
; INIT
; Performs Vic-20 specific initialization
.export __vic20_init
.proc __vic20_init
	jsr __text_init
	rts
.endproc
