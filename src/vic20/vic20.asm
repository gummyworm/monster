.import __text_init

.segment "SETUP"

;*******************************************************************************
; INIT
; Performs Vic-20 specific initialization
.export __vic20_init
.proc __vic20_init
	; set current bank to FASTTEXT
	lda #FINAL_BANK_FASTTEXT
	sta $9c02

	jsr __text_init
	rts
.endproc
