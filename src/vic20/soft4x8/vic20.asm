.include "../finalex.inc"

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
	; TODO: enable write-protection for the $2000-$8000 blocks when
	; all SMC is removed from the segments in that range
	lda #$a1
	sta $9c02	; enable 35K of RAM for final expansion
	rts
.endproc
