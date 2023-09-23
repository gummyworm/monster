BEEP_TONE = $a4
BEEP_VOL  = $0c

SHORT_BEEP_DURATION = 10
LONG_BEEP_DURATION  = 30

.BSS
;******************************************************************************
beep_tmr: .byte 0

;******************************************************************************
.export __beep_short
.proc __beep_short
	lda #SHORT_BEEP_DURATION
	.byte $2c
	; fallthrough
.endproc

;******************************************************************************
.export __beep_long
.proc __beep_long
	lda #LONG_BEEP_DURATION

	sta beep_tmr

	lda #BEEP_VOL
	ora $900e
	sta $900e
	rts
.endproc

;******************************************************************************
.export __beep_update
.proc __beep_update
	lda beep_tmr
	beq @done
	dec beep_tmr
	bne @done

	; turn off volume
	lda $900e
	and #$f0
	sta $900e

@done:	rts
.endproc
