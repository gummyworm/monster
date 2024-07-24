.include "config.inc"
.include "macros.inc"

.BSS
;******************************************************************************
beep_tmr: .byte 0

.CODE
;******************************************************************************
; BEEP_SHORT
; Plays a short beep. Call beep::update to run the sounds player
.export __beep_short
.proc __beep_short
	lda #SHORT_BEEP_DURATION
	skw
	; fallthrough
.endproc

;******************************************************************************
; BEEP_LONG
; Plays a long beep. Call beep::update to run the sounds player
.export __beep_long
.proc __beep_long
	lda #LONG_BEEP_DURATION
	sta beep_tmr

	lda #BEEP_VOL
	ora $900e
	sta $900e

	lda #BEEP_TONE
	sta $900b
	rts
.endproc

;******************************************************************************
; BEEP_UPDATE
; Runs one step of the sound player. Call this every frame if you are playing
; sounds.
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
