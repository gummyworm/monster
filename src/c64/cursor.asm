.include "../zeropage.inc"

.include "c64.inc"

.import __cur_status

.CODE

;*******************************************************************************
; TOGGLE
; Toggles the cursor (turns it off if its on or vise-versa)
.export __cur_toggle
.proc __cur_toggle
@dst=r0
	; get the row to toggle
	ldx zp::cury
	lda c64::rowslo,x
	sta @dst
	lda c64::rowshi,x
	sta @dst+1
	ldy zp::curx
	lda (@dst),y
	eor #$80		; reverse
	sta (@dst),y

	lda #1
	eor __cur_status
	sta __cur_status

	rts
.endproc
