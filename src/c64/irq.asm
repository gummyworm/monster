.segment "IRQ"

.segment "BANKCODE"

;*******************************************************************************
; IRQ OFF
.export __irq_off
.proc __irq_off
	sei
	rts
.endproc

;*******************************************************************************
; IRQ ON
; Syncs to the configured scanline and sets up an IRQ that will trigger whenever
; that location is reached.
.export __irq_on
.proc __irq_on
	cli
	rts
.endproc
