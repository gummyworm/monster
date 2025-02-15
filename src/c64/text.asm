.include "../macros.inc"
.include "../zeropage.inc"

.include "c64.inc"

;******************************************************************************
; PUTS
; Displays the given string at the given row.  Regardless of the contents of
; the string, text::len characters are displayed (including 0's etc.)
; IN:
;  - .XY: the string to display
;  - .A:  the row to display the text at
.export puts
.proc puts
@txtsrc=zp::text
@txtdst=zp::text+2
	stxy @txtsrc
	; get the address of the row containing the text
	tax
	lda c64::rowslo,x
	sta @txtdst
	lda c64::rowshi,x
	sta @txtdst+1

	ldy #$00
@l0:	lda (@txtsrc),y
	sta (@txtdst),y
	iny
	cpy #40
	bcc @l0
	rts
.endproc
