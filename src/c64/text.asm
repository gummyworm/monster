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

;*******************************************************************************
; SCROLLUP
; Scrolls all lines from .X to .A up
; IN:
;  - .X: the top line that characters are scrolled to
;  - .A: the bottom line that is scrolled
.proc __text_scrollup
.export __text_scrollup
	; TODO:
.endproc

;*******************************************************************************
; SCROLLDOWN
; Scrolls all rows from .A to .X
; IN:
;  - .A: the first column to scroll down
;  - .X: the last column to scroll down to
.export __text_scrolldown
.proc __text_scrolldown
	; TODO:
.endproc

;*******************************************************************************
; SCROLLDOWNN
; Scrolls all rows in the given range down by the given number of rows
; IN:
;  - .A: the first row to scroll down
;  - .X: the last row to scroll down
;  - .Y: the number of characters to scroll each row by
.export __text_scrolldownn
.proc __text_scrolldownn
	; TODO:
.endproc

