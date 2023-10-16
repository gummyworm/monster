; origin at $1000 (internal RAM)
; this program demonstrates multiple
; values that are backed up by the
; debugger while debugging
.org $1000
	lda $1001
	sta $1002
	jmp *
