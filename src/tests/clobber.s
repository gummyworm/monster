; the debugger should stop the program
; from storing to $0316-$0319 when
; stepping
.org $1000
	lda #$f2
	sta $0316
