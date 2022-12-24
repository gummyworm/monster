.include "key.inc"
.include "macros.inc"
.include "text.inc"
.include "ui.inc"
.include "zeropage.inc"
.CODE

HELP_START_ROW=1
;--------------------------------------
; help displays usage information for the application
.export help
.proc help
	msgbox #40,#3,#HELP_START_ROW,#0,#@lines

@lines: .word @m0,@m1,@m2
@m0: .byte "f1:rename",0
@m1: .byte "f3:save",0
@m2: .byte "f5:load",0
.endproc

