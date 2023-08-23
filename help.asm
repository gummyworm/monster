.include "key.inc"
.include "macros.inc"
.include "text.inc"
.include "ui.inc"
.include "zeropage.inc"
.CODE

HELP_START_ROW=1
;******************************************************************************
; help displays usage information for the application
.export help
.proc help
	msgbox #40,#@numlines,#HELP_START_ROW,#0,#@lines

@lines: .word @m0,@m1,@m2,@m3,@m4,@m5,@m6,@m7,@m8
@numlines=(*-@lines)/2

@m0: .byte "f1: save",0
@m1: .byte "f2: save as",0
@m2: .byte "f3: assemble",0
@m3: .byte "f5: load",0
@m4: .byte "c= c: refresh",0
@m5: .byte "c= l: list directory",0
@m6: .byte "c= r: rename",0
@m7: .byte "c= v: view memory",0
@m8: .byte "c= g: go <label>",0
.endproc

