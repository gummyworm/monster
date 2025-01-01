.include "zeropage.inc"

;*******************************************************************************
; VERIFY
; This flag, when set, will tell the assembler to loosen its error checking
; and skip certain operations (like including a file).
; This is used for checking the user's input in realtime.
; During assembly, this is disabled (set to 0).
.exportzp __state_verify
__state_verify = zp::verify
