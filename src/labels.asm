.include "macros.inc"

GLOBAL_VALUE  = $ffff	; magic value used for global labels

;*******************************************************************************
; IS GLOBAL
; Checks if the given label value matches the magic GLOBAL value
; IN:
;   - .XY: the label value to check
; OUT:
;   - .Z: set if the value represents a global variable
.export __label_is_global
.proc __label_is_global
	cmpw #GLOBAL_VALUE
	rts
.endproc
