TEXT_COLOR	 = 0	      		; text color for the editor
BORDER_COLOR	 = $0|0	      		; border color for the editor
BG_COLOR	 = $9	      		; bg color for the editor
ERROR_COLOR	 = ($00|$08) 		; $900f color of error highlight
DEBUG_LINE_COLOR = ($70|BORDER_COLOR)	; row cursor color during debug

CUR_BLINK_SPEED  = 40	; number of updates before blink

;******************************************************************************
; debugger config settings
DEBUG_MEMVIEW_COLOR = 2
DEBUG_REGVIEW_COLOR = 0
