TEXT_COLOR	 = 6	      		; text color for the editor
BORDER_COLOR	 = $3|8	      		; border color for the editor
BG_COLOR	 = $1	      		; bg color for the editor
ERROR_COLOR	 = ($00|$08) 		; $900f color of error highlight
DEBUG_LINE_COLOR = ($70|BORDER_COLOR)	; row cursor color during debug

CUR_BLINK_SPEED  = 40	; number of updates before blink

;******************************************************************************
; source formatting
.define INDENT_LEVEL 2			; .define'd for use as macro param

;******************************************************************************
; editor configuration
MAX_COPY_SIZE = 640

;******************************************************************************
; debugger config settings
DEBUG_MEMVIEW_COLOR = 2
DEBUG_REGVIEW_COLOR = 0

DEBUG_REG_CHANGED_COLOR = 2  ; highlight color for register than was changed during STEP

;******************************************************************************
; beep settings
BEEP_TONE     = $e4
BEEP_ERR_TONE = $a4
BEEP_VOL      = $0c

SHORT_BEEP_DURATION = 10
LONG_BEEP_DURATION  = 30
