TEXT_COLOR	 = 6	      		; text color for the editor
BORDER_COLOR	 = $3|8	      		; border color for the editor
BG_COLOR	 = $1	      		; bg color for the editor

DEFAULT_900F     = (BG_COLOR<<4 | BORDER_COLOR)
DEFAULT_RVS      = DEFAULT_900F^$08

; colors for breakpoints that are enabled and disabled
BREAKPOINT_ON_COLOR  = (9<<4 | BORDER_COLOR)
BREAKPOINT_OFF_COLOR = (14<<4 | BORDER_COLOR)

ERROR_COLOR	 = ($00|$08) 		; $900f color of error highlight
DEBUG_LINE_COLOR = ($70|BORDER_COLOR)	; row cursor color during debug

ASM_SUCCESS_COLOR = (13<<4 | BORDER_COLOR)      ; color for asm done message

CUR_BLINK_SPEED  = 250	; number of updates before blink

;******************************************************************************
; source formatting
.define INDENT_LEVEL 10			; .define'd for use as macro param
TAB_WIDTH = INDENT_LEVEL

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
