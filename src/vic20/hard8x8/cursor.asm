;*******************************************************************************
; CURSOR.ASM
; This file contains procedures for interacting with the cursor.
; This includes functionality like limiting the bounds of the cursor as well
; as code to render it.
;*******************************************************************************

.include "../../config.inc"
.include "../../edit.inc"
.include "../../macros.inc"
.include "../../memory.inc"
.include "../../text.inc"
.include "../../zeropage.inc"

;*******************************************************************************
; CONSTANTS
L_INSERT_MASK  = $80	; mask for left half of 8x8 char in INSERT mode
R_INSERT_MASK  = $08	; mask for right half of 8x8 char in INSERT mode

END_INSERT_MASK = $01	; mask for offscreen column

L_REPLACE_MASK = $f0	; mask for left half of 8x8 char in REPLACE mode
R_REPLACE_MASK = $0f	; mask for right half of 8x8 char in REPLACE mode

.import __cur_status

.CODE

;*******************************************************************************
; TOGGLE
; Toggles the cursor (turns it off if its on or vise-versa)
.export __cur_toggle
__cur_toggle:
@dst=r0
	ldx zp::curx
	ldy zp::cury
	rts
