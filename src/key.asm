.include "keycodes.inc"
.include "macros.inc"
.include "zeropage.inc"

;******************************************************************************
; CONSTANTS
UNSHIFTED_KEY_TABLE = $ec5e
SHIFTED_KEY_TABLE   = $ec9f
CBM_KEY_TABLE       = $ece0
CTRL_KEY_TABLE      = $eda3
CURSOR_LR_MASK      = 2

.CODE

;******************************************************************************
; GETCH
; Gets a key from the keyboard and returns it
; OUT:
;  - .A: the key code of the pressed key or 0 if no key is pressed
.export __key_getch
.proc __key_getch
	jsr $f1f9
	beq @done
	cmp #$41
	bcc @done
	cmp #$5a+1
	bcs :+
	eor #$20
	rts

:	cmp #$c1
	bcc @done
	cmp #$db
	bcs @done
	eor #$80
@done:	cmp #$00
	rts
.endproc

;******************************************************************************
; GETHEX
; Gets a key from the keyboard, and returns its value ONLY
; if it is a hex value, a DELETE, a RETURN, or a QUIT (<-)
; OUT:
;  - .A: the key pressed (if valid)
.export __key_gethex
.proc __key_gethex
	jsr __key_getch
	cmp #K_DEL	; allow delete
	beq :+
	cmp #K_RETURN
	beq :+
	cmp #K_QUIT
	beq :+
	jsr __key_ishex
	bcs :+
	lda #$00	; don't accept non-hex characters
:	cmp #$00
	rts
.endproc

;******************************************************************************_
; ISHEX
; Returns .C set if the given key is 0-9 or A-F
; IN:
;  - .A: the key to check if is hex
; OUT:
;  - .C: set if the given key is 0-9 or A-F
.export __key_ishex
.proc __key_ishex
	cmp #'0'
	bcc @nothex
	cmp #'f'+1
	bcs @nothex
	cmp #'a'
	bcs @done
	cmp #'9'+1
	bcs @nothex
	sec
	rts
@nothex:
	clc
@done:	rts
.endproc
;******************************************************************************_
; ISDEC
; Returns .C set if the given key is 0-9
; IN:
;  - .A: the key to check if is hex
; OUT:
;  - .C: set if the given key is 0-9
.export __key_isdec
.proc __key_isdec
	cmp #'0'
	bcc @notdec
	cmp #'9'+1
	bcs @notdec
	sec
	rts
@notdec:
	clc
@done:	rts
.endproc
