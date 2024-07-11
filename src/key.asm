.include "errors.inc"
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
;  - .Z: set if no key is pressed
.export __key_getch
.proc __key_getch
	lda $c6		; get keyboard buffer length
	beq @done	; if buffer empty, return

	ldy $0277
	ldx #$00
:	lda $0277+1,x
	sta $0277,x
	inx
	cpx $c6
	bne :-
	dec $c6		; dec keyboard buffer index
	tya		; get key

	cmp #$41
	bcc @done
	cmp #$5a+1
	bcs :+
	eor #$20	; if alpha, EOR $20
	rts		; rts with .C clear

:	cmp #$c1
	bcc @done
	cmp #$db
	bcs @done
	eor #$80	; if control code, EOR $80
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
	bcc @done
	cmp #'f'+1
	bcs @nothex
	cmp #'a'
	bcs @done
	cmp #'9'+1
	bcs @nothex
@ishex:	sec
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
	bcc @done
	cmp #'9'+1
	bcs @notdec
	sec
	rts
@notdec:
	clc
@done:	rts
.endproc

;******************************************************************************_
; IS PRINTING
; Returns with .C set if the given character is printable
; IN:
;  - .A: the character to check for printability
; OUT:
;  - .C: set if the character is not printable
.export __key_is_printing
.proc __key_is_printing
	cmp #$09
	beq @yes
	cmp #' '
	bcc @no
	cmp #$7f
	bcs @done
@yes:	RETURN_OK
@no:	sec
@done:	rts
.endproc

;******************************************************************************_
; ISUP
; Checks if the given key is UP or 'k'
; IN:
;  - .A: the key value
; OUT:
;  - .Z: set if the given key is UP or 'k'
.export __key_isup
.proc __key_isup
	cmp #$6b	; 'k'
	beq :+
	cmp #K_UP
:	rts
.endproc

;******************************************************************************_
; ISDOWN
; Checks if the given key is DOWN or 'j'
; IN:
;  - .A: the key value
; OUT:
;  - .Z: set if the given key is DOWN or 'j'
.export __key_isdown
.proc __key_isdown
	cmp #$6a	; 'j'
	beq :+
	cmp #K_DOWN
:	rts
.endproc

;******************************************************************************_
; ISLEFT
; Checks if the given key is LEFT or 'h'
; IN:
;  - .A: the key value
; OUT:
;  - .Z: set if the given key is LEFT or 'h'
.export __key_isleft
.proc __key_isleft
	cmp #K_LEFT
	beq :+
	cmp #$68	; 'h'
:	rts
.endproc

;******************************************************************************_
; ISRIGHT
; Checks if the given key is RIGHT or 'l'
; IN:
;  - .A: the key value
; OUT:
;  - .Z: set if the given key is RIGHT or 'l'
.export __key_isright
.proc __key_isright
	cmp #K_RIGHT
	beq :+
	cmp #$6c	; 'l'
:	rts
.endproc
