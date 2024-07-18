;******************************************************************************
; ERRLOG.ASM
; This file contains the code for the error window, which displays a log of
; errors to the user. It is activated by the editor when an error occurs and
; is closed by the editor when the errors have all been addressed or when the
; user closes it.
;******************************************************************************

.include "bitmap.inc"
.include "debuginfo.inc"
.include "edit.inc"
.include "errors.inc"
.include "gui.inc"
.include "key.inc"
.include "keycodes.inc"
.include "macros.inc"
.include "source.inc"
.include "string.inc"
.include "strings.inc"
.include "text.inc"
.include "zeropage.inc"

;******************************************************************************
; CONSTANTS
MAX_ERRORS = 10
MAX_HEIGHT = 4

.BSS
errcodes:   .res MAX_ERRORS
errlineslo: .res MAX_ERRORS
errlineshi: .res MAX_ERRORS
errfileids: .res MAX_ERRORS

.export __errlog_numerrs
__errlog_numerrs:
numerrs: .byte 0

.CODE

;******************************************************************************
; ACTIVATE
; Displays the error window and resizes the editor to fit it.
; IN:
;  - .A: the row to base the window at (grows upward)
.export __errlog_activate
.proc __errlog_activate
	ldxy #@menu
	stxy r0
	ldy numerrs
	jmp gui::listmenu
@menu:
.byte MAX_HEIGHT	; max height
.word @keyhandler	; key handler
.word @getline		; get line handler
.word strings::errors	; title

; callback to get the item in .A
@getline:
@err=ra
	sta @err
	tax

	cpx numerrs
	bcc :+
	rts
:	lda errcodes,x
	jsr err::get
	jsr str::uncompress
	; push error string
	tya
	pha
	txa
	pha

	; push line #
	ldx @err
	lda errlineslo,x
	pha
	lda errlineshi,x
	pha

	ldxy #strings::edit_line_err
	jmp gui::ret

; callback to handle keypress
@keyhandler:
	cmp #K_RETURN
	beq :+
@ret:	clc			; flag to stay in menu
	rts

:	ldy errlineshi,x
	lda errlineslo,x
	tax
	cmpw #0
	beq @ret
	jsr edit::gotoline	; go to the line # corresponding to the error
	sec			; flag to exit menu
	rts
.endproc

;******************************************************************************
; CLEAR
; Removes all errors and closes the error window (if it's active)
.export __errlog_clear
.proc __errlog_clear
	lda #$00
	sta numerrs
	rts
.endproc

;******************************************************************************
; LOG
; Adds the given error to the error log.  The current source line number and
; file is mapped to it if applicable
; IN:
;  - .A:  the error code
; OUT:
;  - .C: set if MAX_ERRORS have been logged since log was cleared
.export __errlog_log
.proc __errlog_log
	ldx numerrs
	cpx #MAX_ERRORS-1
	bcs @done		; if already at max errors, return with .C set
	sta errcodes,x

	; map the line # and file ID
	lda dbgi::srcline
	sta errlineslo,x
	lda dbgi::srcline+1
	sta errlineshi,x
	lda dbgi::file
	sta errfileids,x

	inc numerrs
	clc
@done:	rts
.endproc
