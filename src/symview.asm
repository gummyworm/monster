;******************************************************************************
; SYMVIEW.ASM
; This file contains the code for the symbol viewer, which allows the user to
; see all (non-local/anonymous) symbols in their program.
; Using debug information, the user can navigate to the location of the symbol

.include "draw.inc"
.include "edit.inc"
.include "key.inc"
.include "finalex.inc"
.include "keycodes.inc"
.include "labels.inc"
.include "macros.inc"
.include "screen.inc"
.include "text.inc"
.include "zeropage.inc"

SCREEN_H = 23
MAX_LABELS = 768

.BSS
.CODE

;******************************************************************************
; ENTER
; Enters the symbol viewer.
.export __symview_enter
.proc __symview_enter
@cnt=r7
@addr=r9
@row=rb
	jsr scr::reset

	ldxy lbl::num
	cmpw #0
	beq @done

	ldxy #$00
	stxy @cnt

@l0:	stx @row
	jsr edit::clear
@l1:	ldxy #$100
	stxy r0			; destination buffer for getname
	ldxy @cnt
	jsr lbl::getname	; get the symbol name
	lda #$01
	pha
	lda #$00
	pha

	ldxy @cnt
	jsr lbl::getaddr	; get the symbol address
	txa
	pha
	tya
	pha

	lda @row
	ldxy #@sym_line
	jsr text::print

	inc @row
	lda @row
	cmp #SCREEN_H
	beq @done		; end of screen
	incw @cnt
	ldxy @cnt
	cmpw lbl::num
	bne @l1

@done:	; wait for a key
	jsr key::getch
	beq @done
	cmp #$11		; down
	beq @pgdown
	cmp #$91		; up
	beq @pgup
	cmp #K_QUIT		; <-
	bne @done
	jmp scr::restore

@pgdown:
	ldxy @cnt		; @cnt is already +SCREEN_H
	cmpw lbl::num		; are we at the end of the symbols?
	bcs @done		; yes, don't switch pages
	bcc @cont

@pgup:	ldxy @cnt
	sub16 #(SCREEN_H*2)-1
	bpl @cont
	ldxy #$00

@cont:	stxy @cnt
	ldx #$00
	jmp @l0

.PUSHSEG
.RODATA
@sym_line:
	.byte "$",ESCAPE_VALUE,": ", ESCAPE_STRING, 0
.POPSEG
.endproc
