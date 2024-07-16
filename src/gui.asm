;******************************************************************************
; GUI.ASM
; This file contains procedures for GUI functionality like graphical menus.
;******************************************************************************

.include "bitmap.inc"
.include "config.inc"
.include "draw.inc"
.include "edit.inc"
.include "key.inc"
.include "keycodes.inc"
.include "macros.inc"
.include "text.inc"
.include "zeropage.inc"

.BSS
;******************************************************************************
baserow:	.byte 0
scroll:		.byte 0
select:		.byte 0
num:		.byte 0

guidata:
height:		.byte 0
getkey:		.word 0
getdata:	.word 0
title:		.word 0

.CODE

;******************************************************************************
; REFRESH
; Redraws the active GUI (if any) without activating it
.export  __gui_refresh
.proc __gui_refresh
.endproc

;******************************************************************************
; REENTER
; Activates the most recently created GUI without reinitializing it.
.export __gui_reenter
.proc __gui_reenter
	lda baserow
	ldx height
	ldy num
	jmp listmenu_cont
.endproc

;******************************************************************************
; LIST_MENU
; Displays a user selectable menu of options as rows of text that the given
; callbacks provide.
; The list will take up to the given number of rows, but will draw less if
; there isn't enough data to fill them.
;
; The provided key handler will be called with the offset to the item
; that is currently being selected in .A
; This occurs on any key other than <-, which quits the selection list and
; the up/down keys
;
; The provided get data handler is called to populate the lines of options.
; It is also called with the offset of the item in .A
;
; IN:
;  - .A: the row to start the list at
;  - .Y: the number of items
;  - r0: the menu data:
;    - 0: height of the view
;    - 1: address of key handler
;    	- IN:  .A: the key code, .X: the item index
;    	- OUT: .C: set if the menu should exit
;    - 3: address to the get data handler
;    	- IN: .A: the item index to get the line of data for
;    - 5: address of menu title
.export  __gui_listmenu
__gui_listmenu:
	sta baserow
	sty num
	ldx #$00
	stx scroll
	stx select

;--------------------------------------
.proc listmenu_cont
	ldy #6
:	lda (r0),y
	sta guidata,y
	dey
	bpl :-

	lda num
	cmp height
	bcs :+
	sta height
	sta height

:	lda baserow
	sec
	sbc height
	pha
	sbc #$01
	jsr edit::resize

	pla
	pha
	ldxy title
	jsr text::print
	pla
	tax
	lda #DEFAULT_900F^$08
	jsr draw::hline

	dec height
	jsr __gui_draw_listmenu

@loop:	jsr key::getch
	beq @loop

	pha

	; unhighlight the current line
	lda baserow
	sec
	sbc select
	tax
	lda #DEFAULT_900F
	jsr draw::hline

	pla
	cmp #K_QUIT
	bne @chkup
@quit:	; save current offset/scroll/etc.
	lda select
	sta select
	lda scroll
	sta scroll
	rts

@chkup:	jsr key::isup
	bne @chkdown
@up:	lda select
	clc
	adc scroll
	adc #$01		; need to check num-1
	cmp num
	bcs @redraw		; out of bounds

	lda select
	cmp height
	bcc @goup		; if selection is < maxheight, just move cursor

@scrollup:
	lda select
	cmp height
	bcc @goup		; if < maxheight, just move cursor
	clc
	inc scroll
	bne @redraw		; redraw the scrolled display
@goup:	inc select
	bne @redraw

@chkdown:
	jsr key::isdown
	bne @getch		; if not down, call handler for all other keys

	lda select
	beq @scrolldown

	dec select
	bpl @redraw
@scrolldown:
	clc
	adc scroll
	beq @redraw		; can't move

	dec scroll
	bpl @redraw		; redraw the scrolled display

;--------------------------------------
@getch:
	jsr @keycallback
	bcs @quit
@redraw:
	jsr __gui_draw_listmenu	; refresh the display
	jmp @loop

;--------------------------------------
@keycallback:
	; get the item index
	pha
	lda scroll
	clc
	adc select
	tax
	pla
	jmp (getkey)
.endproc

;******************************************************************************
.export __gui_draw_listmenu
.proc __gui_draw_listmenu
; draw all visible lines and highlight the selected one
@row=zp::gui
	lda baserow
	sta @row
	sec
	sbc height
	sta @rowstop
	lda #$00
	sta @i

@dloop:	lda @row
@rowstop=*+1
	cmp #$00			; are we at the top row yet?
	bcc @highlight_selection	; if so, continue to highlight selected

@i=*+1
	lda #$00
	clc
	adc scroll

	jmp (getdata)			; get the next line of data

@guireturn:				; getdata will jump back here
	lda @row
	jsr text::print
	dec @row
	inc @i
	bne @dloop

;--------------------------------------
@highlight_selection:
	lda baserow
	sec
	sbc select
	tax
	lda #DEFAULT_900F^$08
	jmp draw::hline

;--------------------------------------
; GUIRETURN
; Entrypoint for "getline" handlers to return from
; This is done because these handlers often push values to be printed and this
; saves them from having to do stack management
.export __gui_return
__gui_return = @guireturn
.endproc
