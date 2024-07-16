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
baserow: .byte 0
scroll:  .byte 0
height:  .byte 0
num:     .byte 0

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
	; fall through to list_menu
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
.proc __gui_listmenu
@baserow=zp::gui
@select=zp::gui+1	; selection offset
@scroll=zp::gui+2	; scroll amount
@guivars=zp::gui+3
@maxheight=@guivars
@getkey=@guivars+1
@getdata=@guivars+3
@title=@guivars+5
	sta baserow
	sta @baserow
	sty @num
	sty num

	ldxy #@maxheight
	stxy r2
	ldy #6
:	lda (r0),y
	sta (r2),y
	dey
	bpl :-

	lda num
	cmp @maxheight
	bcs :+
	sta @maxheight
	sta height

:	lda @baserow
	sec
	sbc @maxheight
	pha
	sbc #$01
	jsr edit::resize

	pla
	pha
	ldxy @title
	jsr text::print
	pla
	tax
	lda #DEFAULT_900F^$08
	jsr draw::hline

	dec @maxheight

	ldx #$00
	stx @scroll
	stx @select
	beq @redraw

@loop:	jsr key::getch
	beq @loop

	pha

	; unhighlight the current line
	lda @baserow
	sec
	sbc @select
	tax
	lda #DEFAULT_900F
	jsr draw::hline

	pla
	cmp #K_QUIT
	bne @chkup
@quit:	rts

@chkup:	jsr key::isup
	bne @chkdown
@up:	lda @select
	clc
	adc @scroll
	adc #$01		; need to check num-1
@num=*+1
	cmp #$00
	bcs @redraw		; out of bounds

	lda @select
	cmp @maxheight
	bcc @goup		; if selection is < maxheight, just move cursor

@scrollup:
	lda @select
	cmp @maxheight
	bcc @goup		; if < maxheight, just move cursor
	clc
	inc @scroll
	bne @redraw		; redraw the scrolled display
@goup:	inc @select
	bne @redraw

@chkdown:
	jsr key::isdown
	bne @getch		; if not down, call handler for all other keys

	lda @select
	beq @scrolldown

	dec @select
	bpl @redraw
@scrolldown:
	clc
	adc @scroll
	beq @redraw		; can't move

	dec @scroll
	bpl @redraw		; redraw the scrolled display

;--------------------------------------
@getch:
	jsr @keycallback
	bcs @quit

	; fall through to redraw

;--------------------------------------
; draw all visible lines and highlight the selected one
@redraw:
@row=@guivars+7
	lda @baserow
	sta @row
	sec
	sbc @maxheight
	sta @rowstop
	lda #$00
	sta @i

@dloop:	lda @row
@rowstop=*+1
	cmp #$00
	bcc @highlight_selection
@i=*+1
	lda #$00
	clc
	adc @scroll

;--------------------------------------
; gets a line of data at the active scroll/selection
; OUT: .XY: the line of text
@getline:
	jmp (@getdata)

; getdata will jump back here
@guireturn:
	lda @row
	jsr text::print
	dec @row
	inc @i
	bne @dloop

;--------------------------------------
@highlight_selection:
	lda @baserow
	sec
	sbc @select
	tax
	lda #DEFAULT_900F^$08
	jsr draw::hline
	jmp @loop

;--------------------------------------
@keycallback:
	; get the item index
	pha
	lda @scroll
	clc
	adc @select
	tax
	pla
	jmp (@getkey)

;--------------------------------------
; GUIRETURN
; Entrypoint for "getline" handlers to return from
; This is done because these handlers often push values to be printed and this
; saves them from having to do stack management
.export __gui_return
__gui_return = @guireturn
.endproc
