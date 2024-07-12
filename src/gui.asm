;******************************************************************************
; GUI.ASM
; This file contains procedures for GUI functionality like graphical menus.
;******************************************************************************

.include "bitmap.inc"
.include "config.inc"
.include "draw.inc"
.include "key.inc"
.include "keycodes.inc"
.include "macros.inc"
.include "text.inc"
.include "zeropage.inc"

.CODE

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
;  - .X: the height of the view
;  - .Y: the number of items
;  - r0: the key handler
;    - IN:  .A: the key code, .X: the item index
;    - OUT: .C: set if the menu should exit
;  - r2: the get data handler
;    - IN: .A: the item index to get the line of data for
.export  __gui_listmenu
.proc __gui_listmenu
@keyhandler=r0
@datahandler=r2
@select=r6	; selection offset
@scroll=r5	; scroll amount
@baserow=r7
@maxheight=r8
@num=r9
	sta @baserow
	stx @maxheight
	sty @num

	ldx #$00
	stx @scroll
	stx @select

	ldxy r0
	stxy @handlekey
	ldxy r2
	stxy @getdata
	jmp @redraw

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
	cmp @num
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
	bne @getkey		; if not down, call handler for all other keys

	lda @select
	beq @scrolldown

	dec @select
	bpl @redraw
@scrolldown:
	clc
	adc @scroll
	beq @redraw		; can't move

	inc @scroll
	bne @redraw		; redraw the scrolled display

;--------------------------------------
@getkey:
	pha
	lda @scroll
	clc
	adc @select
	tax
	pla
@handlekey=*+1
	jsr $f00d
	bcc @loop
	rts

;--------------------------------------
; draw all visible lines and highlight the selected one
@redraw:
@row=rd
@rowstop=re
@i=rf
	lda @baserow
	sta @row
	sec
	sbc @maxheight
	sta @rowstop
	lda #$00
	sta @i

@dloop:	lda @row
	cmp @rowstop
	bcc @highlight_selection
	lda @i
	clc
	adc @scroll
	jsr @getline
	bcs @highlight_selection	; out of data

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
; gets a line of data at the active scroll/selection
; OUT: .XY: the line of text
@getline:
@getdata=*+1
	jmp $f00d
.endproc
