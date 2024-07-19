;******************************************************************************
; GUI.ASM
; This file contains procedures for GUI functionality like graphical menus.
; GUI windows are created by defining a structure containing handlers for
; retrieving the lines of data to draw and handling key presses.
; Handlers should stay away from the gui zeropage area (see zeropage.inc)
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

;******************************************************************************
MAX_WINDOWS = 3

;******************************************************************************
.BSS

guidata = zp::gui
guidata_size=$c
; the following is the sequence stored
height	= zp::gui	; height of the GUI window
getkey	= zp::gui+1	; pointer to get key handler function
getdata = zp::gui+3	; pointer to get data handler function
numptr  = zp::gui+5	; pointer to the number of items in the GUI
title	= zp::gui+7	; pointer to the title string for the GUI

; the following are not part of the initialization struct, they are initialized
; upon constructing the window and persisted (along side the above fields)
; for the duration of it
scroll  = zp::gui+9	; amount that the GUI window is scrolled
select	= zp::gui+$a	; offset that is currently selected (highlighted)
baserow = zp::gui+$b	; base row for active GUI (set on creation of GUI)

; the following are not stored in the stack, but just used locally within
; a single procedure call
num    = zp::gui+$c	; number of items (cached read from numptr)
guitmp = zp::gui+$d

;******************************************************************************
; GUISTACK
; The guistack holds the gui data for each active window.
; Activating a new window will push the current window (if any), and
; deactivating the window will pop it back into the active gui data memory.
guistack:	.res MAX_WINDOWS*guidata_size

;******************************************************************************
.DATA
guisp:		.word guistack

;******************************************************************************
.CODE

;******************************************************************************
; REENTER
; Activates the most recently created GUI without reinitializing it.
.export __gui_reenter
.proc __gui_reenter
	lda baserow
	ldx height
	ldy num
	jmp __gui_activate
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
;  - .XY: pointer to the menu data struct
;	- 0: height of the view
;	- 1: address of key handler
;    		- IN:  .A: the key code, .X: the item index
;    		- OUT: .C: set if the menu should exit
;	- 3: address to the get data handler
;    		- IN:  .A: the item index to get the line of data for
;		- OUT: mem::linebuffer: the line to display
;	- 5: pointer to number of items in the GUI
;	- 7: address of menu title
;	- 9: scroll value for the menu
;	- A: selection offset for the menu
.export  __gui_listmenu
.proc __gui_listmenu
@src=r0
@stack=r2
	pha
	stxy @src

	ldxy guisp
	stxy @stack

	; update GUI stack pointer
	lda #guidata_size
	clc
	adc @stack
	sta guisp
	bcc :+
	inc guisp+1

:	; copy the GUI data to the GUI stack
	ldy #guidata_size-1
	pla
	sta (@stack),y	; base row
	; initialize scroll and selection offset to 0
	dey
	lda #$00
	sta (@stack),y	; select
	dey
	sta (@stack),y	; scroll
	dey
@l0:	lda (@src),y
	sta (@stack),y
	dey
	bpl @l0

	; fall through to __gui_activate
.endproc

;******************************************************************************
; ACTIVATE
; Activates the most recently created (top of the GUI stack) GUI window.
.export __gui_activate
.proc __gui_activate
@stack=r0
	; copy the persistent GUI state to the zeropage
	jsr copyvars

	; draw GUI before entering the main GUI loop
	dec height
@loop:	inc height
	jsr redraw_state
	dec height
:	jsr key::getch
	beq :-
	pha		; save the key

	; unhighlight the current line in case we move lines
	lda baserow
	sec
	sbc select
	tax
	lda #DEFAULT_900F
	jsr draw::hline

	pla		; get the key
	cmp #K_QUIT
	bne @chkup

@quit:	; TODO: copy current state back to (guisp)
	rts

@chkup:	jsr key::isup
	bne @chkdown
@up:	lda select
	clc
	adc scroll
	adc #$01		; need to check num-1
	cmp num
	bcs @loop		; out of bounds

	lda select
	cmp height
	bcc @goup		; if selection is < maxheight, just move cursor

@scrollup:
	lda select
	cmp height
	bcc @goup		; if < maxheight, just move cursor
	clc
	inc scroll
	bne @loop		; redraw the scrolled display
@goup:	inc select
	bne @loop

@chkdown:
	jsr key::isdown
	bne @getch		; if not down, call handler for all other keys

	lda select
	beq @scrolldown

	dec select
	bpl @loop
@scrolldown:
	clc
	adc scroll
	beq @loop		; can't move

	dec scroll
	bpl @loop		; redraw the scrolled display

;--------------------------------------
@getch:
	jsr @keycallback
	bcs @quit
	bcc @loop

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
; REFRESH
; Redraws the active GUI. This should be called after making changes that
; would affect the state of the GUI window, e.g. setting a breakpoint could
; cause the breakpoints GUI to populate with new data.
.export __gui_refresh
__gui_refresh:
	; copy the persistent GUI state to the zeropage
	jsr copyvars
	bcc redraw_state
:	rts				; no GUI to draw

; entrypoint to draw the already copied zeropage state
.proc redraw_state
	; resize the main editor window to fit the GUI (may increase editor
	; size if the GUI window has shrunk since the last call)
	lda baserow
	sec
	sbc height
	sbc #$01
	jsr edit::resize

@row=guitmp
	lda baserow
	sta @row
	sec
	sbc height
	sta @rowstop

	; draw the title
	ldxy title
	jsr text::print
	ldx @rowstop
	lda #DEFAULT_900F^$08
	jsr draw::hline

	inc @rowstop

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

	lda #DEFAULT_900F		; unhighlight
	ldx @row
	jsr draw::hline

	dec @row
	inc @i
	bne @dloop

;--------------------------------------
@highlight_selection:
	lda baserow
	sec
	sbc select
	tax
	lda #DEFAULT_RVS
	jmp draw::hline

;******************************************************************************
; GUIRETURN
; Jump target for "getline" handlers to return from.  These handlers should
; end with a `jmp gui::return` instead of `rts`
; This is done because these handlers often push values to be printed and this
; saves them from having to do stack management
.export __gui_return
__gui_return = @guireturn
.endproc

;******************************************************************************
; DEACTIVATE
; Exits the GUI that is at the top of the GUI stack
.export __gui_deactivate
.proc __gui_deactivate
	ldxy guisp
	cmpw #guistack
	beq @done		; do nothing if stack is empty
	lda guisp
	sec
	sbc #guidata_size
	sta guisp
	bcs @done
	dec guisp+1
@done:	rts
.endproc

;******************************************************************************
; COPYVARS
; Copies the GUI state to the zeropage
; OUT:
;  - .C: set if there is no GUI active
.proc copyvars
@stack=r0
	; peek the address of the top element and set @stack to it
	ldxy guisp
	cmpw #guistack
	beq @done

	sub16 #guidata_size
	stxy @stack

	; copy the data from the GUI stack to the zeropage area
	ldy #guidata_size-1
@l0:	lda (@stack),y
	sta guidata,y
	dey
	bpl @l0

	; get the number of items in the GUI (may change between activations)
	iny
	lda (numptr),y
	sta num

	cmp height
	bcs @done	; if # of items is > max height, use full height
	sta height	; else, only resize to the size needed to fit all items
	clc
@done:	rts
.endproc
