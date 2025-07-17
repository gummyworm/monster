;;******************************************************************************
; DIRECTORY.ASM
; This file contains the code to list the directory of a disk and provide a
; menu for selecting a file to load.
;******************************************************************************

.include "config.inc"
.include "draw.inc"
.include "edit.inc"
.include "file.inc"
.include "irq.inc"
.include "key.inc"
.include "keycodes.inc"
.include "layout.inc"
.include "macros.inc"
.include "memory.inc"
.include "settings.inc"
.include "screen.inc"
.include "strings.inc"
.include "text.inc"
.include "util.inc"
.include "zeropage.inc"

.CODE

;******************************************************************************
; DIR VIEW
; Enters the directory viewer
; NOTE: this routine is limited to 128 files
; The max supported by the 1541 is 144 and this routine could easily be
; modified to support as many.
; It could also easily be modified to support more (e.g. for the 1581)
.export __dir_view
.proc __dir_view
@file=r8
@line=r8
@row=ra
@select=rb
@cnt=rc			; number of files extracted from listing
@scrollmax=rd		; maximum amount to allow scrolling
@scroll=re
@dirbuff=mem::spare+42		; 0-40 will be corrupted by text routines
				; 40-41 is load address
@namebuff=mem::spareend-40	; buffer for the file name
@fptrs=mem::spareend-(128*2)	; room for 128 files
	jsr irq::off

	ldxy #strings::dir
	jsr file::open_r_prg
	sta @file
	bcc :+
@err:	jmp irq::on		; error

	; load the directory into dirbuff
:	ldxy #@dirbuff-2
	stxy file::loadaddr
	jsr file::loadbin

	php
	lda @file
	jsr file::close		; close the directory file
	plp
	bcs @err

	jsr irq::on

	; reset the screen so that we can print the file names normally
	jsr scr::save

	ldxy #@dirbuff+5
	stxy @line

	ldx #$01
	stx @select
	dex			; .X=0
	stx @scroll
	stx @cnt
	stx @row

	; highlight disk name row
	lda #DEFAULT_RVS
	jsr draw::hline

	; and the bottom (status) row
	ldx #SCREEN_HEIGHT-1
	lda #DEFAULT_RVS
	jsr draw::hline

@getdiskname:
	ldx #@dirmsglen
:	lda @dirmsg-1,x
	sta @namebuff-1,x
	dex
	bne :-

	; parse the name of the file
	lda #>(@namebuff+@dirmsglen-1)
	sta r0+1
	lda #<(@namebuff+@dirmsglen-1)
	sta r0
	ldxy #@dirbuff+5
	jsr util::parse_enquoted_string
	jmp @l2

@l0:    incw @line	; skip line #
	incw @line

	lda @cnt
	asl
	tax
	lda @line+1
	sta @fptrs+1,x	; save pointer to this filename
	tay
	lda @line
	sta @fptrs,x
	tax

	; parse the name of the file
	lda #<@namebuff
	sta r0
	lda #>@namebuff
	sta r0+1
	jsr util::parse_enquoted_string

@l2:	ldy #$00
	lda (@line),y
	incw @line
	tay		; set .Z if 0
	bne @l2

@next:  ; read line link
	ldy #$00
	lda (@line),y
	bne :+
	iny
	lda (@line),y
	beq @cont
:	incw @line
	incw @line

	; print the line
	ldxy #@namebuff
	lda @row
	cmp #SCREEN_HEIGHT-1
	bcs :+			; if line isn't visible, don't draw
	jsr text::print
	inc @row

:	; next line
	inc @cnt
	jmp @l0

@cont:	; max a user can scroll is (# of files - SCREEN_HEIGHT-1)
	ldx #$00
	lda @cnt
	cmp #SCREEN_HEIGHT-1
	bcc :+
	sbc #SCREEN_HEIGHT-1
	tax
:	stx @scrollmax

	; highlight the first item
	jsr highlight_selection

; at the end of the screen, get user input for page up/down
@key:	jsr key::waitch
	cmp #K_QUIT
	bne @checkdown
@exit:  jmp scr::restore

; check the arrow keys (used to select a file)
@checkdown:
	jsr key::isdown
	bne @checkup
@rowdown:
	jsr unhighlight_selection
	inc @select
	lda @select
	cmp @row
	bcc @hiselection
	dec @select

@scrolldown:
	lda @scroll
	cmp @scrollmax
	bcs @hiselection

	inc @scroll

	; scroll up and redraw the bottom line
	ldx #$01
	lda #SCREEN_HEIGHT-1-1
	jsr text::scrollup

	ldxy #@namebuff
	stxy r0
	lda @scroll
	clc
	adc @select
	asl
	tax
	ldy @fptrs+1,x
	lda @fptrs,x
	tax
	jsr util::parse_enquoted_string
	lda #SCREEN_HEIGHT-1-1			; bottom row
	ldxy #@namebuff
	jsr text::print
	jmp @hiselection

@checkup:
	jsr key::isup
	bne @checkret

@rowup:
	jsr unhighlight_selection
	dec @select
	bne @hiselection
	inc @select		; lowest selectable row is 1
	lda @scroll
	beq @hiselection	; if nothing to scroll, continue

	; scroll down and redraw the bottom line
	lda #1
	ldx #SCREEN_HEIGHT-1-1
	jsr text::scrolldown

	ldxy #@namebuff
	stxy r0
	dec @scroll
	lda @scroll
	clc
	adc @select
	asl
	tax
	ldy @fptrs+1,x
	lda @fptrs,x
	tax
	jsr util::parse_enquoted_string
	ldxy #@namebuff
	lda #1			; top row
	jsr text::print

@hiselection:
	jsr highlight_selection
@nextkey:
	jmp @key

; check the RETURN key (to open a file)
@checkret:
	cmp #$0d		; select file and load
	beq @loadselection

; if 'G', go to bottom of directory list
@checkgototop:
	cmp #$67		; 'g'
	bne @checkbottom
	jsr key::waitch
	cmp #$67		; gg?
	bne @nextkey

	jsr unhighlight_selection

	ldx #$01
	stx @select
	dex			; .X=0
	stx @scroll
	beq @redraw		; branch always

; if 'G', go to bottom of directory list
@checkbottom:
	cmp #$47		; 'G'
	bne @nextkey

	jsr unhighlight_selection

	; set scroll to scrollmax
	lda @scrollmax
	sta @scroll

	; set selection (row) to min(SCREEN_HEIGHT-1, @cnt)
	ldx @cnt
	cpx #SCREEN_HEIGHT-1
	bcc :+
	ldx #SCREEN_HEIGHT-1
:	dex
	stx @select
@redraw:
	jsr @refresh
	jmp @hiselection

; user selected a file (RETURN), load it and exit the directory view
@loadselection:
	lda @select
	clc
	adc @scroll
	asl
	tax
	lda @fptrs,x
	ldy @fptrs+1,x
	tax
	lda #<@namebuff
	sta r0
	lda #>@namebuff
	sta r0+1
	jsr util::parse_enquoted_string

	jsr scr::restore
	ldxy #@namebuff
	jmp edit::load		; load the file

;--------------------------------------
; refresh (redraw) all visible rows
@refresh:
@i=r8
	lda #$01
	sta @i

:	lda @i
	clc
	adc @scroll
	asl
	tax
	ldy @fptrs+1,x
	lda @fptrs,x
	tax
	lda #<@namebuff
	sta r0
	lda #>@namebuff
	sta r0+1
	jsr util::parse_enquoted_string
	ldxy #@namebuff
	lda @i
	jsr text::print

	inc @i
	lda @i
	cmp #SCREEN_HEIGHT-1
	bcs @refresh_done
	adc @scroll
	cmp @cnt
	bcc :-

@refresh_done:
	rts

.PUSHSEG
.RODATA
@dirmsg: .byte "disk:",0
@dirmsglen=*-@dirmsg
.POPSEG
.endproc

;*******************************************************************************
; UNHIGHLIGHT SELECTION
; Unhighlights the selection (in rb)
; IN:
;   - rb: the row to highlight
.proc unhighlight_selection
@select=rb
	ldx @select
	lda #DEFAULT_900F
	jmp draw::hline		; deselect the current selection
.endproc

;*******************************************************************************
; HIGHLIGHT SELECTION
; Highlights the selection (in rb)
; IN:
;   - rb: the row to highlight
.proc highlight_selection
@select=rb
	ldx @select
	lda #DEFAULT_RVS
	jmp draw::hline		; deselect the current selection
.endproc
