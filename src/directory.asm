;******************************************************************************
; DIRECTORY.ASM
; This file contains the code to list the directory of a disk and provide a
; menu for selecting a file to load.
;******************************************************************************

.include "bitmap.inc"
.include "config.inc"
.include "draw.inc"
.include "edit.inc"
.include "file.inc"
.include "irq.inc"
.include "key.inc"
.include "keycodes.inc"
.include "macros.inc"
.include "memory.inc"
.include "screen.inc"
.include "strings.inc"
.include "text.inc"
.include "util.inc"
.include "zeropage.inc"

SCREEN_H = 23

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
@scrollmax=rc		; maximum amount to allow scrolling
@scroll=rd
@dirbuff=mem::spare+42		; 0-40 will be corrupted by text routines
				; 40-41 is load address
@namebuff=mem::spareend-40	; buffer for the file name
@fptrs=mem::spareend-(128*2)	; room for 128 files
	jsr irq::disable

	ldxy #strings::dir
	jsr file::open_r_prg
	sta @file
	bcc :+
@err:	jmp irq::raster		; error

	; load the directory into dirbuff
:	ldxy #@dirbuff-2
	stxy __file_load_address
	jsr file::loadbin
	bcs @err

	lda @file
	jsr file::close

	jsr irq::raster

	; reset the screen so that we can print the file names normally
	jsr scr::reset

	ldxy #@dirbuff+5
	stxy @line

	lda #$00
	sta @scroll
	sta @cnt
	sta @row
	lda #$01
	sta @select

	; highlight disk name row
	ldx #$00
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
	cmp #SCREEN_H
	bcs :+			; if line isn't visible, don't draw
	jsr text::print
	inc @row

:	; next line
	inc @cnt
	jmp @l0

@cont:	; max a user can scroll is (# of files - SCREEN_H)
	ldx #$00
	lda @cnt
	cmp #SCREEN_H
	bcc :+
	sbc #SCREEN_H
	tax
:	stx @scrollmax

	; highlight the first item
	ldx @select
	lda #DEFAULT_RVS
	jsr draw::hline

; at the end of the screen, get user input for page up/down
@key:	jsr key::getch
	beq @key
	cmp #K_QUIT
	bne @checkdown
	jmp @exit

; check the arrow keys (used to select a file)
@checkdown:
	jsr key::isdown
	bne @checkup
@rowdown:
	ldx @select
	lda #DEFAULT_900F
	jsr draw::hline		; deselect the current selection
	inc @select
	lda @select
	cmp @row
	bcc @hiline
	dec @select

@scrolldown:
	lda @scroll
	cmp @scrollmax
	bcs @hiselection

	inc @scroll

	; scroll up and redraw the bottom line
	ldx #1
	lda #SCREEN_H-1
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
	lda #SCREEN_H-1			; bottom row
	ldxy #@namebuff
	jsr text::print
	jmp @hiselection

@checkup:
	jsr key::isup
	bne @checkret

@rowup:
	ldx @select
	lda #DEFAULT_900F
	jsr draw::hline		; deselect the current selection
	dec @select
	bne @hiselection
	inc @select		; lowest selectable row is 1
	lda @scroll
	beq @hiselection	; if nothing to scroll, continue

	; scroll down and redraw the bottom line
	lda #1
	ldx #SCREEN_H-1
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
	lda @select
@hiline:
	tax
	lda #DEFAULT_RVS
	jsr draw::hline
	jmp @key

; check the RETURN key (to open a file)
@checkret:
	cmp #$0d		; select file and load
	beq @loadselection
	cmp #$5f		; <-
	beq @exit
	jmp @key

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

@exit:  jmp scr::restore
.PUSHSEG
.RODATA
@dirmsg: .byte "disk:",0
@dirmsglen=*-@dirmsg
.POPSEG
.endproc

