;******************************************************************************
; SYMVIEW.ASM
; This file contains the code for the symbol viewer, which allows the user to
; see all (non-local/anonymous) symbols in their program.
; Using debug information, the user can navigate to the location of the symbol

.include "config.inc"
.include "debuginfo.inc"
.include "debug.inc"
.include "draw.inc"
.include "edit.inc"
.include "errors.inc"
.include "key.inc"
.include "finalex.inc"
.include "keycodes.inc"
.include "labels.inc"
.include "macros.inc"
.include "screen.inc"
.include "strings.inc"
.include "text.inc"
.include "zeropage.inc"

HEIGHT     = 23
MAX_LABELS = 768

SORT_ALPHA = 0	; sort by label name alphabetically
SORT_ADDR  = 1	; sort by label address

lbl      = r5
addr     = r9
filename = zp::tmp10
line     = zp::tmp12
sortby   = zp::tmp14
name     = $100

.CODE

;******************************************************************************
; GET ITEM
; Returns the label ID at the given index based on the current sortby value.
; IN:
;   - .XY: the item index
; OUT:
;   - lbl: the ID of the label at a given index
;   - addr: the address of the label at the requested index
;   - name: the name of the symbol
;   - filename: the name of the file containing the symbol
;   - line: the line number that contains the symbol
;   - .XY: the ID of the label at the given index (determined by sortby)
.proc get_item
	; destination buffer for getname
	lda #<$100
	sta r0
	lda #>$100
	sta r0+1

	lda sortby
	beq @sortalpha
@sortaddr:
	jsr lbl::idbyaddrindex	; lookup via sorted addresses
@sortalpha:
	stxy lbl		; store the ID for the label
	jsr lbl::getname	; read the symbol name into buffer ($100)
	ldxy lbl
	jsr lbl::getaddr	; get the symbol address
	stxy addr
	jsr dbgi::addr2line	; get file and line #
	bcs @done		; if no mapping, skip 
				; (this will filter out constants)
	stxy line
	jsr dbgi::get_filename
	stxy filename
@done:	rts
.endproc

;******************************************************************************
; ENTER
; Enters the symbol viewer.
.export __symview_enter
.proc __symview_enter
@tmp=r5
@scroll=r7
@row=rb
@selection=zp::tmp15
@prevscroll=zp::tmp16
	jsr scr::reset
	lda #$00
	sta sortby
	sta @selection

@start:	ldxy lbl::num
	cmpw #0
	bne :+
	jmp @done

:	ldxy #$00
	stxy @scroll

@l0:	jsr edit::clear
	ldxy #@info
	lda #23
	jsr text::print
	lda #$00
	sta @row

@l1:	ldxy @scroll
	jsr get_item		; get the item for this row (@scroll)

	lda line		; push line #
	pha
	lda line+1
	pha

	tya			; push filename
	pha
	txa
	pha

@linedone:
	lda #>name		; push the symbol name (written by getname)
	pha
	lda #<name
	pha

	lda addr
	pha
	lda addr+1
	pha

	ldxy #@sym_line
	lda @row
	jsr text::print

	inc @row
	lda @row
	cmp #HEIGHT
	beq @done		; end of screen
@nextitem:
	incw @scroll
	ldxy @scroll
	cmpw lbl::num
	bne @l1

; the screen has been drawn, enter the main user loop
@done:  ; @scroll is now set to the index of the item at the bottom
@menu:	ldx @selection
	lda #DEFAULT_RVS
	jsr draw::hline		; unhighlight the current selection

@menuloop:
	; wait for a key
	jsr key::getch
	beq @menuloop

	pha
	ldx @selection
	lda #DEFAULT_900F
	jsr draw::hline
	pla

	cmp #$85		; F1 (change sort order)
	beq @changesort
	cmp #$11		; down
	beq @down
	cmp #$91		; up
	beq @up
	cmp #K_RETURN		; RETURN
	beq @select
	cmp #K_QUIT		; <-
	bne @done
	jmp scr::restore

@select:
	; TODO: fix
	lda @scroll
	sec
	sbc @row
	sta @scroll
	lda @scroll+1
	sbc #$00
	sta @scroll+1
	jmp dbg::gotoaddr	; go to the line of the symbol definition
@down:	inc @selection
	lda @selection
	cmp @row
	bcc @menu

	lda @scroll
	; sec
	adc #$01	; +2
	tax
	lda @scroll+1
	adc #$00
	tay
	cmpw lbl::num
	bcc @scrolldown

	; can't scroll, we're at the end of labels
	dec @selection
	jmp @menu

@scrolldown:
	stxy @scroll
	lda #$00
	sta @selection
	jmp @l0

@up:	dec @selection
	bpl @menu
	inc @selection
	lda @scroll+1
	bne @scrollup
	lda @scroll
	cmp #HEIGHT
	bcc @menu

@scrollup:
	; @scroll -= (@row + HEIGHT-1)
	lda @scroll
	sec
	sbc @row
	bcs :+
	dec @scroll+1
:	sbc #HEIGHT
	sta @scroll
	bcs :+
	dec @scroll+1

:	lda #HEIGHT-1
	sta @selection
	jmp @l0

@changesort:
	lda sortby
	eor #$01
	sta sortby
	jmp @start

.PUSHSEG
.RODATA
	@sym_line: .byte "$", ESCAPE_VALUE, ": ", ESCAPE_STRING, " ", ESCAPE_STRING, " ", "l:", ESCAPE_VALUE_DEC, 0
@info:     .byte "f1 sort",0
.POPSEG
.endproc
