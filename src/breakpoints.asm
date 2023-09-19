.include "bitmap.inc"
.include "debug.inc"
.include "draw.inc"
.include "key.inc"
.include "labels.inc"
.include "layout.inc"
.include "macros.inc"
.include "source.inc"
.include "text.inc"
.include "zeropage.inc"

;******************************************************************************
HEIGHT = BRKVIEW_STOP - BRKVIEW_START - 1
BREAKPOINT_ENABLED = 1

.BSS
;******************************************************************************
scroll: .byte 0
row:	.byte 0

.CODE
;******************************************************************************
; INIT
; Initializes the breakpoint editor
.export __breakpoint_init
.proc __breakpoint_init
	lda #$00
	sta row
	sta scroll
	rts
.endproc

;******************************************************************************
; EDIT
; Begins the breakpoint editor loop.
.export __breakpoint_edit
.proc __breakpoint_edit
	lda #$00
	sta row
	sta scroll
	jmp @redraw	; highlight the first row

@loop:	jsr key::getch
	beq @loop
	cmp #$5f	; <-
	bne @up
	rts

@up:	cmp #$91	; up
	bne @down
	dec row
	bpl @redraw
	inc row
	dec scroll
	bpl @redraw
	inc scroll
	jmp @redraw

@down:	cmp #$11	; down
	bne @enter
	inc row
	lda row
	cmp dbg::numbreakpoints
	bcs :+
	cmp #HEIGHT
	bcc @redraw
:	dec row
	inc scroll
	lda scroll
	clc
	adc row
	cmp dbg::numbreakpoints
	bcc @redraw
	dec scroll
	jmp @redraw

@enter:	cmp #$0d
	bne @del
	; toggle the breakpoint's active status
	lda row
	clc
	adc scroll
	jsr toggle_breakpoint
	jmp @redraw

@del:	cmp #$11		; DEL
	bne @loop
	lda row
	clc
	adc scroll
	tax
	ldy dbg::breakpoints+1,x
	lda dbg::breakpoints,x
	tax
	jsr dbg::removebreakpoint

@redraw:
	jsr __breakpoint_view
	lda row
	clc
	adc #BRKVIEW_START+1
	jsr bm::rvsline
	jmp @loop
.endproc

;******************************************************************************
; VIEW
; Displays the breakpoints
.export __breakpoint_view
.proc __breakpoint_view
@cnt=zp::tmp13
@addr=zp::tmp14
@file=zp::tmp16
	lda #BRKVIEW_START
	jsr draw::hline

	; get the last breakpoint
	lda scroll
	sta @cnt

@l0:	lda @cnt
	cmp dbg::numbreakpoints
	bcs @done
	asl

	; push the address of the breakpoint
	tay
	lda dbg::breakpoints,y
	pha
	tax
	lda dbg::breakpoints+1,y
	pha
	tay

	; get/push the symbol name for this address (if there is one)
	stxy @addr
	jsr lbl::by_addr
	bcc @getname
@noname:
	lda #>@unknown_symbol
	pha
	lda #<@unknown_symbol
	pha
	jmp @lineno

@getname:
	jsr lbl::name_by_id
	tya
	pha
	txa
	pha

@lineno:
	; get the line number and file
	ldxy @addr
	jsr dbg::addr2line
	bcc @getlineno

; an error should not occur (there must be debug info to set a breakpoint,
; but we handle this scenario by just pushing 0 for the line number and ???
; for the filename
@err:	lda #$00
	pha
	pha

	lda #>@unknown_symbol
	pha
	lda #<@unknown_symbol
	pha
	jmp @print

@getlineno:
	sta @file
	; push the line # and filename
	txa
	pha
	tya
	pha
	lda @file
	jsr dbg::get_filename
	tya
	pha
	txa
	pha

@print:; display a symbol if the breakpoint is active
	ldx @cnt
	ldy #BREAKPOINT_OFF_CHAR
	lda dbg::breakpoint_flags,x
	beq :+
	ldy #BREAKPOINT_CHAR

:	sty @brkline

	; print the breakpoint info
	ldxy #@brkline
	lda @cnt
	sec
	sbc scroll
	adc #BRKVIEW_START	; +1 (carry set)
	jsr text::print

	lda @cnt
	sec
	sbc scroll
	cmp #HEIGHT-1
	bcs @done
	inc @cnt
	jmp @l0		; next breakpoint
@done:	rts

; <filename> l: <line no.> <symbol> : <addr>
; <addr>:<sybmol> L:<line no.> in <filename
@brkline:
.byte " ", ESCAPE_STRING, " l:", ESCAPE_VALUE_DEC, " [", ESCAPE_STRING, "] $", ESCAPE_VALUE,0
@unknown_symbol: .byte "???",0
.endproc

;******************************************************************************
; TOGGLE_BREAKPOINT
; TODO: check that file in editor is same as file of breakpoint
; IN:
;  - .A: the breakpoint to toggle active/inactive
.proc toggle_breakpoint
@line=zp::tmp0
@startline=zp::tmp2
@endline=zp::tmp4
@row=zp::tmp6
	pha

	tax
	lda dbg::breakpoint_flags,x
	eor #BREAKPOINT_ENABLED
	sta dbg::breakpoint_flags,x

	; get the line # of the breakpoint
	txa
	asl
	tax
	ldy dbg::breakpoints+1,x
	lda dbg::breakpoints,x
	tax
	jsr dbg::addr2line
	bcs @done		; no line #
	stxy @line

	; check if the breakpoint is visible in the editor
	lda src::line
	sec
	sbc zp::cury
	sta @startline
	lda src::line+1
	sbc #$00
	sta @startline+1

	lda @startline
	clc
	adc #BRKVIEW_START
	sta @endline
	lda @startline+1
	adc #$00
	sta @endline+1

	pla

	ldxy @line
	cmpw @startline
	bcc @done
	cmpw @endline
	bcs @done

	tax

	lda @line
	sec
	sbc @startline		; will be [0, BRKVIEW_START)
	sta @row

	lda #TEXT_REPLACE
	sta text::insertmode

	lda dbg::breakpoint_flags,x
	and #BREAKPOINT_ENABLED
	beq :+
	lda #BREAKPOINT_CHAR
	.byte $2c
:	lda #BREAKPOINT_OFF_CHAR
	ldx #$00
	ldy @row
	jsr text::plot

@done:	rts
.endproc
