.include "bitmap.inc"
.include "debug.inc"
.include "draw.inc"
.include "edit.inc"
.include "key.inc"
.include "keycodes.inc"
.include "labels.inc"
.include "layout.inc"
.include "macros.inc"
.include "memory.inc"
.include "source.inc"
.include "strings.inc"
.include "text.inc"
.include "zeropage.inc"

;******************************************************************************
; CONSTANTS
HEIGHT             = BRKVIEW_STOP-BRKVIEW_START-1
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
	; display the title
	ldxy #strings::breakpoints_title
	lda #MEMVIEW_START
	jsr text::print
	lda #MEMVIEW_START
	jsr bm::rvsline

	; if there are no breakpoints, just wait for user to quit
	lda dbg::numbreakpoints
	bne :++
:	jsr key::getch
	cmp #K_QUIT	; <-
	bne :-
	rts

:	lda #$00
	sta row
	sta scroll
	jmp @redraw	; highlight the first row

@loop:	jsr key::getch
	beq @loop
	cmp #K_QUIT	; <-
	bne @up
	rts

@up:	cmp #K_UP	; up
	bne @down
	dec row
	bpl @redraw
	inc row
	dec scroll
	bpl @redraw
	inc scroll
	jmp @redraw

@down:	cmp #K_DOWN	; down
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

@enter:	cmp #K_RETURN
	bne @del
	; toggle the breakpoint's active status
	lda row
	clc
	adc scroll
	jsr toggle_breakpoint
	jmp @redraw

@del:	cmp #K_DEL		; DEL
	bne @loop
	lda row
	clc
	adc scroll
	tax
	ldy dbg::breakpointshi,x
	lda dbg::breakpointslo,x
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
@namebuff=mem::spare+40
	; get the first visible breakpoint's offset
	lda scroll
	sta @cnt

@l0:	ldy @cnt
	cpy dbg::numbreakpoints
	bcc :+
	rts

:	; push the address of the breakpoint
	lda dbg::breakpointslo,y
	pha
	tax
	lda dbg::breakpointshi,y
	pha
	tay

	; get/push the symbol name for this address (if there is one)
	stxy @addr
	jsr lbl::by_addr
	bcc @getname
@noname:
	lda #>strings::question_marks
	pha
	lda #<strings::question_marks
	pha
	jmp @lineno

@getname:
	lda #>@namebuff
	pha
	sta zp::tmp0+1
	lda #<@namebuff
	pha
	sta zp::tmp0
	jsr lbl::getname

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

	lda #>strings::question_marks
	pha
	lda #<strings::question_marks
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

:	sty strings::breakpoints_line

	; print the breakpoint info
	ldxy #strings::breakpoints_line
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
.endproc

;******************************************************************************
; TOGGLE_BREAKPOINT
; IN:
;  - .A: the breakpoint to toggle active/inactive
.proc toggle_breakpoint
	tax
	lda dbg::breakpoint_flags,x
	eor #BREAKPOINT_ENABLED
	sta dbg::breakpoint_flags,x
	rts
.endproc
