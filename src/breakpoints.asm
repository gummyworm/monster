.include "bitmap.inc"
.include "config.inc"
.include "debug.inc"
.include "debuginfo.inc"
.include "draw.inc"
.include "edit.inc"
.include "errors.inc"
.include "gui.inc"
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

.CODE

;******************************************************************************
; EDIT
; Begins the breakpoint editor
.export __breakpoint_edit
.proc __breakpoint_edit
	; display the title
	ldxy #@menu
	lda #BRKVIEW_STOP
	jmp gui::listmenu

.PUSHSEG
.RODATA
@menu:
.byte HEIGHT				; max height
.word @getkey				; key handler
.word __brkpt_tostring			; get line handler
.word dbg::numbreakpoints		; # of breakpoints pointer
.word strings::breakpoints_title	; title
.POPSEG

;--------------------------------------
@getkey:
	cmp #K_RETURN
	bne @chkdel
	; toggle the breakpoint's active status
	txa
	jsr toggle_breakpoint
	RETURN_OK
@chkdel:
	cmp #K_DEL		; DEL
	bne @done
	ldy dbg::breakpointshi,x
	lda dbg::breakpointslo,x
	tax
	jsr dbg::removebreakpoint
@done:	RETURN_OK

;--------------------------------------
@getdata:
	jmp __brkpt_tostring
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

	; if the breakpoint is visible, toggle its color
	lda dbg::breakpoint_lineslo,x
	ldy dbg::breakpoint_lineshi,x
	tax
	jsr edit::src2screen
	tax
	bcs :+

	lda mem::rowcolors,x
	eor #(BREAKPOINT_OFF_COLOR)^(BREAKPOINT_ON_COLOR)
	sta mem::rowcolors,x

:	rts
.endproc

;******************************************************************************
; GETBYLINE
; Returns the ID of the breakpoint at the given line (if one exists)
; IN:
;  - .XY: the line # to get the breakpoint at
;  - .A:  the file ID of the breakpoint
; OUT:
;  - .A: the flags for the breakpoint
;  - .X: the ID of the breakpoint at the given line (if one exists)
;  - .C: set if there is no breakpoint at the given line
.export __brkpt_getbyline
.proc __brkpt_getbyline
@line=r2
@file=r4
	stxy @line
	sta @file

	; find the matching line #
	ldx dbg::numbreakpoints
	beq @notfound
	dex
@l0:	lda @file
	cmp dbg::breakpoint_fileids,x
	bne @next
	lda @line
	cmp dbg::breakpoint_lineslo,x
	bne @next
	lda @line+1
	cmp dbg::breakpoint_lineshi,x
	bne @next
	lda dbg::breakpoint_flags,x
	RETURN_OK

@next:	dex
	bpl @l0
@notfound:
	sec		; not found
@done:	rts
.endproc

;******************************************************************************
; TOSTRING
; Returns the rendered string for the given breakpoint.
; This is displayed in both the TUI and GUI breakpoint viewer
; IN:
;  - .A: the breakpoint to get the string for
; OUT:
;  - .XY: the rendered string for that watch
;  - .C:  set if there is no breakpoint for the given ID
.export __brkpt_tostring
.proc __brkpt_tostring
@offset=zp::tmp13
@addr=zp::tmp14
@namebuff=mem::spare+40
	sta @offset
	tax

	cpx dbg::numbreakpoints
	bcs @datadone

	; push the address of the breakpoint
	ldy dbg::breakpointshi,x
	lda dbg::breakpointslo,x
	pha
	tax
	tya
	pha

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
	sta r0+1
	lda #<@namebuff
	pha
	sta r0
	jsr lbl::getname

@lineno:
	ldx @offset
	; get the line number and file name
	lda dbg::breakpoint_lineslo,x
	pha
	lda dbg::breakpoint_lineshi,x
	pha

	lda dbg::breakpoint_fileids,x
	jsr dbgi::get_filename
	tya
	pha
	txa
	pha

@print:
	; display a symbol if the breakpoint is active
	ldx @offset
	ldy #BREAKPOINT_OFF_CHAR
	lda dbg::breakpoint_flags,x
	beq :+
	dey				; ldy #BREAKPOINT_CHAR
:	sty strings::breakpoints_line

	; push the breakpoint ID
	lda @offset
	pha

	; print the breakpoint info
	ldxy #strings::breakpoints_line
	jsr text::render

@datadone:
	rts
.endproc

;******************************************************************************
; NUM
; Returns the number of breakpoints
; OUT:
;  - .A: the number of breakpoints
.export __brkpt_num
.proc __brkpt_num
	lda dbg::numbreakpoints
	rts
.endproc
