.include "../settings.inc"
.include "../../asmflags.inc"
.include "../../debug.inc"
.include "../../macros.inc"
.include "../../memory.inc"
.include "../../sim6502.inc"
.include "../../source.inc"
.include "../../string.inc"
.include "../../text.inc"
.include "../../util.inc"
.include "../../zeropage.inc"

COLMEM_ADDR=$9400
STATUS_COL=0		; start column for status line

.CODE

;*******************************************************************************
; UPDATE STATUSLINE
; Updates mem::statusline with new information (cursor pos, etc.)
; SIDE-EFFECTS:
;  - mem::statusline: contains the new status info
.export __ui_update_statusline
.proc __ui_update_statusline
.ifndef LORES
@filename=zp::text
@tmp=zp::text
@leftend=zp::text+2
@columnstart=STATUS_COL+3
@linestart=STATUS_COL+6
@sizestart=STATUS_COL+13
@modestart=STATUS_COL
	lda #' '
	ldx #39
@clr:	sta mem::statusline,x
	dex
	bpl @clr

	lda zp::curx
	jsr util::todec8

	; display the column
	ldy #$00
	cpx #'0'
	beq :+
	stx mem::statusline+@columnstart
	iny
:	sta mem::statusline+@columnstart,y

	lda #','
	sta mem::statusline+@columnstart+1,y

	; display current line
	lda text::statusfmt
	beq @fmt_default

@fmtxy:	lda zp::cury
	jsr util::todec8

	; display the row
	ldy #$00
	cpx #'0'
	beq :+
	stx mem::statusline+@linestart
	iny
:	sta mem::statusline+@linestart,y
	jmp @mode

@fmt_default:
	ldxy src::line
	jsr util::todec
	ldx #$00
@l0:	lda mem::spare,x
	beq :+
	sta mem::statusline+@linestart,x
	inx
	bne @l0

:	lda #'/'
	sta mem::statusline+@linestart,x

	stx @tmp

	; display total lines
	ldxy src::lines
	jsr util::todec

	ldx @tmp
	ldy #$00
@l1:	lda mem::spare,y
	beq @mode
	sta mem::statusline+@linestart+1,x
	iny
	inx
	bne @l1

@mode:	; add the editor mode
	lda text::statusmode
	sta mem::statusline+@modestart

	ldy #$00
@copyinfo:
	lda mem::statusinfo,y
	beq @copy_filename
	sta mem::statusline+@linestart+2,x
	iny
	inx
	cpx #38
	bcc @copyinfo

@copy_filename:
	; filename
	lda src::activebuff
	jsr src::filename
	stxy @filename
	jsr str::len
	tay
	dey
	beq @drive
	ldx #39
:	lda (@filename),y
	sta mem::statusline,x
	dex
	dey
	bpl :-

; display a '*' if the file is dirty
	jsr src::isdirty
	beq @drive
	lda #'*'
	sta mem::statusline,x

; display the drive name followed by a colon to the left of the filename
@drive:	lda #':'
	sta mem::statusline-1,x

	stx @tmp
	lda zp::device
	jsr util::todec8
	ldy @tmp
	sta mem::statusline-2,y
	cpx #'0'
	beq :+
	txa
	dey
	sta mem::statusline-2,y
:	lda #'#'
	sta mem::statusline-3,y
@done:	rts
.else
	rts
.endif
.endproc

;*******************************************************************************
; REGS CONTENTS
; Returns a line containing the contents of the registers
; OUT: mem::linebuffer: a line of text containing the values for each tegister
;      matches the format: PC  A  X  Y  SP NV-BDIZC ADDR
.export __ui_regs_contents
.proc __ui_regs_contents
@tmp=zp::disasm+6
@flag=zp::disasm+7
@buff=mem::linebuffer2
	ldy #39
	lda #' '
:	sta @buff,y
	dey
	bpl :-

	; draw .Y
	lda sim::reg_y
	jsr util::hextostr
	sty @buff+11
	stx @buff+12

	; draw .X
	lda sim::reg_x
	jsr util::hextostr
	sty @buff+8
	stx @buff+9

	; draw .A
	lda sim::reg_a
	jsr util::hextostr
	sty @buff+5
	stx @buff+6

	; draw .P (status)
	lda sim::reg_p
	sta @tmp
	lda #$a1
	sta @flag
	ldx #$00

@getstatus:
	and @tmp
	bne :+
	lda #'0'
	skw
:	lda #'1'
	sta @buff+17,x
:	lsr @flag
	inx
	cpx #2
	beq :-
	lda @flag
	bne @getstatus

@getsp:
	lda sim::reg_sp
	jsr util::hextostr
	sty @buff+14
	stx @buff+15

@getaddr:
	lda sim::pc+1
	jsr util::hextostr
	sty @buff
	stx @buff+1

	lda sim::pc
	jsr util::hextostr
	sty @buff+2
	stx @buff+3

; if registers were affected, highlight them (GUI only)
	lda __debug_interface
	bne @colordone

	ldx #TEXT_COLOR
	lda sim::affected
	and #OP_REG_A
	beq :+
	ldx #DEBUG_REG_CHANGED_COLOR
:	stx COLMEM_ADDR+(20*$b)+2
	stx COLMEM_ADDR+(20*$b)+3

	ldx #TEXT_COLOR
	lda sim::affected
	and #OP_REG_X
	beq :+
	ldx #DEBUG_REG_CHANGED_COLOR
:	stx COLMEM_ADDR+(20*$b)+4

	ldx #TEXT_COLOR
	lda sim::affected
	and #OP_REG_Y
	beq :+
	ldx #DEBUG_REG_CHANGED_COLOR
:	stx COLMEM_ADDR+(20*$b)+5
	stx COLMEM_ADDR+(20*$b)+6

	ldx #TEXT_COLOR
	lda sim::affected
	and #OP_STACK
	beq :+
	ldx #DEBUG_REG_CHANGED_COLOR
:	stx COLMEM_ADDR+(20*$b)+7

	; if memory was WRITTEN to, highlight it as well
	ldx #TEXT_COLOR
	lda sim::affected
	and #OP_STORE
	beq :+
	ldx #DEBUG_REG_CHANGED_COLOR
:	stx COLMEM_ADDR+(20*$b)+13
	stx COLMEM_ADDR+(20*$b)+14

@colordone:
; if memory was loaded or stored, show the effective address
@memaddr:
	lda sim::affected
	and #OP_LOAD|OP_STORE
	bne :+
	lda #'-'
	sta @buff+26
	sta @buff+27
	sta @buff+28
	sta @buff+29
	bne @clk

:	lda sim::effective_addr+1
	jsr util::hextostr
	sty @buff+26
	stx @buff+27
	lda sim::effective_addr
	jsr util::hextostr
	sty @buff+28
	stx @buff+29

@clk:	lda dbg::sw_valid
	bne :+
	; if stopwatch is invalid, show ???
	lda #'?'
	sta @buff+37
	sta @buff+38
	sta @buff+39
	bne @print

:	ldx sim::stopwatch
	ldy sim::stopwatch+1
	lda sim::stopwatch+2
	jsr util::todec24

	; set the last character of the stopwatch always
	lda $100+7
	sta @buff+32+7

	; ignore leading zeroes
	ldx #$00
:	inx
	cpx #$07
	beq @print
	lda $100,x
	cmp #'0'
	beq :-

@cpyclk:
	lda $100,x
	sta @buff+32,x
	cpx #$07
	inx
	bcc @cpyclk

@print:	lda #$00
	sta @buff+40
	ldxy #@buff
	rts
.endproc
