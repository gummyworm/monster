.include "asm.inc"
.include "labels.inc"
.include "macros.inc"
.include "memory.inc"
.include "util.inc"
.include "text.inc"
.include "zeropage.inc"


;--------------------------------------
; START
; Begins debugging at the given address
; Execution will continue until a BRK instruction occurs at which point the
; debugger will take over and allow for interactive debugging from the user.
; in:
;  - .XY: the address to begin debugging at
.export __debug_start
.proc __debug_start
.endproc

;--------------------------------------
; SAVE_DEBUGSTATE
; saves memory likely to be clobbered by the user's
; program (namely the screen)
.proc save_debugstate
.endproc

;--------------------------------------
; SAVE_PROGSTATE
; saves the user program's state so that the debugger may use the memory for
; screen etc.
.proc save_progstate
.endproc

;--------------------------------------
; RESTORE_DEBUGSTATE
; restores the saved debugger state
.proc restore_debugstate
.endproc

;--------------------------------------
; RESTORE_PROGSTATE
; restores the saved program state
.proc restore_progstate
.endproc

;--------------------------------------
; RUN
; Runs unitl the next BRK instruction and returns to the debugger
.proc run

.endproc

;--------------------------------------
; STEP
; Runs the next instruction from the .PC and returns to the debug prompt.
; This works by inserting a BRK instruction after
; the current instruction and RUNning.
.proc step

.endproc

;--------------------------------------
; WATCH
; Adds a watch for the given memory location. If this location is written to,
; execution will return to the debugger
; in:
;  - .XY: the address to add a watch for
.proc watch

.endproc


;--------------------------------------
; SHOWREGS
; prints the contents of the registers in the format
;  ADDR A  X  Y  SP NV-BDIZC
;  f59c 02 02 00 f7 00100000
.proc showregs
@reg_a=zp::asm
@reg_x=zp::asm+1
@reg_y=zp::asm+2
@reg_sp=zp::asm+3
@addr=zp::asm+4
@tmp=zp::asm+6
@flag=zp::asm+7
	; save the registers
	php
	txs
	sta @reg_a
	stx @reg_x
	sty @reg_y
	txs
	stx @reg_sp

	; display the register names
	ldxy #@regsline
	lda zp::cury
	jsr text::puts

	; .Y
	pla
	sta @reg_y
	jsr util::hextostr
	tya
	sta mem::linebuffer+11,x
	txa
	sta mem::linebuffer+12,x

	; .X
	pla
	sta @reg_x
	jsr util::hextostr
	tya
	sta mem::linebuffer+8,x
	txa
	sta mem::linebuffer+9,x

	; draw .A
	pla
	sta @reg_a
	jsr util::hextostr
	tya
	sta mem::linebuffer+5,x
	txa
	sta mem::linebuffer+6,x

	; status
	pla
	sta @tmp
	lda #$80
	sta @flag
	ldx #$00

@getstatus:
	lda @flag
	and @tmp
	bne :+
	lda #'0'
	skw
:	lda #'1'
	sta mem::linebuffer,x
:	lsr @flag
	beq @getaddr
	inx
	cpx #2
	beq :-
	bne @getstatus

@getaddr:
	pla
	sta @addr
	pla
	sta @addr+1
	jsr util::hextostr
	tya
	sta mem::linebuffer,x
	txa
	sta mem::linebuffer+1,x
	lda @addr
	jsr util::hextostr
	tya
	sta mem::linebuffer+2,x
	txa
	sta mem::linebuffer+3,x


	rts

@regsline: .byte "addr a  x  y  sp  nv-bdizc",0
.endproc
