;******************************************************************************
; DEBUGCMD.ASM
; This file contains the code for handling debug commands.
; This is accessed by typing '/' while in the main debugger view or one of the
; sub-views (memory editor, etc.)
;******************************************************************************

.include "debug.inc"
.include "errors.inc"
.include "expr.inc"
.include "line.inc"
.include "macros.inc"
.include "memory.inc"
.include "string.inc"
.include "util.inc"
.include "view.inc"
.include "vmem.inc"
.include "watches.inc"
.include "zeropage.inc"

;******************************************************************************
; DBGCMD RUN
; Handles the given debug command input. The given string is parsed and handled.
; This can be used to add/remove watches, breakpoints, etc.
; IN:
;  - .XY: the command to handle
; OUT:
;  - .C: set if the command wasn't understood or couldn't be executed
.export __dbgcmd_run
.proc __dbgcmd_run
@cnt=r0
	jsr str::toupper
	stxy zp::line

	ldy #$00
	ldx #$00
	sty @cnt

@l0:	lda (zp::line),y
	beq @cmdfound
	jsr util::is_whitespace
	beq @cmdfound

@l1:	cmp commands,x
	bne @next
	iny
	inx
	bne @l0

@next:	ldy #$00
:	lda commands,x
	inx
	cmp #$00
	bne :-

	inc @cnt
	lda @cnt
	cmp #num_commands
	bne @l0
@err:	sec
	rts		; no command found

@cmdfound:
	lda commands,x	; check if end of command
	bne @next	; if not 0, we're not actually done checking commands

	tya
	clc
	adc zp::line
	sta zp::line
	bcc :+
	inc zp::line+1

:	jsr line::process_ws
@run:	ldx @cnt
	lda commandslo,x
	sta zp::jmpvec
	lda commandshi,x
	sta zp::jmpvec+1
	jmp (zp::jmpvec)
.endproc

;******************************************************************************
; ADD WATCH
; /wa <expression> [, expression]
; Prompts for a start address and (optional) stop address and adds a watch
; at that location
; IN:
;  - .XY: the parameters for the command
.proc add_watch
@addr=r8
	; evaluate the expression to get start address
	jsr expr::eval
	bcs @err
	stxy @addr
	stxy r0

	jsr line::nextch
	beq @err

	; evaluate the 2nd expression (if any) to get stop address
	jsr expr::eval
	bcs @err
	stxy r0

@set:	ldxy @addr
	jsr watch::add		; add the watch
	jsr dbg::edit_watches
	clc
@err:	rts
.endproc

;******************************************************************************
; LIST WATCHES
; /w
; Lists all active watches
.proc list_watches
	; TODO
.endproc

;******************************************************************************
; REMOVE WATCH
; /wr <id>
; Deletes the watch with the given ID. The ID's can be found by listing watches
; with the w command or going to the watch viewer.
; IN:
;  - .XY: the parameters for the command
.proc remove_watch
@addr=r0
	; get the ID
	ldxy zp::line
	jsr atoi
	bcc @ok
@done:	rts
@ok:	cpy #$00
	bne @done	; there can't be > $ff watches
	txa
	jsr watch::remove
	jsr dbg::edit_watches
	clc
	rts
.endproc

;******************************************************************************
; ADD BREAK
; /br <expr>
; Adds a breakpoint at the given address/expression
; IN:
;  - .XY: the parameters for the command
.proc add_break
.endproc

;******************************************************************************
; REMOVE BREAK
; /br <id>
; Deletes the breakpoint with the given ID. The IDs can be found by listing
; breakpoints (bl) or going to the breakpoint viewer
; IN:
;  - .XY: the parameters for the command
.proc remove_break
.endproc

;******************************************************************************
; FILL
; /f <start>, <stop> a [, b, c, ...]
; Fills the range between the two addresses/expressions with the given fill
; list.  The given list is repeated in memory from the start address until the
; stop address is reached
; IN:
;  - .XY: the parameters for the command
.proc fill
@start=r4
@stop=r6
@listlen=r8
@i=r0
@list=ra
	; get the start address
	ldxy zp::line
	jsr expr::eval
	stxy @start
	bcs @done

	; move past separator
	jsr line::nextch
	beq @err

	; get the stop address
	jsr expr::eval
	stxy @stop
	bcs @done

	; move past separator
	jsr line::nextch
	beq @err

	lda #$00
	sta @listlen

	; get the fill values
@l0:	jsr expr::eval
	bcs @err
	cmp #$02		; 2 bytes?
	bcc :+
	tya
	ldy @listlen
	sta @list+1,y		; store MSB of the expression as a fill val
	inc @listlen
	skw			; don't reload listlen
:	ldy @listlen
	stx @list,y		; store LSB of expression as fill val
	inc @listlen
	jsr line::nextch
	bne @l0

	ldxy @start
	stxy view::addr

	lda #$00
	sta @i
	beq @chk		; branch to check if start == stop on 1st iter
@fill:	ldx @i
	lda @list,x
	ldxy @start
	jsr vmem::store
	ldxy @start
	inc @i
	lda @i
	cmp @listlen
	bcc :+
	lda #$00
	sta @i
:	incw @start
@chk:	ldxy @start
	cmpw @stop
	bne @fill

	; active mem viewer
	jsr view::mem
	RETURN_OK

@err:	sec
@done:	rts
.endproc

;******************************************************************************
.RODATA

;******************************************************************************
; commands
commands:
.byte "wa",0	; watch add
.byte "wr",0	; watch remove
.byte "ba",0	; breakpoint add
.byte "br",0	; breakpoint remove
.byte "f",0	; fill memory in the given address range with the given data
.byte "m",0	; move memory from the given address range to the target address
.byte "g",0	; goto given expression/address
.byte "c",0	; compare the memory in the two given range
.byte "h",0	; hunts the given address range for the given data

.linecont +
.define command_vectors add_watch, remove_watch, add_break, remove_break, fill
.linecont -
commandslo: .lobytes command_vectors
commandshi: .hibytes command_vectors
num_commands=*-commandshi
