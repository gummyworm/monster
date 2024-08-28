;******************************************************************************
; DEBUGCMD.ASM
; This file contains the code for handling debug commands.
; This is accessed by typing '/' while in the main debugger view or one of the
; sub-views (memory editor, etc.)
;******************************************************************************

.include "asm.inc"
.include "console.inc"
.include "debug.inc"
.include "errors.inc"
.include "expr.inc"
.include "finalex.inc"
.include "line.inc"
.include "macros.inc"
.include "memory.inc"
.include "string.inc"
.include "strings.inc"
.include "util.inc"
.include "view.inc"
.include "vmem.inc"
.include "watches.inc"
.include "zeropage.inc"

.segment "CONSOLE"

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
	CALL FINAL_BANK_MAIN, #str::toupper
	stxy zp::line

	ldy #$00
	ldx #$00
	sty @cnt

@l0:	lda (zp::line),y
	beq @cmdfound
	jsr is_whitespace
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

:	CALL FINAL_BANK_MAIN, #line::process_ws
@run:	ldx @cnt
	lda commandslo,x
	sta zp::jmpvec
	lda commandshi,x
	sta zp::jmpvec+1
	jmp (zp::jmpvec)
.endproc

;******************************************************************************
; ADD WATCH
; !wa <expression> [, expression]
; Prompts for a start address and (optional) stop address and adds a watch
; at that location
; IN:
;  - .XY: the parameters for the command
.proc add_watch
@addr=r8
	; evaluate the expression to get start address
	CALL FINAL_BANK_MAIN, #expr::eval
	bcs @err
	stxy @addr
	stxy r0

	CALL FINAL_BANK_MAIN, #line::nextch
	beq @err

	; evaluate the 2nd expression (if any) to get stop address
	CALL FINAL_BANK_MAIN, #expr::eval
	bcs @err
	stxy r0

@set:	ldxy @addr
	CALL FINAL_BANK_MAIN, #watch::add		; add the watch
	clc
@err:	rts
.endproc

;******************************************************************************
; LIST WATCHES
; !w
; Lists all active watches
.proc list_watches
	; TODO
.endproc

;******************************************************************************
; REMOVE WATCH
; !wr <id>
; Deletes the watch with the given ID. The ID's can be found by listing watches
; with the w command or going to the watch viewer.
; IN:
;  - .XY: the parameters for the command
.proc remove_watch
@addr=r0
	; get the ID
	ldxy zp::line
	CALL FINAL_BANK_MAIN, #atoi
	bcc @ok
@done:	rts
@ok:	cpy #$00
	bne @done	; there can't be > $ff watches
	txa
	CALL FINAL_BANK_MAIN, #watch::remove
	clc
	rts
.endproc

;******************************************************************************
; ADD BREAK
; !br <expr>
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
	CALL FINAL_BANK_MAIN, #expr::eval
	stxy @start
	bcc :+
@err:	sec
@done:	rts

:	; move past separator
	CALL FINAL_BANK_MAIN, #line::nextch
	beq @err

	; get the stop address
	CALL FINAL_BANK_MAIN, #expr::eval
	stxy @stop
	bcs @done

	; move past separator
	CALL FINAL_BANK_MAIN, #line::nextch
	beq @err

	lda #$00
	sta @listlen

	; get the fill values
@l0:	CALL FINAL_BANK_MAIN, #expr::eval
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
	CALL FINAL_BANK_MAIN, #line::nextch
	bne @l0

	ldxy @start
	stxy view::addr

	lda #$00
	sta @i
	beq @chk		; branch to check if start == stop on 1st iter
@fill:	ldx @i
	lda @list,x
	ldxy @start
	CALL FINAL_BANK_MAIN, #vmem::store
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
	RETURN_OK
.endproc

;******************************************************************************
.proc goto
.endproc

;******************************************************************************
.proc compare
.endproc

;******************************************************************************
.proc move
.endproc

;******************************************************************************
.proc hunt
.endproc

;******************************************************************************
; REGISTERS
; Displays the current contents of the registers.
.export __dbgcmd_regs
.proc __dbgcmd_regs
	ldxy #strings::debug_registers
	jsr con::puts
	CALL FINAL_BANK_MAIN, #dbg::regs_contents
	jsr con::puts
	RETURN_OK
.endproc

;******************************************************************************
; DISASM
; Disassembles from the given expression
.proc disasm
@addr=rd
@lines=rf
	; get the address to start disassembling at
	lda #20
	sta @lines
	CALL FINAL_BANK_MAIN, #expr::eval
	bcs @ret
	stxy @addr

@l0:	ldxy #$100
	stxy r0
	ldxy @addr
	CALL FINAL_BANK_MAIN, #asm::disassemble

	jsr @drawline
	dec @lines
	bne @l0
@done:	clc
@ret:	rts

@drawline:
	tax
	; push the disassembled string
	lda #>$100
	pha
	lda #<$100
	pha

	; push the address
	lda @addr
	pha
	lda @addr+1
	pha

	; update the address pointer
	txa		; get size of instruction
	clc
	adc @addr
	sta @addr
	bcc :+
	inc @addr+1

:	ldxy #@disasm_msg
	jmp con::puts

.RODATA
@disasm_msg:
	.byte $fe," ", $ff,0	; <address> <instruction>
.segment "CONSOLE"
.endproc

;******************************************************************************
; ASSEMBLE
; Assembles the given instruction at the address of the given expression
; e.g.
; >.A $1000, lda #$00
.proc assemble
@addr=rd
@lines=rf
	; get the address to assemble at
	lda #20
	sta @lines
	CALL FINAL_BANK_MAIN, #expr::eval
	bcs @ret		; return if address is invalid expression
	stxy @addr
	CALL FINAL_BANK_MAIN, #asm::setpc

	ldy #$00
	lda (zp::line),y
	beq @nexti		; if no instruction provided, prompt for more
	incw zp::line
	ldxy zp::line
	lda #FINAL_BANK_MAIN
	CALL FINAL_BANK_MAIN, #asm::tokenize	; assemble the instruction
	bcs @err
@nexti:	rts

@err:	CALL FINAL_BANK_MAIN, #err::get
	CALL FINAL_BANK_MAIN, #str::uncompress
	jsr con::puts		; print the error
	clc
@ret:	rts
.endproc

;******************************************************************************
; SHOWMEM
; Shows the contents of memory at the target of the given expression
.proc showmem
@addr=r6
@lines=r8
	; default to 8 lines (64 bytes total)
	lda #$08
	sta @lines
	lda #$00
	sta @lines+1

	; get the address to start showing memory at
	CALL FINAL_BANK_MAIN, #expr::eval
	bcs @ret
	stxy @addr

	; get the (optional) end address and divide by 8 to get # of lines
	ldy #$00
	lda (zp::line),y
	beq @l0
	incw zp::line		; move past separator
	CALL FINAL_BANK_MAIN, #expr::eval
	bcs @ret		; error
	sub16 @addr
	stx @lines
	tya
	lsr
	ror @lines
	lsr
	ror @lines
	lsr
	ror @lines
	sta @lines+1

	ldxy @lines
	cmpw #0
	bne @l0
	inc @lines		; minimum of 1 line

@l0:	ldxy @addr
	CALL FINAL_BANK_MAIN, #view::memline
	jsr @print

	; move to address for next row
	lda @addr
	clc
	adc #$08
	sta @addr
	bcc :+
	inc @addr+1
:	dec @lines		; (max 255 lines)
	bne @l0
	clc			; OK
@ret:	rts

@print:	jsr con::puts
.endproc

;******************************************************************************
; QUIT
; Quits the debugger, returning to the editor
.proc quit
	; eat the caller's return address 
	pla
	pla
	rts
.endproc

;******************************************************************************
; STEP
; Steps to the next instruction while debugging
.proc step
	CALL FINAL_BANK_MAIN, #dbg::step
	jmp quit		; let the debugger take over to comple STEP
.endproc

;******************************************************************************
; STEP_OVER
; Steps over the next instruction while debugging.  Subroutines (JSR) are
; treated as one instruction
; instruction
.proc step_over
.endproc

;******************************************************************************
; GO
; Continues program execution at the current PC
.proc go
.endproc

;******************************************************************************
; BACKTRACE
; Prints the call (JSR) stack (with symbols if possible).
; This is based on the contents of the stack, so any data on the stack may
; result in a bad rendering.
; An optional offset from the stack pointer can be given to adjust the
; stack's start location
.proc backtrace
.endproc

;******************************************************************************
; STEP_OUT
; Continues execution til the current subroutine returns with an RTS
.proc step_out
.endproc

;******************************************************************************
; IS_WHITESPACE
; Checks if the given character is a whitespace character
; IN:
;  - .A: the character to test
; OUT:
;  - .Z: set if if the character in .A is whitespace
.export is_whitespace
.proc is_whitespace
	cmp #$0d	; newline
	beq :+
	cmp #$09	; TAB
	beq :+
	cmp #' '
:	rts
.endproc

;******************************************************************************
; commands
commands:
.byte "wa",0	; watch add
.byte "wr",0	; watch remove
.byte "ba",0	; breakpoint add
.byte "br",0	; breakpoint remove
.byte "f",0	; fill memory in the given address range with the given data
.byte "move",0	; move memory from the given address range to the target address
.byte "g",0	; goto given expression/address
.byte "c",0	; compare the memory in the two given range
.byte "h",0	; hunts the given address range for the given data
.byte "r",0	; shows the contents of the registers
.byte "d",0	; disassembles from the given address
.byte "a",0	; assembles the given instruction given address
.byte "m",0	; show contents of memory at the given address
.byte "x",0	; quit the debugger
.byte "z",0	; step to the next instruction
.byte "n",0	; step over the next instruction
.byte "g",0	; go (continue program execution)
.byte "bt",0	; backgrace
.byte "zo",0	; step out

.linecont +
.define command_vectors add_watch, remove_watch, add_break, remove_break, \
	fill, move, goto, compare, hunt, __dbgcmd_regs, disasm, assemble, \
	showmem, quit, step, step_over, go, backtrace, step_out
.linecont -
commandslo: .lobytes command_vectors
commandshi: .hibytes command_vectors
num_commands=*-commandshi
