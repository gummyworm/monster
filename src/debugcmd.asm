;*******************************************************************************
; DEBUGCMD.ASM
; This file contains the code for handling debug commands.
; Debug commands are those invoked by the TUI (monitor) interface.
;*******************************************************************************

.include "asm.inc"
.include "breakpoints.inc"
.include "console.inc"
.include "cursor.inc"
.include "debug.inc"
.include "debuginfo.inc"
.include "edit.inc"
.include "errors.inc"
.include "expr.inc"
.include "file.inc"
.include "flags.inc"
.include "labels.inc"
.include "line.inc"
.include "irq.inc"
.include "macros.inc"
.include "memory.inc"
.include "sim6502.inc"
.include "string.inc"
.include "strings.inc"
.include "text.inc"
.include "util.inc"
.include "view.inc"
.include "vmem.inc"
.include "watches.inc"
.include "zeropage.inc"

.include "ram.inc"

CMD_BUFF = $101

.segment "CONSOLE"

;*******************************************************************************
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
	CALL FINAL_BANK_MAIN, str::toupper	; commands are case insensitive
	stxy zp::line

	ldy #$00
	ldx #$00
	sty @cnt

	; eat whitespace to get to the command
@l0:	lda (zp::line),y
	beq @cmdfound
	jsr is_whitespace
	beq @cmdfound

	; check if command matches one in the command list
@l1:	cmp commands,x
	bne @next
	iny
	inx
	bne @l0

@next:	; command didn't match, move .X to index of next one to check
	ldy #$00
:	lda commands,x
	inx
	cmp #$00
	bne :-

	inc @cnt
	lda @cnt
	cmp #num_commands
	bne @l0
@err:	RETURN_ERR ERR_INVALID_COMMAND

@cmdfound:
	lda commands,x	; check if end of command
	bne @next	; if not 0, we're not actually done checking commands

	tya
	clc
	adc zp::line
	sta zp::line
	bcc :+
	inc zp::line+1

:	jsr process_ws
@run:	ldx @cnt
	lda commandslo,x
	sta zp::jmpvec
	lda commandshi,x
	sta zp::jmpvec+1
	jmp (zp::jmpvec)
.endproc

;*******************************************************************************
; ADD WATCH LOAD
; was <expression> [, expression]
; Prompts for a start address and (optional) stop address and adds a watch
; at that location
; The set watch will only trigger when the watched value is read from
; IN:
;  - .XY: the parameters for the command
.proc add_watch_load
	lda #WATCH_LOAD
	bne add_watch
	; fall through
.endproc

;*******************************************************************************
; ADD WATCH STORE
; was <expression> [, expression]
; Prompts for a start address and (optional) stop address and adds a watch
; at that location
; The set watch will only trigger when the watched value is written to
; IN:
;  - .XY: the parameters for the command
.proc add_watch_store
	lda #WATCH_STORE
	skw
	; fall through
.endproc

;*******************************************************************************
; ADD WATCH
; wa <expression> [, expression]
; Prompts for a start address and (optional) stop address and adds a watch
; at that location
; IN:
;  - .XY: the parameters for the command
.proc add_watch
@addr=r8
@mode=ra
	lda #WATCH_LOAD|WATCH_STORE
	sta @mode

	; evaluate the expression to get start address
	jsr eval
	bcs @err
	stxy @addr
	stxy r0

	jsr eat_whitespace
	ldy #$00
	lda (zp::line),y
	cmp #$00				; was there a 2nd argument?
	beq @set				; if not, continue

	; evaluate the 2nd expression (if any) to get stop address
	jsr eval
	bcs @err
	stxy r0

@set:	ldxy @addr
	lda @mode
	CALL FINAL_BANK_MAIN, watch::add		; add the watch
	clc
@err:	rts
.endproc

;*******************************************************************************
; LIST WATCHES
; w
; Lists all active watches
.proc list_watches
@cnt=zp::tmp13
@num=zp::tmp14
	lda #$00
	sta @cnt

	CALL FINAL_BANK_MAIN, watch::getdata
	stx @num
	cpx #$00
	beq @done

@loop:	lda @cnt
	jsr @print
	inc @cnt
	lda @cnt
	cmp @num
	bcc @loop
@done:	RETURN_OK

@print:	CALL FINAL_BANK_MAIN, watch::tostring
	jmp con::puts
.endproc

;******************************************************************************
; REMOVE WATCH
; wr <id>
; Deletes the watch with the given ID. The ID's can be found by listing watches
; with the w command or going to the watch viewer.
; IN:
;  - .XY: the parameters for the command
.proc remove_watch
	; get the ID
	ldxy zp::line
	CALL FINAL_BANK_MAIN, atoi
	bcs @done
@ok:	cpy #$00
	bne @done				; there can't be > $ff watches
	txa
	CALL FINAL_BANK_MAIN, watch::remove
	clc
@done:	rts
.endproc

;*******************************************************************************
; LIST BREAKPOINTS
; b
; List all breakpoints
.proc list_breakpoints
@cnt=zp::debuggertmp
@num=zp::debuggertmp+1
	lda #$00
	sta @cnt

	CALL FINAL_BANK_MAIN, brkpt::num
	sta @num
	cmp #$00
	beq @done

@loop:	lda @cnt
	jsr @print
	inc @cnt
	lda @cnt
	cmp @num
	bcc @loop
@done:	RETURN_OK

@print:	CALL FINAL_BANK_MAIN, brkpt::tostring
	jmp con::puts
.endproc

;*******************************************************************************
; ADD BREAK ADDR
; ba <expr>
; Adds a breakpoint at the given address/expression
; IN:
;  - .XY: the parameters for the command
.proc add_break_addr
@addr=zp::debuggertmp
@line=zp::debuggertmp+2
	; evaluate the expression to get break address
	jsr eval
	bcs @done

	stxy @addr

	; get the line/file for the given address
	CALL FINAL_BANK_MAIN, dbgi::addr2line
	bcs @skip_line
	pha			; save file id
	stxy @line
	CALL FINAL_BANK_MAIN, dbg::setbrkatline

	lda @addr
	sta r0
	lda @addr+1
	sta r0+1
	pla			; restore file id
	ldxy @line
	CALL FINAL_BANK_MAIN, dbg::brksetaddr
	RETURN_OK

@skip_line:
	; no line number for the address requested
	ldxy @addr
	CALL FINAL_BANK_MAIN, dbg::setbrkataddr
	clc						; ok
@done:	rts
.endproc

;*******************************************************************************
; ADD BREAK LINE
; bl file <expr>
; Adds a breakpoint at the given line/expression
; IN:
;  - .XY: the parameters for the command
.proc add_break_line
@fileid=zp::debuggertmp
@line=zp::debuggertmp+1
	; get the filename of the file to add the breakpoint in
	skb
@err:	rts
	ldy #$ff
:	iny
	lda (zp::line),y
	beq @err		; no filename
	jsr is_whitespace
	bne :-

	lda #$00
	sta (zp::line),y
	tya
	ldxy zp::line
	sec			; +1
	adc zp::line
	sta zp::line
	bcc :+
	inc zp::line+1

:	CALL FINAL_BANK_MAIN, dbgi::getfileid
	bcs @done				; no file found
	pha					; save file ID

	; move past the filename and whitespace
	jsr process_ws

	; evaluate the expression to get break line
	jsr eval
	stxy @line
	pla
	sta @fileid
	bcs @err		; invalid line #

	; add the breakpoint
	lda @fileid
	CALL FINAL_BANK_MAIN, dbg::setbrkatline

	; get the address for the given line
	ldxy @line
	lda @fileid
	CALL FINAL_BANK_MAIN, dbgi::line2addr
	bcs @done				; no matching line found

	stxy r0
	; map the address we looked up to the line
	ldxy @line
	lda @fileid
	CALL FINAL_BANK_MAIN, dbg::brksetaddr
	clc
@done:	rts
.endproc

;*******************************************************************************
; REMOVE BREAK
; br <id>
; Deletes the breakpoint with the given ID. The IDs can be found by listing
; breakpoints (bl) or going to the breakpoint viewer
; IN:
;  - .XY: the parameters for the command
.proc remove_break
	; get the ID
	ldxy zp::line
	CALL FINAL_BANK_MAIN, atoi
	bcs @done

@ok:	cpy #$00
	bne @done	; there can't be > $ff breakpoints

	; .X is the ID to remove
	CALL FINAL_BANK_MAIN, dbg::removebreakpointbyid
	clc
@done:	rts
.endproc

;******************************************************************************
; POKE
; p <address> <value>
; Sets the given address to the provided value
; IN:
;  - .XY: the parameters for the command
.proc poke
@addr=zp::debuggertmp
	jsr eval
	bcs @ret
	stxy @addr

	jsr eat_whitespace

	; get the byte value
	jsr eval
	cmp #$02
	bcc :+
	RETURN_ERR ERR_OVERSIZED_OPERAND

:	txa
	ldxy @addr
	jsr vmem_store
	clc		; ok
@ret:	rts
.endproc

;******************************************************************************
; FILL
; f <start> <stop> a [, b, c, ...]
; Fills the range between the two addresses/expressions with the given fill
; list.  The given list is repeated in memory from the start address until the
; stop address is reached
; IN:
;  - .XY: the parameters for the command
.proc fill
@start   = zp::debuggertmp
@stop    = zp::debuggertmp+2
@listlen = zp::debuggertmp+4
@list    = zp::debuggertmp+6
@i       = r0
	jsr get_range
	bcs @ret

	jsr parse_exprs		; parse the list of values to fill
	bcs @ret
	lda #$00
	sta @i
	beq @chk		; branch to check if start == stop on 1st iter
@fill:	lda con::int
	bne @done		; SIGINT, quit
	ldx @i
	lda @list,x
	ldxy @start
	jsr vmem_store
	ldx @i
	inx
	cpx @listlen
	bcc :+
	ldx #$00
:	stx @i
	incw @start
@chk:	ldxy @start
	cmpw @stop
	bne @fill
@done:	clc		; OK
@ret:	rts
.endproc

;*******************************************************************************
; PARSE EXPRS
; Parses as many expressions as the contents of the line contain and joins them
; into a list of bytes.
; Returns the list
; IN:
;  - zp::line: the line to parse
; OUT:
;  - .A:               the number of values extracted
;  - .C:               set if an error occurred during parsing
;  - zp::debuggertmp+4 the length of the byte array created
;  - zp::debuggertmp+6 the list of values that were read
.proc parse_exprs
@listlen = zp::debuggertmp+4
@list    = zp::debuggertmp+6
	lda #$00
	sta @listlen

@l0:	jsr eval
	bcs @ret

	cmp #$02		; was expression 2 bytes?
	tya
	ldy @listlen
	bcc :+			; if < 2 bytes, only store LSB
	sta @list+1,y		; store MSB of the expression as a fill val
	inc @listlen
:	stx @list,y		; store LSB of expression as fill val
	inc @listlen
	ldy #$00
	lda (zp::line),y	; are we at the end?
	beq @ok

	incw zp::line		; move past separator
	jsr eat_whitespace
	jmp @l0

@ok:	clc			; OK
@ret:	rts
.endproc

;*******************************************************************************
; PRINT WORD
; Prints the given 16 bit value to the console in hex
; IN:
;  - .XY: the value to print
.proc print_word
@buff=zp::debuggertmp
	tya
	pha
	txa
	jsr hextostr
	stx @buff+4
	sty @buff+3
	pla
	jsr hextostr
	stx @buff+2
	sty @buff+1
	lda #'$'
	sta @buff
	lda #$00
	sta @buff+5		; 0 terminate the buffer
	ldxy #@buff
	jmp con::puts
.endproc

;*******************************************************************************
; COMPARE
; c <expr> <expr> <expr>
; Compares the given number of bytes at the two given memory addresses and
; displays any disparities.
; Example:
;  `c $100 $200 $5`
; Will show the differences between the 5 bytes in [$100, $105) and [$200, $205)
.proc compare
@block0 = zp::debuggertmp
@block1 = zp::debuggertmp+2
@num = zp::debuggertmp+4
@tmp = zp::debuggertmp+5
	; get the start of one of the blocks to compare
	jsr eval
	stxy @block0
	bcs @done
	jsr process_ws

	; get the start of the other block to compare
	jsr eval
	stxy @block1
	bcs @done
	jsr process_ws

	; get the number of bytes to compare
	jsr eval
	bcs @done
	stxy @num
	txa
	ora @num+1
	beq @done		; if comparing 0 bytes, we're done

@l0:	lda con::int
	bne @ok		; SIGINT, quit
	ldxy @block0
	jsr vmem_load	; get a byte from block 0
	sta @tmp
	ldxy @block1
	jsr vmem_load	; get a byte from block 1
	cmp @tmp
	beq @next

	; display the address that had a mismatch
	jsr @display_item

@next:	incw @block0
	incw @block1
	decw @num
	bne @l0
@ok:	clc
@done:	rts

;--------------------------------------
@display_item:
	; push the value from the other block
	pha

	; push the value from the first block
	lda @tmp
	pha

	; push the address in the other block
	lda @block1
	pha
	lda @block1+1
	pha

	; push the address in the first block
	lda @block0
	pha
	lda @block0+1
	pha

	ldxy #@compare_msg
	jmp con::puts

.PUSHSEG
.RODATA
@compare_msg: .byte ESCAPE_VALUE, " ", ESCAPE_VALUE, " $", ESCAPE_BYTE, " $", ESCAPE_BYTE, 0
.POPSEG
.endproc

;*******************************************************************************
; GOTO
; Sets the program counter to the given value
; Example:
;  `g $1234`
.proc goto
	jsr debugging
	bcc :+				; can't step if not debugging
	CALL FINAL_BANK_MAIN, dbg::go
:	rts
.endproc

;*******************************************************************************
; MOVE
; m <expr> <expr> <expr>
; Moves the given range of memory to the specified destination address.
; Example:
;  `m $1000 $2000 $3000`
; Will move the memory in [$1000, $2000) to the address $3000.
.proc move
@start = zp::debuggertmp
@end = zp::debuggertmp+2
@target = zp::debuggertmp+4
	jsr get_range
	bcs @ret

	; get the target address
	jsr eval
	stxy @target
	bcs @ret

	; move the data
@l0:	lda con::int
	bne @done		; SIGINT, quit
	ldxy @start
	jsr vmem_load
	ldxy @target
	jsr vmem_store
	incw @target
	incw @start
	ldxy @start
	cmpw @end
	bne @l0
@done:	clc
@ret:	rts
.endproc

;*******************************************************************************
; HUNT
; h <addr> <val1> <val2> ...
; Hunts for the given value(s) starting at the given address.  Does not wrap
; past $FFFF.
; Example:
;  `h $1000 1 2 3`
.proc hunt
@start   = zp::debuggertmp
@i       = zp::debuggertmp+2
@listlen = zp::debuggertmp+4
@list    = zp::debuggertmp+6
	; get the start address
	jsr eval
	stxy @start
	bcs @ret
	jsr eat_whitespace

	jsr parse_exprs		; get the values to hunt for

	lda #$00
	sta @i
@l0:	lda con::int
	bne @done		; SIGINT, quit
	ldxy @start
	jsr vmem_load
	ldx @i
	inc @i
	cmp @list,x
	beq :+
	lda #$00
	sta @i			; reset i to 0

:	; if i >= listlen, we found all the values we were looking for
	lda @i
	cmp @listlen
	bcs @found

	; start++
	inc @start
	bne @l0
	inc @start+1
	bne @l0
	sec			; we wrapped back to $0000, value(s) not found
@ret:	rts

@found:	; subtract @listlen-1 to get HUNT address
	lda @start
	; sec
	sbc @listlen
	tax
	lda @start+1
	sbc #$00
	tay
	inx
	bne :+
	iny
:	jsr print_word	; print the address where we found the value

@done:	RETURN_OK
.endproc

;*******************************************************************************
; REGISTERS
; Displays the current contents of the registers.
.export __dbgcmd_regs
.proc __dbgcmd_regs
	jsr debugging
	bcc @done	; registers aren't meaningful to user if not debugging

	ldxy #strings::debug_registers
	jsr con::puts
	CALL FINAL_BANK_MAIN, dbg::regs_contents
	jsr con::puts
	clc
@done:	rts
.endproc

;*******************************************************************************
; DISASM
; Disassembles from the given expression
.proc disasm
@addr=zp::debuggertmp
@stopaddr=zp::debuggertmp+2
@buff=mem::spare+40
	; default stop address is start address + $20
	lda #$20
	jsr get_range_or_default
	bcs @ret

@l0:	lda con::int
	bne @done		; SIGINT, quit
	ldxy #@buff
	stxy r0
	ldxy @addr
	CALL FINAL_BANK_MAIN, asm::disassemble
	bcc @ok

	; if invalid, just draw a literal byte value
	jsr @drawbyte

	jmp @next
@ok:	jsr @drawline
@next:	ldxy @addr
	cmpw @stopaddr
	bcc @l0
@done:	clc
@ret:	rts

@drawbyte:
	; unknown instruction
	ldxy @addr
	jsr vmem_load	; get the byte
	pha

	; if outputting to file, don't render addresses
	lda con::outfile
	beq @db_with_addr
	ldxy #@byte_msg_no_addr
	jmp con::puts

@db_with_addr:
	; push the address
	lda @addr
	pha
	lda @addr+1
	pha

	incw @addr
	ldxy #@byte_msg
	jmp con::puts

@drawline:
	tax		; save instruction size

	; if outputting to file, don't render addresses
	lda con::outfile
	beq @with_addr

	; update the address pointer
	txa		; get size of instruction
	clc
	adc @addr
	sta @addr
	bcc :+
	inc @addr+1

:	ldxy #@buff
	jmp con::puts	; if address rendering is off, just print

@with_addr:
	; push the disassembled string
	lda #>@buff
	pha
	lda #<@buff
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
.PUSHSEG
.RODATA
@byte_msg:
	.byte "$", ESCAPE_VALUE, " "
@byte_msg_no_addr:
	.byte "$", ESCAPE_BYTE, 0
@disasm_msg:
	.byte "$", ESCAPE_VALUE, " ", ESCAPE_STRING,0	; <address> <instruction>
.POPSEG
.endproc

;*******************************************************************************
; ASSEMBLE
; Assembles the given instruction at the address of the given expression
; e.g.
;  `>A $1000, lda #$00`
.proc assemble
@addr=zp::debuggertmp
	; get the address to assemble at
	jsr eval
	bcs @ret		; return if address is invalid expression
	stxy @addr
	CALL FINAL_BANK_MAIN, asm::setpc

	jsr process_ws
	lda (zp::line),y
	bne @getop
	RETURN_OK		; if no instruction provided, we're done

@getop:	lda #FINAL_BANK_MAIN
	ldxy zp::line
	CALL FINAL_BANK_MAIN, asm::tokenize	; assemble the instruction
	bcc @nexti

@err:	CALL FINAL_BANK_MAIN, err::get
	CALL FINAL_BANK_MAIN, str::uncompress
	jsr con::puts				; print the error
	clc
@ret:	rts

@nexti:	; prepopulate input buffer with ".A <next address> "
	lda #$61
	sta mem::linebuffer
	lda #' '
	sta mem::linebuffer+1
	lda #'$'
	sta mem::linebuffer+2
	lda zp::asmresult+1
	jsr hextostr
	sty mem::linebuffer+3
	stx mem::linebuffer+4
	lda zp::asmresult
	jsr hextostr
	sty mem::linebuffer+5
	stx mem::linebuffer+6
	lda #' '
	sta mem::linebuffer+7
	lda #$00
	sta mem::linebuffer+8

	lda con::line
	sta zp::cury

	ldx #$08
	stx zp::curx
	ldy #$00
	CALL FINAL_BANK_MAIN, cur::setmin

	ldxy #con::getch
	CALL FINAL_BANK_MAIN, edit::gets
	ldxy #mem::linebuffer
	jsr __console_puts
	ldxy #mem::linebuffer
	jmp __dbgcmd_run
.endproc

;*******************************************************************************
; SHOWMEM
; Shows the contents of memory at the target of the given expression
; e.g.
;  `>M $1000 $1020`
.proc showmem
@addr=zp::debuggertmp
@stop=zp::debuggertmp+2
@lines=zp::debuggertmp+2
	lda #8*8		; default to 8 lines (64 bytes)
	jsr get_range_or_default
	bcs @ret

	ldxy @stop
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

@l0:	lda con::int
	bne @done		; SIGINT, quit
	ldxy @addr
	CALL FINAL_BANK_MAIN, view::memline
	jsr con::puts

	; move to address for next row
	lda @addr
	clc
	adc #$08
	sta @addr
	bcc :+
	inc @addr+1
:	dec @lines		; (max 255 lines)
	bne @l0
@done:	clc			; OK
@ret:	rts
.endproc

;*******************************************************************************
; QUIT
; Quits the debugger, returning to the editor
.proc quit
	lda #$00
	sta dbg::interface
	inc con::quit		; send QUIT signal
	rts
.endproc

;*******************************************************************************
; STEP
; Steps to the next instruction while debugging
.proc step
	jsr debugging
	bcc @done				; can't step if not debugging
	CALL FINAL_BANK_MAIN, dbg::step
	jmp __dbgcmd_regs
@done:	rts
.endproc

;*******************************************************************************
; STEP_OVER
; Steps over the next instruction while debugging.  Subroutines (JSR) are
; treated as one instruction
; instruction
.proc step_over
	jsr debugging
	bcc @done				; can't step if not debugging
	CALL FINAL_BANK_MAIN, dbg::step_over
	jmp __dbgcmd_regs
@done:	rts
.endproc

;*******************************************************************************
; TRACE
; Starts TRACE'ing the program.
.proc trace
	jsr debugging
	bcc @done				; can't step if not debugging

	CALL FINAL_BANK_MAIN, dbg::trace
	jmp __dbgcmd_regs
@done:	rts
.endproc

;*******************************************************************************
; GO
; Continues program execution at the current PC
.proc go
	JUMP FINAL_BANK_MAIN, dbg::go
.endproc

;*******************************************************************************
; BACKTRACE
; Prints the call (JSR) stack (with symbols if possible).
; This is based on the contents of the stack, so any data on the stack may
; result in a bad rendering.
; An optional offset from the stack pointer can be given to adjust the
; stack's start location
; .e.g.
;  `>BT 8`
.proc backtrace
@sp=zp::debuggertmp
@offset=zp::debuggertmp+2
@lbl=zp::debuggertmp+2
@addr=zp::debuggertmp+4
@namebuff=mem::spare+40
	; check if an offset was given
	ldy #$00
	lda (zp::line),y
	tax
	beq @cont			; no offset specified, continue

	; get the offset
	incw zp::line			; move past separator
	jsr eval
	cpy #$01
	bcc @cont
:	RETURN_ERR ERR_OVERSIZED_OPERAND	; offset must be <$80
	cpx #$80
	bcs :-

@cont:	stx @offset
	ldxy #sim::reg_sp
	jsr getb
	sec			; +1
	adc @offset
	sta @sp
	lda #>$0100		; MSB of stack base
	sta @sp+1

@l0:	lda con::int
	bne @done		; SIGINT, quit
	jsr @draw_item		; draw the stack contents for this offset
	inc @sp			; move to next procedure in the stack
	beq @ok
	inc @sp
	bne @l0
@ok:	clc
@done:	rts

;--------------------------------------
@draw_item:
	; get the address of the procedure call
	ldxy @sp				; LSB of stack address
	jsr vmem_load
	sec
	sbc #$02
	php
	sta @addr
	ldxy @sp
	inx					; MSB of stack address
	jsr vmem_load
	plp
	sbc #$00
	sta @addr+1

	; get the symbol name for this address (if there is one)
	ldxy @addr
	CALL FINAL_BANK_MAIN, lbl::by_addr
	stxy @lbl		; save the id of the label

	; subtract the address we found from the address we were looking for
	CALL FINAL_BANK_MAIN, lbl::getaddr
	stxy r0
	ldxy @addr
	sub16 r0
	txa
	pha
	tya
	pha

@label:	lda #>@namebuff
	pha
	sta r0+1
	lda #<@namebuff
	pha
	sta r0
	ldxy @lbl
	CALL FINAL_BANK_MAIN, lbl::getname

@push_addr:
	; push the address of the procedure call
	lda @addr
	pha					; push LSB
	lda @addr+1
	pha

	; push the stack pointer
	lda @sp
	pha

	ldxy #@backtrace_msg
	jmp con::puts

.PUSHSEG
.RODATA
@backtrace_msg:
	; <stack address> <address> <symbol>+<offset>
	.byte "$", ESCAPE_BYTE, " $", ESCAPE_VALUE, " ", ESCAPE_STRING, "+$", ESCAPE_VALUE,0
.POPSEG
.endproc

;*******************************************************************************
; STEP OUT
; Continues execution til the current subroutine returns with an RTS
.proc step_out
	jsr debugging
	bcc @done				; can't step if not debugging
	CALL FINAL_BANK_MAIN, dbg::step_out
	jmp __dbgcmd_regs
@done:	rts
.endproc

;*******************************************************************************
; SAVEMEM
; Saves the given memory range to the specified file
; e.g.
;  `>S $1000 $2000 FILE.PRG`
.proc savemem
@startaddr=zp::debuggertmp
@stopaddr=zp::debuggertmp+2
	jsr get_range
	bcs @done
	ldxy @stopaddr
	stxy file::save_address_end

	CALL FINAL_BANK_MAIN, irq::off

	; open the output file for writing
	ldxy zp::line
	CALL FINAL_BANK_MAIN, file::open_w
	bcs @err
	pha

	; save the given memory range
	ldxy @startaddr
	CALL FINAL_BANK_MAIN, file::savebin

	pla
	bcs @err				; if file save failed -> done

	; close the file
	CALL FINAL_BANK_MAIN, file::close
	jsr @err				; restore IRQ
	RETURN_OK

@err:	pha					; save error code
	CALL FINAL_BANK_MAIN, irq::on
	pla
	sec
@done:	rts
.endproc

;*******************************************************************************
; DUMP
; Outputs a dump of the given memory range in a format that can be assembled
; (as .db statements)
.proc dump
@addr=zp::debuggertmp
@stop=zp::debuggertmp+2
@cnt=zp::debuggertmp+4
@line=zp::debuggertmp+5
@buff=mem::spare+40
	lda #8*8			; default size of range
	jsr get_range_or_default
	bcc @l0
:	jmp @done

@l0:	lda con::int
	bne :-				; SIGINT, quit
	ldxy #@buff+4
	stxy @line

	lda #'.'
	sta @buff
	lda #'d'
	sta @buff+1
	lda #'b'
	sta @buff+2
	lda #' '
	sta @buff+3

	; get 8 bytes (1 row of .DB's)
	lda #$08
	sta @cnt

@l1:	ldxy @addr
	cmpw @stop
	bcs @cont

	ldy #$00
	lda #'$'
	sta (@line),y

	ldy @addr+1	; restore .Y
	jsr vmem_load
	jsr hextostr
	tya
	ldy #$01
	sta (@line),y
	iny
	txa
	sta (@line),y
	iny
	lda #','
	sta (@line),y
	lda @line
	clc
	adc #$04
	sta @line
	dec @cnt
	bne @l1

@cont:	decw @line	; delete the last ','
	lda #$00
	tay
	sta (@line),y	; terminate buffer

	ldxy #@buff
	jsr con::puts

	lda @addr
	clc
	adc #$08
	sta @addr
	bcc :+
	inc @addr+1
:	tax
	ldy @addr+1
	cmpw @stop
	bcs :+
	jmp @l0		; next row
:	clc		; ok
@done:	rts
.endproc

;*******************************************************************************
; CLEAR
; Clears the terminal
.proc clear
	jsr con::clear
	RETURN_OK
.endproc

;*******************************************************************************
; GET RANGE OR DEFAULT
; Evaluate one-two arguments representing an address range and stores the
; results.
; If no end to the range is provided, returns the start address + the given
; default range size
; IN:
;   - .A: the default size of the range
; OUT:
;   - .C:                set if a (valid) range was not given
;   - zp::debuggertmp:   the start of the range
;   - zp::debuggertmp+2: the end of the range
.proc get_range_or_default
@start=zp::debuggertmp
@stop=zp::debuggertmp+2
	sta @size
	; get the start address
	jsr eval
	stxy @start
	bcs @ret

	; are we at the end of the line?
	jsr eat_whitespace
	ldy #$00
	lda (zp::line),y
	bne @cont

@default:
@size=*+1
	lda #$00
	clc
	adc @start
	sta @stop
	lda @start+1
	adc #$00
	sta @stop+1
	jsr eat_whitespace
	RETURN_OK

@cont:	; get the stop address
	jsr eat_whitespace
	jsr eval
	stxy @stop
	jsr eat_whitespace
	clc			; ok
@ret:	rts
.endproc

;*******************************************************************************
; GET RANGE
; Evaluate two arguments representing an address range and stores the results
; OUT:
;   - .C:                set if a (valid) range was not given
;   - zp::debuggertmp:   the start of the range
;   - zp::debuggertmp+2: the end of the range
.proc get_range
@start=zp::debuggertmp
@stop=zp::debuggertmp+2
	; get the start address
	jsr eval
	stxy @start
	bcs @ret

	; get the stop address
	jsr eat_whitespace
	jsr eval
	stxy @stop
	jsr eat_whitespace
@ret:	rts
.endproc

;*******************************************************************************
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

;*******************************************************************************
; EAT_WHITESPACE
; Updates zp::line to point past any whitespace.
.proc eat_whitespace
	php
	ldy #$00
@l0:	lda (zp::line),y
	beq @done
	jsr is_whitespace
	bne @done
	incw zp::line
	bne @l0			; branch always

@done:	plp
	rts
.endproc

;*******************************************************************************
; HEXTOSTR
; Returns the string representation of the value in .A
; IN:
;  - .A: the value to get the string representation of
; OUT:
;  - .X: the character representation of the low nybble
;  - .Y: the character representation of the  high nybble
.proc hextostr
	pha
	lsr
	lsr
	lsr
	lsr
	cmp #$0a
	bcs :+
	adc #'0'
	bcc :++
:	adc #'a'-$a-1
:	tay

	pla
	and #$0f
	cmp #$0a
	bcs :+
	adc #'0'
	bcc :++
:	adc #'a'-$a-1
:	tax
	rts
.endproc

;*******************************************************************************
; DEBUGGING
; Checks if the user is currently debugging a program.
; Some commands are only valid while debugging
; OUT:
;  - .C: set if the user is debugging a program
.proc debugging
	; get the debugging flag
	ldxy #edit::debugging
	jsr getb
	cmp #$01
	bcs :+

	ldxy #@not_debugging_msg
	jsr con::puts
	clc
:	rts
.PUSHSEG
.RODATA
@not_debugging_msg: .byte "not debugging",0
.POPSEG
.endproc

;*******************************************************************************
; EVAL
; Calls "expr::eval" and returns
; IN:
;   - zp::line: the text for the expression to evaluate
; OUT:
;  - .A:       the size of the returned value in bytes or the error code
;  - .XY:      the result of the evaluated expression
;  - .C:       clear on success or set on failure
;  - zp::line: updated to point beyond the parsed expression
.proc eval
	JUMP FINAL_BANK_MAIN, expr::eval
.endproc

;*******************************************************************************
; VMEM_LOAD
; Calls vmem::load
.proc vmem_load
	JUMP FINAL_BANK_MAIN, vmem::load
.endproc

;*******************************************************************************
; VMEM STORE
; Calls vmem::store
.proc vmem_store
	JUMP FINAL_BANK_MAIN, vmem::store
.endproc

;*******************************************************************************
; GETB
; Calls "ram::get_byte"
.proc getb
	JUMP FINAL_BANK_MAIN, ram::get_byte
.endproc

;*******************************************************************************
; PROCESS_WS
; Calls "line::process_ws" in the MAIN bank
.proc process_ws
	JUMP FINAL_BANK_MAIN, line::process_ws
.endproc

;*******************************************************************************
; COMMANDS
commands:
.byte "clear",0	; clear the terminal
.byte "wa",0	; watch add
.byte "wal",0	; watch add load
.byte "was",0	; watch add store
.byte "wr",0	; watch remove
.byte "w",0	; list watches
.byte "b",0	; list breakpoints
.byte "ba",0	; breakpoint add by addr
.byte "bl",0	; breakpoint add by line
.byte "br",0	; breakpoint remove
.byte "f",0	; fill memory in the given address range with the given data
.byte "dump",0	; dumps the given address range
.byte "move",0	; move memory from the given address range to the target address
.byte "g",0	; goto given expression/address
.byte "c",0	; compare the memory in the two given ranges
.byte "h",0	; hunts the given address range for the given data
.byte "r",0	; shows the contents of the registers (if debugging)
.byte "d",0	; disassembles from the given address
.byte "a",0	; assembles the given instruction given address
.byte "m",0	; show contents of memory at the given address
.byte "t",0	; start TRACE'ing
.byte "x",0	; quit the debugger
.byte "z",0	; step to the next instruction (if debugging)
.byte "n",0	; step over the next instruction (if debugging)
.byte "g",0	; go (continue program execution) (if debugging)
.byte "bt",0	; backtrace (if debugging)
.byte "zo",0	; step out of current subroutine (if debugging)
.byte "p",0	; poke a single byte to the given address
.byte "s",0	; save memory

.linecont +
.define command_vectors clear, add_watch, add_watch_load, add_watch_store, \
	remove_watch, list_watches, list_breakpoints, add_break_addr, \
	add_break_line, remove_break, fill, dump, move, goto, compare, hunt, \
	__dbgcmd_regs, disasm, assemble, showmem, trace, quit, step, \
	step_over, go, backtrace, step_out, poke, savemem
.linecont -
commandslo: .lobytes command_vectors
commandshi: .hibytes command_vectors
num_commands=*-commandshi
