;*******************************************************************************
; DEBUG.ASM
; This file contains the debugger code, which is the main loop while debugging
; and assembled program.
;*******************************************************************************

.include "asm.inc"
.include "asmflags.inc"
.include "breakpoints.inc"
.include "console.inc"
.include "cursor.inc"
.include "debuginfo.inc"
.include "debugcmd.inc"
.include "draw.inc"
.include "edit.inc"
.include "errors.inc"
.include "fastcopy.inc"
.include "file.inc"
.include "flags.inc"
.include "gui.inc"
.include "labels.inc"
.include "irq.inc"
.include "key.inc"
.include "keycodes.inc"
.include "layout.inc"
.include "macros.inc"
.include "memory.inc"
.include "screen.inc"
.include "settings.inc"
.include "sim6502.inc"
.include "source.inc"
.include "string.inc"
.include "strings.inc"
.include "text.inc"
.include "util.inc"
.include "watches.inc"
.include "view.inc"
.include "vmem.inc"
.include "zeropage.inc"

.include "ram.inc"

.import __DEBUGGER_LOAD__
.import __DEBUGGER_SIZE__

;*******************************************************************************
; BRK/NMI HANDLER ADDRESSES
; address in user program where the BRK handler will reside
; NOTE: the user program cannot use the space occupied by these handlers
BRK_HANDLER_TOP  = $8000
NMI_HANDLER_ADDR = BRK_HANDLER_TOP-brkhandler1_size
BRK_HANDLER_ADDR = BRK_HANDLER_TOP-brkhandler1_size+5-1 ; 5 = sizeof NMI portion
RTI_ADDR         = BRK_HANDLER_ADDR+3

.export STEP_HANDLER_ADDR
.export STEP_EXEC_BUFFER
STEP_HANDLER_ADDR = NMI_HANDLER_ADDR-stephandler_size

STEP_EXEC_BUFFER  = STEP_HANDLER_ADDR+16
STEP_RESTORE_A    = STEP_EXEC_BUFFER-2

TRAMPOLINE = STEP_HANDLER_ADDR - trampoline_size
TRAMPOLINE_ADDR = TRAMPOLINE+13
.export TRAMPOLINE_ADDR
.export STEP_HANDLER_ADDR

.segment "NMI_HANDLER"
.res 17 ;brkhandler1_size

;*******************************************************************************
MAX_BREAKPOINTS = 16	; max number of breakpoints that may be set

AUX_NONE = 0		; flag for no viewer enabled
AUX_MEM  = 1		; enables the memory viewer in the debug view
AUX_GUI  = 2		; enables viewers that use the standard GUI list menu

; layout constants for the register view
REG_PC_OFFSET = 0
REG_A_OFFSET = 5
REG_X_OFFSET = 8
REG_Y_OFFSET = 11
REG_SP_OFFSET = 14

;*******************************************************************************
; ACTION constants
; These tell us what command the user last executed when we return to the
; debugger via a BRK or NMI
ACTION_STEP           = 1	; action for STEP command
ACTION_STEP_OVER      = 2	; action for STEP OVER command
ACTION_GO_START       = 4	; action for first instruction of GO command
ACTION_GO             = 5	; action for subsequent GO instructions
ACTION_TRACE_START    = 6
ACTION_TRACE          = 7	; action for TRACE command
ACTION_STEP_OUT_START = 8
ACTION_STEP_OUT       = 9	; action for STEP OUT command (must be last)

;*******************************************************************************
; IFACE (interface) constants
; These are the valid values for dbg::interface. This value determines the
; interface that is entered in the debug breakpoint handler
DEBUG_IFACE_GUI  = 0	; GUI interface (returns to visual debugger)
DEBUG_IFACE_TEXT = 1	; text interface (returns to TUI)

; Initial stack offset for user program.
; The debugger will utilize everything above this for its own purposes when
; stepping, tracing, etc.
; When using the "GO" command, you may use the entire stack
; TODO: calculate the maximum value for this
PROGRAM_STACK_START = $e0

; Stop tracing state/NMI
; This NMI is installed programatically and catches the RESTORE key as a
; signal to stop a trace
; These values must be between PRORGAM_STACK_START and $100
STOP_TRACING_NMI = PROGRAM_STACK_START+1
stop_tracing     = STOP_TRACING_NMI+5

; Max depth the debugger may reach during handling of a step during the TRACE
; command. This amount will be saved/restored by the debugger before handling
; the debugged program's next instruction.
TRACE_STACK_DEPTH = $100-PROGRAM_STACK_START

;*******************************************************************************
debugtmp = zp::debuggertmp	; scratchpad

;*******************************************************************************
; Program state variables

.segment "SHAREBSS"

.export __debug_interface
__debug_interface: .byte DEBUG_IFACE_GUI

.BSS

;******************************************************************************
; Up to 4 bytes need to be saved in a given STEP.
;  1. The instruction at the PC (up to 3 bytes)
;  2. The memory value read/written by the instruction (up to 1 byte)
;
; We need to save all these before we complete a STEP and execute that
; instruction:
;  1. save user instruction at (PC)
;  2. get effective address of instruction's operand
;   2a. if there is an address and it's in [0,$2000), save the value there
;
; To restore the debugger:
;  1. save user value at sim::effective_addr (address of effected memory)
;  2. restore 3 bytes of debug memory at (prev_pc)
stepsave:  .byte 0	; opcode to save under BRK
brkaddr:   .word 0 	; address where our brakpoint is set

;*******************************************************************************
; Debug state values for internal RAM locations
; NOTE:
; these must be stored next to each other as they are backed up/restored as
; a unit.
debug_state_save:
debug_instruction_save: .res 3	; buffer for debugger RAM at current instruction
mem_debugsave:          .byte 0 ; byte under effective address during STEP
debug_stepsave: 	.byte 0 ; debugger byte under BRK (if internal)

;*******************************************************************************
; previous values for registers etc.
prev_reg_a:  .byte 0
prev_reg_x:  .byte 0
prev_reg_y:  .byte 0
prev_reg_p:  .byte 0
prev_reg_sp: .byte 0
prev_pc:     .word 0

sw_valid:    .byte 0    ; if !0, stopwatch is valid

prev_mem_save:     .byte 0
prev_mem_saveaddr: .word 0

step_mode: .byte 0	; which type of stepping we're doing (INTO, OVER)
lineset:   .byte 0	; not zero if we know the line number we're on

aux_mode:       .byte 0	; the active auxiliary view
highlight_line: .word 0 ; the line we are highlighting
highlight_file: .word 0	; filename of line we are highlighting

brkcond: .word 0	; if !0, address to handler to break TRACE on

is_brk: .byte 0		; set if interrupt was triggered by BRK

step_out_depth: .byte 0 ; # of RTS's to wait for when "stepping out"

;stop_tracing: .byte 0	; if !0, debugger will stop a TRACE at the next STEP


;******************************************************************************
; BREAKPOINTS
.export __debug_breakpointslo
.export __debug_breakpointshi
.export __debug_breakpoint_lineslo
.export __debug_breakpoint_lineshi
.export __debug_breakpoint_fileids

.export __debug_numbreakpoints
__debug_numbreakpoints:     .byte 0

;******************************************************************************
; The following tables must be stored together and
; NUM_BREAKPOINT_TABLES must be set to the number of them
; We iteratively modify the values within these tables
NUM_BREAKPOINT_TABLES=7
breakpoint_data:
__debug_breakpointslo:
breakpointslo:              .res MAX_BREAKPOINTS	; LSB's of the break points
__debug_breakpointshi:
breakpointshi:              .res MAX_BREAKPOINTS	; MSB's of the break points
__debug_breakpoint_lineslo: .res MAX_BREAKPOINTS	; breakpoint line # (LSB)
__debug_breakpoint_lineshi: .res MAX_BREAKPOINTS	; breakpoint line # (MSB)
__debug_breakpoint_fileids: .res MAX_BREAKPOINTS	; breakpoint file ID's

.export __debug_breakpoint_flags
__debug_breakpoint_flags:
breakpoint_flags: .res MAX_BREAKPOINTS ; active state of breakpoints
breaksave:        .res MAX_BREAKPOINTS ; backup of instructions under the BRKs

.export in_rom
in_rom:   .byte 0	; !0: the next instruction is in $c000-$ffff
is_step:  .byte 0	; !0: we are running a STEP instruction

;******************************************************************************
; RESTORE DEBUG ZP TRACE
; Restores the state of the debugger's zeropage for traces
.macro restore_debug_zp_trace
	ldx #$0f
	; TODO: figure out what exactly must be saved
:	lda mem::dbg00,x
	sta $00,x
	lda mem::dbg00+$d0,x
	sta $d0,x
	dex
	bpl :-
.endmacro

;******************************************************************************
; SAVE USER ZP
; Saves the state of the user's zeropage
.macro save_user_zp
	ldx #$00
:	lda $00,x
	sta mem::prog00,x
	lda $100,x
	sta mem::prog00+$100,x
	lda $200,x
	sta mem::prog00+$200,x
	lda $300,x
	sta mem::prog00+$300,x
	dex
	bne :-
.endmacro

;******************************************************************************
; RESTORE USER ZP
; Restores the state of the user's zeropage
.macro restore_user_zp
	ldx #$00
:	lda mem::prog00,x
	sta $00,x
	lda mem::prog00+$100,x
	sta $100,x
	lda mem::prog00+$200,x
	sta $200,x
	lda mem::prog00+$300,x
	sta $300,x
	dex
	bne :-
.endmacro

.CODE

;******************************************************************************
; RESTORE_DEBUG_STATE
; Restores the saved debugger state
.proc restore_debug_state
@vicsave=mem::dbg9000
@colorsave=mem::dbg9400
	ldx #$10
:	lda @vicsave-1,x
	sta $9000-1,x
	dex
	bne :-

	ldx #$f0
; save $9400-$94f0
:	lda @colorsave-1,x
	sta $9400-1,x
	dex
	bne :-

	; reinit the bitmap
	jsr scr::init

	; restore the screen ($1100-$2000)
	jmp scr::restore
.endproc

;******************************************************************************
; RESTORE_PROGSTATE
; restores the saved program state
.proc __debug_restore_progstate
; restore $9000-$9010
	ldx #$10
:	lda mem::prog9000-1,x
	sta $9000-1,x
	dex
	bne :-

; restore $1000-$1100
:	lda mem::prog1000,x
	sta $1000,x
	dex
	bne :-

	ldx #$10
; restore $9000-$9010
:	lda mem::prog9000-1,x
	sta $9000-1,x
	dex
	bne :-

	ldx #$f0
; restore $9400-$94f0
:	lda mem::prog9400-1,x
	sta $9400-1,x
	dex
	bne :-

	lda #$4c
	sta zp::jmpaddr
	; restore the user $1100 data
	CALL FINAL_BANK_FASTCOPY, fcpy::restore
	rts
.endproc

;******************************************************************************
; START
; Begins debugging at the given address
; Execution will continue until a BRK instruction occurs at which point the
; debugger will take over and allow for interactive debugging from the user.
; in:
;  - .XY: the address to begin debugging at
.export __debug_start
.proc __debug_start
	lda #CUR_SELECT
	sta cur::mode

	; set the simulator's initial PC value
	stxy sim::pc
	stxy prev_pc

	; init state
	lda #$00
	sta aux_mode		; initialize auxiliary views
	sta watch::num		; clear watches

	; set color for the message row
	lda #DEFAULT_RVS
	ldx #DEBUG_MESSAGE_LINE
	jsr draw::hline

	jsr reset_stopwatch
	jsr install_brk		; install the BRK/NMI handler
	jsr install_step	; install the STEP handler
	jsr install_trampoline

	; initialize the user program stack
	lda #PROGRAM_STACK_START
	sta sim::reg_sp

	ldxy #BRK_HANDLER_ADDR+1
	stxy $0316			; BRK
	ldxy #NMI_HANDLER_ADDR
	stxy $0318			; NMI

	ldx #$ff
	txs

	jmp return_to_debugger		; enter the debugger
.endproc

;******************************************************************************
; BRKHANDLER
; Handles the BRK interrupt by returning control to the main bank
; and continuing execution there.
; Part 1 of the handler is copied to the user program and part 2 is copied to
; the editor, where execution will pick up after switching banks
;
; This is the layout of the code in each bank:
; | User         |  Debugger                     |
; |--------------|-------------------------------|
; | PHA          |	--                       |
; | TXA          |	--                       |
; | PHA          |	--                       |
; | TYA          |	--                       |
; | PHA          |      PHA                      |
; | LDA #$80     |	LDA #$80                 |
; | STA $9C02    |	STA $9C02                |
; | LDA #$82     |      PLA                      |
; | STA $911e    |      JMP DEBUG_BRK            |
; | PLA          |	                         |
; | RTI          | 	                         |
; | -- (0 bytes) |  -- (3 bytes)                 |
;
; handler1 (the User handler) has 1 empty bytes at the end
; handler2 (the Debug handler) has 5 empty bytes at the start
.PUSHSEG
.RODATA
brkhandlers:
brkhandlernmi:
brkhandler1:
; this portion runs in the DEBUGGER bank
	pha		; NMI handler
	txa
	pha
	tya
	pha

	lda #$80	; start of BRK handler
	sta $9c02	; switch to DEBUGGER bank
;--------------------------------------
; this portion runs in the USER bank
	lda #$82
	sta $911e	; enable CA1 (RESTORE key) interrupts
	pla
	rti		; return from BRK/NMI
brkhandler1_size=*-brkhandler1
.export brkhandler1_size

brkhandler2:
; this portion runs in the user bank
	pha
	lda #$80
	sta $9c02	; switch to DEBUGGER bank
;--------------------------------------
; this portion runs in the debugger bank
	jmp debug_brk
brkhandler2_size=*-brkhandler2

;******************************************************************************
; STEPHANDLER
; The step handler runs a single instruction and returns to the
; debugger.
.import step_done
stephandler:
	; switch to USER bank
	lda #FINAL_BANK_USER
	sta $9c02
	pla			; restore .A
	sta STEP_RESTORE_A

	pla			; get status flags
	and #$04		; set I flag
	pha			; push altered status

	lda #$00		; SMC - restore A
	plp			; restore altered status flags

	; run the instruction
step_buffer:
	nop
	nop
	nop

	php
	pha
	sei			; disable IRQs

	; switch back to DEBUGGER bank
	lda #FINAL_BANK_MAIN
	sta $9c02
	pla
	jmp step_done
stephandler_size=*-stephandler

;******************************************************************************
; TRAMPOLINE
trampoline:
	lda #FINAL_BANK_USER
	sta $9c02
	lda #$82		; enable RESTORE key interrupt
	sta $911e
	pla			; restore .A
	plp			; restore status
	jmp $f00d
trampoline_size=*-trampoline

.POPSEG

;******************************************************************************
; INSTALL STEP
; Installs the STEP code at the top of the user and debug RAM
; This code includes a buffer that switches to the user RAM, executes an
; instruction there, switches back to the debugger RAM, and jumps back to the
; debugger's "return from step" function to capture any changes that took place.
.proc install_step
@cnt=r0
@dst=r2
	ldxy #STEP_HANDLER_ADDR
	stxy @dst
	lda #stephandler_size-1
	sta @cnt
; copy the STEP handler to the user program and our RAM
@l0:	ldy @cnt
	lda stephandler,y
	sta STEP_HANDLER_ADDR,y
	sta zp::bankval
	sty zp::bankoffset
	ldxy @dst
	lda #FINAL_BANK_USER
	jsr ram::store_off
	dec @cnt
	bpl @l0

	rts
.endproc

;******************************************************************************
; INSTALL TRAMPOLINE
; Installs the "trampoline" code at the top of the user and debug RAM
; This code lets us switch to the user bank and begin executing code there
.proc install_trampoline
@cnt=r0
@dst=r2
	ldxy #TRAMPOLINE
	stxy @dst
	lda #trampoline_size-1
	sta @cnt
; copy the STEP handler to the user program and our RAM
@l0:	ldy @cnt
	lda trampoline,y
	sta TRAMPOLINE,y
	sta zp::bankval
	sty zp::bankoffset
	ldxy @dst
	lda #FINAL_BANK_USER
	jsr ram::store_off
	dec @cnt
	bpl @l0

	rts
.endproc

;******************************************************************************
; INSTALL BRK
; Installs the debugger BRK handler to the BRK and NMI vectors and copies the
; code to enter the handler to the user program's RAM at BRK_HANDLER_ADDR.
; This handler switches back to the debuggers RAM bank, where the debugger takes
; over.
; Also installs the active breakpoints
; IN:
;  - .XY: the address to install the BRK handler to
.proc install_brk
@dst=r0
@cnt=r2
	sei
	ldxy #BRK_HANDLER_ADDR+1
	stxy $0316		; BRK
	ldxy #NMI_HANDLER_ADDR
	stxy @dst
	stxy $0318		; NMI
	cli

	lda #brkhandler1_size-1
	sta @cnt
; copy part 1 of the BRK handler to the user program
@l0:	ldy @cnt
	lda brkhandlers,y
	sta zp::bankval
	sty zp::bankoffset
	ldxy @dst
	lda #FINAL_BANK_USER
	jsr ram::store_off
	dec @cnt
	bpl @l0

	ldy #brkhandler2_size-1
; copy part 2 of the BRK handler to our memory
@l1:	lda brkhandler2,y
	sta BRK_HANDLER_ADDR,y
	dey
	bpl @l1

	; fall through to install_breakpoints
.endproc

;******************************************************************************
; INSTALL_BREAKPOINTS
; Install all breakpoints from "breakpoints" EXCEPT for those at the current
; PC. This is to prevent a loop of breakpoints being repeatedly set and
; immediately hit. For a breakpoint to be effective, it must be set at least
; one instruction from the current PC.
.proc install_breakpoints
@brkaddr=r0
@cnt=r2
	ldx __debug_numbreakpoints
	beq @done
	dex
	stx @cnt

@installbrks:
	ldx @cnt
	lda breakpoint_flags,x
	and #BREAKPOINT_ENABLED
	beq @next

	lda breakpointslo,x
	sta @brkaddr
	lda breakpointshi,x
	sta @brkaddr+1

	; if this breakpoint is at the current PC, don't install it
	ldxy sim::pc
	cmpw @brkaddr
	beq @next

	; get the opcode to save before we overwrite it with a BRK
	ldxy @brkaddr
	jsr vmem::load
	ldx @cnt
	sta breaksave,x		; save the instruction under the new BRK

	lda #$00		; BRK
	ldxy @brkaddr
	jsr vmem::store

@next:	dec @cnt
	bpl @installbrks
@done:	rts
.endproc

;******************************************************************************
; UNINSTALL_BREAKPOINTS
; Restores the source code by removing all breakpoints installed by the
; debugger
.proc uninstall_breakpoints
@addr=r0
@cnt=r2
	ldx __debug_numbreakpoints
	beq @done
	dex
	stx @cnt
@uninstall:
	ldx @cnt
	lda breakpoint_flags,x
	and #BREAKPOINT_ENABLED	; if breakpoint is disabled, skip
	beq @next

	lda breakpointslo,x
	sta @addr
	lda breakpointshi,x
	sta @addr+1

	; restore the opcode we replaced with a BRK
	lda breaksave,x
	ldxy @addr
	jsr vmem::store
@next:	dec @cnt
	bpl @uninstall
@done:	rts
.endproc

;******************************************************************************
; SAVE_DEBUG_STATE
; saves memory likely to be clobbered by the user's
; program (namely the screen)
.proc save_debug_state
@vicsave=mem::dbg9000
@colorsave=mem::dbg9400
	ldx #$10
@savevic:
	lda $9000-1,x
	sta @vicsave-1,x
	dex
	bne @savevic

	ldx #$f0
; save $9400-$94f0
@savecolor:
	lda $9400-1,x
	sta @colorsave-1,x
	dex
	bne @savecolor

	; backup the screen
	jmp scr::save
.endproc

;******************************************************************************
; CLRSTATE
; Clears the state of the user program memory area and initializes it the
; state that BASIC would leave it in.
.export __debug_clrstate
.proc __debug_clrstate
	sei
	jsr save_debug_state
	jsr save_debug_zp

	lda #$7f
	sta $911e			; disable NMI's

	; TODO: check memory configuration; skip this for 8+K expanded
	; initalize RAM ($00-$400)
	lda #$00
	tax
:	sta $00,x
	sta $100,x
	sta $200,x
	sta $300,x
	dex
	bne :-

	jsr $fd52	; restore default I/O vectors
	jsr $fdf9	; initialize I/O registers
	jsr $fdca	; set top of RAM to $2000 (for unexpanded)
	jsr $e518	; initialize hardware
	jsr $e45b	; init BASIC vectors
	jsr $e3a4	; init BASIC RAM locations
	jsr $e404	; print startup message and init pointers
	save_user_zp

	sei
	lda #$7f
	sta $911e			; disable NMI's

	; restore the debug (Monster's) low memory
	; this has the routines (in the shared RAM space) we need to do the rest
	; of the banekd program state save (save_prog_state)
	jsr restore_debug_zp
	jsr restore_debug_low

	; save the initialized hi RAM ($1000-$2000) and other misc locations of
	; the user program
	jsr __debug_save_prog_state

	; restore the rest of Monster's RAM and enter the application
	jsr restore_debug_state
	jsr irq::on
	jmp edit::init
.endproc

;******************************************************************************
; SAVE_PROG_STATE
; saves memory clobbered by the debugger (screen, VIC registers and color)
.export __debug_save_prog_state
.proc __debug_save_prog_state
@vicsave=mem::prog9000
@internalmem=mem::prog1000
@colorsave=mem::prog9400
	ldx #$10
@savevic:
	lda $9000-1,x
	sta @vicsave-1,x
	dex
	bne @savevic

; save $1000-$1100
@save1000:
	lda $1000,x
	sta @internalmem,x
	dex
	bne @save1000

	ldx #$f0
; save $9400-$94f0
@savecolor:
	lda $9400-1,x
	sta @colorsave-1,x
	dex
	bne @savecolor

	; backup the user $1100-$2000 data
	JUMP FINAL_BANK_FASTCOPY, fcpy::save
.endproc

;******************************************************************************
; DEBUG_BRK
; This is the BRK handler for the debugger.
; It saves the user program's state and other sensitive memory areas that are
; needed by the debugger for display etc, then it restores the debugger's state
; and finally transfers control to the debugger
.proc debug_brk
	lda $912e
	sta sim::via2+$e

	; save the registers pushed by the KERNAL interrupt handler ($FF72)
	lda #$7f
	sta $911e	; disable all NMI's
	sta $912e	; disable all NMI's

	; TODO: save VIA timers?
	;lda $9114
	;sta sim::via1_t1
	;lda $9114+1
	;sta sim::via1_t1+1
	;lda $9118
	;sta sim::via1_t2
	;lda $9118+1
	;sta sim::via1_t2+1
	;lda $9124
	;sta sim::via2_t1
	;lda $9124+1
	;sta sim::via2_t1+1
	;lda $9128
	;sta sim::via2_t2
	;lda $9128+1
	;sta sim::via2_t2+1

	; save the registers pushed by the KERNAL interrupt handler ($FF72)
	pla
	sta sim::reg_y
	pla
	sta sim::reg_x
	pla
	sta sim::reg_a

	pla
	sta sim::reg_p
	and #$10	; mask BRK flag
	sta is_brk

	pla
	sta sim::pc
	pla
	sta sim::pc+1
	tsx
	stx sim::reg_sp

	; clear decimal in case user set it
	cld

	lda is_brk		; was interrupt triggered a BRK?
	beq @restore_debugger	; if not, skip PC decrement

@notnmi:
	decw sim::pc	; BRK pushes PC + 2, subtract 2 from PC
	decw sim::pc

@restore_debugger:
	sei

	; reinit the debugger's SP
	ldx #$ff
	txs

	; save the user's zeropage and restore the debugger's
	save_user_zp
	jsr restore_debug_low
	jsr restore_debug_zp

	; save program state and swap the debugger state in
	jsr swapout
        jsr irq::on		; reinstall the main IRQ
	jsr uninstall_breakpoints

	; fall through to return_to_debugger
.endproc

;******************************************************************************
; RETURN TO DEBUGGER
; Entrypoint to the debugger
.export return_to_debugger
.proc return_to_debugger
; we're done updating state, check which interface we should transfer
; execution to
@enter_iface:
	lda #$00
	sta lineset		; flag that line # is (yet) unknown
	lda __debug_interface
	beq @iface_gui		; if interface is GUI, continue

@iface_tui:
	; display the contents of the registers
	CALL FINAL_BANK_CONSOLE, dbgcmd::regs

@debugloop_tui:
	CALL FINAL_BANK_CONSOLE, con::reenter
	jmp @loopdone

@iface_gui:
	jsr show_aux		; display the auxiliary mode

@showbrk:
	; get the address before the BRK and go to it
	jsr cur::off
	ldxy sim::pc
	jsr __debug_gotoaddr
	bcs @print		; if we failed to get line #, continue
	jsr edit::sethighlight
	inc lineset

@print:	jsr showstate		; show regs/BRK message
	jsr cur::on

; main (graphical) debug loop
@debugloop:
	cli
	lda __debug_interface
	bne @debugloop_tui

@debugloop_gui:
	jsr text::update
	jsr key::getch
	beq @debugloop_gui

	pha
	jsr cur::off
	pla

	ldx zp::editor_mode
	cpx #MODE_INSERT
	beq @nocmd		; in INSERT mode, propagate all keys to editor

 ; check if the key has a debugger command associated with it
	ldx #num_commands-1
@getcmd:
	cmp commands,x
	beq @runcmd
	dex
	bpl @getcmd

; check if the key should be ignored (not sent to be handled by the editor)
	ldx #num_disabled_commands-1
@chkdisabled:
	cmp disabled_commands,x
	beq @loopdone		; if key is marked as disabled, ignore it
	dex
	bpl @chkdisabled

; propagate the key to the editor
@nocmd:	jsr edit::handlekey
	jmp @debugloop

@runcmd:
	lda #$4c		; JMP
	sta $00
	lda command_vectorslo,x	; vector LSB
	sta zp::jmpvec
	lda command_vectorshi,x	; vector MSB
	sta zp::jmpvec+1
	jsr zp::jmpaddr		; call the command
	jmp @enter_iface

@loopdone:
	jmp @debugloop
.endproc

;******************************************************************************
; LOAD_FILE
; Loads the file (in the editor) for the given filename
; IN:
;  - .A: the file (as returned from dbgi::addr2line)
; OUT:
;  - .C: set if the file couldn't be opened
.export __debug_load_file
.proc __debug_load_file
	jsr dbgi::get_filename
	jmp edit::load
.endproc

;******************************************************************************
; GOTO BREAK
; Navigates the editor to the line that corresponds to the address where the
; debugger is currently at in the user program.
.proc goto_break
.endproc
	ldxy brkaddr

	; fall through to __debug_gotoaddr

;******************************************************************************
; GOTOADDR
; Navigates the editor to the file/line associated with the give address
; IN:
;  - .XY: the address to "goto"
; OUT:
;  - .C:  set on failure (no line/file could be matched with given address)
;  - .XY: the line that was navigated to
.export __debug_gotoaddr
.proc __debug_gotoaddr
@line=debugtmp+2
	jsr dbgi::addr2line	; get the line #
	bcs @done		; error
	sta dbgi::file
	stxy @line
	jsr __debug_load_file	; load file (if not already)
	bcs @done		; error

	ldxy @line
	jsr edit::gotoline
	ldxy @line
	clc			; ok
@done:	rts
.endproc

;*****************************************************************************
; RESTORE DEBUG ZP
; Restores the $00-$100 values for the debugger
.proc restore_debug_zp
	ldx #$00
:	lda mem::dbg00,x
	sta $00,x
	dex
	bne :-
	rts
.endproc

;******************************************************************************
; RESTORE DEBUG ZP
; Restores the state of the debugger's zeropage
.proc restore_debug_low
	; get return address (stack will be clobbered)
	pla
	clc
	adc #$01
	sta @ret
	pla
	adc #$00
	sta @ret+1

	ldx #$00
:	lda mem::dbg00+$100,x
	sta $100,x
	lda mem::dbg00+$200,x
	sta $200,x
	dex
	bne :-

	; copy around the NMI vector
	ldx #$100-$1a
:	lda mem::dbg00+$31a,x
	sta $31a,x
	dex
	bne :-

	ldx #$18-1
:	lda mem::dbg00+$300,x
	sta $300,x
	dex
	bpl :-

@ret=*+1
	jmp $f00d
.endproc

;******************************************************************************
; SAVE DEBUG ZP
; Saves the state of the debugger's zeropage
.proc save_debug_zp
@zp=mem::dbg00
	ldx #$00
@l0:	lda $00,x
	sta @zp,x
	lda $100,x
	sta @zp+$100,x
	lda $200,x
	sta @zp+$200,x
	lda $300,x
	sta @zp+$300,x
	dex
	bne @l0
	rts
.endproc

;******************************************************************************
; GO
; Runs the user program until the next breakpoint or an NMI occurs
.export __debug_go
.proc __debug_go
	lda #$00
	sta sw_valid		; invalidate stopwatch

	; bounce to the user's program
	lda sim::pc
	sta zp::bankval
	ldxy #TRAMPOLINE_ADDR
	lda #FINAL_BANK_USER
	jsr ram::store

	lda sim::pc+1
	sta zp::bankval
	ldxy #TRAMPOLINE_ADDR+1
	lda #FINAL_BANK_USER
	jsr ram::store

	jsr swapin

	sei
	lda #<$eb15
	sta $0314
	lda #>$eb15
	sta $0314+1
	lda #DEFAULT_900F
	sta $900f
	jsr save_debug_zp
	restore_user_zp

	; reinstall NMI
	lda #<NMI_HANDLER_ADDR
	sta $0318
	lda #>NMI_HANDLER_ADDR
	sta $0318+1
	lda #<(BRK_HANDLER_ADDR+1)
	sta $0316
	lda #>(BRK_HANDLER_ADDR+1)
	sta $0316+1

	lda #$7f
	sta $911e
	sta $911d	; ack all interrupts
	sta $912d

	ldx sim::reg_sp
	txs			; restore user stack

	lda sim::reg_p
	pha			; save status (will pull after bank select)
	lda sim::reg_a
	sta prev_reg_a
	pha			; save .A (to be pulled after bank select)

	ldx sim::reg_x
	stx prev_reg_x
	ldy sim::reg_y
	sty prev_reg_y

	jmp TRAMPOLINE
.endproc

;******************************************************************************
; JUMP
; Runs the user program at the line the cursor is currently on
.proc jump
	ldxy sim::pc
	stxy prev_pc

	jsr edit::currentfile	; get current line # (.XY) and file ID (.A)
	jsr dbgi::line2addr
	bcs @done		; couldn't resolve address
	stxy sim::pc
	jsr __debug_gotoaddr
	jmp edit::sethighlight
@done:	rts
.endproc

;******************************************************************************
; STEP OUT
; Runs the user program until the next RTS is executed
; OUT:
;   - .C: set if we should stop tracing (e.g. if a watch was activated)
.export __debug_step_out
.proc __debug_step_out
	lda #$00
	sta step_out_depth
	sta stop_tracing

@trace: lda stop_tracing
	bne @done
	jsr __debug_step	; run one STEP
	bcs @done		; stop tracing if STEP OUT says we should

	lda sim::op		; get opcode we just ran
	cmp #$20		; did we run a JSR?
	bne :+
	inc step_out_depth	; if we called another subroutine, inc depth
:	cmp #$60		; did we RTS?
	bne @trace
	dec step_out_depth
	bpl @trace		; continue trace until depth is negative
@done:	rts
.endproc

;******************************************************************************
; TRACE
; Puts the debugger into TRACE mode and steps to the next instruction to
; begin the trace
; OUT:
;   - .C: set if we should stop tracing (e.g. if a watch was activated)
.export __debug_trace
.proc __debug_trace
	lda #$00
	sta stop_tracing
@trace: lda stop_tracing
	bne @done
	jsr __debug_step
	jmp @trace

@done:  clc
	rts
.endproc

;******************************************************************************
; QUIT
; Prompts the user for confirmation to quit then exits the debugger upon
; receiving it
.proc quit
	ldxy #strings::debug_stop_debugging
	lda #DEBUG_MESSAGE_LINE
	jsr text::print

:	jsr key::waitch

	; 'N' or QUIT: return back to debugger, 'Y': exit debugger
	cmp #$4e		; N
	beq @done
	cmp #K_QUIT
	beq @done		; don't quit
	cmp #$59		; Y
	bne :-

@quit:	lda #$00		; clear BRK flag
	sta edit::highlight_en	; disable highlighting
	pha			; push 0 status
	plp			; clear flags (.P)

	jsr scr::clrcolor

	jmp edit::init
@done:	rts
.endproc

;******************************************************************************
; EDIT_SOURCE
; Disable all views and reenable (almost) fullscreen editing
.proc edit_source
	lda #$00
	sta aux_mode
	lda #DEBUG_MESSAGE_LINE-1
	jmp edit::resize
.endproc

;******************************************************************************
; EDIT_MEM
; Transfers control to the memory viewer/editor until the user exits it
.proc edit_mem
	lda #DEBUG_INFO_START_ROW-1
	jsr edit::resize

	pushcur
	lda #(DEBUG_INFO_START_ROW)
	jsr scr::clrpart
	jsr showstate		; restore the state

	lda #AUX_MEM
	sta aux_mode

	jsr view::edit
	popcur
	rts
.endproc

;******************************************************************************
; EDIT_BREAKPOINTS
; Transfers control to the breakpoint viewer/editor until the user exits it
.proc edit_breakpoints
	jsr showstate		; restore the state

	lda #AUX_GUI
	sta aux_mode
	jmp brkpt::edit
.endproc

;******************************************************************************
; EDIT_WATCHES
; Transfers control to the watch viewer/editor until the user exits it
.export __debug_edit_watches
.proc __debug_edit_watches
	lda #DEBUG_INFO_START_ROW-1
	jsr edit::resize
	lda #(DEBUG_INFO_START_ROW)
	jsr scr::clrpart
	jsr showstate		; restore the state

	lda #AUX_GUI
	sta aux_mode
	jmp watch::edit
.endproc

;******************************************************************************
; SWAP_USER_MEM
; Command that swaps in the user program memory, waits for a keypress, and
; returns with the debugger's memory swapped back in
.export __debug_swap_user_mem
.proc __debug_swap_user_mem
	; disable coloring in the IRQ
	jsr draw::coloroff

	jsr save_debug_state
	jsr __debug_restore_progstate

	; wait for a key to swap the state back
	jsr key::waitch

	; reenable coloring
	inc mem::coloron

	; restore debugger state
	jmp restore_debug_state
.endproc

;******************************************************************************
; PRINT TRACING
; Prints the "tracing" info
.proc print_tracing
	ldxy #strings::tracing
	lda #REGISTERS_LINE-1
	jsr text::print			; print the TRACING message
	rts
.endproc

;******************************************************************************
; STEP OVER
; Runs the next instruction from the .PC and returns to the debug prompt.
; Unlike STEP, if the next instruction is a JSR, execution will continue
; at the line after the subroutine (after the subroutine has run)
.export __debug_step_over
.proc __debug_step_over
	jsr __debug_step	; run one STEP
	bcs @done		; stop tracing if STEP errored
	lda sim::op		; get opcode we just ran
	cmp #$20		; did we run a JSR?
	bne @done		; if not, we're done
	jsr __debug_step_out	; if we did enter a subroutine, STEP OUT
@done:	rts
.endproc

;******************************************************************************
; STEP
; Runs the next instruction from the .PC and returns to the debug prompt.
; This works by inserting a BRK instruction after
; the current instruction and RUNning.
; OUT:
;   - .C: set if we should stop tracing (e.g. if a watch was activated)
.export __debug_step
.proc __debug_step
	; save the state of the registers before the STEP
	lda sim::reg_a
	sta prev_reg_a
	lda sim::reg_x
	sta prev_reg_x
	lda sim::reg_y
	sta prev_reg_y

	ldxy #$f00d		; use ROM address because we don't need string
	stxy r0
	ldxy sim::pc		; get address of next instruction
	jsr asm::disassemble	; disassemble it to get its size (BRK offset)
	stx sim::op_mode
	bcc @ok

	inc sim::illegal
	; TODO: handle illegals here?
	;sec
	rts

@ok:	pha				; save instruction size
	ldxy sim::pc			; address of instruction
	jsr sim::get_side_effects	; get state that will be clobbered/used

; for updating watches and just general info for the user, save the current
; state of memory that will be altered
	; clear watch flags
	ldx watch::num
	beq @check_effects
:	lda #$ff^WATCH_DIRTY
	and watch::flags-1,x
	sta watch::flags-1,x
	dex
	bne :-

@check_effects:
	lda sim::affected
	and #OP_LOAD|OP_STORE		; was there a memory read/write?
	beq @step			; if not, skip ahead to setting the next BRK
	ldxy sim::effective_addr	; if yes, mark the watch if there is one
	jsr watch::mark			; check if there's a watch at this addr
	bcc @step			; if there's no watch at addr, continue

	; activate the watch window so user sees change
	; restore zp (if we were tracing, must be restored)
	pha
	jsr save_debug_zp
	jsr restore_debug_zp
	pla
	jsr watch_triggered ; display a message indicating watch was triggered
	rts

; perform the step
@step:	pla			; get instruction size
	ldxy sim::pc		; and address of instruction to-be-executed
	jsr sim::step		; execute the STEP
	jsr safety_check	; check for JAMs/suspicious writes/etc.
	bcc @countcycles	; if ok, continue to count cycles
	;sec
	rts			; unsafe instruction- return to debugger

@countcycles:
	; count the number of cycles that the next instruction will take
	jsr sim::count_cycles	; get the # of cycles for the instruction
	clc
	adc sim::stopwatch
	sta sim::stopwatch
	bcc @done
	inc sim::stopwatch+1
	bne @done
	inc sim::stopwatch+2

@done:	RETURN_OK		; return to the debugger
.endproc

;*******************************************************************************
; SAFETY_CHECK
; Checks if the CPU will be jammed after executing the next instruction or if
; a critical memory location will be clobbered.
.proc safety_check
	; is the next instruction a JAM?
	lda sim::jammed
	bne jam_detected

	; does the next instruction write to memory?
	lda sim::affected
	and #OP_STORE
	beq @ok

	ldx #@num_safe_addrs-1
@l0:	lda sim::effective_addr
	cmp @safeaddrs_lo,x
	bne @next
	lda sim::effective_addr+1
	cmp @safeaddrs_hi,x
	bne @next

; an important memory location will be clobbered
@clobber:
	ldx #<strings::vital_addr_clobber
	ldy #>strings::vital_addr_clobber
	bne print_msg

@next:	dex
	bpl @l0
@ok:	RETURN_OK

.PUSHSEG
.RODATA
.define safety_addrs $0316, $0317, $0318, $0319
	@safeaddrs_lo: .lobytes safety_addrs
	@safeaddrs_hi: .hibytes safety_addrs
	@num_safe_addrs=*-@safeaddrs_hi
.POPSEG
.endproc

;*******************************************************************************
; JAM_DETECTED
; This procedure is called when STEP (via step, trace, etc.) encounters a JAM
; instruction
.proc jam_detected
	ldx #<strings::jam_detected
	ldy #>strings::jam_detected
	bne print_msg
.endproc

;*******************************************************************************
; WATCH_TRIGGERED
; This procedure is called when STEP (via step, trace, etc.) reads/writes to a
; memory location that is being watched
.proc watch_triggered
	; display the watch that was triggered
	jsr watch::tostring

	; fall through to print_msg
.endproc

;*******************************************************************************
; PRINT MSG
; Prints a message for the TUI or GUI (whichever is active)
; IN:
;   - .XY: the message to print
.proc print_msg
	lda __debug_interface
	beq @gui

@tui:	lda #REGISTERS_LINE-1
	CALL FINAL_BANK_CONSOLE, con::puts
	RETURN_OK

@gui:	lda #REGISTERS_LINE-1
	jsr text::print
	jsr scr::clrcolor
	jsr key::waitch		; wait for keypress
	sec
	rts
.endproc

;*******************************************************************************
; ILLEGAL DETECTED
; Prints a message that an illegal opcode was detected and waits for
; user input
.proc illegal_detected
	; illegal instruction detected, give a warning
	ldx #<strings::illegal_detected
	ldy #>strings::illegal_detected
	bne print_msg			 ; branch always
.endproc

;*******************************************************************************
; SWAPIN
; This routine is called _before_ returning from the BRK interrupt and executing
; the user program.
.proc swapin
	; swap entire user RAM in (needed if we don't know what memory will
	; be changed before next BRK)
	jsr save_debug_state
	jmp __debug_restore_progstate
.endproc

;******************************************************************************
; SWAPOUT
; Swaps *out* the user memory that needs to be saved in order to restore the
; debug state.
.proc swapout
	; save the program state before we restore the debugger's
	jsr __debug_save_prog_state
	jmp restore_debug_state		; restore debugger state
.endproc

;*******************************************************************************
; SETBRKATLINE
; Sets a breakpoint at the given line.  During assembly the address will be
; populated.
; IN:
;  - .XY: the line number to set the breakpoint at
;  - .A:  the file ID to set the breakpoint in
; OUT:
;  - .A: the ID of the breakpoint that was added
.export __debug_setbrkatline
.proc __debug_setbrkatline
	pha				; save file ID

	; store the line #
	txa
	ldx __debug_numbreakpoints
	sta __debug_breakpoint_lineslo,x
	tya
	sta __debug_breakpoint_lineshi,x

	pla				; restore file ID
	sta __debug_breakpoint_fileids,x
	lda #BREAKPOINT_ENABLED
	sta breakpoint_flags,x

	lda __debug_numbreakpoints
	inc __debug_numbreakpoints
	rts
.endproc

;*******************************************************************************
; SETBRKATADDR
; Sets a breakpoint at the given address.
; This can be used while debugging to set breakpoints at addresses directly,
; even if they don't correspond to a line or file
; IN:
;  - .XY: the address to set the breakpoint at
.export __debug_setbrkataddr
.proc __debug_setbrkataddr
	txa
	ldx __debug_numbreakpoints
	sta breakpointslo,x
	tya
	sta breakpointshi,x

	lda #BREAKPOINT_ENABLED
	sta breakpoint_flags,x

	inc __debug_numbreakpoints
	rts
.endproc


;*******************************************************************************
; BRKSETADDR
; Sets the address for the given (existing) breakpoint.
; If no matching breakpoint is found, does nothing
; IN:
;   - .XY: the line # of the breakpoint to set the address for
;   - .A:  the file ID for the breakpoint to set
;   - r0:  the address to store for the breakpoint
; OUT:
;   - .C: set if there is no breakpoint at the given line/file
.export __debug_brksetaddr
.proc __debug_brksetaddr
@addr=r0
	jsr brkpt::getbyline	; get breakpoint # in .X
	bcs @done		; no match
@found:	; store address
	lda @addr
	sta breakpointslo,x
	lda @addr+1
	sta breakpointshi,x
@done:	rts
.endproc

;*******************************************************************************
; REMOVEBREAKPOINT
; Removes a breakpoint at the address in .XY
; IN:
;  - .XY: the address of the breakpoint to remove
; OUT:
;  - .C: set if breakpoint doesn't exist
.export __debug_remove_breakpoint
__debug_remove_breakpoint:
.proc remove_breakpoint
	jsr get_breakpoint
	bcs :+			; no breakpoint to remove -> rts

	tax
	; fall through to removebreakpointbyid
.endproc

;*******************************************************************************
; REMOVEBREAKPOINTBYID
; Removes the ID with the given handle.
; IN:
;  - .X: the breakpoint to remove
; OUT:
;  - .C: set if no breakpoint was removed
.export __debug_removebreakpointbyid
.proc __debug_removebreakpointbyid
@addr=debugtmp
@data=debugtmp+2
@id=debugtmp+4
	cpx __debug_numbreakpoints
	bcs :+				; no breakpoint of given ID

	stx @id
	ldxy #breakpoint_data
	stxy @data

	ldx #NUM_BREAKPOINT_TABLES

	; shift breakpoints down
@l0:	ldy @id
	iny
@l1:	lda (@data),y
	dey
	sta (@data),y
	iny
	iny
	cpy __debug_numbreakpoints
	bcc @l1

	lda @data
	; sec
	adc #MAX_BREAKPOINTS-1
	sta @data
	bcc @next
	inc @data+1

@next:	dex
	bne @l0
	dec __debug_numbreakpoints

@done:	clc
:	rts
.endproc

;*******************************************************************************
; SHIFT BREAKPOINTS D
; Shifts the line numbers for all breakpoints on lines below the current one
; IN:
;  - .XY: the line number to shift
;  - .A:  the offset to shift
;  - r0:  the file ID of the file to shift
.export __debug_shift_breakpointsd
.proc __debug_shift_breakpointsd
@fileid=r0
@line=r1
@offset=r3
	stxy @line
	sta @offset
	ldx __debug_numbreakpoints
	beq @done
	dex
@l0:	lda @fileid
	cmp __debug_breakpoint_fileids,x
	bne @next
	lda __debug_breakpoint_lineshi,x
	cmp @line+1
	bcc @next
	lda __debug_breakpoint_lineslo,x
	adc #$00
	cmp @line
	bcc @next
	sbc #$01
	clc
	adc @offset
	sta __debug_breakpoint_lineslo,x
	bcc @next
	inc __debug_breakpoint_lineshi,x
@next:	dex
	bpl @l0
@done:	rts
.endproc

;*******************************************************************************
; SHIFT BREAKPOINTS U
; Shifts UP the line numbers for all breakpoints on lines below the current one
; given by the given offset.
; IN:
;  - .XY: the line number to shift
;  - .A:  the offset to shift
;  - r0:  the file ID of the file to shift within
.export __debug_shift_breakpointsu
.proc __debug_shift_breakpointsu
@fileid=r0
@line=r1
@offset=r3
	stxy @line
	sta @offset
	ldx __debug_numbreakpoints
	beq @done
	dex
@l0:	lda @fileid
	cmp __debug_breakpoint_fileids,x
	bne @next
	lda __debug_breakpoint_lineshi,x
	cmp @line+1
	bcc @next
	lda __debug_breakpoint_lineslo,x
	cmp @line
	beq @next
	bcc @next
	sec
	sbc @offset
	sta __debug_breakpoint_lineslo,x
	bcs @next
	dec __debug_breakpoint_lineshi,x
@next:	dex
	bpl @l0
@done:	rts
.endproc

;*******************************************************************************
; GET BREAKPOINT
; Returns the ID (index) of the breakpoint corresponding to the given address
; IN:
;  - .XY: the address of the breakpoint to get the ID of
; OUT:
;  - .C: set if no breakpoint is found
;  - .A: the id of the breakpoint
.proc get_breakpoint
@addr=r0
	stxy @addr
	ldx __debug_numbreakpoints
	beq @notfound
@l0:	lda @addr
	cmp breakpointslo,x
	bne @next
	lda @addr+1
	cmp breakpointshi,x
	beq @found
@next:	dex
	bpl @l0
@notfound:
	ldxy @addr
	sec
	rts		; not found

@found: txa
	RETURN_OK
.endproc

;*******************************************************************************
; EDIT STATE
; Moves the cursor to the registers area and allows the user to modify
; their values with hex input
.proc edit_state
	pushcur
	lda #$00
	sta zp::curx
	lda #REGISTERS_LINE+1
	sta zp::cury

	jsr showstate		; fill linebuffer with register state

@edit:	jsr cur::on
	jsr key::waitch
	pha
	jsr cur::off
	pla

	cmp #K_LEFT
	beq @back
	cmp #K_DEL
	bne @ret
@back:	lda zp::curx
	beq @edit		; can't move LEFT
	dec zp::curx
	ldx #@numoffsets-1
@prevx:	cmp @offsets,x		; was cursor at start of offset?
	bcc @prev		; if cursor is < offset, check prev offset
	bne @ok			; if it was NOT at offset start, it is now
	lda @offsets-1,x
	adc #$00		; +1
	sta zp::curx
	jmp @refresh
@prev:	dex
	bpl @prevx
	bmi @edit

@ret:	cmp #K_RETURN
	beq @updatevals
@quit:	cmp #K_QUIT
	beq @exit
@right:	cmp #K_RIGHT
	beq @fwd

@hex:	jsr key::ishex
	bcc @refresh

	ldx zp::curx
	sta mem::linebuffer2,x

@fwd:   lda zp::curx
	cmp #REG_SP_OFFSET+1	; check offset
	bcs @refresh		; if already at last column, don't advance
	inc zp::curx		; bump up curx
	ldx #$00
; align x-position to either a value in @offsets or a value in @offests+1
@nextx:	cmp @offsets,x		; was X at the start of the offset?
	beq @refresh		; if so, incrementing it by 1 was sufficient
	bcs @next		; if X
@ok:	lda @offsets,x		; if @offsets,x <= curx, set curx to it
	sta zp::curx
	jmp @refresh
@next:	inx
	cpx #@numoffsets
	bne @nextx

@refresh:
	lda #REGISTERS_LINE+1
	ldxy #mem::linebuffer2
	jsr text::puts
	jmp @edit

;--------------------------------------
; parse the linebuffer2 and update all registers
@updatevals:
@val=r0
	ldxy #mem::linebuffer2
	stxy @val

	ldx #@numoffsets-1
@l0:	ldy @offsets,x
	lda (@val),y
	jsr util::chtohex	; get MSB
	asl
	asl
	asl
	asl
	sta sim::register_state,x

	iny
	lda (@val),y		; get LSB
	jsr util::chtohex
	ora sim::register_state,x
	sta sim::register_state,x

	dex
	bpl @l0

	; swap PC LSB and MSB
	ldx sim::pc
	lda sim::pc+1
	sta sim::pc
	stx sim::pc+1

@exit:	popcur
	rts

.PUSHSEG
.RODATA
@offsets:
.byte REG_PC_OFFSET, REG_PC_OFFSET+2, REG_A_OFFSET, REG_X_OFFSET, REG_Y_OFFSET
.byte REG_SP_OFFSET
@numoffsets=*-@offsets
.POPSEG
.endproc

;******************************************************************************
; SHOWSTATE
; Shows the current debug state (registers and BRK line)
.proc showstate
	jsr showbrk

	; fall through to showregs
.endproc

;*******************************************************************************
; SHOWREGS
; prints the contents of the registers in the format
;   PC  A  X  Y  SP NV-BDIZC ADDR
;  f59c 02 02 00 f7 00100000 1003
.proc showregs
	; display the register names
	ldxy #strings::debug_registers
	lda #REGISTERS_LINE
	jsr text::print

	jsr __debug_regs_contents
	lda #REGISTERS_LINE+1
	jmp text::puts
.endproc

;*******************************************************************************
; REGS_CONTENTS
; Returns a line containing the contents of the registers
; OUT: mem::linebuffer: a line of text containing the values for each tegister
;      matches the format: PC  A  X  Y  SP NV-BDIZC ADDR
.export __debug_regs_contents
.proc __debug_regs_contents
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

@colordone:
; if memory was loaded or stored, show the effective address
@memaddr:
	lda sim::affected
	and #OP_LOAD|OP_STORE
	bne :+
	lda #'-'
	sta @buff+27
	sta @buff+28
	sta @buff+29
	sta @buff+30
	bne @clk

:	lda sim::effective_addr+1
	jsr util::hextostr
	sty @buff+27
	stx @buff+28
	lda sim::effective_addr
	jsr util::hextostr
	sty @buff+29
	stx @buff+30

@clk:	lda sw_valid
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

;*******************************************************************************
; SHOWBRK
; Display the BRK line number or address
.proc showbrk
	lda lineset		; is the line # known?
	bne @showline		; if so, show it

@showaddr:
	; we couldn't find the line #; display the address of the BRK
	ldxy #$100
	stxy r0
	ldxy sim::pc
	jsr asm::disassemble
	bcc :+

	; couldn't disassemble (illegal opcode?), just get the hex value for
	; the byte
	ldxy sim::pc
	jsr vmem::load
	jsr util::hextostr
	sty $100
	stx $101
	lda #$00
	sta $102

:	lda #>$100
	pha
	lda #<$100
	pha

	lda sim::pc
	pha
	lda sim::pc+1
	pha
	ldxy #strings::debug_brk_addr
	jmp @print

@showline:
	; display the BRK message
	lda edit::highlight_line	; push line #
	pha
	lda edit::highlight_line+1
	pha

	lda dbgi::file
	jsr dbgi::get_filename
	tya
	pha
	txa
	pha

	ldxy #strings::debug_brk_line
@print:	lda #DEBUG_MESSAGE_LINE
	jsr text::print		; break in line <line #>
	rts
.endproc

;*******************************************************************************
; SHOW_AUX
; Displays the memory viewer, breakpoint viewer, or watchpoint viewer depending
; on which is enabled
.proc show_aux
	lda aux_mode
	beq @none		; no aux mode
	cmp #AUX_MEM
	bne @gui
@mem:	jmp view::mem		; refresh the memory viewer
@gui:	jmp gui::refresh	; refresh the active GUI
@none:	rts
.endproc

;*******************************************************************************
; RESET STOPWATCH
; Resets the stopwatch
.proc reset_stopwatch
	lda #$01
	sta sw_valid
	lda #$00
	sta sim::stopwatch
	sta sim::stopwatch+1
	sta sim::stopwatch+2
	rts
.endproc

;*******************************************************************************
; ACTIVATE MONITOR
; Activates the text user interface debugger (monitor)
.proc activate_monitor
	lda #DEBUG_IFACE_TEXT
	sta __debug_interface
	jsr save_debug_state
	jmp edit::enterconsole
.endproc

.RODATA
;*******************************************************************************
; COMMANDS
; This table contains the keys used to invoke the corresponding command
; within the debugger
commands:
	.byte K_QUIT_DEBUGGER
	.byte K_QUIT
	.byte K_STEP
	.byte K_STEPOVER
	.byte K_GO
	.byte K_JUMP
	.byte K_STEPOUT
	.byte K_TRACE
	.byte K_SRCVIEW
	.byte K_MEMVIEW
	.byte K_BRKVIEW
	.byte K_WATCHVIEW
	.byte K_SWAP_USERMEM
	.byte K_RESET_STOPWATCH
	.byte K_EDIT_STATE
	.byte K_GOTO_BREAK
	.byte K_CONSOLE
num_commands=*-commands

.linecont +
.define command_vectors quit, edit_source, __debug_step, __debug_step_over, \
	__debug_go, jump, __debug_step_out, __debug_trace, edit_source, \
	edit_mem, edit_breakpoints, __debug_edit_watches, \
	__debug_swap_user_mem, reset_stopwatch, edit_state, \
	goto_break, activate_monitor
.linecont -
command_vectorslo: .lobytes command_vectors
command_vectorshi: .hibytes command_vectors

;*******************************************************************************
; DISABLED COMMANDS
; the following commands are NOT propagated to the editor. they become a nop
; when handled by the debugger
disabled_commands:
	.byte K_CLOSE_BUFF
	.byte K_ASM
	.byte K_ASM_DEBUG
	.byte K_REFRESH
	.byte $76		; v (enter visual)
	.byte $56		; V (enter visual line)
num_disabled_commands=*-disabled_commands
