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
ACTION_START          = 3	; action for initial debug entry
ACTION_GO_START       = 4	; action for first instruction of GO command
ACTION_GO             = 5	; action for subsequent GO instructions
ACTION_TRACE_START    = 6
ACTION_TRACE          = 7	; action for TRACE command
ACTION_STEP_OUT_START = 8
ACTION_STEP_OUT       = 9	; action for STEP OUT command

;*******************************************************************************
; IFACE (interface) constants
; These are the valid values for dbg::interface. This value determines the
; interface that is entered in the debug breakpoint handler
DEBUG_IFACE_GUI  = 0	; GUI interface (returns to visual debugger)
DEBUG_IFACE_TEXT = 1	; text interface (returns to TUI)

; The number of cycles to load into the VIA timer for the timer "step"
; interrupt.  This value is exactly enough to countdown to 1 by the time the
; next user instruction is executed
NMI_TIMER_VAL = (2+3+4+4+6)+5
NMI_IER       = NMI_HANDLER_ADDR+11	; address of the value to set $911e to
					; before returning from debugger

; Max depth the debugger may reach during handling of a step during the TRACE
; command. This amount will be saved/restored by the debugger before handling
; the debugged program's next instruction.
TRACE_STACK_DEPTH = 16

;*******************************************************************************
debugtmp = zp::debuggertmp	; scratchpad

;*******************************************************************************
; Program state variables

.segment "SHAREBSS"

.export __debug_interface
__debug_interface: .byte DEBUG_IFACE_GUI

.BSS
swapmem: .byte 0	; not zero if we need to swap in user RAM

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
startsave:
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
advance:   .byte 0	; not zero if a command continues program execution

aux_mode:       .byte 0	; the active auxiliary view
highlight_line: .word 0 ; the line we are highlighting
highlight_file: .word 0	; filename of line we are highlighting

action:	.byte 0		; the last action performed e.g. ACTION_STEP

brkcond: .word 0	; if !0, address to handler to break TRACE on

is_brk: .byte 0		; set if interrupt was triggered by BRK

step_out_depth: .byte 0 ; # of RTS's to wait for when "stepping out"

stop_tracing: .byte 0	; if !0, debugger will stop a TRACE at the next STEP


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

	ldx #TRACE_STACK_DEPTH-1
:	lda mem::dbg00+$200-TRACE_STACK_DEPTH,x
	sta $200-TRACE_STACK_DEPTH,x
	dex
	bpl :-
.endmacro

;******************************************************************************
; SAVE USER ZP TRACE
; Saves just the state of the user's zeropage that is clobbered while tracing
.macro save_user_zp_trace
	; TODO: what is clobbering this?
	ldx #$0a
:	lda $d0,x
	sta mem::prog00+$d0,x
	dex
	bpl :-

	lda zp::bankval
	sta mem::prog00+zp::bankval
	lda zp::banktmp
	sta mem::prog00+zp::banktmp
	lda zp::banktmp+1
	sta mem::prog00+zp::banktmp+1
	lda zp::bankoffset
	sta mem::prog00+zp::bankoffset
	lda r0
	sta mem::prog00+r0
	lda r1
	sta mem::prog00+r1
	lda r2
	sta mem::prog00+r2
	lda r3
	sta mem::prog00+r3
	lda r4
	sta mem::prog00+r4
	lda r5
	sta mem::prog00+r5
	lda r6
	sta mem::prog00+r6
	lda r7
	sta mem::prog00+r7
	lda r8
	sta mem::prog00+r8
	lda r9
	sta mem::prog00+r9
	lda ra
	sta mem::prog00+ra
	lda rb
	sta mem::prog00+rb

	ldx #TRACE_STACK_DEPTH-1
:	lda $200-TRACE_STACK_DEPTH,x
	sta mem::prog00+$200-TRACE_STACK_DEPTH,x
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
; RESTORE USER ZP TRACE
; Restores the state of the user's zeropage that is clobbered during a trace
.macro restore_user_zp_trace
	ldx #$0f

	; TODO: what is clobbering this?
:	lda mem::prog00+$d0,x
	sta $d0,x
	dex
	bpl :-

	lda mem::prog00+zp::bankval
	sta zp::bankval
	lda mem::prog00+zp::banktmp
	sta zp::banktmp
	lda mem::prog00+zp::banktmp+1
	sta zp::banktmp+1
	lda mem::prog00+zp::bankoffset
	sta zp::bankoffset

	lda mem::prog00+r0
	sta r0
	lda mem::prog00+r1
	sta r1
	lda mem::prog00+r2
	sta r2
	lda mem::prog00+r3
	sta r3
	lda mem::prog00+r4
	sta r4
	lda mem::prog00+r5
	sta r5
	lda mem::prog00+r6
	sta r6
	lda  mem::prog00+r7
	sta r7
	lda mem::prog00+r8
	sta r8
	lda mem::prog00+r9
	sta r9
	lda mem::prog00+ra
	sta ra
	lda mem::prog00+rb
	sta rb

	ldx #TRACE_STACK_DEPTH-1
:	lda mem::prog00+$200-TRACE_STACK_DEPTH,x
	sta $200-TRACE_STACK_DEPTH,x
	dex
	bpl :-
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
	lda #ACTION_START
	sta action

	; set the simulator's PC value
	stxy sim::pc
	stxy prev_pc

	; backup the byte that will be overwritten by our BRK
	jsr vmem::load
	sta startsave

	; and install the first BRK at the debug start address
	ldxy sim::pc
	lda #$00
	sta aux_mode		; initialize auxiliary views
	sta watch::num		; clear watches
	jsr vmem::store

	lda #DEFAULT_RVS
	ldx #DEBUG_MESSAGE_LINE
	jsr draw::hline

	jsr reset_stopwatch

	jsr install_brk			; install the BRK handler IRQ

	jsr save_debug_state 		; save the debug state
	jsr save_debug_zp

	jsr __debug_restore_progstate	; copy in user program

	sei	; disable IRQ's (user's interrupt will be installed)

	restore_user_zp

	; initialize the user program stack
	ldx #$e0		; TODO: calculate max value
	txs

	lda #$01
	sta swapmem			; on 1st iteration, swap entire RAM back

@basic:	; initialize machine state
	ldxy #BRK_HANDLER_ADDR+1
	stxy $0316			; BRK
	ldxy #NMI_HANDLER_ADDR
	stxy $0318			; NMI

	ldx #@bootloader_size
:	lda @bootloader-1,x
	sta r0-1,x
	dex
	bne :-
	ldxy sim::pc
	stxy re
	jmp r0
.PUSHSEG
.RODATA
@bootloader:
	lda #FINAL_BANK_USER
	; TODO: use different FE3 mode to enable write-protect
	; when running unexpanded program
	sta $9c02
	jmp (re)
@bootloader_size=*-@bootloader

.POPSEG
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
.POPSEG

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

	ldxy #BRK_HANDLER_ADDR
	stxy @dst
	lda #brkhandler2_size-1
	sta @cnt
; copy part 2 of the BRK handler to our memory
@l1:	ldy @cnt
	lda brkhandler2,y
	sta zp::bankval
	sty zp::bankoffset
	ldxy @dst
	lda #FINAL_BANK_MAIN
	jsr ram::store_off
	dec @cnt
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

	; TODO: check memory configuration; skip this for 8+K expanded
	;jsr $fd8d	; initialize and test RAM
	jsr $fd52	; restore default I/O vectors
	jsr $fdf9	; initialize I/O registers
	jsr $fdca	; set top of RAM to $2000 (for unexpanded)
	jsr $e518	; initialize hardware
	jsr $e45b	; init BASIC vectors
	jsr $e3a4
	jsr $e404
	save_user_zp
	jsr __debug_save_prog_state

	sei
	lda #$7f
	sta $911e			; disable NMI's

	jsr restore_debug_zp
	jsr restore_debug_low
	jsr restore_debug_state
	jsr irq::on
	jmp edit::init
.endproc

;******************************************************************************
; SAVE_PROG_STATE
; saves memory clobbered by the debugger (screen, VIC registers and color)
.export __debug_save_prog_state
.proc __debug_save_prog_state
@losave=mem::prog00+$100
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
; DEBUG_RESTORE
; Handles the RESTORE NMI while the debugger is active
.proc debug_restore
	inc stop_tracing
	rti
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

	tsx
	lda $104,x	; get status
	and #$10	; mask BRK flag
	sta is_brk

	; save the registers pushed by the KERNAL interrupt handler ($FF72)
	pla
	sta sim::reg_y
	pla
	sta sim::reg_x
	pla
	sta sim::reg_a
	pla
	sta sim::reg_p
	pla
	sta sim::pc
	pla
	sta sim::pc+1
	tsx
	stx sim::reg_sp

	; TODO: save timer values

	; clear decimal in case user set it
	cld

	lda is_brk	; was interrupt triggered  by a BRK?
	bne @notnmi	; if so, skip NMI handler

; An NMI interrupt occurred while we were returning to the user program (while
; we were still in the NMI/BRK ISR).
; Most commonly this will occur during tracing.  When tracing, we don't
; acknowledge NMI's because that is how the user breaks from the trace.
; If there is a pending NMI (there was a rising edge on CA1 while the debugger
; was running the next step of the TRACE) when we reenable interrupts,
; it will trigger.
; There are also a few cycles when a user could theoretically trigger an
; NMI by pressing RESTORE immediately after executing a STEP or similar
; instruction.  We acknowledge NMI's right before running the user
; program, but it is possible to trigger one while we are still in the process.
; In either case, when this is detected (by the PC being in the NMI/BRK handler
; range), we return from the interrupt to allow the ISR (debugger) to finish
; returning to the user program.
; If we were tracing, we also set the action to STEP so that tracing halts.
@nmi:	ldxy sim::pc
	cmpw #NMI_HANDLER_ADDR
	bcc @restore_debugger
	cmpw #BRK_HANDLER_TOP+1
	bcs @restore_debugger
	lda #$7f
	sta $911d	; ack all interrupts
	jmp debug_rti	; finish the ISR that was interrupted by the NMI

@notnmi:
	decw sim::pc	; BRK pushes PC + 2, subtract 2 from PC
	decw sim::pc

@restore_debugger:
	sei

	; reinit the debugger's SP
	ldx #$ff
	txs

	jsr tracing
	beq @backup_for_trace
	jmp @notrace

@backup_for_trace:
	; if tracing, for the duration the debugger is active, enable an NMI to
	; catch the user's signal to stop the trace (RESTORE)
	ldxy #debug_restore
	stxy $0318

	lda #$82
	sta $911e	; enable CA1 (RESTORE key) NMIs while in debugger

	save_user_zp_trace
	restore_debug_zp_trace
	jmp @swapout

	; save the user's zeropage and restore the debugger's
@notrace:
	save_user_zp
	jsr restore_debug_low
	jsr restore_debug_zp

@swapout:
	; save program state and swap the debugger state in
	jsr swapout

	; reinstall the main IRQ
        jsr irq::on

	; unless we can figure out the exact RAM we will affect, we'll have to
	; swap in the entire user RAM before we return from this BRK
	lda #$01
	sta swapmem

	lda #$00
	sta lineset		; flag that line # is (yet) unknown

@uninstall_brks:
	jsr tracing
	beq :+
	; unless tracing, uninstall breakpoints and reinstall updated ones
	; if there is a breakpoint at the current PC, we will ignore it
	jsr uninstall_breakpoints
	jsr install_breakpoints

:	; update TRACE_START, STEP_OUT_START, and GO to their subsequent actions
	lda action
	cmp #ACTION_TRACE_START
	beq @update_trace_action
	cmp #ACTION_STEP_OUT_START
	bne :+

@update_trace_action:
	; move from TRACE_START -> TRACE, install breakpoints, and begin trace
	inc action
	ldxy #strings::tracing
	lda #REGISTERS_LINE-1
	jsr text::print
	jmp @continue_debug

:	; if we're beginning a GO, get on with it
	cmp #ACTION_GO_START
	bne @update_watches

	; continue the GO command
	jsr step_restore
	inc action

@continue_debug:
	jmp __debug_done	; continue execution

@update_watches:
	jsr watch::update

	; check if action was STEP or TRACE
	lda action
	cmp #ACTION_STEP
	beq @handle_action
	cmp #ACTION_TRACE
	beq @handle_action

@reset_affected:
	; if action wasn't STEP or TRACE, we don't know enough to say
	; what was affected since last BRK
	lda #$00
	sta sim::affected

@handle_action:
	lda action
	cmp #ACTION_START
	bne :+
	lda startsave		; restore opcode
	ldxy sim::pc
	jsr vmem::store
	jmp return_to_debugger

:	jsr stepping		; are we stepping?
	bne return_to_debugger
@restore_step:
	jsr step_restore

	; if we are doing a TRACE, install breakpoints and continue
	; TODO: replace below with more flexbile condition based BRK (brkcond)
	lda action
	cmp #ACTION_STEP_OUT
	bne @chktrace
@stepout:
	lda stop_tracing	; should we stop the trace?
	bne @exit_trace		; if so, return to debugger
	lda stepsave		; get the opcode we BRK'd on
	pha
	jsr __debug_step_out_next	; run one more STEP to get out of subroutine
	pla				; get previous opcode
	rol stop_tracing	; stop tracing if STEP OUT says we should

	cmp #$20		; did we run a JSR?
	bne :+
	inc step_out_depth	; if we called another subroutine, inc depth
:	cmp #$60		; did we RTS?
	bne @continue_trace
	dec step_out_depth
	bpl @continue_trace	; continue trace

@stepout_done:
	lda #ACTION_STEP	; if we found the RTS, quit
	sta action		; flag that we're done stepping out
	bne @continue_trace

@chktrace:
	cmp #ACTION_TRACE
	bne return_to_debugger	; if not TRACE/STEP_OUT, return to debugger

@trace:	; check if the BRK that was triggered is a breakpoint or just the
	; step point. If the latter, continue tracing
	ldxy sim::pc
	jsr get_breakpoint
	bcc @exit_trace		; breakpoint, return control to debugger
	lda stop_tracing	; should we stop the trace?
	bne @exit_trace		; if so, return to debugger
	jsr __debug_trace_next	; run the next step of the trace
	rol stop_tracing	; stop tracing if STEP OUT says we should
@continue_trace:
	jmp __debug_done

@exit_trace:
	lda #$7f
	sta $911e		; disable all NMI's
	jsr restore_debug_zp
	jsr restore_debug_low
	jsr restore_debug_state
	inc swapmem
	jsr irq::on
	jsr uninstall_breakpoints
	jsr install_breakpoints
.endproc

;******************************************************************************
; RETURN TO DEBUGGER
; Entrypoint to the debugger
.proc return_to_debugger
	lda #$00
	sta action
	sta advance	; by default, don't return to program after command

; we're done updating state, check which interface we should transfer
; execution to
@enter_iface:
	lda __debug_interface
	beq @iface_gui		; if interface is GUI, continue

@iface_tui:
	; display the contents of the registers
	CALL FINAL_BANK_CONSOLE, dbgcmd::regs

@debugloop_tui:
	CALL FINAL_BANK_CONSOLE, con::reenter
	jmp @finishlooptui

@iface_gui:
	jsr show_aux		; display the auxiliary mode

@showbrk:
	; get the address before the BRK and go to it
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
	jmp @loopdone

@runcmd:
	lda command_vectorslo,x	 ; vector LSB
	sta zp::jmpvec
	lda command_vectorshi,x  ; vector MSB
	sta zp::jmpvec+1

	jsr zp::jmpaddr		; call the command
	jsr cur::on

@finishlooptui:
	lda __debug_interface
	bne @loopdone

@finishloopgui:
	jsr showstate		; restore the register display (may be changed)

@loopdone:
	lda advance		; are we ready to execute program? (GO, STEP)
	beq @debugloop		; not yet, loop and get another command
	jsr cur::off

	; fall through to __debug_done
.endproc

;******************************************************************************
; DONE
; Returns from the debug BRK interrupt.
; The text debugger also returns here to restore the program state and
; continue execution of the program.
.export __debug_done
.proc __debug_done
	jsr swapin

@dummyirq:
	sei
	lda #<$eb15
	sta $0314
	lda #>$eb15
	sta $0314+1
	lda #DEFAULT_900F
	sta $900f

	; restore VIA state that was clobbered
	lda sim::via2+$e
	sta $912e

	; can't call anything after restoring stack/sp
	; get state variables for "tracing" and "stepping"
	jsr stepping
	lda #$00
	rol
	sta is_step
	jsr tracing
	bne @notrace

	jsr save_debug_zp_trace
	restore_user_zp_trace
	jmp restore_regs

@notrace:
	jsr save_debug_zp
	restore_user_zp

restore_regs:
	ldx sim::reg_sp	; restore SP
	txs

	; from top to bottom: [STATUS, <PC, >PC]
	lda sim::pc+1
	sta prev_pc+1
	pha
	lda sim::pc	; restore PC
	sta prev_pc
	pha

	lda sim::reg_p	; restore processor status (RTI will pull)
	pha

	lda #$7f
	sta $911d	; ack all interrupts
	sta $912d
.endproc

.proc debug_rti
	lda sim::reg_a
	sta prev_reg_a
	ldx sim::reg_x
	stx prev_reg_x
	ldy sim::reg_y
	sty prev_reg_y

	; TODO: restore timer values

	pha		; save .A (to be pulled after bank select)

	lda #$7f
	sta $911e	; disable NMI's

	lda action
	cmp #ACTION_GO
	bne @dummynmi

	; install the GO NMI handler for the user program
	lda #<NMI_HANDLER_ADDR
	sta $0318
	lda #>NMI_HANDLER_ADDR
	sta $0319
	bne @rti

@dummynmi:
	lda #<$ff56
	sta $0318
	lda #>$ff56
	sta $0319

	; if we are executing in ROM, we couldn't set a breakpoint
	; use an alternate means of breking: set timer 2 on VIA 1 to immediately
	; expire. This will generate an NMI which will return us to the debugger
	; after one instruction runs
	; The timer value is the sum of all the instructions executed up to
	; (and including) the RTI
	; When the user program starts executing, the timer value will be 1
	lda is_step
	beq @rti
	lda in_rom
	beq @rti	; if not in ROM, we added a BRK

@go_rom_nmi:
	; install the GO NMI handler for the user program
	lda #<NMI_HANDLER_ADDR
	sta $0318
	lda #>NMI_HANDLER_ADDR
	sta $0319

@nmidone:
	lda #$00	; enable Timer 2 (one-shot mode) on VIA #1
	sta $911b

	lda #<NMI_TIMER_VAL
	sta $9118		; set low-order latch for the timer
	lda #>NMI_TIMER_VAL
	sta $9119		; start the timer

@rti:	; return from the BRK/NMI
	lda #FINAL_BANK_USER	; 2
	jmp RTI_ADDR		; 3
	; sta $9c02		; 4
	; pla			; 4
	; rti			; 6
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
; SAVE DEBUG ZP TRACE
; Saves the state of the debugger's zeropage
.proc save_debug_zp_trace
@zp=mem::dbg00
	; no need to save zeropage (it's either temp or static from start of
	; trace)

	ldx #TRACE_STACK_DEPTH-1
:	lda $200-TRACE_STACK_DEPTH,x
	sta @zp+$200-TRACE_STACK_DEPTH,x
	dex
	bpl :-
	rts
.endproc

;******************************************************************************
; GO
; Runs the user program until the next breakpoint or an NMI occurs
.export __debug_go
.proc __debug_go
	; for the first instruction, just STEP, this lets us keep the breakpoint
	; we are on (if there is one) intact
	jsr __debug_step
	bcc :+
	rts

:	lda #$00
	sta sw_valid		; invalidate stopwatch
	lda #ACTION_GO_START
	sta action
	inc advance		; continue program execution

	ldxy #NMI_IER
	lda #$80|$02		; enable RESTORE interrupts only
	jmp vmem::store
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

	jsr __debug_step_out_next
	bcs @done

	inc swapmem
	lda #ACTION_STEP_OUT_START
	sta action
	clc
@done:	rts
.endproc

;******************************************************************************
; STEP OUT NEXT
; Runs the next step of a "STEP OUT" command.  This is called each step after
; executing a "STEP OUT" until the RTS we're looking for is executed
; OUT:
;   - .C: set if we should stop tracing (e.g. if a watch was activated)
.proc __debug_step_out_next
	; for the first instruction, just STEP, this lets us keep the breakpoint
	; we are on (if there is one) intact
	jsr __debug_step
	lda #ACTION_STEP_OUT
	sta action
	rts
.endproc

;******************************************************************************
; TRACE
; Puts the debugger into TRACE mode and steps to the next instruction to
; begin the trace
.export __debug_trace
.proc __debug_trace
	lda #$00
	sta stop_tracing
	jsr __debug_trace_next
	inc swapmem
	lda #ACTION_TRACE_START
	sta action
	rts
.endproc

;******************************************************************************
; TRACE NEXT
; Runs the next step of a TRACE
; OUT:
;   - .C: set if we should stop tracing (e.g. if a watch was activated)
.proc __debug_trace_next
	jsr __debug_step
	bcc :+
	rts

:	lda stop_tracing
	bne @done
	lda #ACTION_TRACE
	sta action
@done:	rts
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
; STEP_OVER
; Runs the next instruction from the .PC and returns to the debug prompt.
; This works by inserting a BRK instruction after
; the current instruction and RUNning.
; Unlike STEP, if the next instruction is a JSR, execution will continue
; at the line after the subroutine (after the subroutine has run)
.export __debug_step_over
.proc __debug_step_over
	lda #ACTION_STEP_OVER
	skw			; fall through to __debug_step
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
	lda #ACTION_STEP
	sta action		; flag that we are STEPing

	ldxy #NMI_IER
	lda #$80|$20		; enable TIMER interrupts only
	jsr vmem::store

	ldxy #$f00d		; use ROM address because we don't need string
	stxy r0
	ldxy sim::pc		; get address of next instruction
	jsr asm::disassemble	; disassemble it to get its size (BRK offset)
	stx sim::op_mode
	bcc @ok

	jsr illegal_detected
	lda #$01		; unrecognized op, treat next opcode as 1 byte
@ok:	pha			; save instruction size

	; preemptively disable mem swapping
	; unless we encounter ROM for the next target (in which case we will
	; increment this again) we know exactly what will be written
	dec swapmem

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
	beq @setbrk			; if not, skip ahead to setting the next BRK
	ldxy sim::effective_addr	; if yes, mark the watch if there is one
	jsr watch::mark			; check if there's a watch at this addr
	bcc @setbrk			; if there's no watch at addr, continue

	; activate the watch window so user sees change
	; restore zp (if we were tracing, must be restored)
	pha
	jsr save_debug_zp
	jsr restore_debug_zp

	; display a message indicating watch was triggered
	pla
	jsr watch_triggered

	lda #ACTION_STEP
	sta action

@setbrk:
	pla			; get instruction size
	ldxy sim::pc		; and address of instruction to-be-executed

	; get the address of the next instruction into sim::next_pc
	jsr sim::next_instruction
	stxy brkaddr

	jsr safety_check	; check for JAMs/suspicious writes/etc.
	bcc @safe
	rts			; return to debugger

@safe:	lda sim::op
	cmp #$20		; JSR?
	bne @countcycles
	lda #ACTION_STEP_OVER
	cmp action		; are we stepping over?
	bne @chkrom		; skip if not

; if stepping over a JSR, set breatpoint at current PC + 3
; also flag that we need to save all RAM
@stepover:
	lda #$00
	sta sw_valid	; invalidate stopwatch
	inc swapmem

	lda sim::pc
	clc
	adc #$03
	sta brkaddr
	lda sim::pc+1
	adc #$00
	sta brkaddr+1

; check if next instruction is in ROM. If so, we can't step IN, we must step
; OVER. Re-increment swapmem to force full RAM swap
@chkrom:
	ldxy brkaddr
	cmpw #$c000
	lda #$00
	rol
	sta in_rom
	beq @countcycles

	; if next STEP is in ROM, we won't be able to set a BRK
	; enable a timer NMI to return to debugger
	ldxy #NMI_IER
	lda #$80|$20		; enable TIMER interrupts only
	jsr vmem::store

; count the number of cycles that the next instruction will take
@countcycles:
	lda stepsave		; get the opcode
	jsr sim::count_cycles	; get the # of cycles for the instruction
	clc
	adc sim::stopwatch
	sta sim::stopwatch
	bcc @addbrk
	inc sim::stopwatch+1
	bne @addbrk
	inc sim::stopwatch+2

; add the breakpoint
@addbrk:
	ldxy brkaddr
	jsr vmem::load
	sta stepsave

	; add breakpoint (note that this is a NOP if the address is in ROM)
	lda #$00		; BRK
	ldxy brkaddr
	jsr vmem::store

@brkdone:
	inc advance		; continue program execution
	RETURN_OK		; return to the debugger
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
; STEP RESTORE
; Restores the opcode destroyed by the last STEP.
.proc step_restore
	lda stepsave
	ldxy brkaddr
	jmp vmem::store
.endproc

;*******************************************************************************
; SWAPIN
; Either swaps in the 3-4 bytes that were saved (if we were able to resolve the
; exact memory affected by an instruction or the _entire_ internal RAM
; state [$00-$2000) if we were unable to resolve the exact memory addresses
; affected.
; This routine is called _before_ returning from the BRK interrupt and executing
; the user program.
.proc swapin
@cnt=r0
@addr=r1
@tosave=r3
	lda swapmem
	beq @fastswap

@swapall:
	; swap entire user RAM in (needed if we don't know what memory will
	; be changed before next BRK)
	jsr save_debug_state
	jmp __debug_restore_progstate

@fastswap:
	; save [effective_addr], [pc, pc+2], and [brkaddr] for the debugger
	lda sim::pc+1
	sta @tosave+1
	sta @tosave+3
	sta @tosave+5

	ldx sim::pc
	stx @tosave
	inx
	bne :+
	inc @tosave+3
	inc @tosave+5
:	stx @tosave+2
	inx
	bne :+
	inc @tosave+5
:	stx @tosave+4

	ldxy sim::effective_addr
	stxy @tosave+6
	ldxy brkaddr
	stxy @tosave+8

; save the debugger's memory values at affected addresses
	ldy #8
	sty @cnt
	ldx #5-1
@save:	ldy @cnt
	lda @tosave,y
	sta @addr
	lda @tosave+1,y
	cmp #$02
	bcc :+			; skip zeropage/stack (saved elsewhere)
	sta @addr+1
	jsr is_internal_address
	bne :+
	ldy #$00
	lda (@addr),y
	sta debug_state_save,x
:	dec @cnt
	dec @cnt
	dex
	bpl @save

; read the virtual memory values for the affected locations and store
; them to their physical addresses
	ldx #8
	stx @cnt
@store: ldx @cnt
	lda @tosave,x
	sta @addr
	ldy @tosave+1,x
	beq @next
	sty @addr+1
	tax
	jsr is_internal_address
	bne @next		; skip if not internal address

	jsr vmem::load
	ldx @addr+1
	cpx #$02
	bcc @next
	ldy #$00
	sta (@addr),y		; store it to the physical address
@next:	dec @cnt
	dec @cnt
	bpl @store

	rts			; done
.endproc

;******************************************************************************
;******************************************************************************
; SWAPOUT
; Swaps *out* the user memory that needs to be saved in order to restore the
; debug state.
; This occurs after we encounter a BRK.  If we were able to figure out the exact
; areas that would be affected before we encountered the BRK, only swap those
; values for the debugger's values of those.
; If not, swap the entire internal RAM state
; The following state is saved/swapped when we can avoid swapping all memory:
;  [prevpc, prevpc+2]: area of the instruction that will be executed
;  (sim::next_pc):     address of the BRK that we will install
;  (msave):            effective address of the instruction we will execute
.proc swapout
@cnt=r0
@addr=r1
@ysave=r3
@tosave=r4
	lda swapmem
	beq @fastswap
@swapall:
	; save the program state before we restore the debugger's
	jsr __debug_save_prog_state
	jmp restore_debug_state		; restore debugger state

@fastswap:
	; save [prevpc, prevpc+2], (msave), and (sim::next_pc) for the user
	; program
	lda prev_pc+1
	sta @tosave+1
	sta @tosave+3
	sta @tosave+5
	ldx prev_pc
	stx @tosave
	inx
	bne :+
	inc @tosave+3
	inc @tosave+5
:	stx @tosave+2
	inx
	bne :+
	inc @tosave+5
:	stx @tosave+4

	ldxy sim::effective_addr
	stxy @tosave+6
	ldxy brkaddr
	stxy @tosave+8

	; save the user values affected by the previous instruction
	; to their virtual address
	ldx #8
	stx @cnt
@save:	ldx @cnt
	lda @tosave,x
	sta @addr
	ldy @tosave+1,x
	cpy #$02
	bcc :+			; skip ZP and stack (saved elsewhere)
	sty @addr+1
	tax
	jsr is_internal_address
	bne :+			; if not internal, don't do this
	ldy #$00
	lda (@addr),y		; read the physical address
	ldy @addr+1
	jsr vmem::store		; store to the virtual (user) address
:	dec @cnt
	dec @cnt
	bpl @save

	; restore the debug values at the affected locations
	ldx #8
	stx @cnt
@store: lda @cnt
	tax
	lsr
	sta @ysave

	ldy @tosave+1,x
	cpy #$02
	bcc @next		; skip ZP, stack, & VIC (already handled)
	sty @addr+1
	lda @tosave,x
	sta @addr
	tax
	jsr is_internal_address	; if not an internal address, leave it alone
	bne @next

	ldy @ysave
	lda debug_state_save,y	; get the byte to restore

	ldx @addr+1
	bne :+
	ldx @addr
	sta mem::dbg00,x
	jmp @next

:	ldy #$00
	sta (@addr),y		; restore the byte
@next:	dec @cnt
	dec @cnt
	bpl @store
	rts			; done
.endproc

;******************************************************************************
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
	lda #>$100
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
; IS INTERNAL ADDRESS
; Returns with .Z set if the given address is outside of the address ranges
; [$2000,$8000) or [$a000,$c000)
; IN:
;  - .XY: the address to test
; OUT:
;  - .Z: set if the address in [$00,$2000), [$8000,$a000), or [$c000,$10000)
.proc is_internal_address
	cmpw #$2000
	bcc @internal
	cmpw #$8000
	bcc @external
	cmpw #$9000
	bcc @internal	; $9000-$94f0 is internal
	cmpw #$94f0
	bcc @internal	; $94f0-$9600 unused by debugger, pretend it's external
	cmpw #$c000
	bcc @internal	; ROM is untouchable, consider external
@external:
	lda #$ff
	rts
@internal:
	lda #$00
	rts
.endproc

;*******************************************************************************
; TRACING
; Checks if the debugger is currently "tracing"
; The definition of tracing is any command that automatically STEPs
; OUT:
;   - .Z: set if the active action is a "tracing" command
;   - .C: set if the active action is a "tracing" command
.proc tracing
	ldx #@num_tracecmds-1
	lda action
:	cmp @tracecmds,x
	beq @done
	dex
	bpl :-
	clc		; not trace
@done:	rts
.PUSHSEG
.RODATA
@tracecmds:
	.byte ACTION_TRACE, ACTION_STEP_OUT
@num_tracecmds=*-@tracecmds
.POPSEG
.endproc

;******************************************************************************
; STEPPING
; Checks if we are currently "stepping"
; This means that we are inserting a BRK after each instruction during
; execution so that the debugger can control how to proceed based on various
; circumstances
; OUT:
;   - .Z: set if the active action is one which executes by stepping
;   - .C: set if the active action is one which executes by stepping
.proc stepping
	ldx #@num_stepcmds-1
	lda action
:	cmp @stepcmds,x
	beq @done
	dex
	bpl :-
	clc		; not step
@done:	rts
.PUSHSEG
.RODATA
@stepcmds:
	.byte ACTION_GO_START, ACTION_TRACE_START, ACTION_STEP, ACTION_STEP_OVER
	.byte ACTION_STEP_OUT, ACTION_TRACE, ACTION_STEP_OUT_START
@num_stepcmds=*-@stepcmds
.POPSEG
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
