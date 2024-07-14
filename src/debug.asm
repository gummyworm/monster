;******************************************************************************
; DEBUG.ASM
; This file contains the debugger code, which is the main loop while debugging
; and assembled program.
;******************************************************************************

.include "asm.inc"
.include "asmflags.inc"
.include "bitmap.inc"
.include "breakpoints.inc"
.include "config.inc"
.include "cursor.inc"
.include "debuginfo.inc"
.include "debugcmd.inc"
.include "draw.inc"
.include "edit.inc"
.include "errors.inc"
.include "fastcopy.inc"
.include "file.inc"
.include "finalex.inc"
.include "flags.inc"
.include "labels.inc"
.include "irq.inc"
.include "key.inc"
.include "keycodes.inc"
.include "layout.inc"
.include "macros.inc"
.include "memory.inc"
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

.import __DEBUGGER_LOAD__
.import __DEBUGGER_SIZE__

; address in user program where the BRK handler will reside
BRK_HANDLER_ADDR = $8000-(brkhandler1_size+brkhandler2_size-2)
RTI_ADDR         = BRK_HANDLER_ADDR + 3

;******************************************************************************
MAX_BREAKPOINTS = 16	; max number of breakpoints that may be set
MAX_WATCHPOINTS = 8	; max number of watchpoints that may be set

AUX_MEM   = 1		; enables the memory viewer in the debug view
AUX_BRK   = 2		; enables the breakpoint view in the debug view
AUX_WATCH = 3		; enables the watchpoint view in the debug view

; layout constants for the register view
REG_PC_OFFSET = 0
REG_A_OFFSET = 5
REG_X_OFFSET = 8
REG_Y_OFFSET = 11
REG_SP_OFFSET = 14

;******************************************************************************
; ACTION constants
; These tell us what command the user last executed when we return to the
; debugger via a BRK or NMI
ACTION_STEP      = 1	; action for STEP command
ACTION_STEP_OVER = 2	; action for STEP OVER command
ACTION_START     = 3	; action for initial debug entry
ACTION_GO_START  = 4	; action for first instruction of GO command
ACTION_GO        = 5	; action for subsequent GO instructions
ACTION_TRACE     = 6	; action for TRACE command

;******************************************************************************
swapmem        = zp::debug+$10	; not zero if we need to swap in user RAM
debugtmp       = zp::debug+$11

;******************************************************************************
; Program state variables
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
startsave:
stepsave:  .byte 0	; opcode to save under BRK
brkaddr:   .word 0 	; address where our brakpoint is set

;******************************************************************************
; Debug state values for internal RAM locations
; NOTE:
; these must be stored next to each other as they are backed up/restored as
; a unit.
debug_state_save:
debug_instruction_save: .res 3	; buffer for debugger's
mem_debugsave:          .byte 0 ; byte under effective address during STEP
debug_stepsave: 	.byte 0 ; debugger byte under BRK (if internal)

;******************************************************************************
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

aux_mode:         .byte 0	; the active auxiliary view
highlight_line:	  .word 0 	; the line we are highlighting
highlight_file:   .word 0	; filename of line we are highlighting

action:	.byte 0		; the last action performed e.g. ACTION_STEP

.export debugger_sp
debugger_sp: .byte 0	; stack pointer (SP) for debugger

;******************************************************************************
; WATCHES
;******************************************************************************
__debug_numwatches:  .byte 0		    ; number of active watches
__debug_watcheslo:   .res MAX_WATCHPOINTS   ; addresses of the set watchpoints
__debug_watcheshi:   .res MAX_WATCHPOINTS   ; addresses of the set watchpoints
__debug_watch_vals:  .res MAX_WATCHPOINTS   ; values of the set watchpoints
__debug_watch_prevs: .res MAX_WATCHPOINTS   ; previous values of watches
__debug_watch_flags: .res MAX_WATCHPOINTS   ; flags for watches (e.g. DIRTY)

; the following are used for watches that represent a range of values
; e.g. [$1000, $1100)
__debug_watches_changedlo: .res MAX_WATCHPOINTS ; the address that was changed
__debug_watches_changedhi: .res MAX_WATCHPOINTS ; the address that was changed
__debug_watches_stoplo:    .res MAX_WATCHPOINTS ; end address of watch range
__debug_watches_stophi:    .res MAX_WATCHPOINTS ; end address of watch range

.export __debug_watcheslo
.export __debug_watcheshi
.export __debug_watch_vals
.export __debug_watch_prevs
.export __debug_numwatches
.export __debug_watch_flags
.export __debug_watches_stoplo
.export __debug_watches_stophi

;******************************************************************************
; BREAKPOINTS
.export __debug_breakpointslo
.export __debug_breakpointshi
.export __debug_breakpoint_lineslo
.export __debug_breakpoint_lineshi
.export __debug_breakpoint_fileids

numbreakpoints = dbgi::numbreakpoints

__debug_breakpointslo:
breakpointslo:      .res MAX_BREAKPOINTS	; LSB's of the break points
__debug_breakpointshi:
breakpointshi:      .res MAX_BREAKPOINTS	; MSB's of the break points
__debug_breakpoint_lineslo: .res MAX_BREAKPOINTS	; breakpoint line # (LSB)
__debug_breakpoint_lineshi: .res MAX_BREAKPOINTS	; breakpoint line # (MSB)
__debug_breakpoint_fileids: .res MAX_BREAKPOINTS	; breakpoint file ID's

.export __debug_breakpoint_flags
__debug_breakpoint_flags:
breakpoint_flags: .res MAX_BREAKPOINTS ; active state of breakpoints
breaksave:        .res MAX_BREAKPOINTS ; backup of instructions under the BRKs

;******************************************************************************
; RESTORE DEBUG ZP
; Restores the state of the debugger's zeropage
.macro restore_debug_zp
	ldx #$00
:	lda mem::dbg00,x
	sta $00,x
	lda mem::dbg00+$100,x
	sta $100,x
	lda mem::dbg00+$200,x
	sta $200,x
	lda mem::dbg00+$300,x
	sta $300,x
	dex
	bne :-
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
@zp=mem::prog00
	ldx #$00
:	lda @zp,x
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
	ldx #$10
:	lda mem::dbg9000-1,x
	dex
	bne :-

	ldx #$f0
; save $9400-$94f0
:	lda mem::dbg9400-1,x
	sta $9400-1,x
	dex
	bne :-

	; reinit the bitmap
	jsr bm::init

	; restore the screen ($1100-$2000)
	CALL FINAL_BANK_FASTCOPY2, #fcpy::restore
	rts
.endproc

;******************************************************************************
; RESTORE_PROGSTATE
; restores the saved program state
.proc __debug_restore_progstate
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
	CALL FINAL_BANK_FASTCOPY, #fcpy::restore
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
	jsr vmem::store

	; initialize auxiliary views
	lda #$00
	sta aux_mode

	; init state
	sta __debug_numwatches
	jsr reset_stopwatch

	jsr install_brk			; install the BRK handler IRQ

	jsr save_debug_state 		; save the debug state
	;jsr __debug_save_prog_state
	jsr __debug_restore_progstate	; and copy in entire user state to start

	lda #$01
	sta swapmem			; on 1st iteration, swap entire RAM back

	; the zeropage is sensitive, when we're done with all other setup,
	; swap the user and debug zeropages
	jsr save_debug_zp
	tsx
	stx debugger_sp

	JUMP FINAL_BANK_USER, sim::pc	; execute the user program until BRK
.endproc

;******************************************************************************
; DUMMY IRQ
; Replaces the IRQ with a dummy (NOP) one
.proc dummy_irq
	sei
	ldxy #$eb15
	stxy $0314
	cli
	rts
.endproc

;******************************************************************************
; INSTALL BRK
; Installs the debugger BRK handler to the BRK and NMI vectors and copies the
; code to enter the handler to the user program's RAM at BRK_HANDLER_ADDR.
; This handler switches back to the debuggers RAM bank, where the debugger takes
; over.
; IN:
;  - .XY: the address to install the BRK handler to
.proc install_brk
@dst=r0
@cnt=r2
	sei
	ldxy #BRK_HANDLER_ADDR
	stxy @dst
	stxy $0316		; BRK
	stxy $0318		; NMI
	cli

	lda #brkhandler1_size-1
	sta @cnt
; copy part 1 of the BRK handler to the user program
@l0:	ldy @cnt
	lda brkhandler1,y
	sta zp::bankval
	sty zp::bankoffset
	ldxy @dst
	lda #FINAL_BANK_USER
	jsr fe3::store_off
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
	jsr fe3::store_off
	dec @cnt
	bpl @l1
	rts
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
; | PHA          |	PHA                      |
; | LDA #$80     |	LDA #$80                 |
; | STA $9C02    |	STA $9C02                |
; | PLA          |	PLA                      |
; | RTI          | 	JMP DEBUG_BRK            |
; | -- (2 bytes) |  -- (0 bytes)                 |
;
; handler1 (the User handler) has 2 empty bytes at the end
;
brkhandler1:
	pha
	lda #$80
	sta $9c02
	pla
	rti
brkhandler1_size=*-brkhandler1
brkhandler2:
	pha
	lda #$80
	sta $9c02
	pla
	jmp debug_brk
brkhandler2_size=*-brkhandler2

;******************************************************************************
; INSTALL_BREAKPOINTS
; Install all breakpoints from "breakpoints" EXCEPT for those at the current
; PC. This is to prevent a loop of breakpoints being repeatedly set and
; immediately hit. For a breakpoint to be effective, it must be set at least
; one instruction from the current PC.
.proc install_breakpoints
@brkaddr=r0
@cnt=r2
	ldx numbreakpoints
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

	ldxy @brkaddr
	jsr vmem::load
	beq @next		; already a BRK
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
; Restores the source code by removing all breakpoints installed by the debugger
.proc uninstall_breakpoints
@addr=r0
@cnt=r2
	ldx numbreakpoints
	beq @done
	dex
	stx @cnt
@uninstall:
	ldx @cnt
	lda breakpoint_flags,x
	and #BREAKPOINT_ENABLED
	beq @next

	lda breakpointslo,x
	sta @addr
	lda breakpointshi,x
	sta @addr+1

	lda breaksave,x
	beq @next				; already a BRK
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
	CALL FINAL_BANK_FASTCOPY2, #fcpy::save
	rts
.endproc

;******************************************************************************
; SAVE_PROG_STATE
; saves memory clobbered by the debugger (screen, ZP, etc.)
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
	CALL FINAL_BANK_FASTCOPY, #fcpy::save
	rts
.endproc

;******************************************************************************
; DEBUG_BRK
; This is the BRK handler for the debugger.
; It saves the user program's state and other sensitive memory areas that are
; needed by the debugger for display etc, then it restores the debugger's state
; and finally transfers control to the debugger
.proc debug_brk
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

	; BRK pushes PC + 2, subtract 2 from PC
	lda sim::pc
	sec
	sbc #$02
	sta sim::pc
	lda sim::pc+1
	sbc #$00
	sta sim::pc+1

	sei

	; restore the debugger's SP
	ldx debugger_sp
	txs

	; save the user's zeropage and restore the debugger's
	save_user_zp
	restore_debug_zp

	; swap the debugger state in
	jsr swapout

	; reinstall the main IRQ
        jsr irq::raster

	; unless we can figure out the exact RAM we will affect, we'll have to
	; swap in the entire user RAM before we return from this BRK
	lda #$01
	sta swapmem

	lda #$00
	sta lineset		; flag that line # is (yet) unknown

@uninstall_brks:
	; uninstall breakpoints (will reinstall the ones we want later)
	jsr uninstall_breakpoints

	; if we're beginning a GO, get on with it
	lda action
	cmp #ACTION_GO_START
	bne @update_watches

	; continue the GO command
	jsr step_restore
	lda #ACTION_GO
	sta action

@continue_debug:
	jsr install_breakpoints	 ; reinstall rest of breakpoints
	jmp @debug_done		 ; continue execution

@update_watches:
	jsr watch::update
	jsr show_aux		; display the auxiliary mode

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
	jmp @reset_state
:	jsr stepping		; are we stepping?
	bne @reset_state
@restore_step:
	jsr step_restore

	; if we are doing a TRACE, install breakpoints and continue
	lda action
	cmp #ACTION_TRACE
	bne @reset_state
@trace:
	; check if the BRK that was triggered is a breakpoint or just the
	; step point. If the latter, continue tracing
	ldxy sim::pc
	jsr get_breakpoint
	bcs :+			; not a breakpoint, continue tracing
	bcc @reset_state	; return control to debugger
:	jsr trace
	jmp @continue_debug

@reset_state:
	lda #$00
	sta action

@showbrk:
	; get the address before the BRK and go to it
	ldxy sim::pc
	jsr __debug_gotoaddr
	bcs @print		; if we failed to get line #, continue
	jsr edit::sethighlight
	inc lineset

@print:	jsr showstate		; show regs/BRK message
	jsr cur::on

; main debug loop
@debugloop:
	cli
	jsr text::update
	jsr key::getch
	beq @debugloop

	pha

	lda #$00
	sta advance	; by default, don't return to program after command
	jsr cur::off
	pla

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
	beq @finishloopiter	; if key is marked as disabled, ignore it
	dex
	bpl @chkdisabled

; propagate the key to the editor
@nocmd:	jsr edit::handlekey
	jmp @finishloopiter

@runcmd:
	lda command_vectorslo,x	 ; vector LSB
	sta zp::jmpvec
	lda command_vectorshi,x  ; vector MSB
	sta zp::jmpvec+1

	jsr zp::jmpaddr		; call the command
	jsr cur::on
	jsr showstate		; restore the register display (may be changed)

@finishloopiter:
	lda advance		; are we ready to execute program? (GO, STEP)
	beq @debugloop		; not yet, loop and get another command

@debug_done:
	jsr cur::off
	jsr swapin

	jsr save_debug_zp
	restore_user_zp

@restore_regs:
	ldx sim::reg_sp	; restore SP
	txs

	; from top to bottom: [STATUS, <PC, >PC]
	lda sim::pc+1
	sta prev_pc+1
	pha
	lda sim::pc	; restore PC
	sta prev_pc
	pha

	lda sim::reg_p	; restore processor status
	pha

	lda sim::reg_a
	sta prev_reg_a
	ldx sim::reg_x
	stx prev_reg_x
	ldy sim::reg_y
	sty prev_reg_y

	; TODO: restore timer values

	; return from the BRK
	pha
	lda #FINAL_BANK_USER
	jmp RTI_ADDR
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
	asl
	asl
	asl
	asl
	adc #<dbgi::filenames
	tax
	lda #>dbgi::filenames
	adc #$00
	tay
	jmp edit::load
.endproc

;******************************************************************************
; GOTOADDR
; Navigates the editor to the file/line associated with the give address
; IN:
;  - .XY: the address to "goto"
; OUT:
;  - .C:  set on failure
;  - .XY: the line that was navigated to
.export __debug_gotoaddr
.proc __debug_gotoaddr
@line=debugtmp
	jsr dbgi::addr2line	; get the line #
	bcs @done		; error
	sta dbgi::file
	stxy @line
	jsr __debug_load_file	; load file (if not already)
	bcs @done		; error

	ldxy @line
	jsr edit::gotoline
	ldxy @line
	clc
@done:	rts
.endproc

;******************************************************************************
; SAVE DEBUG ZP
; Saves the state of the debugger's zeropage
; TODO: only save/restore the ZP locations clobbered by the debugger
; (will require some overall restructure of ZP usage)
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
.proc go
	; for the first instruction, just STEP, this lets us keep the breakpoint
	; we are on (if there is one) intact
	jsr step
	lda #$00
	sta sw_valid		; invalidate stopwatch
	lda #ACTION_GO_START
	sta action
	inc advance		; continue program execution
	rts
.endproc

;******************************************************************************
; TRACE
; Puts the debugger into TRACE mode and steps to the next instruction to
; begin the trace
.proc trace
	jsr step
	lda #ACTION_TRACE
	sta action
	rts
.endproc

;******************************************************************************
; QUIT
; Exits the debugger
.proc quit
	ldxy #strings::debug_stop_debugging
	lda #DEBUG_MESSAGE_LINE
	jsr text::print

:	jsr key::getch
	beq :-

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
	ldx debugger_sp
	txs
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
	lda #(DEBUG_INFO_START_ROW)*8
	jsr bm::clrpart
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
	lda #DEBUG_INFO_START_ROW-1
	jsr edit::resize
	lda #(DEBUG_INFO_START_ROW)*8
	jsr bm::clrpart
	jsr showstate		; restore the state

	lda #AUX_BRK
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
	lda #(DEBUG_INFO_START_ROW)*8
	jsr bm::clrpart
	jsr showstate		; restore the state

	lda #AUX_WATCH
	sta aux_mode
	jmp watch::edit
.endproc

;******************************************************************************
; SET_BREAKPOINT
; Sets a breakpoint at the current line selection
.proc set_breakpoint
@line=r0
	ldxy src::line
	jsr __debug_setbrkatline	; set the line #

	ldxy src::line
	jsr dbgi::line2addr
	jmp __debug_brksetaddr	; map the address to the line
.endproc

;******************************************************************************
; SWAP_USER_MEM
; Command that swaps in the user program memory, waits for a keypress, and
; returns with the debugger's memory swapped back in
.proc swap_user_mem
	jsr save_debug_state
	jsr __debug_restore_progstate

	; wait for a key to swap the state back
:	jsr key::getch
	beq :-
	jsr __debug_save_prog_state
	jmp restore_debug_state	; restore debugger state
.endproc

;******************************************************************************
; STEP
; Runs the next instruction from the .PC and returns to the debug prompt.
; This works by inserting a BRK instruction after
; the current instruction and RUNning.
.proc step
@mode=r0
	ldxy #$100		; TODO: use ROM addr? (we don't need the string)
	stxy r0			; TODO: make way to not disassemble to string
	ldxy sim::pc		; get address of next instruction
	jsr asm::disassemble  ; disassemble it to get its size (next BRK offset)
	stx sim::op_mode
	bcc @ok
	rts		; return error

@ok:	pha		; save instruction size

	; preemptively disable mem swapping
	; unless we encounter ROM for the next target (in which case we will
	; increment this again) we know exactly what will be written
	dec swapmem

	stx @mode			; address mode (set by asm::disassemble)
	ldxy sim::pc			; address of instruction
	jsr sim::get_side_effects	; get state that will be clobbered/used

	lda #ACTION_STEP
	sta action		; flag that we are STEPing

; for updating watches and just general info for the user, save the current
; state of memory that will be altered

	; clear watch flags
	lda #$00
	ldx __debug_numwatches
	beq @check_effects
:	sta __debug_watch_flags-1,x
	dex
	bne :-

@check_effects:
	lda sim::affected
	and #OP_LOAD|OP_STORE	; was there a write to memory?
	beq @setbrk		; if not, skip ahead to setting the next BRK

	ldxy sim::effective_addr	; if so, mark the watch if there is one
	jsr watch::mark			; if there's a watch at this addr, mark it
	bcc @setbrk			; if there's no watch, contiue

	; activate the watch window so user sees change
	lda #(DEBUG_INFO_START_ROW+1)*8
	jsr bm::clrpart
	lda #AUX_WATCH
	sta aux_mode
	jsr watch::view

@setbrk:
	pla			; get instruction size
	ldxy sim::pc		; and address of instruction to-be-executed

	; get the address of the next instruction into sim::next_pc
	jsr sim::next_instruction
	stxy brkaddr

	; check if next instruction is in ROM
	; If so, we can't step, our "step" wil actually be more like a "go".
	; re-increment swapmem to force full RAM swap
	cmpw #$c000
	bcc @notrom
	lda #ACTION_STEP_OVER
	sta action
@notrom:
	lda sim::op
	cmp #$20		; JSR?
	bne @countcycles
	lda #ACTION_STEP_OVER
	cmp action		; are we stepping over
	bne @countcycles	; skip if not

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

; count the number of cycles that the next instruction will take
@countcycles:
	lda stepsave		; get the opcode
	jsr sim::count_cycles	; get the # of cycles for the instruction
	clc
	adc sim::stopwatch
	sta sim::stopwatch
	lda sim::stopwatch+1
	adc #$00
	sta sim::stopwatch+1
	lda sim::stopwatch+2
	adc #$00
	sta sim::stopwatch+2

; add the breakpoint
@addbrk:
	ldxy brkaddr
	jsr vmem::load
	sta stepsave
	lda #$00		; BRK
	ldxy brkaddr
	jsr vmem::store

	inc advance		; continue program execution
	rts			; return to the debugger
.endproc

;******************************************************************************
; STEP_RESTORE
; Restores the opcode destroyed by the last STEP.
.proc step_restore
	lda stepsave
	ldxy brkaddr
	jmp vmem::store
.endproc

;******************************************************************************
; STEP_OVER
; Runs the next instruction from the .PC and returns to the debug prompt.
; This works by inserting a BRK instruction after
; the current instruction and RUNning.
; Unlike STEP, if the next procedure is a JSR, execution will continue
; at the line after the subroutine (after the subroutine has run)
.proc step_over
	lda #$00
	sta sw_valid		; invalidate stopwatch
	jmp step
.endproc

;******************************************************************************
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
	; save [mem_saveaddr], [step_point], and [pc, pc+2] for the debugger
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
@store: lda @cnt
	tax

	lda @tosave,x
	sta @addr
	ldy @tosave+1,x
	sty @addr+1
	tax
	jsr is_internal_address
	bne @next		; skip if not internal address

	jsr vmem::load
	ldx @addr+1
	bne :+			; skip zeropage for now (we're still using it)

	ldx @addr
	sta mem::prog00,x	; update virtual ZP
	jmp @next

:	ldy #$00
	sta (@addr),y		; store it to the physical address
@next:	dec @cnt
	dec @cnt
	bpl @store

	rts			; done
.endproc

;******************************************************************************
; SWAPOUT
; Swaps *out* the user memory that needs to be saved in order to restore the
; debug state.
; This occurs after we encounter a BRK.  If we were able to figure out the exact
; areas that would be affected before we encountered the BRK, only swap those
; values for the debugger's values of those.
; If not, swap the entire internal RAM state
.proc swapout
@cnt=r0
@addr=r1
@ysave=r3
@tosave=r4
	lda swapmem
	beq @fastswap
	jmp @swapall

@fastswap:
	; save [prevpc, prevpc+2], [msave], and [sim::next_pc] for the user
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

@swapall:
	; save the program state before we restore the debugger's
	jsr __debug_save_prog_state
	jmp restore_debug_state	; restore debugger state
.endproc

;******************************************************************************
; SETBRKATLINE
; Sets a breakpoint at the given line.  During assembly the address will be
; populated.
; IN:
;  - .XY: the line number to set the breakpoint at
;  - .A:  the file ID to set the breakpoint in
.export __debug_setbrkatline
.proc __debug_setbrkatline
	; store line #
	pha
	txa
	ldx numbreakpoints
	sta __debug_breakpoint_lineslo,x
	tya
	sta __debug_breakpoint_lineshi,x
	pla
	sta __debug_breakpoint_fileids,x
	lda #BREAKPOINT_ENABLED
	sta breakpoint_flags,x

	inc numbreakpoints
	rts
.endproc

;******************************************************************************
; BRKSETADDR
; Sets the address for the given breakpoint. If no matching breakpoint is found,
; does nothing
; IN:
;   - .XY: the line # of the breakpoint to set the address for
;   - r0:  the address to store for the breakpoint
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

;******************************************************************************
; REMOVEBREAKPOINT
; Removes a breakpoint at the address in .XY
; IN:
;  - .XY: the address of the breakpoint to remove
; OUT:
;  - .C: set if breakpoint doesn't exist
.export __debug_remove_breakpoint
__debug_remove_breakpoint:
.proc remove_breakpoint
@addr=debugtmp
@end=debugtmp+2
	jsr get_breakpoint
	bcs @ret
	tax

@remove:
	; shift breakpoints down
	lda numbreakpoints
	sta @end
	cpx @end
	beq @removed
@l0:	lda breakpointshi+1,x
	sta breakpointshi,x
	lda breakpointslo+1,x
	sta breakpointslo,x
	lda __debug_breakpoint_fileids+1,x
	sta __debug_breakpoint_fileids,x
	inx
	cpx @end
	bcc @l0
@removed:
	dec numbreakpoints
@done:	clc
@ret:	rts
.endproc

;******************************************************************************
; SHIFT BREAKPOINTS D
; Shifts DOWN the line numbers for all breakpoints on lines greater than the one
; given by the given offset.
; IN:
;  - .XY: the line number to shift
;  - .A:  the offset to shift
;  - r0:  the file ID of the file to shift
.export __debug_shift_breakpointsd
.proc __debug_shift_breakpointsd
@fileid=r0
@line=r1
@offset=r2
	stxy @line
	sta @offset
	ldx numbreakpoints
@l0:	lda @fileid
	cmp __debug_breakpoint_fileids,x
	bne @next
	lda __debug_breakpoint_lineshi,x
	cmp @line+1
	bcc @next
	lda __debug_breakpoint_lineslo,x
	cmp @line
	bcc @next
	clc
	adc @offset
	bcc @next
	inc __debug_breakpoint_lineshi,x
@next:	dex
	bpl @l0
.endproc

;******************************************************************************
; SHIFT BREAKPOINTS U
; Shifts UP the line numbers for all breakpoints on lines less than the one
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
	ldx numbreakpoints
@l0:	lda @fileid
	cmp __debug_breakpoint_fileids,x
	bne @next
	lda __debug_breakpoint_lineshi,x
	cmp @line+1
	bcs @next
	lda __debug_breakpoint_lineslo,x
	cmp @line
	bcs @next
	clc
	adc @offset
	bcc @next
	inc __debug_breakpoint_lineshi,x
@next:	dex
	bpl @l0
.endproc


;******************************************************************************
; GET BREAKPOINT
; Returns the ID (index) of the breakpoint corresponding to the given address
; IN:
;  - .XY: the address of the breakpoint to get the ID of
; OUT:
;  - .C: set if no breakpoint is found
;  - .A: the id of the breakpoint
.proc get_breakpoint
@addr=debugtmp
	stxy @addr
	ldx numbreakpoints
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

;******************************************************************************
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
:	jsr key::getch
	beq :-
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
	ldx #6
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
	cpx #6
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

	ldx #6-1
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

@offsets:
.byte REG_PC_OFFSET, REG_PC_OFFSET+2, REG_A_OFFSET, REG_X_OFFSET, REG_Y_OFFSET
.byte REG_SP_OFFSET
.endproc

;******************************************************************************
; SHOWSTATE
; Shows the current debug state (registers and BRK line)
.proc showstate
	jsr showbrk
	jmp showregs
.endproc

;******************************************************************************
; SHOWREGS
; prints the contents of the registers in the format
;   PC  A  X  Y  SP NV-BDIZC ADDR
;  f59c 02 02 00 f7 00100000 1003
.proc showregs
@tmp=zp::asm+6
@flag=zp::asm+7
@buff=mem::linebuffer2
	; display the register names
	ldxy #strings::debug_registers
	lda #REGISTERS_LINE
	jsr text::print

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

; if registers were affected, highlight them
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

@print:	ldxy #@buff
	lda #REGISTERS_LINE+1
	jmp text::puts
.endproc

;******************************************************************************
; SHOWBRK
; Display the BRK line number or address
.proc showbrk
	lda lineset		; is the line # known?
	bne @showline		; if so, show it

@showaddr:
	; we couldn't find the line #; display the address of the BRK
	lda sim::pc
	pha
	lda sim::pc+1
	pha
	ldxy #strings::debug_brk_addr
	jmp @print

@showline:
	; display the BRK message
	lda edit::highlight_line
	pha
	lda edit::highlight_line+1
	pha
	ldxy #strings::debug_brk_line
@print:	lda #DEBUG_MESSAGE_LINE
	jsr text::print		; break in line <line #>
	rts
.endproc

;******************************************************************************
; SHOW_AUX
; Displays the memory viewer, breakpoint viewer, or watchpoint viewer depending
; on which is enabled
.proc show_aux
	ldx aux_mode
	beq @done		; no aux mode
	cpx #@numauxviews+1
	bcs @done		; invalid selection
	lda @auxlos-1,x
	sta zp::jmpvec
	lda @auxhis-1,x
	sta zp::jmpvec+1

	lda #(DEBUG_INFO_START_ROW+1)*8
	jsr bm::clrpart

	jmp zp::jmpaddr
@done:	rts
.define auxtab view::mem, brkpt::edit, watch::view
@auxlos: .lobytes auxtab
@auxhis: .hibytes auxtab
@numauxviews=*-@auxhis
.endproc

;******************************************************************************
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
	cmpw #$a000
	bcc @internal 	; VIC or I/O- internal

@external:
	lda #$ff	; banked RAM or ROM, no need to back up
	rts
@internal:
	lda #$00
	rts
.endproc

;******************************************************************************
; Stepping
; Returns with the .Z flag set if we are currently "stepping"
; This means that we are executing in a step-by-step fashion as with
;  STEP
;  TRACE
;  STEP-OVER
;  GO-START
.proc stepping
	lda action
	cmp #ACTION_GO_START
	beq @yes
	cmp #ACTION_STEP
	beq @yes
	cmp #ACTION_STEP_OVER
	beq @yes
	cmp #ACTION_TRACE
@yes:	rts
.endproc

;******************************************************************************
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

;******************************************************************************
; GOTO BREAK
; Navigates the editor to the line that corresponds to the address where the
; debugger is currently at in the user program.
.proc goto_break
.endproc
	ldxy brkaddr
	jmp __debug_gotoaddr
.RODATA

;******************************************************************************
; ENTER DEBUG CMD
; Reads command input and returns it (0-terminated) in mem::linebuffer
; used
; IN:
;  - .XY: a prompt to display or $0000 for no prompt
; OUT:
;  - .C: set if no input was read (the user pressed <-)
.proc enter_debug_cmd
@result_offset=r8
	jsr cur::off
	jsr text::savebuff
	jsr text::clrline

	pushcur			; save the cursor state

	lda #$00
	sta mem::linebuffer+1	; 0-terminate the line
	lda #'!'
	sta mem::linebuffer
	lda #1
	sta cur::minx
	sta zp::curx

	lda #DEBUG_MESSAGE_LINE
	sta zp::cury
	jsr text::drawline	; clear line & display prompt
	ldxy #key::getch	; key-input callback
	jsr __edit_gets		; read the user input
	php			; save success state

	lda #40
	sta cur::maxx		; restore cursor x limit

	jsr text::restorebuff
	ldx @result_offset
	ldy #$01

	plp			; get success state
	popcur			; restore cursor
	lda #$00
	sta cur::minx

	jsr dbgcmd::run
	bcc @ok
	; display the error
	jsr err::get
	jsr str::uncompress
	lda #DEBUG_MESSAGE_LINE
	jsr text::drawline
@ok:	rts
.endproc

;******************************************************************************
; COMMANDS
; This table contains the keys used to invoke the corresponding command
; within the debugger
commands:
	.byte K_QUIT
	.byte K_STEP
	.byte K_STEPOVER
	.byte K_GO
	.byte K_TRACE
	.byte K_SRCVIEW
	.byte K_MEMVIEW
	.byte K_BRKVIEW
	.byte K_WATCHVIEW
	.byte K_SET_BREAKPOINT
	.byte K_SWAP_USERMEM
	.byte K_RESET_STOPWATCH
	.byte K_EDIT_STATE
	.byte K_GOTO_BREAK
	.byte K_ENTER_DEBUG_CMD
num_commands=*-commands

.linecont +
.define command_vectors quit, step, step_over, go, \
	trace, edit_source, edit_mem, edit_breakpoints, __debug_edit_watches, \
	set_breakpoint, swap_user_mem, reset_stopwatch, edit_state, \
	goto_break, enter_debug_cmd
.linecont -
command_vectorslo: .lobytes command_vectors
command_vectorshi: .hibytes command_vectors

;******************************************************************************
; DISABLED COMMANDS
; the following commands are NOT propagated to the editor. they become a nop
; when handled by the debugger
disabled_commands:
	.byte K_CLOSE_BUFF
	.byte K_ASM
	.byte K_ASM_DEBUG
	.byte K_REFRESH
	.byte $76	; v (enter visual)
	.byte $56	; V (enter visual line)
num_disabled_commands=*-disabled_commands
