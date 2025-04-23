.include "../macros.inc"
.include "../ram.inc"
.include "../screen.inc"
.include "../zeropage.inc"

.import TRAMPOLINE_ADDR

RETURN_ADDR     = TRAMPOLINE_ADDR-1
JMP_RETURN_ADDR = RETURN_ADDR-5

.export prog00
.export prog1000
.export prog9000
.export prog9110
.export prog9400
.export dbg00
.export dbg9000
.export dbg9400

;******************************************************************************
.BSS
; $00-$200 is stored in the main BSS segment for faster access by the simulator
prog00:	.res $400	; $00-$0400
dbg00:  .res $400	; $00-$400

.SEGMENT "FASTCOPY_BSS"

;******************************************************************************
; PROG
; backup for the user's program during debug
progsave:
; prog00 is stored in MAIN bank
prog1000: .res $1000	; $1000-$2000
prog9000: .res $10	; $9000-$9010
prog9110: .res $20	; $9110-$9130
prog9400: .res $f0	; $9400-$94f0

;******************************************************************************
; DBG
; backup for debugger/editor memory
; we back up less for debug because we can just re-init some state
dbg1000: .res $1000	; $1000-$2000
dbg9000: .res $10	; $9000-$9010
dbg9400: .res $f0	; $9400-$94f0

.CODE

;******************************************************************************
; INIT
.export __fastcopy_init
.proc __fastcopy_init
	rts
.endproc

;******************************************************************************
; SAVE USER ZP
; Saves the state of the user's zeropage
; IN:
;  - .XY: the resturn address (the stack s clobbered by this procedure)
.export __fastcopy_save_user_zp
.proc __fastcopy_save_user_zp
	stxy @ret

	ldx #$00
:	lda $00,x
	sta prog00,x
	lda $100,x
	sta prog00+$100,x
	lda $200,x
	sta prog00+$200,x
	lda $300,x
	sta prog00+$300,x
	dex
	bne :-

@ret=*+1
	jmp $f00d
.endproc

;******************************************************************************
; RESTORE USER ZP
; Restores the state of the user's zeropage
; IN:
;  - .XY: the resturn address (the stack s clobbered by this procedure)
.export __fastcopy_restore_user_zp
.proc __fastcopy_restore_user_zp
	stxy @ret

	ldx #$00
:	lda prog00,x
	sta $00,x
	lda prog00+$100,x
	sta $100,x
	lda prog00+$200,x
	sta $200,x
	lda prog00+$300,x
	sta $300,x
	dex
	bne :-

@ret=*+1
	jmp $f00d
.endproc

;*****************************************************************************
; RESTORE DEBUG ZP
; Restores the $00-$100 values forthe debugger
.export __fastcopy_restore_debug_zp
.proc __fastcopy_restore_debug_zp
	ldx #$00
:	lda dbg00,x
	sta $00,x
	dex
	bne :-
	rts
.endproc

;******************************************************************************
; RESTORE DEBUG LOW
; Restores the state of the debugger's "low" memory
.export __fastcopy_restore_debug_low
.proc __fastcopy_restore_debug_low
	; get return address (stack will be clobbered)
	pla
	clc
	adc #$01
	sta @ret
	pla
	adc #$00
	sta @ret+1

	ldx #$00
:	lda dbg00+$100,x
	sta $100,x
	lda dbg00+$200,x
	sta $200,x
	dex
	bne :-

	; copy around the NMI vector
	ldx #$100-$1a
:	lda dbg00+$31a,x
	sta $31a,x
	dex
	bne :-

	ldx #$18-1
:	lda dbg00+$300,x
	sta $300,x
	dex
	bpl :-
@ret=*+1
	jmp	$f00d
.endproc

;******************************************************************************
; SAVE DEBUG ZP
; Saves the state of the debugger's zeropage
.export __fastcopy_save_debug_zp
.proc __fastcopy_save_debug_zp
@zp=dbg00
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
; SAVE DEBUG STATE
; Saves the state of the debugger's zeropage
.export __fastcopy_save_debug_state
.proc __fastcopy_save_debug_state
	JUMP FINAL_BANK_FASTCOPY, save_debug_state
.endproc

;******************************************************************************
; SAVE PROG STATE
; Saves memory clobbered by the debugger (screen, VIC registers and color)
.export __fastcopy_save_prog_state
.proc __fastcopy_save_prog_state
	JUMP FINAL_BANK_FASTCOPY, save_prog_state
.endproc

;******************************************************************************
; RESTORE PROG STATE
; Restores the saved program state
.export __fastcopy_restore_prog_state
.proc __fastcopy_restore_prog_state
	JUMP FINAL_BANK_FASTCOPY, restore_prog_state
.endproc

;******************************************************************************
; RESTORE_DEBUG_STATE
; Restores the saved debugger state
.export __fastcopy_restore_debug_state
.proc __fastcopy_restore_debug_state
	JUMP FINAL_BANK_FASTCOPY, restore_debug_state
.endproc

.SEGMENT "FASTCOPY"

;******************************************************************************
; RESTORE_DEBUG_STATE
; Restores the saved debugger state
.export restore_debug_state
.proc restore_debug_state
@vicsave=dbg9000
@colorsave=dbg9400
	; disable NMI/IRQs (we will be clobbering their vectors)
	lda #$7f
	sta $911e
	sta $912e

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

; restore $1000-$2000
	lda #>dbg1000
	sta @addr+1
	lda #$10
	sta @addr2+1		; start from $1000
:
@addr=*+1
	lda dbg1000,x
@addr2=*+1
	sta $1000,x
	dex
	bne :-
	inc @addr+1		; next page
	inc @addr2+1
	lda @addr2+1
	cmp #$20		; at $2000 yet?
	bne :-			; loop until we are

	; reinit the bitmap and return
	JUMP FINAL_BANK_MAIN, scr::init
.endproc

;******************************************************************************
; SAVE_DEBUG_STATE
; saves memory likely to be clobbered by the user's
; program (namely the screen)
.export save_debug_state
.proc save_debug_state
@vicsave=dbg9000
@colorsave=dbg9400
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

; save $1000-$2000
@savescreen:
	lda #>dbg1000
	sta @addr2+1
	lda #$10
	sta @addr+1		; start from $1000
:
@addr=*+1
	lda $1000,x
@addr2=*+1
	sta dbg1000,x
	dex
	bne :-
	inc @addr+1		; next page
	inc @addr2+1
	lda @addr+1
	cmp #$20		; at $2000 yet?
	bne :-			; loop until we are

	rts
.endproc

;******************************************************************************
; RESTORE PROG STATE
; restores the saved program state
.export restore_prog_state
.proc restore_prog_state
; restore $9000-$9010
	ldx #$10
:	lda prog9000-1,x
	sta $9000-1,x
	dex
	bne :-

; restore $1000-$2000
	lda #>prog1000
	sta @addr+1
	lda #$10
	sta @addr2+1		; start from $1000
:
@addr=*+1
	lda prog1000,x
@addr2=*+1
	sta $1000,x
	dex
	bne :-
	inc @addr+1		; next page
	inc @addr2+1
	lda @addr2+1
	cmp #$20		; at $2000 yet?
	bne :-			; loop until we are

	ldx #$f0
; restore $9400-$94f0
:	lda prog9400-1,x
	sta $9400-1,x
	dex
	bne :-
	rts
.endproc

;******************************************************************************
; SAVE_PROG_STATE
; Saves memory clobbered by the debugger (screen, VIC registers and color)
.export save_prog_state
.proc save_prog_state
@vicsave=prog9000
@internalmem=prog1000
@colorsave=prog9400
	ldx #$10
@savevic:
	lda $9000-1,x
	sta @vicsave-1,x
	dex
	bne @savevic

; save $1000-$2000
@savescreen:
	lda #$10
	sta @addr+1		; start from $1000
	lda #>prog1000
	sta @addr2+1
:
@addr=*+1
	lda $1000,x
@addr2=*+1
	sta prog1000,x
	dex
	bne :-
	inc @addr+1		; next page
	inc @addr2+1
	lda @addr+1
	cmp #$20		; at $2000 yet?
	bne :-			; loop until we are

	ldx #$f0
; save $9400-$94f0
@savecolor:
	lda $9400-1,x
	sta @colorsave-1,x
	dex
	bne @savecolor
	rts
.endproc
