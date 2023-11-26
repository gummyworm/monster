.include "beep.inc"
.include "finalex.inc"
.include "macros.inc"


.ifdef PAL
LINES           = 312
CYCLES_PER_LINE = 71
.else ;NTSC
LINES           = 261
CYCLES_PER_LINE = 65
.endif
TIMER_VALUE     = LINES * CYCLES_PER_LINE - 2 ; timer value for stable raster int.

.segment "IRQ"
;******************************************************************************
; SYS_UPDATE
; This is the main IRQ for this program. It handles updating the beeper.
; It is relocated to a place where it may be called from any bank
.export __irq_sys_update
.proc __irq_sys_update
	lda $9c02
	pha
	lda #FINAL_BANK_MAIN
	sta $9c02

	jsr beep::update
        lda $f5
        pha
        lda $f6
        pha

        jsr $eb1e               ; scan keyboard

	; inject TAB ($09) into keyboard buffer if the CTRL key is pressed
	lda $028d		; get CTRL flag reg
	cmp $028e		; debounce
	beq :+
	and #$02		; is CTRL (TAB) pressed?
	beq :+			; if 0, no
	ldx #$09		; TAB
	jsr $ebba		; store to keyboard table

:	pla
        sta $f6
        pla
        sta $f5
	pla
	sta $9c02
	jmp $eb15
.endproc

.CODE
;******************************************************************************
; BRK
; Installs the given vector to the BRK and NMI vectors
; The virtual vector ($0334) holds the actual address of the interrupt handler.
; $0316-$0317 and $0318-$0319 are updated to a handle that
; switches back to the main RAM bank and dispatches to this vector.
.export __irq_brk
.proc __irq_brk
	stx $0334
	sty $0335
	lda #<fe3::break
	sta $0316
	sta $0318
	lda #>fe3::break
	sta $0317
	sta $0319
	rts
.endproc

;******************************************************************************
.export __irq_raster
.proc __irq_raster
        sei
	stx $0314
        sty $0315
;init the stable raster irq
        ldx #$7f
        stx $912e       ; disable/ack interrupts
        stx $912d
        stx $911e       ; disable NMI's (restore key)
@sync:
;sync with screen
:       cmp $9004       ; wait until this raster (*2)
        bne :-          ; inaccuracy is 7 clocks at here (processor is 2-9 cycles
                        ; after $9004 change
        ldy #$09
        bit $24
:       ldx $9004
        txa
        bit $24
        bit $24
        ldx #21
        dex
        bne *-1         ; spend some time (so the whole loop will be 2 raster lines)
        cmp $9004
        bcs *+2
        dey
        bne :-

; 6 cycles have passed since last $9004 change we are now on line 2*(START_LINE+9)
@timers:
; initialize the timers
        lda #$40        ; enable timer A free run on both VIAs
        sta $911b
        sta $912b
        lda #<TIMER_VALUE
        ldx #>TIMER_VALUE
        sta $9116       ; load the timer low byte latches
        sta $9126
        ldy #$06        ; delay to get effect @ right spot
        dey
        bne *-1
        bit $24
        stx $9125       ; start IRQ timer A
                        ; 6560-101: 65 cycles from $9004 change
                        ; 6561-101: 77 cycles from $9004 change
        ldy #10         ; spend some time (1+5*9+4 = 55 cycles)
        dey             ; before starting the reference timer
        bne *-1
        stx $9115       ; start the reference timer
@pointers:
        lda #$c0
        sta $912e       ; enable timer A underflow interrupts
        cli
        rts
.endproc

