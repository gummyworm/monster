.include "beep.inc"
.include "config.inc"
.include "key.inc"
.include "finalex.inc"
.include "macros.inc"
.include "memory.inc"

IRQ_START_LINE = $0f

.ifdef PAL
LINES           = 312
CYCLES_PER_LINE = 71
.else ;NTSC
LINES           = 261
CYCLES_PER_LINE = 65
.endif
TIMER_VALUE     = LINES * CYCLES_PER_LINE - 2 ; timer value for stable raster int.
CYCLES_PER_ROW  = 8 * (CYCLES_PER_LINE - 2) - 25

NUM_ROWS = 24

;******************************************************************************
.BSS
rowcnt: .byte 0

.segment "IRQ"
;******************************************************************************
; SYS_UPDATE
; This is the main IRQ for this program. It handles updating the beeper.
; It is relocated to a place where it may be called from any bank
.proc sys_update
	lda $9c02
	sta @savebank
	lda #FINAL_BANK_MAIN
	sta $9c02
	jsr stable_handler
@savebank=*+1
	lda #$00
	sta $9c02
	jmp $eb15
.endproc

.proc row_interrupt
	lda $9c02
	sta @savebank
	lda #FINAL_BANK_MAIN
	sta $9c02
	jsr row_handler
@savebank=*+1
	lda #$00
	sta $9c02
	jmp $eb15
.endproc

;******************************************************************************
.CODE

;******************************************************************************
; IRQ RASTER
; Syncs to the given scanline and sets up an IRQ that will trigger whenever
; that location is reached.
; IN:
;  - .A:  the scanline to sync to
;  - .XY: the address of the IRQ handler
.export __irq_raster
.proc __irq_raster
        sei
	ldxy #sys_update
	stxy $0314

	ldy #IRQ_START_LINE

	lda #<TIMER_VALUE
	sta $9124
@i0:	cpy $9004
	bne @i0
	iny
	iny
@i1:	cpy $9004
	bne @i1
	jsr @i6
	iny
	cpy $9004
	beq @i2
	nop
	nop
@i2:	jsr @i6
	nop
	iny
	cpy $9004
	beq @i3
	bit $24
@i3:	jsr @i6
	nop
	iny
	cpy $9004
	bne @i4
@i4:	ldx #$06	; position
@i5:	dex
	bne @i5

.ifndef PAL
	nop
	nop
.endif
	lda #>TIMER_VALUE
	sta $9125
	cli
	rts

@i6:	ldx #$17	; delay
@i7:	dex
	bne @i7
	nop
	rts
.endproc

;******************************************************************************
; STABLE_HANDLER
.export stable_handler
.proc stable_handler
	cld
	sec
.ifdef PAL
	lda #$58
.else
	; base phase value minus all instructions
	; executed in handler before this
	lda #$15-4-3-2-4-6
.endif
	sbc $9124
	cmp #$0a
	bcc @s0
	rts

@s0:	sta @s1+1
@s1:	bcc @s1
	lda #$a9
	lda #$a9
	lda #$a9
	lda #$a9
	lda #$a5
	nop

	; set up sub-interrupt that executes every character row to draw
	; breakpoints on any line that has one
	; we only do this when at least one row has color to save cycles in the
	; average case (no color)
	lda mem::coloron
	beq @cont
	lda #$80|$20
	sta $912e		; enable T2 interrupts
	ldxy #row_interrupt
	stxy $0314
	ldxy #105
	stx $9128
	sty $9129
	cli

@cont:	inc $900f
	dec $900f
	lda #$00
	sta rowcnt

	; save $f5-$f6
        lda $f5
	sta @savef5
        lda $f6
	sta @savef6

	jsr $eb1e               ; scan keyboard
	;jsr beep::update

	; inject TAB ($09) into keyboard buffer if the CTRL key is pressed
	lda $028d		; get CTRL flag reg
	cmp $028e		; debounce
	beq @keydone
	and #$02		; is CTRL (TAB) pressed?
	beq @keydone		; if 0, no
	ldx #$09		; TAB
	jsr $ebba		; store to keyboard table

@keydone:
@savef5=*+1
	lda #$00
        sta $f5
@savef6=*+1
	lda #$00
        sta $f6
	rts
.endproc

;******************************************************************************
; ROW HANDLER
; Handles the sub-interrupt responsible for drawing breakpoints
.proc row_handler
	cld
	sec
.ifdef PAL
	lda #$58
.else
	;lda #$4+3+4+2+4+6+2+2+8+$10+ 24
	lda #$be
.endif
	sbc $9128	; add signed overflow value from timer
	cmp #$0a
	bcc @s0
	rts

@s0:	sta @s1+1
@s1:	bcc @s1
	lda #$a9
	lda #$a9
	lda #$a9
	lda #$a9
	lda #$a5
	nop

@done:	; reload timer
	ldxy #CYCLES_PER_ROW-52-10
	stxy $9128

	ldx rowcnt
	lda mem::rowcolors,x
	sta $900f

	cpx #NUM_ROWS-1
	inc rowcnt
	bcs :+
	rts

:	; reinstall main IRQ handler
	lda #$00
	sta rowcnt
	ldxy #sys_update
	stxy $0314
	lda #$00|$20		; disable T2 interrupts
	sta $912e
	rts
.endproc
