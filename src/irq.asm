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
.proc sys_update
	cld
	sec
.ifdef PAL
	lda #$58
.else
	lda #$15
.endif
	sbc $9124
	cmp #$0a
	bcc @s0
	jmp $eabf
@s0:	sta @s1+1
@s1:	bcc @s1
	lda #$a9
	lda #$a9
	lda #$a9
	lda #$a9
	lda #$a5
	nop
	lda $9c02
	pha
	lda #FINAL_BANK_MAIN
	sta $9c02
	jsr stable_handler
	pla
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

	tay		; the line to sync to

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
.proc stable_handler
	inc $900f
	dec $900f

	;jsr beep::update
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
	rts
.endproc
