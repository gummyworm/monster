.include "../asm.inc"
.include "../config.inc"
.include "../copybuff.inc"
.include "../debug.inc"
.include "../debuginfo.inc"
.include "../draw.inc"
.include "../edit.inc"
.include "../irq.inc"
.include "../labels.inc"
.include "../macros.inc"
.include "../memory.inc"
.include "../monitor.inc"
.include "../settings.inc"
.include "../screen.inc"
.include "../source.inc"
.include "../vmem.inc"
.include "../zeropage.inc"

.import __BSS_LOAD__
.import __BSS_SIZE__

.segment "SETUP"
;*******************************************************************************
; BASIC header: SYS 2061
.word @head
@head: .word @next
.word .version
.byte $9e
.asciiz "2061"
@next: .word 0
start:
	sei

;--------------------------------------
; zero the BSS segment
	ldxy #__BSS_LOAD__
	stxy r0

	lda #$00
	ldy #$00
@zerobss:
	sta (r0),y
	iny
	bne @zerobss
	inc r0+1
	ldx r0+1
	cpx #>(__BSS_LOAD__+__BSS_SIZE__-1)
	bne @zerobss

	lda #$00
	sta r0

	ldy #<(__BSS_LOAD__+__BSS_SIZE__-1)
	beq @zero_bss_done
@zerobss_last_page:
	sta (r0),y
	dey
	bne @zerobss_last_page
	sta (r0),y		; last byte

@zero_bss_done:
	jmp *
        jsr irq::on

	lda #<start
	sta $0316		; BRK
	lda #>start
	sta $0317		; BRK
	cli
	jmp *

	; initialize the status row reverse
	lda #DEFAULT_RVS
	ldx #23
	jsr draw::hline

	jsr asm::reset
	jmp *
	jsr src::new

	; initialize bitmap
	jmp *
	jsr scr::init
	jmp *
	jsr edit::clear
	jmp *

	jsr dbgi::initonce
	jsr asm::reset
	jmp *
	jsr buff::clear		; clear copy buffer

	jmp *
	jsr mon::init

	jmp dbg::clrstate
